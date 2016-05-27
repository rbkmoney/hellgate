-module(hg_invoice).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("hg_duration.hrl").

%% Public

-export([create/3]).
-export([get/3]).
-export([get_events/4]).
-export([start_payment/4]).
-export([get_payment/3]).
-export([fulfill/4]).
-export([void/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%%

create(UserInfo, InvoiceParams, Opts) ->
    hg_machine:start(?MODULE, {InvoiceParams, UserInfo}, Opts).

get(UserInfo, InvoiceID, Opts) ->
    get_invoice_state(get_state(UserInfo, InvoiceID, Opts)).

get_events(UserInfo, InvoiceID, #'EventRange'{'after' = AfterID, limit = Limit}, Opts) ->
    History = get_history(UserInfo, InvoiceID, Opts),
    select_range(AfterID, Limit, map_history(History)).

start_payment(UserInfo, InvoiceID, PaymentParams, Opts) ->
    hg_machine:call(?MODULE, InvoiceID, {start_payment, PaymentParams, UserInfo}, Opts).

get_payment(UserInfo, PaymentID, Opts) ->
    get_payment(PaymentID, get_state(UserInfo, deduce_invoice_id(PaymentID), Opts)).

fulfill(UserInfo, InvoiceID, Reason, Opts) ->
    hg_machine:call(?MODULE, InvoiceID, {fulfill, Reason, UserInfo}, Opts).

void(UserInfo, InvoiceID, Reason, Opts) ->
    hg_machine:call(?MODULE, InvoiceID, {void, Reason, UserInfo}, Opts).

get_history(_UserInfo, InvoiceID, Opts) ->
    hg_machine:get_history(?MODULE, InvoiceID, Opts).

get_state(UserInfo, InvoiceID, Opts) ->
    collapse_history(get_history(UserInfo, InvoiceID, Opts)).

%%

-type invoice() :: hg_domain_thrift:'Invoice'().
-type payment() :: hg_domain_thrift:'InvoicePayment'().
-type invoice_status() :: hg_domain_thrift:'InvoiceStatus'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type detail() :: binary().
-type error() :: hg_domain_thrift:'OperationError'().

-record(st, {
    invoice :: invoice(),
    payments = [] :: [payment()]
}).

-type st() :: #st{}.

-type ev() ::
    {invoice_created, invoice()} |
    {invoice_status_changed, invoice_status(), detail()} |
    {payment_created, payment()} |
    {payment_succeeded, payment_id()} |
    {payment_failed, payment_id(), error()}.

init(ID, {InvoiceParams, _UserInfo}) ->
    Invoice = create_invoice(ID, InvoiceParams),
    Event = {invoice_created, Invoice},
    ok(Event, set_invoice_timer(Invoice)).

process_signal(timeout, History) ->
    St = #st{invoice = Invoice, payments = Payments} = collapse_history(History),
    case {get_invoice_status(Invoice), Payments} of
        {unpaid, []} ->
            process_expiration(St);
        _ ->
            ok()
    end;

process_signal({repair, _}, History) ->
    #st{invoice = Invoice} = collapse_history(History),
    ok([], set_invoice_timer(Invoice)).

process_expiration(#st{invoice = Invoice}) ->
    {ok, Event} = cancel_invoice(overdue, Invoice),
    ok(Event).

process_call({start_payment, PaymentParams, _UserInfo}, History) ->
    #st{invoice = Invoice, payments = Payments} = collapse_history(History),
    case {get_invoice_status(Invoice), Payments} of
        {unpaid, []} ->
            Payment = create_payment(PaymentParams, Invoice),
            Event = {payment_created, Payment},
            respond({ok, get_payment_id(Payment)}, Event);
        {unpaid, [Payment]} ->
            raise(payment_pending(Payment));
        _ ->
            raise(invalid_invoice_status(Invoice))
    end;

process_call({fulfill, Reason, _UserInfo}, History) ->
    #st{invoice = Invoice} = collapse_history(History),
    case fulfill_invoice(Reason, Invoice) of
        {ok, Event} ->
            respond(ok, Event, set_invoice_timer(Invoice));
        {error, Exception} ->
            raise(Exception, set_invoice_timer(Invoice))
    end;

process_call({void, Reason, _UserInfo}, History) ->
    #st{invoice = Invoice} = collapse_history(History),
    case cancel_invoice({void, Reason}, Invoice) of
        {ok, Event} ->
            respond(ok, Event, set_invoice_timer(Invoice));
        {error, Exception} ->
            raise(Exception, set_invoice_timer(Invoice))
    end.

set_invoice_timer(#'Invoice'{status = unpaid, due = Due}) when Due /= undefined ->
    Ts = genlib_time:daytime_to_unixtime(genlib_format:parse_datetime_iso8601(Due)),
    hg_action:set_timeout(max(Ts - genlib_time:unow(), 0));
set_invoice_timer(_Invoice) ->
    hg_action:new().

ok() ->
    ok([]).
ok(Event) ->
    ok(Event, hg_action:new()).
ok(Event, Action) ->
    {ok, {wrap_event_list(Event), Action}}.

respond(Response, Event) ->
    respond(Response, Event, hg_action:new()).
respond(Response, Event, Action) ->
    {ok, Response, {wrap_event_list(Event), Action}}.

raise(Exception) ->
    raise(Exception, hg_action:new()).
raise(Exception, Action) ->
    {ok, {exception, Exception}, {[], Action}}.

wrap_event_list(Event) when is_tuple(Event) ->
    [Event];
wrap_event_list(Events) when is_list(Events) ->
    Events.

%%

create_invoice(ID, V = #'InvoiceParams'{}) ->
    Version = hg_domain:head(),
    #'Invoice'{
        id          = ID,
        created_at  = get_datetime_utc(),
        status      = unpaid,
        due         = V#'InvoiceParams'.due,
        product     = V#'InvoiceParams'.product,
        description = V#'InvoiceParams'.description,
        context     = V#'InvoiceParams'.context,
        cost        = #'Funds'{
            amount      = V#'InvoiceParams'.amount,
            currency    = hg_domain:get(Version, V#'InvoiceParams'.currency)
        }
    }.

create_payment(V = #'InvoicePaymentParams'{}, Invoice) ->
    #'InvoicePayment'{
        id           = create_payment_id(Invoice),
        created_at   = get_datetime_utc(),
        status       = pending,
        payer        = V#'InvoicePaymentParams'.payer,
        payment_tool = V#'InvoicePaymentParams'.payment_tool,
        session      = V#'InvoicePaymentParams'.session
    }.

create_payment_id(Invoice = #'Invoice'{}) ->
    create_payment_id(get_invoice_id(Invoice));
create_payment_id(InvoiceID) ->
    <<InvoiceID/binary, ":", "0">>.

deduce_invoice_id(PaymentID) ->
    case binary:split(PaymentID, <<":">>) of
        [InvoiceID, _] ->
            InvoiceID;
        _ ->
            <<>>
    end.

get_invoice_id(#'Invoice'{id = ID}) ->
    ID.

get_invoice_status(#'Invoice'{status = Status}) ->
    Status.

get_payment_id(#'InvoicePayment'{id = ID}) ->
    ID.

cancel_invoice(Reason, #'Invoice'{status = unpaid}) ->
    {ok, {invoice_status_changed, cancelled, format_reason(Reason)}};
cancel_invoice(_Reason, Invoice) ->
    {error, invalid_invoice_status(Invoice)}.

fulfill_invoice(Reason, #'Invoice'{status = paid}) ->
    {ok, {invoice_status_changed, fulfilled, format_reason(Reason)}};
fulfill_invoice(_Reason, Invoice) ->
    {error, invalid_invoice_status(Invoice)}.

invalid_invoice_status(Invoice) ->
    #'InvalidInvoiceStatus'{status = get_invoice_status(Invoice)}.
payment_not_found() ->
    #'InvoicePaymentNotFound'{}.
payment_pending(Payment) ->
    #'InvoicePaymentPending'{id = get_payment_id(Payment)}.

%%

-spec collapse_history([ev()]) -> st().

collapse_history(History) ->
    lists:foldl(fun ({_ID, Ev}, St) -> merge_history(Ev, St) end, #st{}, History).

merge_history(Events, St) when is_list(Events) ->
    lists:foldl(fun merge_history/2, St, Events);

merge_history({invoice_created, Invoice}, St) ->
    St#st{invoice = Invoice};
merge_history({invoice_status_changed, Status, Details}, St = #st{invoice = I}) ->
    St#st{invoice = I#'Invoice'{status = Status, details = Details}};

merge_history({payment_created, Payment}, St) ->
    set_payment(Payment, St);
merge_history({payment_succeeded, PaymentID}, St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#'InvoicePayment'{status = succeeded}, St);
merge_history({payment_failed, PaymentID, Error}, St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#'InvoicePayment'{status = failed, err = Error}, St).

get_payment(PaymentID, St) ->
    lists:keyfind(PaymentID, #'InvoicePayment'.id, St#st.payments).
set_payment(Payment, St) ->
    St#st{payments = lists:keystore(get_payment_id(Payment), #'InvoicePayment'.id, St#st.payments, Payment)}.

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #'InvoiceState'{invoice = Invoice, payments = Payments}.

%%

map_history(History) ->
    lists:reverse(element(2, lists:foldl(
        fun ({ID, Evs}, {St, Acc}) -> map_history([{ID, Ev} || Ev <- Evs], St, Acc) end,
        {#st{}, []},
        History
    ))).

map_history(Evs, St, Acc) when is_list(Evs) ->
    lists:foldl(fun ({ID, Ev}, {St0, Acc0}) -> map_history(ID, Ev, St0, Acc0) end, {St, Acc}, Evs).

map_history(ID, Ev, St, Acc) ->
    St1 = merge_history(Ev, St),
    {St1, [{ID, Ev1} || Ev1 <- map_history(Ev, St1)] ++ Acc}.

map_history({invoice_created, _}, #st{invoice = Invoice}) ->
    [#'InvoiceStatusChanged'{invoice = Invoice}];
map_history({invoice_status_changed, _, _}, #st{invoice = Invoice}) ->
    [#'InvoiceStatusChanged'{invoice = Invoice}];

map_history({payment_created, Payment}, _St) ->
    [#'InvoicePaymentStatusChanged'{payment = Payment}];
map_history({payment_succeeded, PaymentID}, St) ->
    [#'InvoicePaymentStatusChanged'{payment = get_payment(PaymentID, St)}];
map_history({payment_failed, PaymentID, _}, St) ->
    [#'InvoicePaymentStatusChanged'{payment = get_payment(PaymentID, St)}].

select_range(undefined, Limit, History) ->
    select_range(Limit, History);
select_range(AfterID, Limit, History) ->
    select_range(Limit, lists:dropwhile(fun ({ID, _}) -> ID =< AfterID end, History)).

select_range(Limit, History) ->
    lists:sublist(History, Limit).

%%

%% TODO: fix this dirty hack
format_reason({Pre, V}) ->
    genlib:format("~s: ~s", [Pre, genlib:to_binary(V)]);
format_reason(V) ->
    genlib:to_binary(V).

get_datetime_utc() ->
    genlib_format:format_datetime_iso8601(calendar:universal_time()).
