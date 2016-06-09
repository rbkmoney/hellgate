-module(hg_invoice).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%%

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {ok, term()} | no_return().

handle_function('Create', {UserInfo, InvoiceParams}, Context, _Opts) ->
    InvoiceID = hg_machine:start(?MODULE, {InvoiceParams, UserInfo}, opts(Context)),
    {ok, InvoiceID};

handle_function('Get', {UserInfo, InvoiceID}, Context, _Opts) ->
    InvoiceState = get_invoice_state(get_state(UserInfo, InvoiceID, opts(Context))),
    {ok, InvoiceState};

handle_function('GetEvents', {UserInfo, InvoiceID, Range}, Context, _Opts) ->
    #'EventRange'{'after' = AfterID, limit = Limit} = Range,
    History = get_history(UserInfo, InvoiceID, opts(Context)),
    {ok, map_events(select_range(AfterID, Limit, map_history(History)))};

handle_function('StartPayment', {UserInfo, InvoiceID, PaymentParams}, Context, _Opts) ->
    Call = {start_payment, PaymentParams, UserInfo},
    PaymentID = hg_machine:call(?MODULE, InvoiceID, Call, opts(Context)),
    {ok, PaymentID};

handle_function('GetPayment', {UserInfo, PaymentID}, Context, _Opts) ->
    St = get_state(UserInfo, deduce_invoice_id(PaymentID), opts(Context)),
    case get_payment(PaymentID, St) of
        Payment = #'InvoicePayment'{} ->
            {ok, Payment};
        false ->
            throw(payment_not_found())
    end;

handle_function('Fulfill', {UserInfo, InvoiceID, Reason}, Context, _Opts) ->
    Result = hg_machine:call(?MODULE, InvoiceID, {fulfill, Reason, UserInfo}, opts(Context)),
    {ok, Result};

handle_function('Void', {UserInfo, InvoiceID, Reason}, Context, _Opts) ->
    Result = hg_machine:call(?MODULE, InvoiceID, {void, Reason, UserInfo}, opts(Context)),
    {ok, Result}.

opts(Context) ->
    #{context => Context}.

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.

%%

get_history(_UserInfo, InvoiceID, Opts) ->
    hg_machine:get_history(?MODULE, InvoiceID, Opts).

get_state(UserInfo, InvoiceID, Opts) ->
    collapse_history(get_history(UserInfo, InvoiceID, Opts)).

map_events(Evs) ->
    [construct_external_event(ID, Ev) || {ID, Ev} <- Evs].

construct_external_event(ID, Ev) ->
    #'Event'{id = ID, ev = wrap_external_event(Ev)}.

wrap_external_event(Ev = #'InvoiceStatusChanged'{}) ->
    {invoice_status_changed, Ev};
wrap_external_event(Ev = #'InvoicePaymentStatusChanged'{}) ->
    {invoice_payment_status_changed, Ev}.

%%

-type invoice() :: hg_domain_thrift:'Invoice'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().
-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type payment() :: hg_domain_thrift:'InvoicePayment'().
-type payment_params() :: hg_payment_processing_thrift:'InvoicePaymentParams'().
-type invoice_status() :: hg_domain_thrift:'InvoiceStatus'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: hg_invoice_payment:st().
-type payment_trx() :: hg_domain_thrift:'TransactionInfo'().
-type detail() :: binary().
-type error() :: hg_domain_thrift:'OperationError'().

-type stage() ::
    idling |
    {processing_payment, payment_id(), payment_st()}.

-record(st, {
    invoice :: invoice(),
    payments = [] :: [payment()],
    stage = idling :: stage()
}).

-type st() :: #st{}.

-type ev() ::
    {stage_changed, stage()} |
    {invoice_created, invoice()} |
    {invoice_status_changed, invoice_status(), detail()} |
    {payment_created, payment()} |
    {payment_state_changed, payment_id(), payment_st()} |
    {payment_bound, payment_id(), payment_trx() | undefined} |
    {payment_succeeded, payment_id()} |
    {payment_failed, payment_id(), error()}.

-spec init(invoice_id(), {invoice_params(), user_info()}) ->
    {ok, hg_machine:result([ev()])}.

init(ID, {InvoiceParams, _UserInfo}) ->
    Invoice = create_invoice(ID, InvoiceParams),
    Event = {invoice_created, Invoice},
    ok(Event, set_invoice_timer(Invoice)).

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    {ok, hg_machine:result([ev()])}.

process_signal(timeout, History) ->
    St = #st{invoice = Invoice, stage = Stage} = collapse_history(History),
    Status = get_invoice_status(Invoice),
    case Stage of
        {processing_payment, PaymentID, PaymentState} ->
            % there's a payment pending
            process_payment(PaymentID, PaymentState, St);
        idling when Status == unpaid ->
            % invoice is expired
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

process_payment(PaymentID, PaymentState0, St = #st{invoice = Invoice}) ->
    % FIXME: code looks shitty, destined to be in payment submachine
    Payment = get_payment(PaymentID, St),
    case hg_invoice_payment:process(Payment, Invoice, PaymentState0) of
        % TODO: check proxy contracts
        %       binding different trx ids is not allowed
        %       empty action is questionable to allow
        {ok, Trx} ->
            % payment finished successfully
            Events = [{payment_succeeded, PaymentID}, {invoice_status_changed, paid, <<>>}],
            ok(construct_payment_events(PaymentID, Trx, Events));
        {{error, Error = #'OperationError'{}}, Trx} ->
            % payment finished with error
            Event = {payment_failed, PaymentID, Error},
            ok(construct_payment_events(PaymentID, Trx, Event));
        {{next, Action, PaymentState}, Trx} ->
            % payment progressing yet
            Event = {payment_state_changed, PaymentID, PaymentState},
            ok(construct_payment_events(PaymentID, Trx, Event), Action)
    end.

construct_payment_events(PaymentID, Trx = #'TransactionInfo'{}, Events) ->
    [{payment_bound, PaymentID, Trx} | wrap_event_list(Events)];
construct_payment_events(_PaymentID, undefined, Events) ->
    Events.

-type call() ::
    {start_payment, payment_params(), user_info()} |
    {fulfill, binary(), user_info()} |
    {void, binary(), user_info()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {ok, response(), hg_machine:result([ev()])}.

process_call({start_payment, PaymentParams, _UserInfo}, History) ->
    #st{invoice = Invoice, stage = Stage} = collapse_history(History),
    Status = get_invoice_status(Invoice),
    case Stage of
        idling when Status == unpaid ->
            Payment = create_payment(PaymentParams, Invoice),
            PaymentID = get_payment_id(Payment),
            Events = [
                {payment_created, Payment},
                {payment_state_changed, PaymentID, undefined}
            ],
            respond({ok, PaymentID}, Events, hg_machine_action:instant());
        {processing_payment, PaymentID, _} ->
            raise(payment_pending(PaymentID));
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
    hg_machine_action:set_timeout(max(Ts - genlib_time:unow(), 0));
set_invoice_timer(_Invoice) ->
    hg_machine_action:new().

ok() ->
    ok([]).
ok(Event) ->
    ok(Event, hg_machine_action:new()).
ok(Event, Action) ->
    {ok, {wrap_event_list(Event), Action}}.

respond(Response, Event, Action) ->
    {ok, Response, {wrap_event_list(Event), Action}}.

raise(Exception) ->
    raise(Exception, hg_machine_action:new()).
raise(Exception, Action) ->
    {ok, {exception, Exception}, {[], Action}}.

wrap_event_list(Event) when is_tuple(Event) ->
    [Event];
wrap_event_list(Events) when is_list(Events) ->
    Events.

%%

create_invoice(ID, V = #'InvoiceParams'{}) ->
    Revision = hg_domain:head(),
    #'Invoice'{
        id              = ID,
        created_at      = get_datetime_utc(),
        status          = unpaid,
        domain_revision = Revision,
        due             = V#'InvoiceParams'.due,
        product         = V#'InvoiceParams'.product,
        description     = V#'InvoiceParams'.description,
        context         = V#'InvoiceParams'.context,
        cost            = #'Funds'{
        amount              = V#'InvoiceParams'.amount,
        currency            = hg_domain:get(Revision, V#'InvoiceParams'.currency)
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
payment_pending(PaymentID) ->
    #'InvoicePaymentPending'{id = PaymentID}.

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
merge_history({payment_state_changed, PaymentID, PaymentState}, St) ->
    set_stage({processing_payment, PaymentID, PaymentState}, St);
merge_history({payment_bound, PaymentID, Trx}, St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#'InvoicePayment'{trx = Trx}, St);
merge_history({payment_succeeded, PaymentID}, St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#'InvoicePayment'{status = succeeded}, set_stage(idling, St));
merge_history({payment_failed, PaymentID, Error}, St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#'InvoicePayment'{status = failed, err = Error}, St).

set_stage(Stage, St) ->
    St#st{stage = Stage}.

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
    [#'InvoicePaymentStatusChanged'{payment = get_payment(PaymentID, St)}];
map_history(_Event, _St) ->
    [].

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
