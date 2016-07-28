-module(hg_invoice).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([init/3]).
-export([process_signal/3]).
-export([process_call/3]).

-export([map_event/1]).

%%
-record(st, {
    invoice :: invoice(),
    payments = [] :: [payment()],
    stage = idling :: stage()
}).

-type st() :: #st{}.
%%

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('Create', {UserInfo, InvoiceParams}, Context0, _Opts) ->
    {InvoiceID, Context} = hg_machine:start(?MODULE, {InvoiceParams, UserInfo}, opts(Context0)),
    {{ok, InvoiceID}, Context};

handle_function('Get', {_UserInfo, InvoiceID}, Context0, _Opts) ->
    {St, Context} = get_state(InvoiceID, Context0),
    InvoiceState = get_invoice_state(St),
    {{ok, InvoiceState}, Context};

handle_function('GetEvents', {_UserInfo, InvoiceID, Range}, Context0, _Opts) ->
    {History, Context} = get_public_history(InvoiceID, Range, Context0),
    {{ok, History}, Context};

handle_function('StartPayment', {UserInfo, InvoiceID, PaymentParams}, Context0, _Opts) ->
    {PaymentID, Context} = hg_machine:call(InvoiceID, {start_payment, PaymentParams, UserInfo}, opts(Context0)),
    {{ok, PaymentID}, Context};

handle_function('GetPayment', {_UserInfo, PaymentID}, Context0, _Opts) ->
    {St, Context} = get_state(deduce_invoice_id(PaymentID), Context0),
    case get_payment(PaymentID, St) of
        Payment = #domain_InvoicePayment{} ->
            {{ok, Payment}, Context};
        false ->
            throw({#payproc_InvoicePaymentNotFound{}, Context})
    end;

handle_function('Fulfill', {UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    {Result, Context} = hg_machine:call(InvoiceID, {fulfill, Reason, UserInfo}, opts(Context0)),
    {{ok, Result}, Context};

handle_function('Rescind', {UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    {Result, Context} = hg_machine:call(InvoiceID, {rescind, Reason, UserInfo}, opts(Context0)),
    {{ok, Result}, Context}.

%%

get_history(InvoiceID, Context) ->
    hg_machine:get_history(InvoiceID, opts(Context)).

get_history(InvoiceID, AfterID, Limit, Context) ->
    hg_machine:get_history(InvoiceID, AfterID, Limit, opts(Context)).

get_state(InvoiceID, Context0) ->
    {History, Context} = get_history(InvoiceID, Context0),
    St = collapse_history(History),
    {St, Context}.

get_public_history(InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}, Context) ->
    get_public_history(InvoiceID, AfterID, Limit, Context).

get_public_history(InvoiceID, AfterID, undefined, Context0) ->
    {History0, Context} = get_history(InvoiceID, AfterID, undefined, Context0),
    {_LastID, History} = map_history(History0),
    {History, Context};

get_public_history(_InvoiceID, _AfterID, 0, Context) ->
    {[], Context};
get_public_history(InvoiceID, AfterID, N, Context0) ->
    {History0, Context1} = get_history(InvoiceID, AfterID, N, Context0),
    {LastID, History} = map_history(History0),
    case length(History0) of
        N when length(History) =:= N ->
            {History, Context1};
        N ->
            NextRange = #payproc_EventRange{'after' = LastID, limit = N - length(History)},
            {HistoryRest, Context2} = get_public_history(InvoiceID, NextRange, Context1),
            {History ++ HistoryRest, Context2};
        M when M < N ->
            {History, Context1}
    end.

map_history(History) ->
    map_history(History, undefined, []).

map_history([], LastID, Evs) ->
    {LastID, lists:reverse(Evs)};
map_history([Ev0 = {ID, _, _, _, _} | Rest], _, Evs) ->
    case map_event(Ev0) of
        Ev when Ev /= undefined ->
            map_history(Rest, ID, [Ev | Evs]);
        undefined ->
            map_history(Rest, ID, Evs)
    end.

opts(Context) ->
    #{client_context => Context}.

%%

-type invoice() :: hg_domain_thrift:'Invoice'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().
-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type payment() :: hg_domain_thrift:'InvoicePayment'().
-type payment_params() :: hg_payment_processing_thrift:'InvoicePaymentParams'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: undefined | binary().

-type stage() ::
    idling |
    {processing_payment, payment_id(), payment_st()}.

-type ev() ::
    hg_payment_processing_thrift:'EventPayload'() |
    {internal_event, payment_state_changed()}.

-type payment_state_changed() :: {payment_state_changed, payment_id(), payment_st()}.

-include("events.hrl").

-define(internal_ev(Body), {internal_event, Body}).

-define(invalid_invoice_status(Invoice),
    #payproc_InvalidInvoiceStatus{status = Invoice#domain_Invoice.status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec map_event(hg_machine:event(ev())) ->
    hg_payment_processing_thrift:'Event'() | undefined.

map_event({ID, Source, Dt, Seq, Ev = ?invoice_ev(_)}) ->
    #payproc_Event{
        id         = ID,
        created_at = Dt,
        source     = {invoice, Source},
        sequence   = Seq,
        payload    = Ev
    };
map_event(_) ->
    undefined.

%%

-spec init(invoice_id(), {invoice_params(), user_info()}, hg_machine:context()) ->
    {{ok, hg_machine:result(ev())}, woody_client:context()}.

init(ID, {InvoiceParams, _UserInfo}, Context) ->
    Invoice = create_invoice(ID, InvoiceParams),
    Event = ?invoice_ev(?invoice_created(Invoice)),
    {ok(Event, set_invoice_timer(Invoice)), Context}.

-spec process_signal(hg_machine:signal(), hg_machine:history(ev()), hg_machine:context()) ->
    {{ok, hg_machine:result(ev())}, woody_client:context()}.

process_signal(timeout, History, Context) ->
    St = #st{invoice = Invoice, stage = Stage} = collapse_history(History),
    Status = get_invoice_status(Invoice),
    case Stage of
        {processing_payment, PaymentID, PaymentState} ->
            % there's a payment pending
            process_payment(PaymentID, PaymentState, St, Context);
        idling when Status == unpaid ->
            % invoice is expired
            process_expiration(St, Context);
        _ ->
            ok()
    end;

process_signal({repair, _}, History, Context) ->
    #st{invoice = Invoice} = collapse_history(History),
    {ok([], set_invoice_timer(Invoice)), Context}.

process_expiration(#st{invoice = Invoice}, Context) ->
    {ok, Event} = cancel_invoice(overdue, Invoice),
    {ok(Event), Context}.

process_payment(PaymentID, PaymentState0, St = #st{invoice = Invoice}, Context0) ->
    % FIXME: code looks shitty, destined to be in payment submachine
    Payment = get_payment(PaymentID, St),
    case hg_invoice_payment:process(Payment, Invoice, PaymentState0, Context0) of
        % TODO: check proxy contracts
        %       binding different trx ids is not allowed
        %       empty action is questionable to allow
        {{ok, Trx}, Context} ->
            % payment finished successfully
            Events = [
                ?invoice_ev(?payment_ev(?payment_status_changed(PaymentID, ?succeeded()))),
                ?invoice_ev(?invoice_status_changed(?paid()))
            ],
            {ok(construct_payment_events(Payment, Trx, Events)), Context};
        {{{error, Error = #domain_OperationError{}}, Trx}, Context} ->
            % payment finished with error
            Event = {invoice_payment_event, ?payment_status_changed(PaymentID, ?failed(Error))},
            {ok(construct_payment_events(Payment, Trx, [Event])), Context};
        {{{next, Action, PaymentState}, Trx}, Context} ->
            % payment progressing yet
            Event = ?internal_ev({payment_state_changed, PaymentID, PaymentState}),
            {ok(construct_payment_events(Payment, Trx, [Event]), Action), Context}
    end.

construct_payment_events(#domain_InvoicePayment{trx = Trx}, #domain_TransactionInfo{} = Trx, Events) ->
    Events;
construct_payment_events(#domain_InvoicePayment{} = Payment, #domain_TransactionInfo{} = Trx, Events) ->
    [?invoice_ev(?payment_ev(?payment_bound(get_payment_id(Payment), Trx))) | Events];
construct_payment_events(#domain_InvoicePayment{trx = Trx}, Trx = undefined, Events) ->
    Events.

-type call() ::
    {start_payment, payment_params(), user_info()} |
    {fulfill, binary(), user_info()} |
    {rescind, binary(), user_info()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev()), woody_client:context()) ->
    {{ok, response(), hg_machine:result(ev())}, woody_client:context()}.

process_call({start_payment, PaymentParams, _UserInfo}, History, Context) ->
    #st{invoice = Invoice, stage = Stage} = collapse_history(History),
    Status = get_invoice_status(Invoice),
    case Stage of
        idling when Status == unpaid ->
            Payment = create_payment(PaymentParams, Invoice),
            PaymentID = get_payment_id(Payment),
            Events = [
                ?invoice_ev(?payment_ev(?payment_started(Payment))),
                ?internal_ev({payment_state_changed, PaymentID, undefined})
            ],
            {respond({ok, PaymentID}, Events, hg_machine_action:instant()), Context};
        {processing_payment, PaymentID, _} ->
            {raise(?payment_pending(PaymentID)), Context};
        _ ->
            {raise(?invalid_invoice_status(Invoice)), Context}
    end;

process_call({fulfill, Reason, _UserInfo}, History, Context) ->
    #st{invoice = Invoice} = collapse_history(History),
    case fulfill_invoice(Reason, Invoice) of
        {ok, Event} ->
            {respond(ok, Event, set_invoice_timer(Invoice)), Context};
        {error, Exception} ->
            {raise(Exception, set_invoice_timer(Invoice)), Context}
    end;

process_call({rescind, Reason, _UserInfo}, History, Context) ->
    #st{invoice = Invoice} = collapse_history(History),
    case cancel_invoice({rescinded, Reason}, Invoice) of
        {ok, Event} ->
            {respond(ok, Event, set_invoice_timer(Invoice)), Context};
        {error, Exception} ->
            {raise(Exception, set_invoice_timer(Invoice)), Context}
    end.

set_invoice_timer(#domain_Invoice{status = ?unpaid(), due = Due}) when Due /= undefined ->
    hg_machine_action:set_deadline(Due);
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
    wrap_event_list([Event]);
wrap_event_list(Events) when is_list(Events) ->
    Events.

%%

create_invoice(ID, V = #payproc_InvoiceParams{}) ->
    Revision = hg_domain:head(),
    #domain_Invoice{
        id              = ID,
        created_at      = get_datetime_utc(),
        status          = ?unpaid(),
        domain_revision = Revision,
        due             = V#payproc_InvoiceParams.due,
        product         = V#payproc_InvoiceParams.product,
        description     = V#payproc_InvoiceParams.description,
        context         = V#payproc_InvoiceParams.context,
        cost            = #domain_Funds{
            amount          = V#payproc_InvoiceParams.amount,
            currency        = hg_domain:get(Revision, V#payproc_InvoiceParams.currency)
        }
    }.

create_payment(V = #payproc_InvoicePaymentParams{}, Invoice) ->
    #domain_InvoicePayment{
        id           = create_payment_id(Invoice),
        created_at   = get_datetime_utc(),
        status       = ?pending(),
        payer        = V#payproc_InvoicePaymentParams.payer,
        payment_tool = V#payproc_InvoicePaymentParams.payment_tool,
        session      = V#payproc_InvoicePaymentParams.session
    }.

create_payment_id(Invoice = #domain_Invoice{}) ->
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

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_invoice_status(#domain_Invoice{status = {Status, _}}) ->
    Status.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

cancel_invoice(Reason, #domain_Invoice{status = ?unpaid()}) ->
    {ok, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(Reason))))};
cancel_invoice(_Reason, Invoice) ->
    {error, ?invalid_invoice_status(Invoice)}.

fulfill_invoice(Reason, #domain_Invoice{status = ?paid()}) ->
    {ok, ?invoice_ev(?invoice_status_changed(?fulfilled(format_reason(Reason))))};
fulfill_invoice(_Reason, Invoice) ->
    {error, ?invalid_invoice_status(Invoice)}.

%%

-spec collapse_history([ev()]) -> st().

collapse_history(History) ->
    lists:foldl(fun ({_ID, _, _, _, Ev}, St) -> merge_history(Ev, St) end, #st{}, History).

merge_history(?invoice_ev(Event), St) ->
    merge_invoice_event(Event, St);
merge_history(?internal_ev(Event), St) ->
    merge_internal_event(Event, St).

merge_invoice_event(?invoice_created(Invoice), St) ->
    St#st{invoice = Invoice};
merge_invoice_event(?invoice_status_changed(Status), St = #st{invoice = I}) ->
    St#st{invoice = I#domain_Invoice{status = Status}};
merge_invoice_event(?payment_ev(Event), St) ->
    merge_payment_event(Event, St).

merge_payment_event(?payment_started(Payment), St) ->
    set_payment(Payment, St);
merge_payment_event(?payment_bound(PaymentID, Trx), St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#domain_InvoicePayment{trx = Trx}, St);
merge_payment_event(?payment_status_changed(PaymentID, Status), St) ->
    Payment = get_payment(PaymentID, St),
    set_payment(Payment#domain_InvoicePayment{status = Status}, set_stage(idling, St)).

merge_internal_event({payment_state_changed, PaymentID, State}, St) ->
    set_stage({processing_payment, PaymentID, State}, St).

set_stage(Stage, St) ->
    St#st{stage = Stage}.

get_payment(PaymentID, St) ->
    lists:keyfind(PaymentID, #domain_InvoicePayment.id, St#st.payments).
set_payment(Payment, St) ->
    PaymentID = get_payment_id(Payment),
    St#st{payments = lists:keystore(PaymentID, #domain_InvoicePayment.id, St#st.payments, Payment)}.

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_InvoiceState{invoice = Invoice, payments = Payments}.

%%

%% TODO: fix this dirty hack
format_reason({Pre, V}) ->
    genlib:format("~s: ~s", [Pre, genlib:to_binary(V)]);
format_reason(V) ->
    genlib:to_binary(V).

get_datetime_utc() ->
    genlib_format:format_datetime_iso8601(calendar:universal_time()).
