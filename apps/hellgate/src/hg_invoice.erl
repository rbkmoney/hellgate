-module(hg_invoice).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-define(NS, <<"invoice">>).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/3]).
-export([process_signal/3]).
-export([process_call/3]).

-export([publish_event/2]).

%%

-record(st, {
    invoice :: invoice(),
    payments = [] :: [payment()],
    stage = idling :: stage(),
    sequence = 0 :: 0 | sequence()
}).

-type st() :: #st{}.

%%

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('Create', {UserInfo, InvoiceParams}, Context0, _Opts) ->
    {InvoiceID, Context} = start(hg_utils:unique_id(), {InvoiceParams, UserInfo}, Context0),
    {{ok, InvoiceID}, Context};

handle_function('Get', {UserInfo, InvoiceID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, InvoiceID, Context0),
    InvoiceState = get_invoice_state(St),
    {{ok, InvoiceState}, Context};

handle_function('GetEvents', {UserInfo, InvoiceID, Range}, Context0, _Opts) ->
    {History, Context} = get_public_history(UserInfo, InvoiceID, Range, Context0),
    {{ok, History}, Context};

handle_function('StartPayment', {UserInfo, InvoiceID, PaymentParams}, Context0, _Opts) ->
    {PaymentID, Context} = call(InvoiceID, {start_payment, PaymentParams, UserInfo}, Context0),
    {{ok, PaymentID}, Context};

handle_function('GetPayment', {UserInfo, UserInfo, PaymentID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, deduce_invoice_id(PaymentID), Context0),
    case get_payment(PaymentID, St) of
        Payment = #domain_InvoicePayment{} ->
            {{ok, Payment}, Context};
        false ->
            throw({#payproc_InvoicePaymentNotFound{}, Context})
    end;

handle_function('Fulfill', {UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    {Result, Context} = call(InvoiceID, {fulfill, Reason, UserInfo}, Context0),
    {{ok, Result}, Context};

handle_function('Rescind', {UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    {Result, Context} = call(InvoiceID, {rescind, Reason, UserInfo}, Context0),
    {{ok, Result}, Context}.

%%

get_history(_UserInfo, InvoiceID, Context) ->
    hg_machine:get_history(?NS, InvoiceID, opts(Context)).

get_history(_UserInfo, InvoiceID, AfterID, Limit, Context) ->
    hg_machine:get_history(?NS, InvoiceID, AfterID, Limit, opts(Context)).

get_state(UserInfo, InvoiceID, Context0) ->
    {History, Context} = get_history(UserInfo, InvoiceID, Context0),
    St = collapse_history(History),
    {St, Context}.

get_public_history(UserInfo, InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}, Context) ->
    hg_history:get_public_history(
        fun (ID, Lim, Ctx) -> get_history(UserInfo, InvoiceID, ID, Lim, Ctx) end,
        fun (Event) -> publish_event(InvoiceID, Event) end,
        AfterID, Limit,
        Context
    ).

start(ID, Args, Context) ->
    hg_machine:start(?NS, ID, Args, opts(Context)).

call(ID, Args, Context) ->
    hg_machine:call(?NS, ID, Args, opts(Context)).

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
-type sequence() :: pos_integer().

-type stage() ::
    idling |
    {processing_payment, payment_id(), payment_st()}.

-type ev() ::
    {public, sequence(), hg_payment_processing_thrift:'EventPayload'()} |
    {private, sequence(), private_event()}.

-type private_event() ::
    {payment_state_changed, payment_id(), payment_st()}.

-include("events.hrl").

-define(invalid_invoice_status(Invoice),
    #payproc_InvalidInvoiceStatus{status = Invoice#domain_Invoice.status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec publish_event(invoice_id(), hg_machine:event(ev())) ->
    {true, hg_machine:event_id(), hg_payment_processing_thrift:'Event'()} |
    {false, hg_machine:event_id()}.

publish_event(InvoiceID, {EventID, Dt, {public, Seq, Ev}}) ->
    {true, EventID, #payproc_Event{
        id         = EventID,
        created_at = Dt,
        source     = {invoice, InvoiceID},
        sequence   = Seq,
        payload    = Ev
    }};
publish_event(_InvoiceID, {EventID, _, _}) ->
    {false, EventID}.

%%

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(invoice_id(), {invoice_params(), user_info()}, hg_machine:context()) ->
    {hg_machine:result(ev()), woody_client:context()}.

init(ID, {InvoiceParams, UserInfo}, Context) ->
    Invoice = create_invoice(ID, InvoiceParams, UserInfo),
    Event = {public, ?invoice_ev(?invoice_created(Invoice))},
    {ok(Event, #st{}, set_invoice_timer(Invoice)), Context}.

-spec process_signal(hg_machine:signal(), hg_machine:history(ev()), hg_machine:context()) ->
    {hg_machine:result(ev()), woody_client:context()}.

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
            {ok(), Context}
    end;

process_signal({repair, _}, History, Context) ->
    St = #st{invoice = Invoice} = collapse_history(History),
    {ok([], St, set_invoice_timer(Invoice)), Context}.

process_expiration(St = #st{invoice = Invoice}, Context) ->
    {ok, Event} = cancel_invoice(overdue, Invoice),
    {ok(Event, St), Context}.

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
                {public, ?invoice_ev(?payment_ev(?payment_status_changed(PaymentID, ?succeeded())))},
                {public, ?invoice_ev(?invoice_status_changed(?paid()))}
            ],
            {ok(construct_payment_events(Payment, Trx, Events), St), Context};
        {{{error, Error = #domain_OperationError{}}, Trx}, Context} ->
            % payment finished with error
            Event = {public, ?invoice_ev(?payment_ev(?payment_status_changed(PaymentID, ?failed(Error))))},
            {ok(construct_payment_events(Payment, Trx, [Event]), St), Context};
        {{{next, Action, PaymentState}, Trx}, Context} ->
            % payment progressing yet
            Event = {private, {payment_state_changed, PaymentID, PaymentState}},
            {ok(construct_payment_events(Payment, Trx, [Event]), St, Action), Context}
    end.

construct_payment_events(#domain_InvoicePayment{trx = Trx}, #domain_TransactionInfo{} = Trx, Events) ->
    Events;
construct_payment_events(#domain_InvoicePayment{} = Payment, #domain_TransactionInfo{} = Trx, Events) ->
    [{public, ?invoice_ev(?payment_ev(?payment_bound(get_payment_id(Payment), Trx)))} | Events];
construct_payment_events(#domain_InvoicePayment{trx = Trx}, Trx = undefined, Events) ->
    Events.

-type call() ::
    {start_payment, payment_params(), user_info()} |
    {fulfill, binary(), user_info()} |
    {rescind, binary(), user_info()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev()), woody_client:context()) ->
    {{response(), hg_machine:result(ev())}, woody_client:context()}.

process_call({start_payment, PaymentParams, _UserInfo}, History, Context) ->
    St = #st{invoice = Invoice, stage = Stage} = collapse_history(History),
    Status = get_invoice_status(Invoice),
    case Stage of
        idling when Status == unpaid ->
            Payment = create_payment(PaymentParams, Invoice),
            PaymentID = get_payment_id(Payment),
            Events = [
                {public, ?invoice_ev(?payment_ev(?payment_started(Payment)))},
                {private, {payment_state_changed, PaymentID, undefined}}
            ],
            {respond({ok, PaymentID}, Events, St, hg_machine_action:instant()), Context};
        {processing_payment, PaymentID, _} ->
            {raise(?payment_pending(PaymentID)), Context};
        _ ->
            {raise(?invalid_invoice_status(Invoice)), Context}
    end;

process_call({fulfill, Reason, _UserInfo}, History, Context) ->
    St = #st{invoice = Invoice} = collapse_history(History),
    case fulfill_invoice(Reason, Invoice) of
        {ok, Event} ->
            {respond(ok, Event, St, set_invoice_timer(Invoice)), Context};
        {error, Exception} ->
            {raise(Exception, set_invoice_timer(Invoice)), Context}
    end;

process_call({rescind, Reason, _UserInfo}, History, Context) ->
    St = #st{invoice = Invoice} = collapse_history(History),
    case cancel_invoice({rescinded, Reason}, Invoice) of
        {ok, Event} ->
            {respond(ok, Event, St, set_invoice_timer(Invoice)), Context};
        {error, Exception} ->
            {raise(Exception, set_invoice_timer(Invoice)), Context}
    end.

set_invoice_timer(#domain_Invoice{status = ?unpaid(), due = Due}) when Due /= undefined ->
    hg_machine_action:set_deadline(Due);
set_invoice_timer(_Invoice) ->
    hg_machine_action:new().

ok() ->
    {[], hg_machine_action:new()}.
ok(Event, St) ->
    ok(Event, St, hg_machine_action:new()).
ok(Event, St, Action) ->
    {sequence_events(wrap_event_list(Event), St), Action}.

respond(Response, Event, St, Action) ->
    {Response, {sequence_events(wrap_event_list(Event), St), Action}}.

raise(Exception) ->
    raise(Exception, hg_machine_action:new()).
raise(Exception, Action) ->
    {{exception, Exception}, {[], Action}}.

wrap_event_list(Event) when is_tuple(Event) ->
    wrap_event_list([Event]);
wrap_event_list(Events) when is_list(Events) ->
    Events.

sequence_events(Evs, St) ->
    {SequencedEvs, _} = lists:mapfoldl(fun sequence_event_/2, St#st.sequence, Evs),
    SequencedEvs.

sequence_event_({public, Ev}, Seq) ->
    {{public, Seq + 1, Ev}, Seq + 1};
sequence_event_({private, Ev}, Seq) ->
    {{private, Seq, Ev}, Seq}.

%%

create_invoice(ID, V = #payproc_InvoiceParams{}, #payproc_UserInfo{id = UserID}) ->
    Revision = hg_domain:head(),
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner           = #domain_PartyRef{id = UserID, revision = 1},
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

create_payment(V = #payproc_InvoicePaymentParams{}, Invoice = #domain_Invoice{cost = Cost}) ->
    #domain_InvoicePayment{
        id           = create_payment_id(Invoice),
        created_at   = get_datetime_utc(),
        status       = ?pending(),
        cost         = Cost,
        payer        = V#payproc_InvoicePaymentParams.payer
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
    {ok, {public, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(Reason))))}};
cancel_invoice(_Reason, Invoice) ->
    {error, ?invalid_invoice_status(Invoice)}.

fulfill_invoice(Reason, #domain_Invoice{status = ?paid()}) ->
    {ok, {public, ?invoice_ev(?invoice_status_changed(?fulfilled(format_reason(Reason))))}};
fulfill_invoice(_Reason, Invoice) ->
    {error, ?invalid_invoice_status(Invoice)}.

%%

-spec collapse_history([ev()]) -> st().

collapse_history(History) ->
    lists:foldl(fun ({_ID, _, Ev}, St) -> merge_history(Ev, St) end, #st{}, History).

merge_history({public, Seq, ?invoice_ev(Event)}, St) ->
    merge_invoice_event(Event, St#st{sequence = Seq});
merge_history({private, Seq, Event}, St) ->
    merge_private_event(Event, St#st{sequence = Seq}).

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

merge_private_event({payment_state_changed, PaymentID, State}, St) ->
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
