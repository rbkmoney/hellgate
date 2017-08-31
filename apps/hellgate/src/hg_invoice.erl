%%% Invoice machine
%%%
%%% TODO
%%%  - REFACTOR WITH FIRE
%%%     - proper concepts
%%%        - simple lightweight lower-level machines (middlewares (?)) for:
%%%           - handling callbacks idempotently
%%%           - state collapsing (?)
%%%           - simpler flow control (?)
%%%           - event publishing (?)
%%%  - unify somehow with operability assertions from hg_party
%%%  - should party blocking / suspension be version-locked? probably _not_
%%%  - if someone has access to a party then it has access to an invoice
%%%    belonging to this party

-module(hg_invoice).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-define(NS, <<"invoice">>).

-export([process_callback/2]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%%

-record(st, {
    activity          :: undefined | invoice | {payment, payment_id()},
    invoice           :: undefined | invoice(),
    payments = []     :: [{payment_id(), payment_st()}]
}).

-type st() :: #st{}.

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(invoicing,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_('Create', [UserInfo, InvoiceParams], _Opts) ->
    InvoiceID = hg_utils:unique_id(),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    PartyID = InvoiceParams#payproc_InvoiceParams.party_id,
    ShopID = InvoiceParams#payproc_InvoiceParams.shop_id,
    _ = assert_party_accessible(PartyID),
    Party = get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_shop_operable(Shop, Party),
    ok = validate_invoice_params(InvoiceParams, Shop),
    ok = start(InvoiceID, [undefined, InvoiceParams]),
    get_invoice_state(get_state(InvoiceID));

handle_function_('CreateWithTemplate', [UserInfo, Params], _Opts) ->
    InvoiceID = hg_utils:unique_id(),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    TplID = Params#payproc_InvoiceWithTemplateParams.template_id,
    InvoiceParams = make_invoice_params(Params),
    ok = start(InvoiceID, [TplID, InvoiceParams]),
    get_invoice_state(get_state(InvoiceID));

handle_function_('Get', [UserInfo, InvoiceID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_invoice_state(St);

handle_function_('GetEvents', [UserInfo, InvoiceID, Range], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice_accessible(get_initial_state(InvoiceID)),
    get_public_history(InvoiceID, Range);

handle_function_('StartPayment', [UserInfo, InvoiceID, PaymentParams], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    get_payment_state(call(InvoiceID, {start_payment, PaymentParams}));

handle_function_('GetPayment', [UserInfo, InvoiceID, PaymentID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_payment_state(get_payment_session(PaymentID, St));

handle_function_('CapturePayment', [UserInfo, InvoiceID, PaymentID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {capture_payment, PaymentID, Reason});

handle_function_('CancelPayment', [UserInfo, InvoiceID, PaymentID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {cancel_payment, PaymentID, Reason});

handle_function_('RefundPayment', [UserInfo, InvoiceID, PaymentID, Params], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {refund_payment, PaymentID, Params});

handle_function_('GetPaymentRefund', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    hg_invoice_payment:get_refund(ID, get_payment_session(PaymentID, St));

handle_function_('CreatePaymentAdjustment', [UserInfo, InvoiceID, PaymentID, Params], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {create_payment_adjustment, PaymentID, Params});

handle_function_('GetPaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    hg_invoice_payment:get_adjustment(ID, get_payment_session(PaymentID, St));

handle_function_('CapturePaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {capture_payment_adjustment, PaymentID, ID});

handle_function_('CancelPaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {cancel_payment_adjustment, PaymentID, ID});

handle_function_('Fulfill', [UserInfo, InvoiceID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, {fulfill, Reason});

handle_function_('Rescind', [UserInfo, InvoiceID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, {rescind, Reason}).

assert_invoice_operable(St) ->
    % FIXME do not lose party here
    Party = get_party(get_party_id(St)),
    Shop  = hg_party:get_shop(get_shop_id(St), Party),
    assert_party_shop_operable(Shop, Party).

assert_party_shop_operable(Shop, Party) ->
    _ = assert_party_operable(Party),
    assert_shop_operable(Shop).

get_party(PartyID) ->
    hg_party_machine:get_party(PartyID).

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_Invoice{
        invoice = Invoice,
        payments = [
            get_payment_state(PaymentSession) ||
                {_PaymentID, PaymentSession} <- Payments
        ]
    }.

get_payment_state(PaymentSession) ->
    #payproc_InvoicePayment{
        payment     = hg_invoice_payment:get_payment(PaymentSession),
        adjustments = hg_invoice_payment:get_adjustments(PaymentSession),
        refunds     = hg_invoice_payment:get_refunds(PaymentSession)
    }.

set_invoicing_meta(InvoiceID) ->
    hg_log_scope:set_meta(#{invoice_id => InvoiceID}).

set_invoicing_meta(InvoiceID, PaymentID) ->
    hg_log_scope:set_meta(#{invoice_id => InvoiceID, payment_id => PaymentID}).

%%

-type tag()               :: dmsl_base_thrift:'Tag'().
-type callback()          :: _. %% FIXME
-type callback_response() :: _. %% FIXME

-spec process_callback(tag(), callback()) ->
    {ok, callback_response()} | {error, invalid_callback | notfound | failed} | no_return().

process_callback(Tag, Callback) ->
    case hg_machine:call(?NS, {tag, Tag}, {callback, Callback}) of
        {ok, {ok, _} = Ok} ->
            Ok;
        {ok, {exception, invalid_callback}} ->
            {error, invalid_callback};
        {error, _} = Error ->
            Error
    end.

%%
-include("invoice_events.hrl").

get_history(InvoiceID) ->
    History = hg_machine:get_history(?NS, InvoiceID),
    map_history_error(unmarshal_history_result(History)).

get_history(InvoiceID, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, InvoiceID, AfterID, Limit),
    map_history_error(unmarshal_history_result(History)).

get_state(InvoiceID) ->
    collapse_history(get_history(InvoiceID)).

get_initial_state(InvoiceID) ->
    collapse_history(get_history(InvoiceID, undefined, 1)).

get_public_history(InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_invoice_event(InvoiceID, Ev) || Ev <- get_history(InvoiceID, AfterID, Limit)].

publish_invoice_event(InvoiceID, {ID, Dt, Event}) ->
    #payproc_Event{
        id = ID,
        source = {invoice_id, InvoiceID},
        created_at = Dt,
        payload = ?invoice_ev(Event)
    }.

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{});
map_error({error, Reason}) ->
    error(Reason).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

unmarshal_history_result({ok, Result}) ->
    {ok, unmarshal(Result)};
unmarshal_history_result(Error) ->
    Error.

%%

-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_tpl_id() :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().
-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type adjustment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type adjustment_id() :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type refund_params() :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type payment_st() :: hg_invoice_payment:st().

-type ev() ::
    [dmsl_payment_processing_thrift:'InvoiceChange'()].

-type msgpack_ev() :: hg_msgpack_marshalling:value().

-define(invalid_invoice_status(Status),
    #payproc_InvalidInvoiceStatus{status = Status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec publish_event(invoice_id(), msgpack_ev()) ->
    hg_event_provider:public_event().

publish_event(InvoiceID, Changes) ->
    {{invoice_id, InvoiceID}, ?invoice_ev(unmarshal({list, changes}, Changes))}.

%%

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(invoice_id(), [invoice_tpl_id() | invoice_params()]) ->
    hg_machine:result().

init(ID, [InvoiceTplID, InvoiceParams]) ->
    Invoice = create_invoice(ID, InvoiceTplID, InvoiceParams),
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    handle_result(#{
        changes => [?invoice_created(Invoice)],
        action  => set_invoice_timer(#st{invoice = Invoice}),
        state   => #st{}
    }).

%%

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result().

process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal(History)))).

handle_signal(timeout, St = #st{activity = {payment, PaymentID}}) ->
    % there's a payment pending
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_signal(timeout, PaymentID, PaymentSession, St);
handle_signal(timeout, St = #st{activity = invoice}) ->
    % invoice is expired
    handle_expiration(St);

handle_signal({repair, _}, St) ->
    #{
        state => St
    }.

handle_expiration(St) ->
    #{
        changes => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(overdue)))],
        state   => St
    }.

%%

-type call() ::
    {start_payment, payment_params()} |
    {refund_payment , payment_id(), refund_params()} |
    {capture_payment, payment_id(), binary()} |
    {cancel_payment, payment_id(), binary()} |
    {create_payment_adjustment , payment_id(), adjustment_params()} |
    {capture_payment_adjustment, payment_id(), adjustment_id()} |
    {cancel_payment_adjustment , payment_id(), adjustment_id()} |
    {fulfill, binary()} |
    {rescind, binary()} |
    {callback, callback()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {hg_machine:response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

handle_call({start_payment, PaymentParams}, St) ->
    % TODO consolidate these assertions somehow
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    start_payment(PaymentParams, St);

handle_call({capture_payment, PaymentID, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    {ok, {Changes, Action}} = hg_invoice_payment:capture(PaymentSession, Reason),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes),
        action => Action,
        state => St
    };

handle_call({cancel_payment, PaymentID, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    {ok, {Changes, Action}} = hg_invoice_payment:cancel(PaymentSession, Reason),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes),
        action => Action,
        state => St
    };

handle_call({fulfill, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(paid, St),
    #{
        response => ok,
        changes  => [?invoice_status_changed(?invoice_fulfilled(hg_utils:format_reason(Reason)))],
        state    => St
    };

handle_call({rescind, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    #{
        response => ok,
        changes  => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(Reason)))],
        action   => hg_machine_action:unset_timer(),
        state    => St
    };

handle_call({refund_payment, PaymentID, Params}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:refund(Params, PaymentSession, get_payment_opts(St)),
        St
    );

handle_call({create_payment_adjustment, PaymentID, Params}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:create_adjustment(Params, PaymentSession, get_payment_opts(St)),
        St
    );

handle_call({capture_payment_adjustment, PaymentID, ID}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:capture_adjustment(ID, PaymentSession, get_payment_opts(St)),
        St
    );

handle_call({cancel_payment_adjustment, PaymentID, ID}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:cancel_adjustment(ID, PaymentSession, get_payment_opts(St)),
        St
    );

handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

dispatch_callback({provider, Payload}, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_call({callback, Payload}, PaymentID, PaymentSession, St);
dispatch_callback(_Callback, _St) ->
    throw(invalid_callback).

assert_invoice_status(Status, #st{invoice = Invoice}) ->
    assert_invoice_status(Status, Invoice);
assert_invoice_status(Status, #domain_Invoice{status = {Status, _}}) ->
    ok;
assert_invoice_status(_Status, #domain_Invoice{status = Invalid}) ->
    throw(?invalid_invoice_status(Invalid)).

assert_no_pending_payment(#st{activity = {payment, PaymentID}}) ->
    throw(?payment_pending(PaymentID));
assert_no_pending_payment(_) ->
    ok.

set_invoice_timer(#st{invoice = #domain_Invoice{due = Due}}) ->
    hg_machine_action:set_deadline(Due).

%%

-include("payment_events.hrl").

start_payment(PaymentParams, St) ->
    PaymentID = create_payment_id(St),
    Opts = get_payment_opts(St),
    % TODO make timer reset explicit here
    {PaymentSession, {Changes1, _}} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts),
    {ok, {Changes2, Action}} = hg_invoice_payment:start_session(?processed()),
    #{
        response => PaymentSession,
        changes  => wrap_payment_changes(PaymentID, Changes1 ++ Changes2),
        action   => Action,
        state    => St
    }.

process_payment_signal(Signal, PaymentID, PaymentSession, St) ->
    Opts = get_payment_opts(St),
    PaymentResult = hg_invoice_payment:process_signal(Signal, PaymentSession, Opts),
    handle_payment_result(PaymentResult, PaymentID, PaymentSession, St).

process_payment_call(Call, PaymentID, PaymentSession, St) ->
    Opts = get_payment_opts(St),
    {Response, PaymentResult} = hg_invoice_payment:process_call(Call, PaymentSession, Opts),
    maps:merge(#{response => Response}, handle_payment_result(PaymentResult, PaymentID, PaymentSession, St)).

handle_payment_result(Result, PaymentID, PaymentSession, St) ->
    case Result of
        {next, {Changes, Action}} ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes),
                action  => Action,
                state   => St
            };
        {done, {Changes1, Action}} ->
            PaymentSession1 = lists:foldl(fun hg_invoice_payment:merge_change/2, PaymentSession, Changes1),
            Payment = hg_invoice_payment:get_payment(PaymentSession1),
            case get_payment_status(Payment) of
                ?processed() ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        action  => Action,
                        state   => St
                    };
                ?captured() ->
                    Changes2 = [?invoice_status_changed(?invoice_paid())],
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1) ++ Changes2,
                        action  => Action,
                        state   => St
                    };
                ?refunded() ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        state   => St
                    };
                ?failed(_) ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        action  => set_invoice_timer(St),
                        state   => St
                    };
                ?cancelled() ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        action  => set_invoice_timer(St),
                        state   => St
                    }
            end
    end.

wrap_payment_changes(PaymentID, Changes) ->
    [?payment_ev(PaymentID, C) || C <- Changes].

wrap_payment_impact(PaymentID, {Response, {Changes, Action}}, St) ->
    #{
        response => Response,
        changes  => wrap_payment_changes(PaymentID, Changes),
        action   => Action,
        state    => St
    }.

get_payment_opts(St = #st{invoice = Invoice}) ->
    #{
        party => checkout_party(St),
        invoice => Invoice
    }.

checkout_party(St = #st{invoice = #domain_Invoice{created_at = CreationTimestamp}}) ->
    PartyID = get_party_id(St),
    hg_party_machine:checkout(PartyID, CreationTimestamp).

handle_result(#{state := St} = Params) ->
    Changes = maps:get(changes, Params, []),
    Action = maps:get(action, Params, hg_machine_action:new()),
    _ = log_changes(Changes, St),
    case maps:get(response, Params, undefined) of
        undefined ->
            {[marshal(Changes)], Action};
        Response ->
            {{ok, Response}, {[marshal(Changes)], Action}}
    end.

%%

create_invoice(ID, InvoiceTplID, V = #payproc_InvoiceParams{}) ->
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner_id        = V#payproc_InvoiceParams.party_id,
        created_at      = hg_datetime:format_now(),
        status          = ?invoice_unpaid(),
        cost            = V#payproc_InvoiceParams.cost,
        due             = V#payproc_InvoiceParams.due,
        details         = V#payproc_InvoiceParams.details,
        context         = V#payproc_InvoiceParams.context,
        template_id     = InvoiceTplID
    }.

create_payment_id(#st{payments = Payments}) ->
    integer_to_binary(length(Payments) + 1).

get_payment_status(#domain_InvoicePayment{status = Status}) ->
    Status.

%%

-spec collapse_history([hg_machine:event(ev())]) -> st().

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Changes}, St0) ->
            lists:foldl(fun merge_change/2, St0, Changes)
        end,
        #st{},
        History
    ).

merge_change(?invoice_created(Invoice), St) ->
    St#st{activity = invoice, invoice = Invoice};
merge_change(?invoice_status_changed(Status), St = #st{invoice = I}) ->
    St#st{invoice = I#domain_Invoice{status = Status}};
merge_change(?payment_ev(PaymentID, Event), St) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    PaymentSession1 = hg_invoice_payment:merge_change(Event, PaymentSession),
    St1 = set_payment_session(PaymentID, PaymentSession1, St),
    case hg_invoice_payment:get_activity(PaymentSession1) of
        A when A /= undefined ->
            % TODO Shouldn't we have here some kind of stack instead?
            St1#st{activity = {payment, PaymentID}};
        undefined ->
            St1#st{activity = invoice}
    end.

get_party_id(#st{invoice = #domain_Invoice{owner_id = PartyID}}) ->
    PartyID.

get_shop_id(#st{invoice = #domain_Invoice{shop_id = ShopID}}) ->
    ShopID.

get_payment_session(PaymentID, St) ->
    case try_get_payment_session(PaymentID, St) of
        PaymentSession when PaymentSession /= undefined ->
            PaymentSession;
        undefined ->
            throw(#payproc_InvoicePaymentNotFound{})
    end.

try_get_payment_session(PaymentID, #st{payments = Payments}) ->
    case lists:keyfind(PaymentID, 1, Payments) of
        {PaymentID, PaymentSession} ->
            PaymentSession;
        false ->
            undefined
    end.

set_payment_session(PaymentID, PaymentSession, St = #st{payments = Payments}) ->
    St#st{payments = lists:keystore(PaymentID, 1, Payments, {PaymentID, PaymentSession})}.

%%

assert_shop_exists(Shop) ->
    hg_invoice_utils:assert_shop_exists(Shop).

assert_party_operable(Party) ->
    hg_invoice_utils:assert_party_operable(Party).

assert_shop_operable(Shop) ->
    hg_invoice_utils:assert_shop_operable(Shop).

%%

validate_invoice_params(#payproc_InvoiceParams{cost = Cost}, Shop) ->
    hg_invoice_utils:validate_cost(Cost, Shop).

assert_invoice_accessible(St = #st{}) ->
    assert_party_accessible(get_party_id(St)),
    St.

assert_party_accessible(PartyID) ->
    hg_invoice_utils:assert_party_accessible(PartyID).

assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

make_invoice_params(#payproc_InvoiceWithTemplateParams{
    template_id = TplID,
    cost = Cost,
    context = Context
}) ->
    #domain_InvoiceTemplate{
        owner_id = PartyID,
        shop_id = ShopID,
        details = Details,
        invoice_lifetime = Lifetime,
        cost = TplCost,
        context = TplContext
    } = hg_invoice_template:get(TplID),
    Party = get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_accessible(PartyID),
    _ = assert_party_shop_operable(Shop, Party),
    InvoiceCost = get_templated_cost(Cost, TplCost, Shop),
    InvoiceDue = make_invoice_due_date(Lifetime),
    InvoiceContext = make_invoice_context(Context, TplContext),
    #payproc_InvoiceParams{
        party_id = PartyID,
        shop_id = ShopID,
        details = Details,
        due = InvoiceDue,
        cost = InvoiceCost,
        context = InvoiceContext
    }.

get_templated_cost(undefined, {fixed, Cost}, Shop) ->
    get_cost(Cost, Shop);
get_templated_cost(undefined, _, _) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_NO_COST]});
get_templated_cost(Cost, {fixed, Cost}, Shop) ->
    get_cost(Cost, Shop);
get_templated_cost(_Cost, {fixed, _CostTpl}, _Shop) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_BAD_COST]});
get_templated_cost(Cost, {range, Range}, Shop) ->
    _ = assert_cost_in_range(Cost, Range),
    get_cost(Cost, Shop);
get_templated_cost(Cost, {unlim, _}, Shop) ->
    get_cost(Cost, Shop).

get_cost(Cost, Shop) ->
    ok = hg_invoice_utils:validate_cost(Cost, Shop),
    Cost.

assert_cost_in_range(
    #domain_Cash{amount = Amount, currency = Currency},
    #domain_CashRange{
        upper = {UType, #domain_Cash{amount = UAmount, currency = Currency}},
        lower = {LType, #domain_Cash{amount = LAmount, currency = Currency}}
    }
) ->
    _ = assert_less_than(LType, LAmount, Amount),
    _ = assert_less_than(UType, Amount, UAmount),
    ok;
assert_cost_in_range(_, _) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_BAD_CURRENCY]}).

assert_less_than(inclusive, Less, More) when Less =< More ->
    ok;
assert_less_than(exclusive, Less, More) when Less < More ->
    ok;
assert_less_than(_, _, _) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_BAD_AMOUNT]}).

make_invoice_due_date(#domain_LifetimeInterval{years = YY, months = MM, days = DD}) ->
    hg_datetime:add_interval(hg_datetime:format_now(), {YY, MM, DD}).

make_invoice_context(undefined, TplContext) ->
    TplContext;
make_invoice_context(Context, _) ->
    Context.

%%

-include("domain.hrl").

log_changes(Changes, St) ->
    lists:foreach(fun (C) -> log_change(C, St) end, Changes).

log_change(Change, St) ->
    case get_log_params(Change, St) of
        {ok, #{type := Type, params := Params, message := Message}} ->
            _ = lager:log(info, [{Type, Params}], Message),
            ok;
        undefined ->
            ok
    end.

get_log_params(?invoice_created(Invoice), _St) ->
    get_invoice_event_log(invoice_created, unpaid, Invoice);
get_log_params(?invoice_status_changed({StatusName, _}), #st{invoice = Invoice}) ->
    get_invoice_event_log(invoice_status_changed, StatusName, Invoice);
get_log_params(?payment_ev(PaymentID, Change), St = #st{invoice = Invoice}) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    case hg_invoice_payment:get_log_params(Change, PaymentSession) of
        {ok, Params} ->
            {ok, maps:update_with(
                params,
                fun (V) ->
                    [{invoice, get_invoice_params(Invoice)} | V]
                end,
                Params
            )};
        undefined ->
            undefined
    end.

get_invoice_event_log(EventType, StatusName, Invoice) ->
    {ok, #{
        type => invoice_event,
        params => [{type, EventType}, {status, StatusName} | get_invoice_params(Invoice)],
        message => get_message(EventType)
    }}.

get_invoice_params(Invoice) ->
    #domain_Invoice{
        id = ID,
        owner_id = PartyID,
        cost = ?cash(Amount, Currency),
        shop_id = ShopID
    } = Invoice,
    [{id, ID}, {owner_id, PartyID}, {cost, [{amount, Amount}, {currency, Currency}]}, {shop_id, ShopID}].

get_message(invoice_created) ->
    "Invoice is created";
get_message(invoice_status_changed) ->
    "Invoice status is changed".

-include("legacy_structures.hrl").
%% Marshalling

marshal(Changes) when is_list(Changes) ->
    [marshal(change, Change) || Change <- Changes].

%% Changes

marshal(change, ?invoice_created(Invoice)) ->
    [2, #{
        <<"change">>    => <<"created">>,
        <<"invoice">>   => marshal(invoice, Invoice)
    }];
marshal(change, ?invoice_status_changed(Status)) ->
    [2, #{
        <<"change">>    => <<"status_changed">>,
        <<"status">>    => marshal(status, Status)
    }];
marshal(change, ?payment_ev(PaymentID, Payload)) ->
    [2, #{
        <<"change">>    => <<"payment_change">>,
        <<"id">>        => marshal(str, PaymentID),
        <<"payload">>   => hg_invoice_payment:marshal(Payload)
    }];

%% Change components

marshal(invoice, #domain_Invoice{} = Invoice) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, Invoice#domain_Invoice.id),
        <<"shop_id">>       => marshal(str, Invoice#domain_Invoice.shop_id),
        <<"owner_id">>      => marshal(str, Invoice#domain_Invoice.owner_id),
        <<"created_at">>    => marshal(str, Invoice#domain_Invoice.created_at),
        <<"cost">>          => hg_cash:marshal(Invoice#domain_Invoice.cost),
        <<"due">>           => marshal(str, Invoice#domain_Invoice.due),
        <<"details">>       => marshal(details, Invoice#domain_Invoice.details),
        <<"context">>       => hg_content:marshal(Invoice#domain_Invoice.context),
        <<"template_id">>   => marshal(str, Invoice#domain_Invoice.template_id)
    });

marshal(details, #domain_InvoiceDetails{} = Details) ->
    genlib_map:compact(#{
        <<"product">> => marshal(str, Details#domain_InvoiceDetails.product),
        <<"description">> => marshal(str, Details#domain_InvoiceDetails.description),
        <<"cart">> => marshal(cart, Details#domain_InvoiceDetails.cart)
    });

marshal(status, ?invoice_paid()) ->
    <<"paid">>;
marshal(status, ?invoice_unpaid()) ->
    <<"unpaid">>;
marshal(status, ?invoice_cancelled(Reason)) ->
    [
        <<"cancelled">>,
        marshal(str, Reason)
    ];
marshal(status, ?invoice_fulfilled(Reason)) ->
    [
        <<"fulfilled">>,
        marshal(str, Reason)
    ];

marshal(cart, #domain_InvoiceCart{lines = Lines}) ->
    [marshal(line, Line) || Line <- Lines];

marshal(line, #domain_InvoiceLine{} = InvoiceLine) ->
    #{
        <<"product">> => marshal(str, InvoiceLine#domain_InvoiceLine.product),
        <<"quantity">> => marshal(int, InvoiceLine#domain_InvoiceLine.quantity),
        <<"price">> => hg_cash:marshal(InvoiceLine#domain_InvoiceLine.price),
        <<"metadata">> => marshal(metadata, InvoiceLine#domain_InvoiceLine.metadata)
    };

marshal(metadata, Metadata) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(str, K), hg_msgpack_marshalling:unmarshal(V), Acc)
        end,
        #{},
        Metadata
    );

marshal(_, Other) ->
    Other.

%% Unmarshalling

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, changes}, Payload)}.

%% Version > 1

unmarshal({list, changes}, Changes) when is_list(Changes) ->
    [unmarshal(change, Change) || Change <- Changes];

%% Version 1

unmarshal({list, changes}, {bin, Bin}) when is_binary(Bin) ->
    Changes = binary_to_term(Bin),
    [unmarshal(change, [1, Change]) || Change <- Changes];

%% Changes

unmarshal(change, [2, #{
    <<"change">>    := <<"created">>,
    <<"invoice">>   := Invoice
}]) ->
    ?invoice_created(unmarshal(invoice, Invoice));
unmarshal(change, [2, #{
    <<"change">>    := <<"status_changed">>,
    <<"status">>    := Status
}]) ->
    ?invoice_status_changed(unmarshal(status, Status));
unmarshal(change, [2, #{
    <<"change">>    := <<"payment_change">>,
    <<"id">>        := PaymentID,
    <<"payload">>   := Payload
}]) ->
    ?payment_ev(
        unmarshal(str, PaymentID),
        hg_invoice_payment:unmarshal(Payload)
    );

unmarshal(change, [1, ?legacy_invoice_created(Invoice)]) ->
    ?invoice_created(unmarshal(invoice, Invoice));
unmarshal(change, [1, ?legacy_invoice_status_changed(Status)]) ->
    ?invoice_status_changed(unmarshal(status, Status));
unmarshal(change, [1, ?legacy_payment_ev(PaymentID, Payload)]) ->
    ?payment_ev(
        unmarshal(str, PaymentID),
        hg_invoice_payment:unmarshal([1, Payload])
    );

%% Change components

unmarshal(invoice, #{
    <<"id">>            := ID,
    <<"shop_id">>       := ShopID,
    <<"owner_id">>      := PartyID,
    <<"created_at">>    := CreatedAt,
    <<"cost">>          := Cash,
    <<"due">>           := Due,
    <<"details">>       := Details
} = Invoice) ->
    Context = maps:get(<<"context">>, Invoice, undefined),
    TemplateID = maps:get(<<"template_id">>, Invoice, undefined),
    #domain_Invoice{
        id              = unmarshal(str, ID),
        shop_id         = unmarshal(str, ShopID),
        owner_id        = unmarshal(str, PartyID),
        created_at      = unmarshal(str, CreatedAt),
        cost            = hg_cash:unmarshal(Cash),
        due             = unmarshal(str, Due),
        details         = unmarshal(details, Details),
        status          = ?invoice_unpaid(),
        context         = hg_content:unmarshal(Context),
        template_id     = unmarshal(str, TemplateID)
    };

unmarshal(invoice,
    ?legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cash, Context, TemplateID)
) ->
    #domain_Invoice{
        id              = unmarshal(str, ID),
        shop_id         = unmarshal(str, ShopID),
        owner_id        = unmarshal(str, PartyID),
        created_at      = unmarshal(str, CreatedAt),
        cost            = hg_cash:unmarshal([1, Cash]),
        due             = unmarshal(str, Due),
        details         = unmarshal(details, Details),
        status          = unmarshal(status, Status),
        context         = hg_content:unmarshal(Context),
        template_id     = unmarshal(str, TemplateID)
    };

unmarshal(status, <<"paid">>) ->
    ?invoice_paid();
unmarshal(status, <<"unpaid">>) ->
    ?invoice_unpaid();
unmarshal(status, [<<"cancelled">>, Reason]) ->
    ?invoice_cancelled(unmarshal(str, Reason));
unmarshal(status, [<<"fulfilled">>, Reason]) ->
    ?invoice_fulfilled(unmarshal(str, Reason));

unmarshal(status, ?legacy_invoice_paid()) ->
    ?invoice_paid();
unmarshal(status, ?legacy_invoice_unpaid()) ->
    ?invoice_unpaid();
unmarshal(status, ?legacy_invoice_cancelled(Reason)) ->
    ?invoice_cancelled(unmarshal(str, Reason));
unmarshal(status, ?legacy_invoice_fulfilled(Reason)) ->
    ?invoice_fulfilled(unmarshal(str, Reason));

unmarshal(details, #{<<"product">> := Product} = Details) ->
    Description = maps:get(<<"description">>, Details, undefined),
    Cart = maps:get(<<"cart">>, Details, undefined),
    #domain_InvoiceDetails{
        product     = unmarshal(str, Product),
        description = unmarshal(str, Description),
        cart        = unmarshal(cart, Cart)
    };

unmarshal(details, ?legacy_invoice_details(Product, Description)) ->
    #domain_InvoiceDetails{
        product     = unmarshal(str, Product),
        description = unmarshal(str, Description)
    };

unmarshal(cart, Lines) when is_list(Lines) ->
    #domain_InvoiceCart{lines = [unmarshal(line, Line) || Line <- Lines]};

unmarshal(line, #{
    <<"product">> := Product,
    <<"quantity">> := Quantity,
    <<"price">> := Price,
    <<"metadata">> := Metadata
}) ->
    #domain_InvoiceLine{
        product = unmarshal(str, Product),
        quantity = unmarshal(int, Quantity),
        price = hg_cash:unmarshal(Price),
        metadata = unmarshal(metadata, Metadata)
    };

unmarshal(metadata, Metadata) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(unmarshal(str, K), hg_msgpack_marshalling:marshal(V), Acc)
        end,
        #{},
        Metadata
    );

unmarshal(_, Other) ->
    Other.
