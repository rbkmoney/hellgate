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
%%%  - if someone has access to a party then it has access to an invoice
%%%    belonging to this party

-module(hg_invoice).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-define(NS, <<"invoice">>).

-export([process_callback/2]).

%% Public interface

-export([get/1]).
-export([get_payment/2]).
-export([get_payment_opts/1]).

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

%% Internal

-export([fail/1]).

%% Internal types

-record(st, {
    activity          :: undefined | invoice | {payment, payment_id()},
    invoice           :: undefined | invoice(),
    payments = []     :: [{payment_id(), payment_st()}]
}).
-type st() :: #st{}.

-type invoice_change() :: dmsl_payment_processing_thrift:'InvoiceChange'().

%% API

-spec get(hg_machine:ref()) ->
    {ok, st()} | {error, notfound}.

get(Ref) ->
    case hg_machine:get_history(?NS, Ref) of
        {ok, History} ->
            {ok, collapse_history(unmarshal_history(History))};
        Error ->
            Error
    end.

-spec get_payment(hg_machine:tag() | payment_id(), st()) ->
    {ok, payment_st()} | {error, notfound}.

get_payment({tag, Tag}, #st{payments = Ps}) ->
    case lists:dropwhile(fun ({_, PS}) -> not lists:member(Tag, get_payment_tags(PS)) end, Ps) of
        [{_ID, PaymentSession} | _] ->
            {ok, PaymentSession};
        [] ->
            {error, notfound}
    end;
get_payment(PaymentID, St) ->
    case try_get_payment_session(PaymentID, St) of
        PaymentSession when PaymentSession /= undefined ->
            {ok, PaymentSession};
        undefined ->
            {error, notfound}
    end.

get_payment_tags(PaymentSession) ->
    hg_invoice_payment:get_tags(PaymentSession).

-spec get_payment_opts(st()) ->
    hg_invoice_payment:opts().

get_payment_opts(St = #st{invoice = Invoice}) ->
    #{
        party => hg_party:get_party(get_party_id(St)),
        invoice => Invoice
    }.

-spec get_payment_opts(hg_party:party_revision() | undefined, hg_datetime:timestamp(), st()) ->
    hg_invoice_payment:opts().

get_payment_opts(undefined, Timestamp, St = #st{invoice = Invoice}) ->
    #{
        party => hg_party:checkout(get_party_id(St), {timestamp, Timestamp}),
        invoice => Invoice
    };
get_payment_opts(Revision, _, St = #st{invoice = Invoice}) ->
    #{
        party => hg_party:checkout(get_party_id(St), {revision, Revision}),
        invoice => Invoice
    }.

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(invoicing,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_('Create', [UserInfo, InvoiceParams], _Opts) ->
    InvoiceID = hg_utils:uid(InvoiceParams#payproc_InvoiceParams.id),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    PartyID = InvoiceParams#payproc_InvoiceParams.party_id,
    ShopID = InvoiceParams#payproc_InvoiceParams.shop_id,
    _ = assert_party_accessible(PartyID),
    Party = hg_party:get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_shop_operable(Shop, Party),
    ok = validate_invoice_params(InvoiceParams, Shop),
    ok = ensure_started(InvoiceID, [undefined, Party#domain_Party.revision, InvoiceParams]),
    get_invoice_state(get_state(InvoiceID));

handle_function_('CreateWithTemplate', [UserInfo, Params], _Opts) ->
    InvoiceID = hg_utils:uid(Params#payproc_InvoiceWithTemplateParams.id),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    TplID = Params#payproc_InvoiceWithTemplateParams.template_id,
    InvoiceParams = make_invoice_params(Params),
    ok = ensure_started(InvoiceID, [TplID | InvoiceParams]),
    get_invoice_state(get_state(InvoiceID));

handle_function_('CapturePaymentNew', Args, Opts) ->
    handle_function_('CapturePayment', Args, Opts);

handle_function_('Get', [UserInfo, InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = assert_invoice_accessible(get_state(InvoiceID, AfterID, Limit)),
    get_invoice_state(St);

%% TODO Удалить после перехода на новый протокол
handle_function_('Get', [UserInfo, InvoiceID, undefined], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_invoice_state(St);

handle_function_('GetEvents', [UserInfo, InvoiceID, Range], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice_accessible(get_initial_state(InvoiceID)),
    get_public_history(InvoiceID, Range);

handle_function_('GetPayment', [UserInfo, InvoiceID, PaymentID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_payment_state(get_payment_session(PaymentID, St));

handle_function_('GetPaymentRefund', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    hg_invoice_payment:get_refund(ID, get_payment_session(PaymentID, St));

handle_function_('GetPaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    hg_invoice_payment:get_adjustment(ID, get_payment_session(PaymentID, St));

handle_function_('ComputeTerms', [UserInfo, InvoiceID, PartyRevision0], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    ShopID = get_shop_id(St),
    PartyID = get_party_id(St),
    Timestamp = get_created_at(St),
    PartyRevision1 = hg_maybe:get_defined(PartyRevision0, {timestamp, Timestamp}),
    ShopTerms = hg_invoice_utils:compute_shop_terms(
        UserInfo,
        PartyID,
        ShopID,
        Timestamp,
        PartyRevision1
    ),
    Revision = hg_domain:head(),
    Cash = get_cost(St),
    pm_party:reduce_terms(ShopTerms, #{cost => Cash}, Revision);

handle_function_(Fun, [UserInfo, InvoiceID | _Tail] = Args, _Opts) when
    Fun =:= 'StartPayment' orelse
    Fun =:= 'CapturePayment' orelse
    Fun =:= 'CancelPayment' orelse
    Fun =:= 'RefundPayment' orelse
    Fun =:= 'CreateManualRefund' orelse
    Fun =:= 'CreatePaymentAdjustment' orelse
    Fun =:= 'CapturePaymentAdjustment' orelse
    Fun =:= 'CancelPaymentAdjustment' orelse
    Fun =:= 'Fulfill' orelse
    Fun =:= 'Rescind'
->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, Fun, Args);

handle_function_('Repair', [UserInfo, InvoiceID, Changes, Action, Params], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice_accessible(get_initial_state(InvoiceID)),
    repair(InvoiceID, {changes, Changes, Action, Params});

handle_function_('RepairWithScenario', [UserInfo, InvoiceID, Scenario], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice_accessible(get_initial_state(InvoiceID)),
    repair(InvoiceID, {scenario, Scenario}).

assert_invoice_operable(St) ->
    % FIXME do not lose party here
    Party = hg_party:get_party(get_party_id(St)),
    Shop  = hg_party:get_shop(get_shop_id(St), Party),
    assert_party_shop_operable(Shop, Party).

assert_party_shop_operable(Shop, Party) ->
    _ = assert_party_operable(Party),
    assert_shop_operable(Shop).

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_Invoice{
        invoice = Invoice,
        payments = [
            get_payment_state(PaymentSession) ||
                {_PaymentID, PaymentSession} <- Payments
        ]
    }.

get_payment_state(PaymentSession) ->
    Refunds = hg_invoice_payment:get_refunds(PaymentSession),
    LegacyRefunds =
        lists:map(
            fun (#payproc_InvoicePaymentRefund{refund = R}) ->
                R
            end,
            Refunds
        ),
    #payproc_InvoicePayment{
        payment     = hg_invoice_payment:get_payment(PaymentSession),
        adjustments = hg_invoice_payment:get_adjustments(PaymentSession),
        route = hg_invoice_payment:get_route(PaymentSession),
        cash_flow = hg_invoice_payment:get_cashflow(PaymentSession),
        legacy_refunds = LegacyRefunds,
        refunds = Refunds,
        sessions = hg_invoice_payment:get_sessions(PaymentSession)
    }.

set_invoicing_meta(InvoiceID) ->
    scoper:add_meta(#{invoice_id => InvoiceID}).

set_invoicing_meta(InvoiceID, PaymentID) ->
    scoper:add_meta(#{invoice_id => InvoiceID, payment_id => PaymentID}).

%%

-type tag()               :: dmsl_base_thrift:'Tag'().
-type callback()          :: {provider, dmsl_proxy_provider_thrift:'Callback'()}.
-type callback_response() :: dmsl_proxy_provider_thrift:'CallbackResponse'().

-spec process_callback(tag(), callback()) ->
    {ok, callback_response()} | {error, invalid_callback | notfound | failed} | no_return().

process_callback(Tag, Callback) ->
    case hg_machine:call(?NS, {tag, Tag}, {callback, Tag, Callback}) of
        {ok, _Reply} = Response ->
            Response;
        {exception, invalid_callback} ->
            {error, invalid_callback};
        {error, _} = Error ->
            Error
    end.

%%

-spec fail(hg_machine:ref()) ->
    ok.

fail(Ref) ->
    case hg_machine:call(?NS, Ref, fail) of
        {error, failed} ->
            ok;
        {error, Error} ->
            erlang:error({unexpected_error, Error});
        {ok, Result} ->
            erlang:error({unexpected_result, Result})
    end.

%%

-include("invoice_events.hrl").

get_history(Ref) ->
    History = hg_machine:get_history(?NS, Ref),
    unmarshal_history(map_history_error(History)).

get_history(Ref, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, Ref, AfterID, Limit),
    unmarshal_history(map_history_error(History)).

get_state(Ref) ->
    collapse_history(get_history(Ref)).

get_state(Ref, AfterID, Limit) ->
    collapse_history(get_history(Ref, AfterID, Limit)).

get_initial_state(Ref) ->
    collapse_history(get_history(Ref, undefined, 1)).

get_public_history(InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_invoice_event(InvoiceID, Ev) || Ev <- get_history(InvoiceID, AfterID, Limit)].

publish_invoice_event(InvoiceID, {ID, Dt, Event}) ->
    #payproc_Event{
        id = ID,
        source = {invoice_id, InvoiceID},
        created_at = Dt,
        payload = ?invoice_ev(Event)
    }.

ensure_started(ID, [TemplateID, PartyRevision, Params]) ->
    SerializedArgs = [TemplateID, PartyRevision, marchal_invoice_params(Params)],
    map_start_error(do_start(ID, SerializedArgs)).

do_start(ID, Args) ->
    hg_machine:start(?NS, ID, Args).

call(ID, Function, Args) ->
    case hg_machine:thrift_call(?NS, ID, invoicing, {'Invoicing', Function}, Args) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {exception, Exception} ->
            erlang:throw(Exception);
        {error, Error} ->
            map_error(Error)
    end.

repair(ID, Args) ->
    map_repair_error(hg_machine:repair(?NS, ID, Args)).

-spec map_error(notfound | any()) ->
    no_return().
map_error(notfound) ->
    erlang:throw(#payproc_InvoiceNotFound{});
map_error(Reason) ->
    erlang:error(Reason).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{}).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, exists}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

map_repair_error({ok, _}) ->
    ok;
map_repair_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{});
map_repair_error({error, working}) ->
    % TODO
    throw(#'InvalidRequest'{errors = [<<"No need to repair">>]});
map_repair_error({error, Reason}) ->
    error(Reason).

%%

-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_tpl_id() :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: hg_invoice_payment:st().

-define(invalid_invoice_status(Status),
    #payproc_InvalidInvoiceStatus{status = Status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec publish_event(invoice_id(), hg_machine:event_payload()) ->
    hg_event_provider:public_event().
publish_event(InvoiceID, Payload) ->
    {{invoice_id, InvoiceID}, ?invoice_ev(unmarshal_event_payload(Payload))}.

%%

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init([invoice_tpl_id() | invoice_params()], hg_machine:machine()) ->
    hg_machine:result().

init([InvoiceTplID, PartyRevision, EncodedInvoiceParams], #{id := ID}) ->
    InvoiceParams = unmarchal_invoice_params(EncodedInvoiceParams),
    Invoice = create_invoice(ID, InvoiceTplID, PartyRevision, InvoiceParams),
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    handle_result(#{
        changes => [?invoice_created(Invoice)],
        action  => set_invoice_timer(#st{invoice = Invoice}),
        state   => #st{}
    }).

%%

-spec process_signal(hg_machine:signal(), hg_machine:machine()) ->
    hg_machine:result().

process_signal(Signal, #{history := History}) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal_history(History)))).

handle_signal(timeout, St = #st{activity = {payment, PaymentID}}) ->
    % there's a payment pending
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_signal(timeout, PaymentID, PaymentSession, St);
handle_signal(timeout, St = #st{activity = invoice}) ->
    % invoice is expired
    handle_expiration(St);

handle_signal({repair, {changes, Changes, RepairAction, Params}}, St0) ->
    Result = case Changes of
        [_ | _] ->
            #{changes => Changes};
        [] ->
            #{}
    end,
    Action = construct_repair_action(RepairAction),
    Result#{
        state  => St0,
        action => Action,
        % Validating that these changes are at least applicable
        validate => should_validate_transitions(Params)
    };

handle_signal({repair, {scenario, _}}, #st{activity = Activity})
    when Activity =:= invoice orelse Activity =:= undefined ->
    throw({exception, invoice_has_no_active_payment});
handle_signal({repair, {scenario, Scenario}}, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    case {Scenario, Activity} of
        {_, idle} ->
            throw({exception, cant_fail_payment_in_idle_state});
        {Scenario, Activity} ->
            try_to_get_repair_state(Scenario, St)
    end.

construct_repair_action(CA) when CA /= undefined ->
    lists:foldl(
        fun merge_repair_action/2,
        hg_machine_action:new(),
        [{timer, CA#repair_ComplexAction.timer}, {remove, CA#repair_ComplexAction.remove}]
    );
construct_repair_action(undefined) ->
    hg_machine_action:new().

merge_repair_action({timer, {set_timer, #repair_SetTimerAction{timer = Timer}}}, Action) ->
    hg_machine_action:set_timer(Timer, Action);
merge_repair_action({timer, {unset_timer, #repair_UnsetTimerAction{}}}, Action) ->
    hg_machine_action:unset_timer(Action);
merge_repair_action({remove, #repair_RemoveAction{}}, Action) ->
    hg_machine_action:mark_removal(Action);
merge_repair_action({_, undefined}, Action) ->
    Action.

should_validate_transitions(#payproc_InvoiceRepairParams{validate_transitions = V}) when is_boolean(V) ->
    V;
should_validate_transitions(undefined) ->
    true.

handle_expiration(St) ->
    #{
        changes => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(overdue)))],
        state   => St
    }.

%%

-type thrift_call()   :: hg_machine:thrift_call().
-type callback_call() :: {callback, tag(), callback()}.
-type call()          :: thrift_call() | callback_call().
-type call_result()   :: #{
    changes   => [invoice_change()],
    action    => hg_machine_action:t(),
    response  => ok | term(),
    state     => st()
}.

-spec process_call(call(), hg_machine:machine()) ->
    {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try
        handle_result(handle_call(Call, St))
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

-spec handle_call(call(), st()) ->
    call_result().

handle_call({{'Invoicing', 'StartPayment'}, [_UserInfo, _InvoiceID, PaymentParams]}, St) ->
    % TODO consolidate these assertions somehow
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    start_payment(PaymentParams, St);

handle_call({{'Invoicing', 'CapturePayment'}, [_UserInfo, _InvoiceID, PaymentID, Params]}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    #payproc_InvoicePaymentCaptureParams{
        reason = Reason,
        cash = Cash,
        cart = Cart
    } = Params,
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = get_payment_opts(St),
    {ok, {Changes, Action}} = capture_payment(PaymentSession, Reason, Cash, Cart, Opts),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes),
        action => Action,
        state => St
    };

handle_call({{'Invoicing', 'CancelPayment'}, [_UserInfo, _InvoiceID, PaymentID, Reason]}, St) ->
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

handle_call({{'Invoicing', 'Fulfill'}, [_UserInfo, _InvoiceID, Reason]}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(paid, St),
    #{
        response => ok,
        changes  => [?invoice_status_changed(?invoice_fulfilled(hg_utils:format_reason(Reason)))],
        state    => St
    };

handle_call({{'Invoicing', 'Rescind'}, [_UserInfo, _InvoiceID, Reason]}, St) ->
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

handle_call({{'Invoicing', 'RefundPayment'}, [_UserInfo, _InvoiceID, PaymentID, Params]}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    start_refund(refund, Params, PaymentID, PaymentSession, St);

handle_call({{'Invoicing', 'CreateManualRefund'}, [_UserInfo, _InvoiceID, PaymentID, Params]}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    start_refund(manual_refund, Params, PaymentID, PaymentSession, St);

handle_call({{'Invoicing', 'CreatePaymentAdjustment'}, [_UserInfo, _InvoiceID, PaymentID, Params]}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Timestamp = hg_datetime:format_now(),
    PaymentOpts = get_payment_opts(St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:create_adjustment(Timestamp, Params, PaymentSession, PaymentOpts),
        St
    );

handle_call({{'Invoicing', 'CapturePaymentAdjustment'}, [_UserInfo, _InvoiceID, PaymentID, ID]}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Adjustment = hg_invoice_payment:get_adjustment(ID, PaymentSession),
    PaymentOpts = get_payment_opts(
        Adjustment#domain_InvoicePaymentAdjustment.party_revision,
        Adjustment#domain_InvoicePaymentAdjustment.created_at,
        St
    ),
    Impact = {_Response, {PaymentChanges, _Action}} =
        hg_invoice_payment:capture_adjustment(ID, PaymentSession, PaymentOpts),
    Result = #{changes := Changes} = wrap_payment_impact(PaymentID, Impact, St),
    #st{invoice = Invoice} = St,
    case wrap_adjustment_changes(PaymentChanges) of
        [Status] when Status /= Invoice#domain_Invoice.status ->
            Result#{changes => Changes ++ [?invoice_status_changed(Status)]};
        _ ->
            Result
    end;

handle_call({{'Invoicing', 'CancelPaymentAdjustment'}, [_UserInfo, _InvoiceID, PaymentID, ID]}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Adjustment = hg_invoice_payment:get_adjustment(ID, PaymentSession),
    PaymentOpts = get_payment_opts(
        Adjustment#domain_InvoicePaymentAdjustment.party_revision,
        Adjustment#domain_InvoicePaymentAdjustment.created_at,
        St
    ),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:cancel_adjustment(ID, PaymentSession, PaymentOpts),
        St
    );

handle_call({callback, Tag, Callback}, St) ->
    dispatch_callback(Tag, Callback, St).

-spec dispatch_callback(tag(), callback(), st()) ->
    call_result().
dispatch_callback(Tag, {provider, Payload}, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_call({callback, Tag, Payload}, PaymentID, PaymentSession, St);
dispatch_callback(_Tag, _Callback, _St) ->
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

capture_payment(PaymentSession, Reason, undefined, Cart, Opts) when
    Cart =/= undefined
->
    Cash = hg_invoice_utils:get_cart_amount(Cart),
    capture_payment(PaymentSession, Reason, Cash, Cart, Opts);
capture_payment(PaymentSession, Reason, Cash, Cart, Opts) ->
    hg_invoice_payment:capture(PaymentSession, Reason, Cash, Cart, Opts).

%%

-include("payment_events.hrl").

start_payment(#payproc_InvoicePaymentParams{id = undefined} = PaymentParams, St) ->
    PaymentID = create_payment_id(St),
    do_start_payment(PaymentID, PaymentParams, St);
start_payment(#payproc_InvoicePaymentParams{id = PaymentID} = PaymentParams, St) ->
    case try_get_payment_session(PaymentID, St) of
        undefined ->
            do_start_payment(PaymentID, PaymentParams, St);
        PaymentSession ->
            #{
                response => get_payment_state(PaymentSession),
                state    => St
            }
    end.

do_start_payment(PaymentID, PaymentParams, St) ->
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    Opts = get_payment_opts(St),
    % TODO make timer reset explicit here
    {PaymentSession, {Changes, Action}} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts),
    #{
        response => get_payment_state(PaymentSession),
        changes  => wrap_payment_changes(PaymentID, Changes),
        action   => Action,
        state    => St
    }.

process_payment_signal(Signal, PaymentID, PaymentSession, St) ->
    {Revision, Timestamp} = hg_invoice_payment:get_party_revision(PaymentSession),
    Opts = get_payment_opts(Revision, Timestamp, St),
    PaymentResult = hg_invoice_payment:process_signal(Signal, PaymentSession, Opts),
    handle_payment_result(PaymentResult, PaymentID, PaymentSession, St).

process_payment_call(Call, PaymentID, PaymentSession, St) ->
    {Revision, Timestamp} = hg_invoice_payment:get_party_revision(PaymentSession),
    Opts = get_payment_opts(Revision, Timestamp, St),
    {Response, PaymentResult0} = hg_invoice_payment:process_call(Call, PaymentSession, Opts),
    PaymentResult1 = handle_payment_result(PaymentResult0, PaymentID, PaymentSession, St),
    PaymentResult1#{response => Response}.

handle_payment_result({next, {Changes, Action}}, PaymentID, _PaymentSession, St) ->
    #{
        changes => wrap_payment_changes(PaymentID, Changes),
        action  => Action,
        state   => St
    };
handle_payment_result({done, {Changes1, Action}}, PaymentID, PaymentSession, #st{invoice = Invoice} = St) ->
    PaymentSession1 = hg_invoice_payment:collapse_changes(Changes1, PaymentSession),
    Payment = hg_invoice_payment:get_payment(PaymentSession1),
    case get_payment_status(Payment) of
        ?processed() ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes1),
                action  => Action,
                state   => St
            };
        ?captured() ->
            Changes2 = case Invoice of
                #domain_Invoice{status = ?invoice_paid()} ->
                    [];
                #domain_Invoice{} ->
                    [?invoice_status_changed(?invoice_paid())]
            end,
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

wrap_adjustment_changes(Changes) ->
    [wrap_adjustment_status(Status) || ?payment_status_changed(Status) <- Changes].

wrap_adjustment_status(?captured()) ->
    ?invoice_paid();
wrap_adjustment_status(?cancelled()) ->
    ?invoice_unpaid();
wrap_adjustment_status(?failed(_)) ->
    ?invoice_unpaid().

handle_result(#{} = Result) ->
    St = validate_changes(Result),
    _ = log_changes(maps:get(changes, Result, []), St),
    MachineResult = handle_result_changes(Result, handle_result_action(Result, #{})),
    case maps:get(response, Result, undefined) of
        undefined ->
            MachineResult;
        ok ->
            {ok, MachineResult};
        Response ->
            {{ok, Response}, MachineResult}
    end.

handle_result_changes(#{changes := Changes = [_ | _]}, Acc) ->
    Acc#{events => [marshal_event_payload(Changes)]};
handle_result_changes(#{}, Acc) ->
    Acc.

handle_result_action(#{action := Action}, Acc) ->
    Acc#{action => Action};
handle_result_action(#{}, Acc) ->
    Acc.

validate_changes(#{validate := false, changes := Changes = [_ | _], state := St}) ->
    collapse_changes(Changes, St, #{});
validate_changes(#{changes := Changes = [_ | _], state := St}) ->
    collapse_changes(Changes, St, #{validation => strict});
validate_changes(#{state := St}) ->
    St.

%%

start_refund(RefundType, RefundParams0, PaymentID, PaymentSession, St) ->
    RefundParams = ensure_refund_id_defined(RefundType, RefundParams0, PaymentSession),
    case get_refund(get_refund_id(RefundParams), PaymentSession) of
        undefined ->
            start_new_refund(RefundType, PaymentID, RefundParams, PaymentSession, St);
        Refund ->
            #{
                response => Refund,
                state    => St
            }
    end.

get_refund_id(#payproc_InvoicePaymentRefundParams{id = RefundID}) ->
    RefundID.

ensure_refund_id_defined(RefundType, Params, PaymentSession) ->
    RefundID = force_refund_id_format(RefundType, define_refund_id(Params, PaymentSession)),
    Params#payproc_InvoicePaymentRefundParams{id = RefundID}.

define_refund_id(#payproc_InvoicePaymentRefundParams{id = undefined}, PaymentSession) ->
    make_new_refund_id(PaymentSession);
define_refund_id(#payproc_InvoicePaymentRefundParams{id = ID}, _PaymentSession) ->
    ID.

-define(MANUAL_REFUND_ID_PREFIX, "m").

%% If something breaks - this is why
force_refund_id_format(manual_refund, Correct = <<?MANUAL_REFUND_ID_PREFIX, _Rest/binary>>) ->
    Correct;
force_refund_id_format(manual_refund, Incorrect) ->
    <<?MANUAL_REFUND_ID_PREFIX, Incorrect/binary>>;
force_refund_id_format(refund, <<?MANUAL_REFUND_ID_PREFIX, _ID/binary>>) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid id format">>]});
force_refund_id_format(refund, ID) ->
    ID.

parse_refund_id(<<?MANUAL_REFUND_ID_PREFIX, ID/binary>>) ->
    ID;
parse_refund_id(ID) ->
    ID.

make_new_refund_id(PaymentSession) ->
    Refunds = hg_invoice_payment:get_refunds(PaymentSession),
    construct_refund_id(Refunds).

construct_refund_id(Refunds) ->
    % we can't be sure that old ids were constructed in strict increasing order, so we need to find max ID
    MaxID = lists:foldl(fun find_max_refund_id/2, 0, Refunds),
    genlib:to_binary(MaxID + 1).

find_max_refund_id(#payproc_InvoicePaymentRefund{refund = Refund}, Max) ->
    #domain_InvoicePaymentRefund{id = ID} = Refund,
    IntID = genlib:to_int(parse_refund_id(ID)),
    erlang:max(IntID, Max).

get_refund(ID, PaymentSession) ->
    try
        hg_invoice_payment:get_refund(ID, PaymentSession)
    catch
        throw:#payproc_InvoicePaymentRefundNotFound{} ->
            undefined
    end.

start_new_refund(RefundType, PaymentID, Params, PaymentSession, St) when
    RefundType =:= refund;
    RefundType =:= manual_refund
->
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:RefundType(Params, PaymentSession, get_payment_opts(St)),
        St
    ).

%%

create_invoice(ID, InvoiceTplID, PartyRevision, V = #payproc_InvoiceParams{}) ->
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner_id        = V#payproc_InvoiceParams.party_id,
        party_revision  = PartyRevision,
        created_at      = hg_datetime:format_now(),
        status          = ?invoice_unpaid(),
        cost            = V#payproc_InvoiceParams.cost,
        due             = V#payproc_InvoiceParams.due,
        details         = V#payproc_InvoiceParams.details,
        context         = V#payproc_InvoiceParams.context,
        template_id     = InvoiceTplID,
        external_id     = V#payproc_InvoiceParams.external_id
    }.

create_payment_id(#st{payments = Payments}) ->
    integer_to_binary(length(Payments) + 1).

get_payment_status(#domain_InvoicePayment{status = Status}) ->
    Status.

try_to_get_repair_state({complex, #payproc_InvoiceRepairComplex{scenarios = Scenarios}}, St) ->
    repair_complex(Scenarios, St);
try_to_get_repair_state(Scenario, St) ->
    process_repair(Scenario, St).

repair_complex([], St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    throw({exception, {activity_not_compatible_with_complex_scenario, Activity}});
repair_complex([Scenario | Rest], St) ->
    try
        process_repair(Scenario, St)
    catch
        throw:{exception, {activity_not_compatible_with_scenario, _, _}} ->
            repair_complex(Rest, St)
    end.

process_repair(Scenario, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    RepairSession = hg_invoice_repair:get_repair_state(Activity, Scenario, PaymentSession),
    process_payment_signal(timeout, PaymentID, RepairSession, St).

%%

-spec collapse_history([hg_machine:event()]) -> st().

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, Dt, Changes}, St0) ->
            collapse_changes(Changes, St0, #{timestamp => Dt})
        end,
        #st{},
        History
    ).

collapse_changes(Changes, St0, Opts) ->
    lists:foldl(fun (C, St) -> merge_change(C, St, Opts) end, St0, Changes).

merge_change(?invoice_created(Invoice), St, _Opts) ->
    St#st{activity = invoice, invoice = Invoice};
merge_change(?invoice_status_changed(Status), St = #st{invoice = I}, _Opts) ->
    St#st{invoice = I#domain_Invoice{status = Status}};
merge_change(?payment_ev(PaymentID, Change), St, Opts) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    PaymentSession1 = hg_invoice_payment:merge_change(Change, PaymentSession, Opts),
    St1 = set_payment_session(PaymentID, PaymentSession1, St),
    case hg_invoice_payment:get_activity(PaymentSession1) of
        A when A =/= idle ->
            % TODO Shouldn't we have here some kind of stack instead?
            St1#st{activity = {payment, PaymentID}};
        idle ->
            St1#st{activity = invoice}
    end.

get_party_id(#st{invoice = #domain_Invoice{owner_id = PartyID}}) ->
    PartyID.

get_shop_id(#st{invoice = #domain_Invoice{shop_id = ShopID}}) ->
    ShopID.

get_created_at(#st{invoice = #domain_Invoice{created_at = CreatedAt}}) ->
    CreatedAt.

get_cost(#st{invoice = #domain_Invoice{cost = Cash}}) ->
    Cash.

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

make_invoice_params(Params) ->
    #payproc_InvoiceWithTemplateParams{
        template_id = TplID,
        cost = Cost,
        context = Context,
        external_id = ExternalID
    } = Params,
    #domain_InvoiceTemplate{
        owner_id = PartyID,
        shop_id = ShopID,
        invoice_lifetime = Lifetime,
        product = Product,
        description = Description,
        details = TplDetails,
        context = TplContext
    } = hg_invoice_template:get(TplID),
    Party = hg_party:get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_accessible(PartyID),
    _ = assert_party_shop_operable(Shop, Party),
    Cart = make_invoice_cart(Cost, TplDetails, Shop),
    InvoiceDetails = #domain_InvoiceDetails{
        product = Product,
        description = Description,
        cart = Cart
    },
    InvoiceCost = hg_invoice_utils:get_cart_amount(Cart),
    InvoiceDue = make_invoice_due_date(Lifetime),
    InvoiceContext = make_invoice_context(Context, TplContext),
    [
        Party#domain_Party.revision,
        #payproc_InvoiceParams{
            party_id = PartyID,
            shop_id = ShopID,
            details = InvoiceDetails,
            due = InvoiceDue,
            cost = InvoiceCost,
            context = InvoiceContext,
            external_id = ExternalID
        }
    ].

make_invoice_cart(_, {cart, Cart}, _) ->
    Cart;
make_invoice_cart(Cost, {product, TplProduct}, Shop) ->
    #domain_InvoiceTemplateProduct{
        product = Product,
        price = TplPrice,
        metadata = Metadata
    } = TplProduct,
    #domain_InvoiceCart{lines = [
        #domain_InvoiceLine{
            product = Product,
            quantity = 1,
            price = get_templated_price(Cost, TplPrice, Shop),
            metadata = Metadata
        }
    ]}.

get_templated_price(undefined, {fixed, Cost}, Shop) ->
    get_cost(Cost, Shop);
get_templated_price(undefined, _, _) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_NO_COST]});
get_templated_price(Cost, {fixed, Cost}, Shop) ->
    get_cost(Cost, Shop);
get_templated_price(_Cost, {fixed, _CostTpl}, _Shop) ->
    throw(#'InvalidRequest'{errors = [?INVOICE_TPL_BAD_COST]});
get_templated_price(Cost, {range, Range}, Shop) ->
    _ = assert_cost_in_range(Cost, Range),
    get_cost(Cost, Shop);
get_templated_price(Cost, {unlim, _}, Shop) ->
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
            _ = logger:log(info, Message, #{Type => Params}),
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

-spec marshal_event_payload([invoice_change()]) ->
    hg_machine:event_payload().
marshal_event_payload(Changes) when is_list(Changes) ->
    wrap_event_payload({invoice_changes, Changes}).

-spec marchal_invoice_params(invoice_params()) ->
    binary().
marchal_invoice_params(Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'InvoiceParams'}},
    hg_proto_utils:serialize(Type, Params).

%% Unmarshalling

-spec unmarshal_history([hg_machine:event()]) ->
    [hg_machine:event([invoice_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) ->
    hg_machine:event([invoice_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) ->
    [invoice_change()].
unmarshal_event_payload(#{format_version := 1, data := {bin, Changes}}) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    {invoice_changes, Buf} = hg_proto_utils:deserialize(Type, Changes),
    Buf;
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, changes}, Changes).

-spec unmarchal_invoice_params(binary()) ->
    invoice_params().
unmarchal_invoice_params(Bin) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'InvoiceParams'}},
    hg_proto_utils:deserialize(Type, Bin).

%% Legacy formats unmarshal

%% Version > 1

unmarshal({list, changes}, Changes) when is_list(Changes) ->
    lists:flatten([unmarshal(change, Change) || Change <- Changes]);

%% Version 1

unmarshal({list, changes}, {bin, Bin}) when is_binary(Bin) ->
    Changes = binary_to_term(Bin),
    lists:flatten([unmarshal(change, [1, Change]) || Change <- Changes]);


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
    PaymentEvents = hg_invoice_payment:unmarshal(Payload),
    [?payment_ev(unmarshal(str, PaymentID), Event) || Event <- PaymentEvents];

unmarshal(change, [1, ?legacy_invoice_created(Invoice)]) ->
    ?invoice_created(unmarshal(invoice, Invoice));
unmarshal(change, [1, ?legacy_invoice_status_changed(Status)]) ->
    ?invoice_status_changed(unmarshal(status, Status));
unmarshal(change, [1, ?legacy_payment_ev(PaymentID, Payload)]) ->
    PaymentEvents = hg_invoice_payment:unmarshal([1, Payload]),
    [?payment_ev(unmarshal(str, PaymentID), Event) || Event <- PaymentEvents];

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
    ExternalID = maps:get(<<"external_id">>, Invoice, undefined),
    #domain_Invoice{
        id              = unmarshal(str, ID),
        shop_id         = unmarshal(str, ShopID),
        owner_id        = unmarshal(str, PartyID),
        party_revision  = maps:get(<<"party_revision">>, Invoice, undefined),
        created_at      = unmarshal(str, CreatedAt),
        cost            = hg_cash:unmarshal(Cash),
        due             = unmarshal(str, Due),
        details         = unmarshal(details, Details),
        status          = ?invoice_unpaid(),
        context         = hg_content:unmarshal(Context),
        template_id     = unmarshal(str, TemplateID),
        external_id     = unmarshal(str, ExternalID)
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

unmarshal(invoice,
    ?legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cash, Context)
) ->
    unmarshal(invoice,
        ?legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cash, Context, undefined)
    );

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

%% Wrap in thrift binary

wrap_event_payload(Payload) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    Bin = hg_proto_utils:serialize(Type, Payload),
    #{
        format_version => 1,
        data => {bin, Bin}
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

create_dummy_refund_with_id(ID) ->
    #payproc_InvoicePaymentRefund{
        refund =
            #domain_InvoicePaymentRefund{
                id              = genlib:to_binary(ID),
                created_at      = hg_datetime:format_now(),
                domain_revision = 42,
                party_revision  = 42,
                status          = ?refund_pending(),
                reason          = <<"No reason">>,
                cash            = 1000,
                cart            = unefined
            }
    }.

-spec construct_refund_id_test() -> _.
construct_refund_id_test() ->
    IDs = [X||{_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 10)])], % 10 IDs shuffled
    Refunds = lists:map(fun create_dummy_refund_with_id/1, IDs),
    ?assert(<<"11">> =:= construct_refund_id(Refunds)).
-endif.
