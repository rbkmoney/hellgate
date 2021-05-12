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
-include("payment_events.hrl").
-include("invoice_events.hrl").
-include("domain.hrl").

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
-export([process_repair/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%% Internal

-export([fail/1]).

-import(hg_invoice_utils, [
    assert_party_accessible/1,
    assert_party_operable/1,
    assert_shop_operable/1,
    assert_shop_exists/1
]).

%% Internal types

-define(invalid_invoice_status(Status), #payproc_InvalidInvoiceStatus{status = Status}).

-record(st, {
    activity :: undefined | activity(),
    invoice :: undefined | invoice(),
    payments = [] :: [{payment_id(), payment_st()}],
    adjustments = [] :: [adjustment()],
    party :: undefined | party()
}).

-type st() :: #st{}.

-type invoice_change() :: dmsl_payment_processing_thrift:'InvoiceChange'().

-type activity() ::
    invoice
    | {payment, payment_id()}
    | {adjustment_new, adjustment_id()}
    | {adjustment_pending, adjustment_id()}.

-type adjustment_id() :: dmsl_domain_thrift:'InvoiceAdjustmentID'().

%% API

-spec get(hg_machine:ref()) -> {ok, st()} | {error, notfound}.
get(Ref) ->
    case hg_machine:get_history(?NS, Ref) of
        {ok, History} ->
            {ok, collapse_history(unmarshal_history(History))};
        Error ->
            Error
    end.

-spec get_payment(hg_machine:tag() | payment_id(), st()) -> {ok, payment_st()} | {error, notfound}.
get_payment({tag, Tag}, #st{payments = Ps}) ->
    case lists:dropwhile(fun({_, PS}) -> not lists:member(Tag, get_payment_tags(PS)) end, Ps) of
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

-spec get_payment_opts(st()) -> hg_invoice_payment:opts().
get_payment_opts(St = #st{invoice = Invoice, party = undefined}) ->
    #{
        party => hg_party:get_party(get_party_id(St)),
        invoice => Invoice,
        timestamp => hg_datetime:format_now()
    };
get_payment_opts(#st{invoice = Invoice, party = Party}) ->
    #{
        party => Party,
        invoice => Invoice,
        timestamp => hg_datetime:format_now()
    }.

-spec get_payment_opts(hg_party:party_revision() | undefined, hg_datetime:timestamp(), st()) ->
    hg_invoice_payment:opts().
get_payment_opts(undefined, Timestamp, St = #st{invoice = Invoice}) ->
    #{
        party => hg_party:checkout(get_party_id(St), {timestamp, Timestamp}),
        invoice => Invoice,
        timestamp => hg_datetime:format_now()
    };
get_payment_opts(Revision, _, St = #st{invoice = Invoice}) ->
    #{
        party => hg_party:checkout(get_party_id(St), {revision, Revision}),
        invoice => Invoice,
        timestamp => hg_datetime:format_now()
    }.

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        invoicing,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function_('Create', {UserInfo, InvoiceParams}, _Opts) ->
    DomainRevision = hg_domain:head(),
    InvoiceID = hg_utils:uid(InvoiceParams#payproc_InvoiceParams.id),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    PartyID = InvoiceParams#payproc_InvoiceParams.party_id,
    ShopID = InvoiceParams#payproc_InvoiceParams.shop_id,
    _ = assert_party_accessible(PartyID),
    Party = hg_party:get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_shop_operable(Shop, Party),
    MerchantTerms = get_merchant_terms(Party, DomainRevision, Shop, hg_datetime:format_now()),
    ok = validate_invoice_params(InvoiceParams, Shop, MerchantTerms),
    ok = ensure_started(InvoiceID, {undefined, Party#domain_Party.revision, InvoiceParams}),
    get_invoice_state(get_state(InvoiceID));
handle_function_('CreateWithTemplate', {UserInfo, Params}, _Opts) ->
    DomainRevision = hg_domain:head(),
    InvoiceID = hg_utils:uid(Params#payproc_InvoiceWithTemplateParams.id),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    TplID = Params#payproc_InvoiceWithTemplateParams.template_id,
    {Party, Shop, InvoiceParams} = make_invoice_params(Params),
    MerchantTerms = get_merchant_terms(Party, DomainRevision, Shop, hg_datetime:format_now()),
    ok = validate_invoice_params(InvoiceParams, Shop, MerchantTerms),
    ok = ensure_started(InvoiceID, {TplID, Party#domain_Party.revision, InvoiceParams}),
    get_invoice_state(get_state(InvoiceID));
handle_function_('CapturePaymentNew', Args, Opts) ->
    handle_function_('CapturePayment', Args, Opts);
handle_function_('Get', {UserInfo, InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = get_state(InvoiceID, AfterID, Limit),
    _ = assert_invoice(accessible, St),
    get_invoice_state(St);
%% TODO Удалить после перехода на новый протокол
handle_function_('Get', {UserInfo, InvoiceID, undefined}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    get_invoice_state(St);
handle_function_('GetEvents', {UserInfo, InvoiceID, Range}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice(accessible, get_initial_state(InvoiceID)),
    get_public_history(InvoiceID, Range);
handle_function_('GetInvoiceAdjustment', {UserInfo, InvoiceID, ID}, _Opts) ->
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    ok = assume_user_identity(UserInfo),
    ok = set_invoicing_meta(InvoiceID),
    get_adjustment(ID, St);
handle_function_('GetPayment', {UserInfo, InvoiceID, PaymentID}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    get_payment_state(get_payment_session(PaymentID, St));
handle_function_('GetPaymentRefund', {UserInfo, InvoiceID, PaymentID, ID}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    hg_invoice_payment:get_refund(ID, get_payment_session(PaymentID, St));
handle_function_('GetPaymentChargeback', {UserInfo, InvoiceID, PaymentID, ID}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    CBSt = hg_invoice_payment:get_chargeback_state(ID, get_payment_session(PaymentID, St)),
    hg_invoice_payment_chargeback:get(CBSt);
handle_function_('GetPaymentAdjustment', {UserInfo, InvoiceID, PaymentID, ID}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    hg_invoice_payment:get_adjustment(ID, get_payment_session(PaymentID, St));
handle_function_('ComputeTerms', {UserInfo, InvoiceID, PartyRevision0}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = get_state(InvoiceID),
    _ = assert_invoice(accessible, St),
    ShopID = get_shop_id(St),
    PartyID = get_party_id(St),
    Timestamp = get_created_at(St),
    PartyRevision1 = hg_maybe:get_defined(PartyRevision0, {timestamp, Timestamp}),
    Party = hg_party:get_party(PartyID),
    Shop = hg_party:get_shop(ShopID, Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Cash = get_cost(St),
    VS = hg_varset:prepare_varset(#{
        party_id => PartyID,
        shop_id => ShopID,
        category => Shop#domain_Shop.category,
        currency => (Shop#domain_Shop.account)#domain_ShopAccount.currency,
        identification_level => hg_invoice_utils:get_identification_level(Contract, Party),
        cost => Cash
    }),
    hg_invoice_utils:compute_shop_terms(
        PartyID,
        ShopID,
        Timestamp,
        PartyRevision1,
        VS
    );
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'StartPayment' orelse
        Fun =:= 'CapturePayment' orelse
        Fun =:= 'CancelPayment' orelse
        Fun =:= 'RefundPayment' orelse
        Fun =:= 'CreateManualRefund' orelse
        Fun =:= 'CreateInvoiceAdjustment' orelse
        Fun =:= 'CaptureAdjustment' orelse
        Fun =:= 'CancelAdjustment' orelse
        Fun =:= 'CreateChargeback' orelse
        Fun =:= 'CancelChargeback' orelse
        Fun =:= 'AcceptChargeback' orelse
        Fun =:= 'RejectChargeback' orelse
        Fun =:= 'ReopenChargeback' orelse
        Fun =:= 'CreatePaymentAdjustment' orelse
        Fun =:= 'CapturePaymentAdjustment' orelse
        Fun =:= 'CancelPaymentAdjustment' orelse
        Fun =:= 'Fulfill' orelse
        Fun =:= 'Rescind'
->
    UserInfo = erlang:element(1, Args),
    InvoiceID = erlang:element(2, Args),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, Fun, Args);
handle_function_('Repair', {UserInfo, InvoiceID, Changes, Action, Params}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice(accessible, get_initial_state(InvoiceID)),
    repair(InvoiceID, {changes, Changes, Action, Params});
handle_function_('RepairWithScenario', {UserInfo, InvoiceID, Scenario}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice(accessible, get_initial_state(InvoiceID)),
    repair(InvoiceID, {scenario, Scenario}).

%%----------------- invoice asserts
assert_invoice(Checks, #st{} = St) when is_list(Checks) ->
    lists:foldl(fun assert_invoice/2, St, Checks);
assert_invoice(operable, #st{party = Party} = St) when Party =/= undefined ->
    assert_party_shop_operable(
        hg_party:get_shop(get_shop_id(St), Party),
        Party
    ),
    St;
assert_invoice(accessible, #st{} = St) ->
    assert_party_accessible(get_party_id(St)),
    St;
assert_invoice({status, Status}, #st{invoice = #domain_Invoice{status = {Status, _}}} = St) ->
    St;
assert_invoice({status, _Status}, #st{invoice = #domain_Invoice{status = Invalid}}) ->
    throw(?invalid_invoice_status(Invalid)).

assert_party_shop_operable(Shop, Party) ->
    _ = assert_party_operable(Party),
    _ = assert_shop_operable(Shop),
    ok.

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_Invoice{
        invoice = Invoice,
        payments = [
            get_payment_state(PaymentSession)
            || {_PaymentID, PaymentSession} <- Payments
        ]
    }.

get_payment_state(PaymentSession) ->
    Refunds = hg_invoice_payment:get_refunds(PaymentSession),
    LegacyRefunds =
        lists:map(
            fun(#payproc_InvoicePaymentRefund{refund = R}) ->
                R
            end,
            Refunds
        ),
    #payproc_InvoicePayment{
        payment = hg_invoice_payment:get_payment(PaymentSession),
        adjustments = hg_invoice_payment:get_adjustments(PaymentSession),
        chargebacks = hg_invoice_payment:get_chargebacks(PaymentSession),
        route = hg_invoice_payment:get_route(PaymentSession),
        cash_flow = hg_invoice_payment:get_final_cashflow(PaymentSession),
        legacy_refunds = LegacyRefunds,
        refunds = Refunds,
        sessions = hg_invoice_payment:get_sessions(PaymentSession),
        last_transaction_info = hg_invoice_payment:get_trx(PaymentSession)
    }.

set_invoicing_meta(InvoiceID) ->
    scoper:add_meta(#{invoice_id => InvoiceID}).

set_invoicing_meta(InvoiceID, PaymentID) ->
    scoper:add_meta(#{invoice_id => InvoiceID, payment_id => PaymentID}).

%%

-type tag() :: dmsl_base_thrift:'Tag'().
-type callback() :: {provider, dmsl_proxy_provider_thrift:'Callback'()}.
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

-spec fail(hg_machine:ref()) -> ok.
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

ensure_started(ID, {TemplateID, PartyRevision, Params}) ->
    SerializedArgs = {TemplateID, PartyRevision, marshal_invoice_params(Params)},
    case hg_machine:start(?NS, ID, SerializedArgs) of
        {ok, _} -> ok;
        {error, exists} -> ok;
        {error, Reason} -> erlang:error(Reason)
    end.

call(ID, Function, Args) ->
    case hg_machine:thrift_call(?NS, ID, invoicing, {'Invoicing', Function}, Args) of
        ok -> ok;
        {ok, Reply} -> Reply;
        {exception, Exception} -> erlang:throw(Exception);
        {error, notfound} -> erlang:throw(#payproc_InvoiceNotFound{});
        {error, Error} -> erlang:error(Error)
    end.

repair(ID, Args) ->
    case hg_machine:repair(?NS, ID, Args) of
        {ok, _Result} -> ok;
        {error, notfound} -> erlang:throw(#payproc_InvoiceNotFound{});
        {error, working} -> erlang:throw(#'InvalidRequest'{errors = [<<"No need to repair">>]});
        {error, Reason} -> erlang:error(Reason)
    end.

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{}).

%%

-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type party() :: dmsl_domain_thrift:'Party'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_tpl_id() :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().

-type adjustment() :: dmsl_payment_processing_thrift:'InvoiceAdjustment'().

-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: hg_invoice_payment:st().

-define(payment_pending(PaymentID), #payproc_InvoicePaymentPending{id = PaymentID}).
-define(adjustment_target_status(Status), #domain_InvoiceAdjustment{
    state =
        {status_change, #domain_InvoiceAdjustmentStatusChangeState{
            scenario = #domain_InvoiceAdjustmentStatusChange{target_status = Status}
        }}
}).

-spec publish_event(invoice_id(), hg_machine:event_payload()) -> hg_event_provider:public_event().
publish_event(InvoiceID, Payload) ->
    {{invoice_id, InvoiceID}, ?invoice_ev(unmarshal_event_payload(Payload))}.

%%

-spec namespace() -> hg_machine:ns().
namespace() ->
    ?NS.

-spec init(
    {invoice_tpl_id() | undefined, hg_party:party_revision() | undefined, binary()},
    hg_machine:machine()
) -> hg_machine:result().
init({InvoiceTplID, PartyRevision, EncodedInvoiceParams}, #{id := ID}) ->
    InvoiceParams = unmarshal_invoice_params(EncodedInvoiceParams),
    Invoice = create_invoice(ID, InvoiceTplID, PartyRevision, InvoiceParams),
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    handle_result(#{
        changes => [?invoice_created(Invoice)],
        action => set_invoice_timer(hg_machine_action:new(), #st{invoice = Invoice}),
        state => #st{}
    }).

%%

-spec process_repair(hg_machine:args(), hg_machine:machine()) -> hg_machine:result() | no_return().
process_repair(Args, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    handle_result(handle_repair(Args, St)).

handle_repair({changes, Changes, RepairAction, Params}, St) ->
    Result =
        case Changes of
            [_ | _] ->
                #{changes => Changes};
            [] ->
                #{}
        end,
    Action = construct_repair_action(RepairAction),
    Result#{
        state => St,
        action => Action,
        % Validating that these changes are at least applicable
        validate => should_validate_transitions(Params)
    };
handle_repair({scenario, _}, #st{activity = Activity}) when Activity =:= invoice orelse Activity =:= undefined ->
    throw({exception, invoice_has_no_active_payment});
handle_repair({scenario, Scenario}, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    case {Scenario, Activity} of
        {_, idle} ->
            throw({exception, cant_fail_payment_in_idle_state});
        {Scenario, Activity} ->
            try_to_get_repair_state(Scenario, St)
    end.

-spec process_signal(hg_machine:signal(), hg_machine:machine()) -> hg_machine:result().
process_signal(Signal, #{history := History}) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal_history(History)))).

handle_signal(timeout, St = #st{activity = {payment, PaymentID}}) ->
    % there's a payment pending
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_signal(timeout, PaymentID, PaymentSession, St);
handle_signal(timeout, St = #st{activity = {adjustment_new, ID}}) ->
    Status = {processed, #domain_InvoiceAdjustmentProcessed{}},
    Change = [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Status))],
    #{changes => Change, state => St};
handle_signal(timeout, St = #st{activity = invoice}) ->
    % invoice is expired
    handle_expiration(St).

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
        state => St
    }.

%%

-type thrift_call() :: hg_machine:thrift_call().
-type callback_call() :: {callback, tag(), callback()}.
-type call() :: thrift_call() | callback_call().
-type call_result() :: #{
    changes => [invoice_change()],
    action => hg_machine_action:t(),
    response => ok | term(),
    state => st()
}.

-spec process_call(call(), hg_machine:machine()) -> {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try
        handle_result(handle_call(Call, St))
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

-spec handle_call(call(), st()) -> call_result().
handle_call({{'Invoicing', 'StartPayment'}, {_UserInfo, _InvoiceID, PaymentParams}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable], St),
    _ = assert_all_adjustments_finalised(St),
    start_payment(PaymentParams, St);
handle_call({{'Invoicing', 'CapturePayment'}, {_UserInfo, _InvoiceID, PaymentID, Params}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable], St),
    #payproc_InvoicePaymentCaptureParams{
        reason = Reason,
        cash = Cash,
        cart = Cart
    } = Params,
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = #{timestamp := OccurredAt} = get_payment_opts(St),
    {ok, {Changes, Action}} = capture_payment(PaymentSession, Reason, Cash, Cart, Opts),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
        action => Action,
        state => St
    };
handle_call({{'Invoicing', 'CancelPayment'}, {_UserInfo, _InvoiceID, PaymentID, Reason}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable], St),
    PaymentSession = get_payment_session(PaymentID, St),
    {ok, {Changes, Action}} = hg_invoice_payment:cancel(PaymentSession, Reason),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes, hg_datetime:format_now()),
        action => Action,
        state => St
    };
handle_call({{'Invoicing', 'Fulfill'}, {_UserInfo, _InvoiceID, Reason}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable, {status, paid}], St),
    #{
        response => ok,
        changes => [?invoice_status_changed(?invoice_fulfilled(hg_utils:format_reason(Reason)))],
        state => St
    };
handle_call({{'Invoicing', 'Rescind'}, {_UserInfo, _InvoiceID, Reason}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable, {status, unpaid}], St),
    _ = assert_no_pending_payment(St),
    #{
        response => ok,
        changes => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(Reason)))],
        action => hg_machine_action:unset_timer(),
        state => St
    };
handle_call({{'Invoicing', 'CreateInvoiceAdjustment'}, {_UserInfo, _InvoiceID, Params}}, St) ->
    _ = assert_invoice(accessible, St),
    ID = create_adjustment_id(St),
    TargetStatus = get_adjustment_params_target_status(Params),
    InvoiceStatus = get_invoice_status(St),
    ok = assert_no_pending_payment(St),
    ok = assert_adjustment_target_status(TargetStatus, InvoiceStatus),
    ok = assert_all_adjustments_finalised(St),
    OccurredAt = hg_datetime:format_now(),
    wrap_adjustment_impact(ID, hg_invoice_adjustment:create(ID, Params, OccurredAt), St, OccurredAt);
handle_call({{'Invoicing', 'CaptureAdjustment'}, {_UserInfo, _InvoiceID, ID}}, St) ->
    _ = assert_invoice(accessible, St),
    _ = assert_adjustment_processed(ID, St),
    OccurredAt = hg_datetime:format_now(),
    ?adjustment_target_status(Status) = get_adjustment(ID, St),
    {Response, {Changes, Action}} = hg_invoice_adjustment:capture(OccurredAt),
    #{
        response => Response,
        changes => wrap_adjustment_changes(ID, Changes, OccurredAt),
        action => set_invoice_timer(Status, Action, St),
        state => St
    };
handle_call({{'Invoicing', 'CancelAdjustment'}, {_UserInfo, _InvoiceID, ID}}, St) ->
    _ = assert_invoice(accessible, St),
    _ = assert_adjustment_processed(ID, St),
    OccurredAt = hg_datetime:format_now(),
    Status = get_invoice_status(St),
    {Response, {Changes, Action}} = hg_invoice_adjustment:cancel(OccurredAt),
    #{
        response => Response,
        changes => wrap_adjustment_changes(ID, Changes, OccurredAt),
        action => set_invoice_timer(Status, Action, St),
        state => St
    };
handle_call({{'Invoicing', 'RefundPayment'}, {_UserInfo, _InvoiceID, PaymentID, Params}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable], St),
    PaymentSession = get_payment_session(PaymentID, St),
    start_refund(refund, Params, PaymentID, PaymentSession, St);
handle_call({{'Invoicing', 'CreateManualRefund'}, {_UserInfo, _InvoiceID, PaymentID, Params}}, St0) ->
    St = St0#st{party = hg_party:get_party(get_party_id(St0))},
    _ = assert_invoice([accessible, operable], St),
    PaymentSession = get_payment_session(PaymentID, St),
    start_refund(manual_refund, Params, PaymentID, PaymentSession, St);
handle_call({{'Invoicing', 'CreateChargeback'}, {_UserInfo, _InvoiceID, PaymentID, Params}}, St) ->
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    PaymentOpts = get_payment_opts(St),
    start_chargeback(Params, PaymentID, PaymentSession, PaymentOpts, St);
handle_call({{'Invoicing', 'CancelChargeback'}, {_UserInfo, _InvoiceID, PaymentID, ChargebackID, Params}}, St) ->
    #payproc_InvoicePaymentChargebackCancelParams{occurred_at = OccurredAt} = Params,
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    CancelResult = hg_invoice_payment:cancel_chargeback(ChargebackID, PaymentSession, Params),
    wrap_payment_impact(PaymentID, CancelResult, St, OccurredAt);
handle_call({{'Invoicing', 'RejectChargeback'}, {_UserInfo, _InvoiceID, PaymentID, ChargebackID, Params}}, St) ->
    #payproc_InvoicePaymentChargebackRejectParams{occurred_at = OccurredAt} = Params,
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    RejectResult = hg_invoice_payment:reject_chargeback(ChargebackID, PaymentSession, Params),
    wrap_payment_impact(PaymentID, RejectResult, St, OccurredAt);
handle_call({{'Invoicing', 'AcceptChargeback'}, {_UserInfo, _InvoiceID, PaymentID, ChargebackID, Params}}, St) ->
    #payproc_InvoicePaymentChargebackAcceptParams{occurred_at = OccurredAt} = Params,
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    AcceptResult = hg_invoice_payment:accept_chargeback(ChargebackID, PaymentSession, Params),
    wrap_payment_impact(PaymentID, AcceptResult, St, OccurredAt);
handle_call({{'Invoicing', 'ReopenChargeback'}, {_UserInfo, _InvoiceID, PaymentID, ChargebackID, Params}}, St) ->
    #payproc_InvoicePaymentChargebackReopenParams{occurred_at = OccurredAt} = Params,
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    ReopenResult = hg_invoice_payment:reopen_chargeback(ChargebackID, PaymentSession, Params),
    wrap_payment_impact(PaymentID, ReopenResult, St, OccurredAt);
handle_call({{'Invoicing', 'CreatePaymentAdjustment'}, {_UserInfo, _InvoiceID, PaymentID, Params}}, St) ->
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = #{timestamp := Timestamp} = get_payment_opts(St),
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:create_adjustment(Timestamp, Params, PaymentSession, Opts),
        St
    );
handle_call({{'Invoicing', 'CapturePaymentAdjustment'}, {_UserInfo, _InvoiceID, PaymentID, ID}}, St) ->
    _ = assert_invoice(accessible, St),
    PaymentSession = get_payment_session(PaymentID, St),
    Adjustment = hg_invoice_payment:get_adjustment(ID, PaymentSession),
    PaymentOpts = get_payment_opts(
        Adjustment#domain_InvoicePaymentAdjustment.party_revision,
        Adjustment#domain_InvoicePaymentAdjustment.created_at,
        St
    ),
    Impact = hg_invoice_payment:capture_adjustment(ID, PaymentSession, PaymentOpts),
    wrap_payment_impact(PaymentID, Impact, St);
handle_call({{'Invoicing', 'CancelPaymentAdjustment'}, {_UserInfo, _InvoiceID, PaymentID, ID}}, St) ->
    _ = assert_invoice(accessible, St),
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

-spec dispatch_callback(tag(), callback(), st()) -> call_result().
dispatch_callback(Tag, {provider, Payload}, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_call({callback, Tag, Payload}, PaymentID, PaymentSession, St);
dispatch_callback(_Tag, _Callback, _St) ->
    throw(invalid_callback).

assert_no_pending_payment(#st{activity = {payment, PaymentID}}) ->
    throw(?payment_pending(PaymentID));
assert_no_pending_payment(_) ->
    ok.

set_invoice_timer(Action, #st{invoice = Invoice} = St) ->
    set_invoice_timer(Invoice#domain_Invoice.status, Action, St).

set_invoice_timer(?invoice_unpaid(), Action, #st{invoice = #domain_Invoice{due = Due}}) ->
    hg_machine_action:set_deadline(Due, Action);
set_invoice_timer(_Status, Action, _St) ->
    Action.

capture_payment(PaymentSession, Reason, undefined, Cart, Opts) when Cart =/= undefined ->
    Cash = hg_invoice_utils:get_cart_amount(Cart),
    capture_payment(PaymentSession, Reason, Cash, Cart, Opts);
capture_payment(PaymentSession, Reason, Cash, Cart, Opts) ->
    hg_invoice_payment:capture(PaymentSession, Reason, Cash, Cart, Opts).

%%

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
                state => St
            }
    end.

do_start_payment(PaymentID, PaymentParams, St) ->
    _ = assert_invoice({status, unpaid}, St),
    _ = assert_no_pending_payment(St),
    Opts = #{timestamp := OccurredAt} = get_payment_opts(St),
    % TODO make timer reset explicit here
    {PaymentSession, {Changes, Action}} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts),
    #{
        response => get_payment_state(PaymentSession),
        changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
        action => Action,
        state => St
    }.

process_payment_signal(Signal, PaymentID, PaymentSession, St) ->
    {Revision, Timestamp} = hg_invoice_payment:get_party_revision(PaymentSession),
    Opts = get_payment_opts(Revision, Timestamp, St),
    PaymentResult = hg_invoice_payment:process_signal(Signal, PaymentSession, Opts),
    handle_payment_result(PaymentResult, PaymentID, PaymentSession, St, Opts).

process_payment_call(Call, PaymentID, PaymentSession, St) ->
    {Revision, Timestamp} = hg_invoice_payment:get_party_revision(PaymentSession),
    Opts = get_payment_opts(Revision, Timestamp, St),
    {Response, PaymentResult0} = hg_invoice_payment:process_call(Call, PaymentSession, Opts),
    PaymentResult1 = handle_payment_result(PaymentResult0, PaymentID, PaymentSession, St, Opts),
    PaymentResult1#{response => Response}.

handle_payment_result({next, {Changes, Action}}, PaymentID, _PaymentSession, St, Opts) ->
    #{timestamp := OccurredAt} = Opts,
    #{
        changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
        action => Action,
        state => St
    };
handle_payment_result({done, {Changes, Action}}, PaymentID, PaymentSession, St, Opts) ->
    Invoice = St#st.invoice,
    #{timestamp := OccurredAt} = Opts,
    PaymentSession1 = hg_invoice_payment:collapse_changes(Changes, PaymentSession),
    Payment = hg_invoice_payment:get_payment(PaymentSession1),
    case get_payment_status(Payment) of
        ?processed() ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
                action => Action,
                state => St
            };
        ?captured() ->
            MaybePaid =
                case Invoice of
                    #domain_Invoice{status = ?invoice_paid()} ->
                        [];
                    #domain_Invoice{} ->
                        [?invoice_status_changed(?invoice_paid())]
                end,
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt) ++ MaybePaid,
                action => Action,
                state => St
            };
        ?refunded() ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
                state => St
            };
        ?charged_back() ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
                state => St
            };
        ?failed(_) ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
                action => set_invoice_timer(Action, St),
                state => St
            };
        ?cancelled() ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
                action => set_invoice_timer(Action, St),
                state => St
            }
    end.

wrap_payment_changes(PaymentID, Changes, OccurredAt) ->
    [?payment_ev(PaymentID, C, OccurredAt) || C <- Changes].

wrap_payment_impact(PaymentID, {Response, {Changes, Action}}, St) ->
    wrap_payment_impact(PaymentID, {Response, {Changes, Action}}, St, undefined).

wrap_payment_impact(PaymentID, {Response, {Changes, Action}}, St, OccurredAt) ->
    #{
        response => Response,
        changes => wrap_payment_changes(PaymentID, Changes, OccurredAt),
        action => Action,
        state => St
    }.

wrap_adjustment_changes(AdjustmentID, Changes, OccurredAt) ->
    [?invoice_adjustment_ev(AdjustmentID, C, OccurredAt) || C <- Changes].

wrap_adjustment_impact(AdjustmentID, {Response, {Changes, Action}}, St, OccurredAt) ->
    #{
        response => Response,
        changes => wrap_adjustment_changes(AdjustmentID, Changes, OccurredAt),
        action => Action,
        state => St
    }.

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
                state => St
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
    RefundType =:= refund; RefundType =:= manual_refund
->
    wrap_payment_impact(
        PaymentID,
        hg_invoice_payment:RefundType(Params, PaymentSession, get_payment_opts(St)),
        St
    ).

%%

start_chargeback(Params, PaymentID, PaymentSession, PaymentOpts, St) ->
    #payproc_InvoicePaymentChargebackParams{id = ID} = Params,

    case get_chargeback_state(ID, PaymentSession) of
        undefined ->
            #payproc_InvoicePaymentChargebackParams{occurred_at = OccurredAt} = Params,
            CreateResult = hg_invoice_payment:create_chargeback(PaymentSession, PaymentOpts, Params),
            wrap_payment_impact(PaymentID, CreateResult, St, OccurredAt);
        ChargebackState ->
            #{
                response => hg_invoice_payment_chargeback:get(ChargebackState),
                state => St
            }
    end.

get_chargeback_state(ID, PaymentState) ->
    try
        hg_invoice_payment:get_chargeback_state(ID, PaymentState)
    catch
        throw:#payproc_InvoicePaymentChargebackNotFound{} ->
            undefined
    end.

%%

create_invoice(ID, InvoiceTplID, PartyRevision, V = #payproc_InvoiceParams{}) ->
    #domain_Invoice{
        id = ID,
        shop_id = V#payproc_InvoiceParams.shop_id,
        owner_id = V#payproc_InvoiceParams.party_id,
        party_revision = PartyRevision,
        created_at = hg_datetime:format_now(),
        status = ?invoice_unpaid(),
        cost = V#payproc_InvoiceParams.cost,
        due = V#payproc_InvoiceParams.due,
        details = V#payproc_InvoiceParams.details,
        context = V#payproc_InvoiceParams.context,
        template_id = InvoiceTplID,
        external_id = V#payproc_InvoiceParams.external_id,
        client_info = V#payproc_InvoiceParams.client_info
    }.

create_payment_id(#st{payments = Payments}) ->
    integer_to_binary(length(Payments) + 1).

get_payment_status(#domain_InvoicePayment{status = Status}) ->
    Status.

try_to_get_repair_state({complex, #payproc_InvoiceRepairComplex{scenarios = Scenarios}}, St) ->
    repair_complex(Scenarios, St);
try_to_get_repair_state(Scenario, St) ->
    repair_scenario(Scenario, St).

repair_complex([], St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    throw({exception, {activity_not_compatible_with_complex_scenario, Activity}});
repair_complex([Scenario | Rest], St) ->
    try
        repair_scenario(Scenario, St)
    catch
        throw:{exception, {activity_not_compatible_with_scenario, _, _}} ->
            repair_complex(Rest, St)
    end.

repair_scenario(Scenario, St = #st{activity = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    Activity = hg_invoice_payment:get_activity(PaymentSession),
    RepairSession = hg_invoice_repair:get_repair_state(Activity, Scenario, PaymentSession),
    process_payment_signal(timeout, PaymentID, RepairSession, St).

%%

-spec collapse_history([hg_machine:event()]) -> st().
collapse_history(History) ->
    lists:foldl(
        fun({_ID, Dt, Changes}, St0) ->
            collapse_changes(Changes, St0, #{timestamp => Dt})
        end,
        #st{},
        History
    ).

collapse_changes(Changes, St0, Opts) ->
    lists:foldl(fun(C, St) -> merge_change(C, St, Opts) end, St0, Changes).

merge_change(?invoice_created(Invoice), St, _Opts) ->
    St#st{activity = invoice, invoice = Invoice};
merge_change(?invoice_status_changed(Status), St = #st{invoice = I}, _Opts) ->
    St#st{invoice = I#domain_Invoice{status = Status}};
merge_change(?invoice_adjustment_ev(ID, Event), St, _Opts) ->
    St1 =
        case Event of
            ?invoice_adjustment_created(_Adjustment) ->
                St#st{activity = {adjustment_new, ID}};
            ?invoice_adjustment_status_changed({processed, _}) ->
                St#st{activity = {adjustment_pending, ID}};
            ?invoice_adjustment_status_changed(_Status) ->
                St#st{activity = invoice}
        end,
    Adjustment = merge_adjustment_change(Event, try_get_adjustment(ID, St1)),
    St2 = set_adjustment(ID, Adjustment, St1),
    case get_adjustment_status(Adjustment) of
        {captured, _} ->
            apply_adjustment_status(Adjustment, St2);
        _ ->
            St2
    end;
merge_change(?payment_ev(PaymentID, Change), St, Opts) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    PaymentSession1 = hg_invoice_payment:merge_change(Change, PaymentSession, Opts),
    St1 = set_payment_session(PaymentID, PaymentSession1, St),
    case hg_invoice_payment:get_activity(PaymentSession1) of
        A when A =/= idle ->
            % TODO Shouldn't we have here some kind of stack instead?
            St1#st{activity = {payment, PaymentID}};
        idle ->
            check_non_idle_payments(St1)
    end.

-spec check_non_idle_payments(st()) -> st().
check_non_idle_payments(#st{payments = Payments} = St) ->
    check_non_idle_payments_(Payments, St).

check_non_idle_payments_([], St) ->
    St#st{activity = invoice};
check_non_idle_payments_([{PaymentID, PaymentSession} | Rest], St) ->
    case hg_invoice_payment:get_activity(PaymentSession) of
        A when A =/= idle ->
            St#st{activity = {payment, PaymentID}};
        idle ->
            check_non_idle_payments_(Rest, St)
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

get_adjustment_params_target_status(#payproc_InvoiceAdjustmentParams{
    scenario = {status_change, #domain_InvoiceAdjustmentStatusChange{target_status = Status}}
}) ->
    Status.

get_invoice_status(#st{invoice = #domain_Invoice{status = Status}}) ->
    Status.

assert_adjustment_target_status(TargetStatus, Status) when TargetStatus =:= Status ->
    throw(#payproc_InvoiceAlreadyHasStatus{status = Status});
assert_adjustment_target_status({TargetStatus, _}, {Status, _}) when
    TargetStatus =:= unpaid, Status =/= paid; TargetStatus =:= paid, Status =/= unpaid
->
    throw(#payproc_InvoiceAdjustmentStatusUnacceptable{});
assert_adjustment_target_status(_TargetStatus, _Status) ->
    ok.

assert_all_adjustments_finalised(#st{adjustments = Adjustments}) ->
    lists:foreach(fun assert_adjustment_finalised/1, Adjustments).

assert_adjustment_finalised(#domain_InvoiceAdjustment{id = ID, status = {Status, _}}) when
    Status =:= pending; Status =:= processed
->
    throw(#payproc_InvoiceAdjustmentPending{id = ID});
assert_adjustment_finalised(_) ->
    ok.

assert_adjustment_processed(ID, #st{adjustments = Adjustments}) ->
    case lists:keyfind(ID, #domain_InvoiceAdjustment.id, Adjustments) of
        #domain_InvoiceAdjustment{status = {processed, _}} ->
            ok;
        #domain_InvoiceAdjustment{status = Status} ->
            throw(#payproc_InvalidInvoiceAdjustmentStatus{status = Status})
    end.

merge_adjustment_change(?invoice_adjustment_created(Adjustment), undefined) ->
    Adjustment;
merge_adjustment_change(?invoice_adjustment_status_changed(Status), Adjustment) ->
    Adjustment#domain_InvoiceAdjustment{status = Status}.

get_adjustment(ID, St) ->
    case try_get_adjustment(ID, St) of
        Adjustment = #domain_InvoiceAdjustment{} ->
            Adjustment;
        undefined ->
            throw(#payproc_InvoiceAdjustmentNotFound{})
    end.

try_get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoiceAdjustment.id, As) of
        V = #domain_InvoiceAdjustment{} ->
            V;
        false ->
            undefined
    end.

set_adjustment(ID, Adjustment, St = #st{adjustments = As}) ->
    St#st{adjustments = lists:keystore(ID, #domain_InvoiceAdjustment.id, As, Adjustment)}.

get_adjustment_status(#domain_InvoiceAdjustment{status = Status}) ->
    Status.

apply_adjustment_status(?adjustment_target_status(Status), St = #st{invoice = Invoice}) ->
    St#st{invoice = Invoice#domain_Invoice{status = Status}}.

create_adjustment_id(#st{adjustments = Adjustments}) ->
    integer_to_binary(length(Adjustments) + 1).

%%

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
    InvoiceParams = #payproc_InvoiceParams{
        party_id = PartyID,
        shop_id = ShopID,
        details = InvoiceDetails,
        due = InvoiceDue,
        cost = InvoiceCost,
        context = InvoiceContext,
        external_id = ExternalID
    },
    {Party, Shop, InvoiceParams}.

validate_invoice_params(#payproc_InvoiceParams{cost = Cost}, Shop, MerchantTerms) ->
    validate_invoice_cost(Cost, Shop, MerchantTerms).

validate_invoice_cost(Cost, Shop, #domain_TermSet{payments = PaymentTerms}) ->
    _ = hg_invoice_utils:validate_cost(Cost, Shop),
    _ = hg_invoice_utils:assert_cost_payable(Cost, PaymentTerms),
    ok.

get_merchant_terms(#domain_Party{id = PartyId, revision = PartyRevision}, Revision, Shop, Timestamp) ->
    {Client, Context} = get_party_client(),
    {ok, TermSet} = party_client_thrift:compute_contract_terms(
        PartyId,
        Shop#domain_Shop.contract_id,
        Timestamp,
        {revision, PartyRevision},
        Revision,
        #payproc_Varset{},
        Client,
        Context
    ),
    TermSet.

make_invoice_cart(_, {cart, Cart}, _Shop) ->
    Cart;
make_invoice_cart(Cost, {product, TplProduct}, Shop) ->
    #domain_InvoiceTemplateProduct{
        product = Product,
        price = TplPrice,
        metadata = Metadata
    } = TplProduct,
    #domain_InvoiceCart{
        lines = [
            #domain_InvoiceLine{
                product = Product,
                quantity = 1,
                price = get_templated_price(Cost, TplPrice, Shop),
                metadata = Metadata
            }
        ]
    }.

get_templated_price(undefined, {fixed, Cost}, Shop) ->
    get_cost(Cost, Shop);
get_templated_price(undefined, _, _Shop) ->
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

get_party_client() ->
    Ctx = hg_context:load(),
    {hg_context:get_party_client(Ctx), hg_context:get_party_client_context(Ctx)}.

log_changes(Changes, St) ->
    lists:foreach(fun(C) -> log_change(C, St) end, Changes).

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
get_log_params(?invoice_adjustment_ev(_ID, Change), #st{invoice = Invoice}) ->
    case hg_invoice_adjustment:get_log_params(Change) of
        {ok, Params} ->
            {ok,
                maps:update_with(
                    params,
                    fun(V) ->
                        [{invoice, get_invoice_params(Invoice)} | V]
                    end,
                    Params
                )};
        undefined ->
            undefined
    end;
get_log_params(?payment_ev(PaymentID, Change), St = #st{invoice = Invoice}) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    case hg_invoice_payment:get_log_params(Change, PaymentSession) of
        {ok, Params} ->
            {ok,
                maps:update_with(
                    params,
                    fun(V) ->
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

-spec marshal_event_payload([invoice_change()]) -> hg_machine:event_payload().
marshal_event_payload(Changes) when is_list(Changes) ->
    wrap_event_payload({invoice_changes, Changes}).

-spec marshal_invoice_params(invoice_params()) -> binary().
marshal_invoice_params(Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'InvoiceParams'}},
    hg_proto_utils:serialize(Type, Params).

%% Unmarshalling

-spec unmarshal_history([hg_machine:event()]) -> [hg_machine:event([invoice_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) -> hg_machine:event([invoice_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) -> [invoice_change()].
unmarshal_event_payload(#{format_version := 1, data := {bin, Changes}}) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    {invoice_changes, Buf} = hg_proto_utils:deserialize(Type, Changes),
    Buf;
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, changes}, Changes).

-spec unmarshal_invoice_params(binary()) -> invoice_params().
unmarshal_invoice_params(Bin) ->
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

unmarshal(change, [
    2,
    #{
        <<"change">> := <<"created">>,
        <<"invoice">> := Invoice
    }
]) ->
    ?invoice_created(unmarshal(invoice, Invoice));
unmarshal(change, [
    2,
    #{
        <<"change">> := <<"status_changed">>,
        <<"status">> := Status
    }
]) ->
    ?invoice_status_changed(unmarshal(status, Status));
unmarshal(change, [
    2,
    #{
        <<"change">> := <<"payment_change">>,
        <<"id">> := PaymentID,
        <<"payload">> := Payload
    }
]) ->
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

unmarshal(
    invoice,
    #{
        <<"id">> := ID,
        <<"shop_id">> := ShopID,
        <<"owner_id">> := PartyID,
        <<"created_at">> := CreatedAt,
        <<"cost">> := Cash,
        <<"due">> := Due,
        <<"details">> := Details
    } = Invoice
) ->
    Context = maps:get(<<"context">>, Invoice, undefined),
    TemplateID = maps:get(<<"template_id">>, Invoice, undefined),
    ExternalID = maps:get(<<"external_id">>, Invoice, undefined),
    #domain_Invoice{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        owner_id = unmarshal(str, PartyID),
        party_revision = maps:get(<<"party_revision">>, Invoice, undefined),
        created_at = unmarshal(str, CreatedAt),
        cost = hg_cash:unmarshal(Cash),
        due = unmarshal(str, Due),
        details = unmarshal(details, Details),
        status = ?invoice_unpaid(),
        context = hg_content:unmarshal(Context),
        template_id = unmarshal(str, TemplateID),
        external_id = unmarshal(str, ExternalID)
    };
unmarshal(
    invoice,
    ?legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cash, Context, TemplateID)
) ->
    #domain_Invoice{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        owner_id = unmarshal(str, PartyID),
        created_at = unmarshal(str, CreatedAt),
        cost = hg_cash:unmarshal([1, Cash]),
        due = unmarshal(str, Due),
        details = unmarshal(details, Details),
        status = unmarshal(status, Status),
        context = hg_content:unmarshal(Context),
        template_id = unmarshal(str, TemplateID)
    };
unmarshal(
    invoice,
    ?legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cash, Context)
) ->
    unmarshal(
        invoice,
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
        product = unmarshal(str, Product),
        description = unmarshal(str, Description),
        cart = unmarshal(cart, Cart)
    };
unmarshal(details, ?legacy_invoice_details(Product, Description)) ->
    #domain_InvoiceDetails{
        product = unmarshal(str, Product),
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
        refund = #domain_InvoicePaymentRefund{
            id = genlib:to_binary(ID),
            created_at = hg_datetime:format_now(),
            domain_revision = 42,
            party_revision = 42,
            status = ?refund_pending(),
            reason = <<"No reason">>,
            cash = ?cash(1000, <<"RUB">>),
            cart = undefined
        },
        sessions = []
    }.

-spec construct_refund_id_test() -> _.
construct_refund_id_test() ->
    % 10 IDs shuffled
    IDs = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- lists:seq(1, 10)])],
    Refunds = lists:map(fun create_dummy_refund_with_id/1, IDs),
    ?assert(<<"11">> =:= construct_refund_id(Refunds)).

-endif.
