-module(hg_invoice_payment_chargeback).

-include("domain.hrl").
-include("payment_events.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export(
    [
        create/2,
        cancel/2,
        reject/3,
        accept/3,
        reopen/3
    ]
).

-export(
    [
        merge_change/2,
        process_timeout/5
    ]
).

-export(
    [
        get/1,
        get_body/1,
        get_cash_flow/1,
        get_status/1,
        get_target_status/1,
        is_pending/1
    ]
).

-export_type(
    [
        id/0,
        opts/0,
        state/0,
        activity/0
    ]
).

-export_type(
    [
        create_params/0,
        cancel_params/0,
        accept_params/0,
        reject_params/0,
        reopen_params/0
    ]
).

-record(chargeback_st, {
    chargeback :: undefined | chargeback(),
    target_status :: undefined | status(),
    cash_flow = [] :: cash_flow(),
    cash_flow_plans = #{
        ?chargeback_stage_chargeback() => [],
        ?chargeback_stage_pre_arbitration() => [],
        ?chargeback_stage_arbitration() => []
    } ::
        cash_flow_plans()
}).

-type state() :: #chargeback_st{}.

-type cash_flow_plans() :: #{
    ?chargeback_stage_chargeback() := [batch()],
    ?chargeback_stage_pre_arbitration() := [batch()],
    ?chargeback_stage_arbitration() := [batch()]
}.

-type opts() :: #{
    payment_state := payment_state(),
    party := party(),
    invoice := invoice(),
    timestamp := hg_datetime:timestamp()
}.

-type payment_state() ::
    hg_invoice_payment:st().

-type clock() ::
    hg_accounting_new:clock().

-type party() ::
    dmsl_domain_thrift:'Party'().

-type invoice() ::
    dmsl_domain_thrift:'Invoice'().

-type chargeback() ::
    dmsl_domain_thrift:'InvoicePaymentChargeback'().

-type id() ::
    dmsl_domain_thrift:'InvoicePaymentChargebackID'().

-type status() ::
    dmsl_domain_thrift:'InvoicePaymentChargebackStatus'().

-type stage() ::
    dmsl_domain_thrift:'InvoicePaymentChargebackStage'().

-type timestamp() ::
    dmsl_base_thrift:'Timestamp'().

-type revision() ::
    dmsl_domain_thrift:'DataRevision'().

-type cash() ::
    dmsl_domain_thrift:'Cash'().

-type cash_flow() ::
    dmsl_domain_thrift:'FinalCashFlow'().

-type batch() ::
    hg_accounting_new:batch().

-type create_params() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackParams'().

-type cancel_params() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackCancelParams'().

-type accept_params() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackAcceptParams'().

-type reject_params() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackRejectParams'().

-type reopen_params() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackReopenParams'().

-type result() ::
    {[change()], action()}.

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChargebackChangePayload'().

-type action() ::
    hg_machine_action:t().

-type activity() ::
    preparing_initial_cash_flow
    | holding_initial_cash_flow
    | updating_chargeback
    | updating_cash_flow
    | holding_updated_cash_flow
    | finalising_accounter.

-spec get(state()) -> chargeback().
get(#chargeback_st{chargeback = Chargeback}) ->
    Chargeback.

-spec get_body(state() | chargeback()) -> cash().
get_body(#chargeback_st{chargeback = Chargeback}) ->
    get_body(Chargeback);
get_body(#domain_InvoicePaymentChargeback{body = Body}) ->
    Body.

-spec get_status(state() | chargeback()) -> status().
get_status(#chargeback_st{chargeback = Chargeback}) ->
    get_status(Chargeback);
get_status(#domain_InvoicePaymentChargeback{status = Status}) ->
    Status.

-spec get_target_status(state()) -> status() | undefined.
get_target_status(#chargeback_st{target_status = TargetStatus}) ->
    TargetStatus.

-spec get_cash_flow(state()) -> cash_flow().
get_cash_flow(#chargeback_st{cash_flow = CashFlow}) ->
    CashFlow.

-spec is_pending(chargeback() | state()) -> boolean().
is_pending(#chargeback_st{chargeback = Chargeback}) ->
    is_pending(Chargeback);
is_pending(#domain_InvoicePaymentChargeback{status = ?chargeback_status_pending()}) ->
    true;
is_pending(#domain_InvoicePaymentChargeback{status = _NotPending}) ->
    false.

%%----------------------------------------------------------------------------
%% @doc
%% create/2 creates a chargeback. A chargeback will not be created if
%% another one is already pending, and it will block refunds from being
%% created as well.
%%
%% Key parameters:
%%    levy: the amount of cash to be levied from the merchant.
%%    body: The sum of the chargeback.
%%            Will default to full remaining amount if undefined.
%% @end
%%----------------------------------------------------------------------------
-spec create(opts(), create_params()) -> {chargeback(), result()} | no_return().
create(Opts, CreateParams) ->
    do_create(Opts, CreateParams).

%%----------------------------------------------------------------------------
%% @doc
%% cancel/1 will cancel the given chargeback. All funds
%% will be trasferred back to the merchant as a result of this operation.
%% @end
%%----------------------------------------------------------------------------
-spec cancel(state(), cancel_params()) -> {ok, result()} | no_return().
cancel(State, CancelParams) ->
    do_cancel(State, CancelParams).

%%----------------------------------------------------------------------------
%% @doc
%% reject/3 will reject the given chargeback, implying that no
%% sufficient evidence has been found to support the chargeback claim.
%%
%% Key parameters:
%%    levy: the amount of cash to be levied from the merchant.
%% @end
%%----------------------------------------------------------------------------
-spec reject(state(), payment_state(), reject_params()) -> {ok, result()} | no_return().
reject(State, PaymentState, RejectParams) ->
    do_reject(State, PaymentState, RejectParams).

%%----------------------------------------------------------------------------
%% @doc
%% accept/3 will accept the given chargeback, implying that
%% sufficient evidence has been found to support the chargeback claim. The
%% cost of the chargeback will be deducted from the merchant's account.
%%
%% Key parameters:
%%    levy: the amount of cash to be levied from the merchant.
%%          Will not change if undefined.
%%    body: The sum of the chargeback.
%%          Will not change if undefined.
%% @end
%%----------------------------------------------------------------------------
-spec accept(state(), payment_state(), accept_params()) -> {ok, result()} | no_return().
accept(State, PaymentState, AcceptParams) ->
    do_accept(State, PaymentState, AcceptParams).

%%----------------------------------------------------------------------------
%% @doc
%% reopen/3 will reopen the given chargeback, implying that
%% the party that initiated the chargeback was not satisfied with the result
%% and demands a new investigation. The chargeback progresses to its next
%% stage as a result of this action.
%%
%% Key parameters:
%%    levy: the amount of cash to be levied from the merchant.
%%    body: The sum of the chargeback. Will not change if undefined.
%% @end
%%----------------------------------------------------------------------------
-spec reopen(state(), payment_state(), reopen_params()) -> {ok, result()} | no_return().
reopen(State, PaymentState, ReopenParams) ->
    do_reopen(State, PaymentState, ReopenParams).

%

-spec merge_change(change(), state()) -> state().
merge_change(?chargeback_created(Chargeback), State) ->
    set(Chargeback, State);
merge_change(?chargeback_levy_changed(Levy), State) ->
    set_levy(Levy, State);
merge_change(?chargeback_body_changed(Body), State) ->
    set_body(Body, State);
merge_change(?chargeback_stage_changed(Stage), State) ->
    set_stage(Stage, State);
merge_change(?chargeback_target_status_changed(Status), State) ->
    set_target_status(Status, State);
merge_change(?chargeback_status_changed(Status), State) ->
    set_target_status(undefined, set_status(Status, State));
merge_change(?chargeback_clock_update(_), State) ->
    State;
merge_change(?chargeback_cash_flow_changed(CashFlow), State) ->
    set_cash_flow(CashFlow, State).

-spec process_timeout(activity(), state(), payment_state(), action(), opts()) -> result().
process_timeout(preparing_initial_cash_flow, State, _PaymentSt, _Action, Opts) ->
    update_cash_flow(State, hg_machine_action:instant(), Opts);
process_timeout(holding_initial_cash_flow, State, PaymentSt, _Action, Opts) ->
    hold_cash_flow(State, PaymentSt, hg_machine_action:new(), Opts);
process_timeout(updating_cash_flow, State, _PaymentSt, _Action, Opts) ->
    update_cash_flow(State, hg_machine_action:instant(), Opts);
process_timeout(holding_updated_cash_flow, State, PaymentSt, _Action, Opts) ->
    hold_cash_flow(State, PaymentSt, hg_machine_action:instant(), Opts);
process_timeout(finalising_accounter, State, PaymentSt, Action, Opts) ->
    finalise(State, PaymentSt, Action, Opts).

%% Private

-spec do_create(opts(), create_params()) -> {chargeback(), result()} | no_return().
do_create(Opts, CreateParams = ?chargeback_params(Levy, Body, _Reason)) ->
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    Invoice = get_opts_invoice(Opts),
    Party = get_opts_party(Opts),
    Payment = get_opts_payment(Opts),
    ShopID = get_invoice_shop_id(Invoice),
    Shop = hg_party:get_shop(ShopID, Party),
    VS = collect_validation_varset(Party, Shop, Payment, Body),
    ServiceTerms = get_merchant_chargeback_terms(Party, Shop, VS, Revision, CreatedAt),
    _ = validate_currency(Body, Payment),
    _ = validate_currency(Levy, Payment),
    _ = validate_body_amount(Body, get_opts_payment_state(Opts)),
    _ = validate_contract_active(get_contract(Party, Shop)),
    _ = validate_service_terms(ServiceTerms),
    _ = validate_eligibility_time(ServiceTerms),
    Chargeback = build_chargeback(Opts, CreateParams, Revision, CreatedAt),
    Action = hg_machine_action:instant(),
    Result = {[?chargeback_created(Chargeback)], Action},
    {Chargeback, Result}.

-spec do_cancel(state(), cancel_params()) -> {ok, result()} | no_return().
do_cancel(State, ?cancel_params()) ->
    % TODO: might be reasonable to ensure that
    %       there actually is a cashflow to cancel
    % _ = validate_cash_flow_held(State),
    _ = validate_chargeback_is_pending(State),
    Action = hg_machine_action:instant(),
    Status = ?chargeback_status_cancelled(),
    Result = {[?chargeback_target_status_changed(Status)], Action},
    {ok, Result}.

-spec do_reject(state(), payment_state(), reject_params()) -> {ok, result()} | no_return().
do_reject(State, PaymentState, RejectParams = ?reject_params(Levy)) ->
    _ = validate_chargeback_is_pending(State),
    _ = validate_currency(Levy, hg_invoice_payment:get_payment(PaymentState)),
    Result = build_reject_result(State, RejectParams),
    {ok, Result}.

-spec do_accept(state(), payment_state(), accept_params()) -> {ok, result()} | no_return().
do_accept(State, PaymentState, AcceptParams = ?accept_params(Levy, Body)) ->
    _ = validate_chargeback_is_pending(State),
    _ = validate_currency(Body, hg_invoice_payment:get_payment(PaymentState)),
    _ = validate_currency(Levy, hg_invoice_payment:get_payment(PaymentState)),
    _ = validate_body_amount(Body, PaymentState),
    Result = build_accept_result(State, AcceptParams),
    {ok, Result}.

-spec do_reopen(state(), payment_state(), reopen_params()) -> {ok, result()} | no_return().
do_reopen(State, PaymentState, ReopenParams = ?reopen_params(Levy, Body)) ->
    _ = validate_chargeback_is_rejected(State),
    _ = validate_not_arbitration(State),
    _ = validate_currency(Body, hg_invoice_payment:get_payment(PaymentState)),
    _ = validate_currency(Levy, hg_invoice_payment:get_payment(PaymentState)),
    _ = validate_body_amount(Body, PaymentState),
    Result = build_reopen_result(State, ReopenParams),
    {ok, Result}.

%%

-spec update_cash_flow(state(), action(), opts()) -> result() | no_return().
update_cash_flow(State, Action, Opts) ->
    FinalCashFlow = build_chargeback_cash_flow(State, Opts),
    {[?chargeback_cash_flow_changed(FinalCashFlow)], Action}.

-spec hold_cash_flow(state(), payment_state(), action(), opts()) -> result() | no_return().
hold_cash_flow(State, PaymentSt, Action, Opts) ->
    CashFlowPlan = get_current_plan(State),
    {ok, Clock} = prepare_cash_flow(get_clock(PaymentSt), State, CashFlowPlan, Opts),
    {[?chargeback_clock_update(Clock)], Action}.

-spec finalise(state(), payment_state(), action(), opts()) -> result() | no_return().
finalise(#chargeback_st{target_status = Status = ?chargeback_status_pending()}, _PaymentSt, Action, _Opts) ->
    {[?chargeback_status_changed(Status)], Action};
finalise(State = #chargeback_st{target_status = Status}, PaymentSt, Action, Opts) when
    Status =:= ?chargeback_status_rejected();
    Status =:= ?chargeback_status_accepted();
    Status =:= ?chargeback_status_cancelled()
->
    case commit_cash_flow(get_clock(PaymentSt), State, Opts) of
        {ok, Clock} ->
            {[?chargeback_clock_update(Clock), ?chargeback_status_changed(Status)], Action};
        {error, not_ready} ->
            woody_error:raise(system, {external, resource_unavailable, <<"Accounter was not ready">>})
    end.

-spec build_chargeback(opts(), create_params(), revision(), timestamp()) -> chargeback() | no_return().
build_chargeback(Opts, Params = ?chargeback_params(Levy, Body, Reason), Revision, CreatedAt) ->
    Revision = hg_domain:head(),
    PartyRevision = get_opts_party_revision(Opts),
    #domain_InvoicePaymentChargeback{
        id = Params#payproc_InvoicePaymentChargebackParams.id,
        levy = Levy,
        body = define_body(Body, get_opts_payment_state(Opts)),
        created_at = CreatedAt,
        stage = ?chargeback_stage_chargeback(),
        status = ?chargeback_status_pending(),
        domain_revision = Revision,
        party_revision = PartyRevision,
        reason = Reason
    }.

-spec build_reject_result(state(), reject_params()) -> result() | no_return().
build_reject_result(State, ?reject_params(ParamsLevy)) ->
    Levy = get_levy(State),
    Action = hg_machine_action:instant(),
    LevyChange = levy_change(ParamsLevy, Levy),
    Status = ?chargeback_status_rejected(),
    StatusChange = [?chargeback_target_status_changed(Status)],
    Changes = lists:append([LevyChange, StatusChange]),
    {Changes, Action}.

-spec build_accept_result(state(), accept_params()) -> result() | no_return().
build_accept_result(State, ?accept_params(ParamsLevy, ParamsBody)) ->
    Body = get_body(State),
    Levy = get_levy(State),
    Action = hg_machine_action:instant(),
    BodyChange = body_change(ParamsBody, Body),
    LevyChange = levy_change(ParamsLevy, Levy),
    Status = ?chargeback_status_accepted(),
    StatusChange = [?chargeback_target_status_changed(Status)],
    Changes = lists:append([BodyChange, LevyChange, StatusChange]),
    {Changes, Action}.

-spec build_reopen_result(state(), reopen_params()) -> result() | no_return().
build_reopen_result(State, ?reopen_params(ParamsLevy, ParamsBody) = Params) ->
    Body = get_body(State),
    Levy = get_levy(State),
    Stage = get_reopen_stage(State, Params),
    Action = hg_machine_action:instant(),
    BodyChange = body_change(ParamsBody, Body),
    LevyChange = levy_change(ParamsLevy, Levy),
    StageChange = [?chargeback_stage_changed(Stage)],
    Status = ?chargeback_status_pending(),
    StatusChange = [?chargeback_target_status_changed(Status)],
    Changes = lists:append([StageChange, BodyChange, LevyChange, StatusChange]),
    {Changes, Action}.

-spec build_chargeback_cash_flow(state(), opts()) -> cash_flow() | no_return().
build_chargeback_cash_flow(#chargeback_st{target_status = ?chargeback_status_cancelled()}, _Opts) ->
    [];
build_chargeback_cash_flow(State, Opts) ->
    CreatedAt = get_created_at(State),
    Revision = get_revision(State),
    Body = get_body(State),
    Payment = get_opts_payment(Opts),
    Invoice = get_opts_invoice(Opts),
    Route = get_opts_route(Opts),
    Party = get_opts_party(Opts),
    ShopID = get_invoice_shop_id(Invoice),
    Shop = hg_party:get_shop(ShopID, Party),
    VS = collect_validation_varset(Party, Shop, Payment, Body),
    ServiceTerms = get_merchant_chargeback_terms(Party, Shop, VS, Revision, CreatedAt),
    PaymentsTerms = hg_routing:get_payments_terms(Route, Revision),
    ProviderTerms = get_provider_chargeback_terms(PaymentsTerms, Payment),
    ServiceCashFlow = get_chargeback_service_cash_flow(ServiceTerms),
    ProviderCashFlow = get_chargeback_provider_cash_flow(ProviderTerms),
    ProviderFees = collect_chargeback_provider_fees(ProviderTerms),
    PaymentInstitutionRef = get_payment_institution_ref(get_contract(Party, Shop)),
    PaymentInst = hg_payment_institution:compute_payment_institution(PaymentInstitutionRef, VS, Revision),
    Provider = get_route_provider(Route, Revision),
    AccountMap = hg_accounting_new:collect_account_map(Payment, Shop, PaymentInst, Provider, VS, Revision),
    ServiceContext = build_service_cash_flow_context(State),
    ProviderContext = build_provider_cash_flow_context(State, ProviderFees),
    ServiceFinalCF = hg_cashflow:finalize(ServiceCashFlow, ServiceContext, AccountMap),
    ProviderFinalCF = hg_cashflow:finalize(ProviderCashFlow, ProviderContext, AccountMap),
    ServiceFinalCF ++ ProviderFinalCF.

build_service_cash_flow_context(State) ->
    #{operation_amount => get_body(State), surplus => get_levy(State)}.

build_provider_cash_flow_context(State, Fees) ->
    FeesContext = #{operation_amount => get_body(State)},
    ComputedFees = maps:map(fun(_K, V) -> hg_cashflow:compute_volume(V, FeesContext) end, Fees),
    case get_target_status(State) of
        ?chargeback_status_rejected() ->
            ?cash(_Amount, SymCode) = get_body(State),
            maps:merge(ComputedFees, #{operation_amount => ?cash(0, SymCode)});
        _NotRejected ->
            maps:merge(ComputedFees, #{operation_amount => get_body(State)})
    end.

get_chargeback_service_cash_flow(#domain_PaymentChargebackServiceTerms{fees = {value, V}}) ->
    V;
get_chargeback_service_cash_flow(_) ->
    throw(#payproc_OperationNotPermitted{}).

get_chargeback_provider_cash_flow(#domain_PaymentChargebackProvisionTerms{cash_flow = {value, V}}) ->
    V;
get_chargeback_provider_cash_flow(_) ->
    throw(#payproc_OperationNotPermitted{}).

collect_chargeback_provider_fees(#domain_PaymentChargebackProvisionTerms{fees = undefined}) ->
    #{};
collect_chargeback_provider_fees(#domain_PaymentChargebackProvisionTerms{fees = {value, Fees}}) ->
    Fees#domain_Fees.fees.

get_merchant_chargeback_terms(#domain_Party{id = PartyId, revision = PartyRevision}, Shop, VS, Revision, Timestamp) ->
    {Client, Context} = get_party_client(),
    {ok, #domain_TermSet{payments = PaymentsTerms}} = party_client_thrift:compute_contract_terms(
        PartyId,
        Shop#domain_Shop.contract_id,
        Timestamp,
        {revision, PartyRevision},
        Revision,
        hg_varset:prepare_varset(VS),
        Client,
        Context
    ),
    PaymentsTerms#domain_PaymentsServiceTerms.chargebacks.

get_contract(Party, #domain_Shop{contract_id = ContractID}) ->
    hg_party:get_contract(ContractID, Party).

get_party_client() ->
    Ctx = hg_context:load(),
    {hg_context:get_party_client(Ctx), hg_context:get_party_client_context(Ctx)}.

get_provider_chargeback_terms(#domain_PaymentsProvisionTerms{chargebacks = Terms}, _Payment) ->
    Terms.

define_body(undefined, PaymentState) ->
    hg_invoice_payment:get_remaining_payment_balance(PaymentState);
define_body(Cash, _PaymentState) ->
    Cash.

prepare_cash_flow(Clock, State, CashFlowPlan, Opts) ->
    #{timestamp := Timestamp} = Opts,
    PlanID = construct_chargeback_plan_id(State, Opts),
    hg_accounting_new:plan(PlanID, CashFlowPlan, Timestamp, Clock).

commit_cash_flow(Clock, State, Opts) ->
    #{timestamp := Timestamp} = Opts,
    CashFlowPlan = get_current_plan(State),
    PlanID = construct_chargeback_plan_id(State, Opts),
    hg_accounting_new:commit(PlanID, CashFlowPlan, Timestamp, Clock).

construct_chargeback_plan_id(State, Opts) ->
    {Stage, _} = get_stage(State),
    hg_utils:construct_complex_id([
        get_opts_invoice_id(Opts),
        get_opts_payment_id(Opts),
        {chargeback, get_id(State)},
        genlib:to_binary(Stage)
    ]).

collect_validation_varset(Party, Shop, Payment, Body) ->
    #domain_Party{id = PartyID} = Party,
    #domain_Shop{
        id = ShopID,
        category = Category,
        account = #domain_ShopAccount{currency = Currency}
    } = Shop,
    #{
        party_id => PartyID,
        shop_id => ShopID,
        category => Category,
        currency => Currency,
        cost => Body,
        payment_tool => get_payment_tool(Payment)
    }.

%% Validations

validate_eligibility_time(#domain_PaymentChargebackServiceTerms{eligibility_time = undefined}) ->
    ok;
validate_eligibility_time(#domain_PaymentChargebackServiceTerms{eligibility_time = {value, EligibilityTime}}) ->
    Now = hg_datetime:format_now(),
    EligibleUntil = hg_datetime:add_time_span(EligibilityTime, Now),
    case hg_datetime:compare(Now, EligibleUntil) of
        later -> throw(#payproc_OperationNotPermitted{});
        _NotLater -> ok
    end.

validate_service_terms(#domain_PaymentChargebackServiceTerms{allow = {constant, true}}) ->
    ok;
validate_service_terms(undefined) ->
    throw(#payproc_OperationNotPermitted{}).

validate_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
validate_contract_active(#domain_Contract{status = Status}) ->
    throw(#payproc_InvalidContractStatus{status = Status}).

validate_body_amount(undefined, _PaymentState) ->
    ok;
validate_body_amount(?cash(_, _) = Cash, PaymentState) ->
    InterimPaymentAmount = hg_invoice_payment:get_remaining_payment_balance(PaymentState),
    PaymentAmount = hg_cash:sub(InterimPaymentAmount, Cash),
    validate_remaining_payment_amount(PaymentAmount, InterimPaymentAmount).

validate_remaining_payment_amount(?cash(Amount, _), _) when Amount >= 0 ->
    ok;
validate_remaining_payment_amount(?cash(Amount, _), Maximum) when Amount < 0 ->
    throw(#payproc_InvoicePaymentAmountExceeded{maximum = Maximum}).

validate_currency(?cash(_, SymCode), #domain_InvoicePayment{cost = ?cash(_, SymCode)}) ->
    ok;
validate_currency(undefined, _Payment) ->
    ok;
validate_currency(?cash(_, SymCode), _Payment) ->
    throw(#payproc_InconsistentChargebackCurrency{currency = SymCode}).

validate_not_arbitration(#chargeback_st{chargeback = Chargeback}) ->
    validate_not_arbitration(Chargeback);
validate_not_arbitration(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_arbitration()}) ->
    throw(#payproc_InvoicePaymentChargebackCannotReopenAfterArbitration{});
validate_not_arbitration(#domain_InvoicePaymentChargeback{}) ->
    ok.

validate_chargeback_is_rejected(#chargeback_st{chargeback = Chargeback}) ->
    validate_chargeback_is_rejected(Chargeback);
validate_chargeback_is_rejected(#domain_InvoicePaymentChargeback{status = ?chargeback_status_rejected()}) ->
    ok;
validate_chargeback_is_rejected(#domain_InvoicePaymentChargeback{status = Status}) ->
    throw(#payproc_InvoicePaymentChargebackInvalidStatus{status = Status}).

validate_chargeback_is_pending(#chargeback_st{chargeback = Chargeback}) ->
    validate_chargeback_is_pending(Chargeback);
validate_chargeback_is_pending(#domain_InvoicePaymentChargeback{status = ?chargeback_status_pending()}) ->
    ok;
validate_chargeback_is_pending(#domain_InvoicePaymentChargeback{status = Status}) ->
    throw(#payproc_InvoicePaymentChargebackInvalidStatus{status = Status}).

%% Getters

-spec get_id(state() | chargeback()) -> id().
get_id(#chargeback_st{chargeback = Chargeback}) ->
    get_id(Chargeback);
get_id(#domain_InvoicePaymentChargeback{id = ID}) ->
    ID.

-spec get_current_plan(state()) -> [batch()].
get_current_plan(#chargeback_st{cash_flow_plans = Plans} = State) ->
    Stage = get_stage(State),
    #{Stage := Plan} = Plans,
    Plan.

-spec get_clock(payment_state()) -> clock().
get_clock(PaymentSt) ->
    hg_invoice_payment:get_clock(PaymentSt).

-spec get_reverted_previous_stage(state()) -> [batch()].
get_reverted_previous_stage(State) ->
    case get_previous_stage(State) of
        undefined ->
            [];
        _Stage ->
            #chargeback_st{cash_flow = CashFlow} = State,
            add_batch(hg_cashflow:revert(CashFlow), [])
    end.

-spec get_revision(state() | chargeback()) -> revision().
get_revision(#chargeback_st{chargeback = Chargeback}) ->
    get_revision(Chargeback);
get_revision(#domain_InvoicePaymentChargeback{domain_revision = Revision}) ->
    Revision.

-spec get_created_at(state() | chargeback()) -> timestamp().
get_created_at(#chargeback_st{chargeback = Chargeback}) ->
    get_created_at(Chargeback);
get_created_at(#domain_InvoicePaymentChargeback{created_at = CreatedAt}) ->
    CreatedAt.

-spec get_levy(state() | chargeback()) -> cash().
get_levy(#chargeback_st{chargeback = Chargeback}) ->
    get_levy(Chargeback);
get_levy(#domain_InvoicePaymentChargeback{levy = Levy}) ->
    Levy.

-spec get_stage(state() | chargeback()) -> stage().
get_stage(#chargeback_st{chargeback = Chargeback}) ->
    get_stage(Chargeback);
get_stage(#domain_InvoicePaymentChargeback{stage = Stage}) ->
    Stage.

-spec get_reopen_stage(state() | chargeback(), reopen_params()) ->
    ?chargeback_stage_pre_arbitration() | ?chargeback_stage_arbitration().
get_reopen_stage(#chargeback_st{chargeback = Chargeback}, ReopenParams) ->
    get_reopen_stage(Chargeback, ReopenParams);
get_reopen_stage(#domain_InvoicePaymentChargeback{stage = CurrentStage} = Chargeback, Params) ->
    case Params#payproc_InvoicePaymentChargebackReopenParams.move_to_stage of
        undefined ->
            get_next_stage(Chargeback);
        Stage when Stage =/= CurrentStage, CurrentStage =:= ?chargeback_stage_chargeback() ->
            Stage;
        _Other ->
            throw(#payproc_InvoicePaymentChargebackInvalidStage{stage = CurrentStage})
    end.

-spec get_next_stage(chargeback()) -> ?chargeback_stage_pre_arbitration() | ?chargeback_stage_arbitration().
get_next_stage(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_chargeback()}) ->
    ?chargeback_stage_pre_arbitration();
get_next_stage(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_pre_arbitration()}) ->
    ?chargeback_stage_arbitration().

-spec get_previous_stage(state() | chargeback()) ->
    undefined | ?chargeback_stage_pre_arbitration() | ?chargeback_stage_chargeback().
get_previous_stage(#chargeback_st{chargeback = Chargeback}) ->
    get_previous_stage(Chargeback);
get_previous_stage(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_chargeback()}) ->
    undefined;
get_previous_stage(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_pre_arbitration()}) ->
    ?chargeback_stage_chargeback();
get_previous_stage(#domain_InvoicePaymentChargeback{stage = ?chargeback_stage_arbitration()}) ->
    ?chargeback_stage_pre_arbitration().

%% Setters

-spec set(chargeback(), state() | undefined) -> state().
set(Chargeback, undefined) ->
    #chargeback_st{chargeback = Chargeback};
set(Chargeback, #chargeback_st{} = State) ->
    State#chargeback_st{chargeback = Chargeback}.

-spec set_cash_flow(cash_flow(), state()) -> state().
set_cash_flow(CashFlow, #chargeback_st{cash_flow_plans = Plans} = State) ->
    Stage = get_stage(State),
    Plan = build_updated_plan(CashFlow, State),
    State#chargeback_st{cash_flow_plans = Plans#{Stage := Plan}, cash_flow = CashFlow}.

-spec set_target_status(status() | undefined, state()) -> state().
set_target_status(TargetStatus, #chargeback_st{} = State) ->
    State#chargeback_st{target_status = TargetStatus}.

-spec set_status(status(), state()) -> state().
set_status(Status, #chargeback_st{chargeback = Chargeback} = State) ->
    State#chargeback_st{
        chargeback = Chargeback#domain_InvoicePaymentChargeback{status = Status}
    }.

-spec set_body(cash(), state()) -> state().
set_body(Cash, #chargeback_st{chargeback = Chargeback} = State) ->
    State#chargeback_st{
        chargeback = Chargeback#domain_InvoicePaymentChargeback{body = Cash}
    }.

-spec set_levy(cash(), state()) -> state().
set_levy(Cash, #chargeback_st{chargeback = Chargeback} = State) ->
    State#chargeback_st{
        chargeback = Chargeback#domain_InvoicePaymentChargeback{levy = Cash}
    }.

-spec set_stage(stage(), state()) -> state().
set_stage(Stage, #chargeback_st{chargeback = Chargeback} = State) ->
    State#chargeback_st{
        chargeback = Chargeback#domain_InvoicePaymentChargeback{stage = Stage}
    }.

%%

get_route_provider(#domain_PaymentRoute{provider = ProviderRef}, Revision) ->
    hg_domain:get(Revision, {provider, ProviderRef}).

%%

get_payment_institution_ref(Contract) ->
    Contract#domain_Contract.payment_institution.

%%

get_opts_party(#{party := Party}) ->
    Party.

get_opts_party_revision(#{party := Party}) ->
    Party#domain_Party.revision.

get_opts_invoice(#{invoice := Invoice}) ->
    Invoice.

get_opts_payment_state(#{payment_state := PaymentState}) ->
    PaymentState.

get_opts_payment(#{payment_state := PaymentState}) ->
    hg_invoice_payment:get_payment(PaymentState).

get_opts_route(#{payment_state := PaymentState}) ->
    hg_invoice_payment:get_route(PaymentState).

get_opts_invoice_id(Opts) ->
    #domain_Invoice{id = ID} = get_opts_invoice(Opts),
    ID.

get_opts_payment_id(Opts) ->
    #domain_InvoicePayment{id = ID} = get_opts_payment(Opts),
    ID.

%%

get_payment_tool(#domain_InvoicePayment{payer = Payer}) ->
    get_payer_payment_tool(Payer).

get_payer_payment_tool(?payment_resource_payer(PaymentResource, _ContactInfo)) ->
    get_resource_payment_tool(PaymentResource);
get_payer_payment_tool(?customer_payer(_CustomerID, _, _, PaymentTool, _)) ->
    PaymentTool;
get_payer_payment_tool(?recurrent_payer(PaymentTool, _, _)) ->
    PaymentTool.

get_resource_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.

%%

get_invoice_shop_id(#domain_Invoice{shop_id = ShopID}) ->
    ShopID.

%%

body_change(Body, Body) -> [];
body_change(undefined, _Body) -> [];
body_change(ParamsBody, _Body) -> [?chargeback_body_changed(ParamsBody)].

levy_change(Levy, Levy) -> [];
levy_change(undefined, _Levy) -> [];
levy_change(ParamsLevy, _Levy) -> [?chargeback_levy_changed(ParamsLevy)].

%%

add_batch([], Batches) ->
    Batches;
add_batch(FinalCashFlow, []) ->
    [{1, FinalCashFlow}];
add_batch(FinalCashFlow, Batches) ->
    {ID, _CF} = lists:last(Batches),
    Batches ++ [{ID + 1, FinalCashFlow}].

build_updated_plan(NewCashFlow, #chargeback_st{cash_flow_plans = Plans, cash_flow = OldCashFlow} = State) ->
    Stage = get_stage(State),
    case Plans of
        #{Stage := []} ->
            Reverted = get_reverted_previous_stage(State),
            add_batch(NewCashFlow, Reverted);
        #{Stage := Plan} ->
            RevertedPrevious = hg_cashflow:revert(OldCashFlow),
            RevertedPlan = add_batch(RevertedPrevious, Plan),
            add_batch(NewCashFlow, RevertedPlan)
    end.
