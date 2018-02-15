%%% Invoice payment submachine
%%%
%%% TODO
%%%  - make proper submachine interface
%%%     - `init` / `start_session` should provide `next` or `done` to the caller
%%%  - distinguish between different error classes:
%%%     - regular operation error
%%%     - callback timeout
%%%     - internal error ?
%%%  - handle idempotent callbacks uniformly
%%%     - get rid of matches against session status
%%%  - tag machine with the provider trx
%%%     - distinguish between trx tags and callback tags
%%%     - tag namespaces
%%%  - clean the mess with error handling
%%%     - abuse transient error passthrough
%%%  - think about safe clamping of timers returned by some proxy
%%%  - why don't user interaction events imprint anything on the state?
%%%  - adjustments look and behave very much like claims over payments
%%%  - payment status transition are caused by the fact that some session
%%%    finishes, which could have happened in the past, not just now

-module(hg_invoice_payment).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).
-export([get_refunds/1]).
-export([get_refund/2]).
-export([get_adjustments/1]).
-export([get_adjustment/2]).

-export([get_activity/1]).
-export([get_tags/1]).

-export([construct_payment_info/2]).

%% Business logic

-export([start_session/1]).

-export([capture/2]).
-export([cancel/2]).
-export([refund/3]).

-export([create_adjustment/3]).
-export([capture_adjustment/3]).
-export([cancel_adjustment/3]).

%% Machine like

-export([init/3]).

-export([process_signal/3]).
-export([process_call/3]).

-export([merge_change/2]).

-export([get_log_params/2]).

%% Marshalling

-export([marshal/1]).
-export([unmarshal/1]).

%%

-type activity()      :: undefined | payment | {refund, refund_id()}.

-record(st, {
    activity          :: activity(),
    payment           :: undefined | payment(),
    risk_score        :: undefined | risk_score(),
    route             :: undefined | route(),
    cash_flow         :: undefined | cash_flow(),
    trx               :: undefined | trx_info(),
    target            :: undefined | target(),
    sessions    = #{} :: #{target() => session()},
    refunds     = #{} :: #{refund_id() => refund_state()},
    adjustments = []  :: [adjustment()],
    opts              :: undefined | opts()
}).

-record(refund_st, {
    refund            :: undefined | refund(),
    cash_flow         :: undefined | cash_flow(),
    session           :: undefined | session()
}).

-type refund_state() :: #refund_st{}.

-type st() :: #st{}.

-export_type([st/0]).

-type party()             :: dmsl_domain_thrift:'Party'().
-type invoice()           :: dmsl_domain_thrift:'Invoice'().
-type payment()           :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()        :: dmsl_domain_thrift:'InvoicePaymentID'().
-type refund()            :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type refund_id()         :: dmsl_domain_thrift:'InvoicePaymentRefundID'().
-type refund_params()     :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type adjustment()        :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()     :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type target()            :: dmsl_domain_thrift:'TargetInvoicePaymentStatus'().
-type risk_score()        :: dmsl_domain_thrift:'RiskScore'().
-type route()             :: dmsl_domain_thrift:'PaymentRoute'().
-type cash_flow()         :: dmsl_domain_thrift:'FinalCashFlow'().
-type trx_info()          :: dmsl_domain_thrift:'TransactionInfo'().
-type session_result()    :: dmsl_payment_processing_thrift:'SessionResult'().
-type proxy_state()       :: dmsl_proxy_provider_thrift:'ProxyState'().
-type tag()               :: dmsl_proxy_provider_thrift:'CallbackTag'().

-type session() :: #{
    target      := target(),
    status      := active | suspended | finished,
    trx         := trx_info(),
    tags        := [tag()],
    result      => session_result(),
    proxy_state => proxy_state()
}.

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

-export_type([opts/0]).

%%

-include("domain.hrl").
-include("payment_events.hrl").

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().

%%

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

-spec get_adjustments(st()) -> [adjustment()].

get_adjustments(#st{adjustments = As}) ->
    As.

-spec get_adjustment(adjustment_id(), st()) -> adjustment() | no_return().

get_adjustment(ID, St) ->
    case try_get_adjustment(ID, St) of
        Adjustment = #domain_InvoicePaymentAdjustment{} ->
            Adjustment;
        undefined ->
            throw(#payproc_InvoicePaymentAdjustmentNotFound{})
    end.

-spec get_refunds(st()) -> [refund()].

get_refunds(#st{refunds = Rs}) ->
    lists:keysort(#domain_InvoicePaymentRefund.id, [R#refund_st.refund || R <- maps:values(Rs)]).

-spec get_refund(refund_id(), st()) -> refund() | no_return().

get_refund(ID, St) ->
    case try_get_refund_state(ID, St) of
        #refund_st{refund = Refund} ->
            Refund;
        undefined ->
            throw(#payproc_InvoicePaymentRefundNotFound{})
    end.

%%

-spec get_activity(st()) -> activity().

get_activity(#st{activity = Activity}) ->
    Activity.

-spec get_tags(st()) -> [tag()].

get_tags(#st{sessions = Sessions, refunds = Refunds}) ->
    lists:usort(lists:flatten(
        [get_session_tags(S)                     || S <- maps:values(Sessions)] ++
        [get_session_tags(get_refund_session(R)) || R <- maps:values(Refunds) ]
    )).

%%

-type result() :: {[_], hg_machine_action:t()}. % FIXME

-spec init(payment_id(), _, opts()) ->
    {payment(), result()}.

init(PaymentID, PaymentParams, Opts) ->
    scoper:scope(
        payment,
        #{
            id => PaymentID
        },
        fun() -> init_(PaymentID, PaymentParams, Opts) end
    ).

-spec init_(payment_id(), _, opts()) ->
    {st(), result()}.

init_(PaymentID, Params, Opts) ->
    Revision = hg_domain:head(),
    Party = get_party(Opts),
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Cost = get_invoice_cost(Invoice),
    Payer = construct_payer(get_payer_params(Params), Shop),
    Flow = get_flow_params(Params),
    CreatedAt = hg_datetime:format_now(),
    MerchantTerms = get_merchant_payments_terms(Invoice, Party, Revision),
    VS0 = collect_varset(Party, Shop, #{}),
    {Payment, VS1} = construct_payment(PaymentID, CreatedAt, Cost, Payer, Flow, MerchantTerms, VS0, Revision),
    {RiskScore, VS2} = validate_risk_score(inspect(Payment, PaymentInstitution, VS1, Opts), VS1),
    Route = case get_predefined_route(Payer) of
        {ok, R} ->
            R;
        undefined ->
            validate_route(
                hg_routing:choose(payment, PaymentInstitution, VS2, Revision),
                Payment
            )
    end,
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    Provider = get_route_provider(Route, Revision),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS2, Revision),
    FinalCashflow = construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS2, Revision),
    _AffectedAccounts = hg_accounting:plan(
        construct_payment_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Events = [?payment_started(Payment, RiskScore, Route, FinalCashflow)],
    {collapse_changes(Events), {Events, hg_machine_action:new()}}.

get_merchant_payments_terms(Opts, Revision) ->
    get_merchant_payments_terms(get_invoice(Opts), get_party(Opts), Revision).

get_merchant_payments_terms(Invoice, Party, Revision) ->
    Shop = hg_party:get_shop(get_invoice_shop_id(Invoice), Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_contract_active(Contract),
    TermSet = hg_party:get_terms(
        Contract,
        get_invoice_created_at(Invoice),
        Revision
    ),
    TermSet#domain_TermSet.payments.

get_provider_payments_terms(Route, Revision) ->
    hg_routing:get_payments_terms(Route, Revision).

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    throw(#payproc_InvalidContractStatus{status = Status}).

get_payer_params(#payproc_InvoicePaymentParams{payer = PayerParams}) ->
    PayerParams.

get_flow_params(#payproc_InvoicePaymentParams{flow = FlowParams}) ->
    FlowParams.

construct_payer({payment_resource, #payproc_PaymentResourcePayerParams{
    resource = Resource,
    contact_info = ContactInfo
}}, _) ->
    ?payment_resource_payer(Resource, ContactInfo);
construct_payer({customer, #payproc_CustomerPayerParams{customer_id = CustomerID}}, Shop) ->
    Customer = get_customer(CustomerID),
    ok = validate_customer_shop(Customer, Shop),
    ActiveBinding = get_active_binding(Customer),
    % by keynfawkes
    % TODO Should we bake recurrent token right in too?
    %      Expect to have some issues related to access control while trying
    %      to fetch this token during deeper payment flow stages
    % by antibi0tic
    % we dont need it for refund, so I think - no
    ?customer_payer(
        CustomerID,
        ActiveBinding#payproc_CustomerBinding.id,
        ActiveBinding#payproc_CustomerBinding.rec_payment_tool_id,
        get_resource_payment_tool(ActiveBinding#payproc_CustomerBinding.payment_resource),
        get_customer_contact_info(Customer)
    ).

validate_customer_shop(#payproc_Customer{shop_id = ShopID}, #domain_Shop{id = ShopID}) ->
    ok;
validate_customer_shop(_, _) ->
    throw_invalid_request(<<"Invalid customer">>).

get_active_binding(#payproc_Customer{bindings = Bindings, active_binding_id = BindingID}) ->
    case lists:keysearch(BindingID, #payproc_CustomerBinding.id, Bindings) of
        {value, ActiveBinding} ->
            ActiveBinding;
        false ->
            throw_invalid_request(<<"Specified customer is not ready">>)
    end.

get_customer_contact_info(#payproc_Customer{contact_info = ContactInfo}) ->
    ContactInfo.

construct_payment(PaymentID, CreatedAt, Cost, Payer, FlowParams, Terms, VS0, Revision) ->
    VS1 = validate_payment_tool(
        get_payer_payment_tool(Payer),
        Terms#domain_PaymentsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS2 = validate_payment_cost(
        Cost,
        Terms#domain_PaymentsServiceTerms.cash_limit,
        VS1,
        Revision
    ),
    {Flow, VS3} = construct_payment_flow(
        FlowParams,
        CreatedAt,
        Terms#domain_PaymentsServiceTerms.holds,
        VS2,
        Revision
    ),
    {
        #domain_InvoicePayment{
            id              = PaymentID,
            created_at      = CreatedAt,
            domain_revision = Revision,
            status          = ?pending(),
            cost            = Cost,
            payer           = Payer,
            flow            = Flow
        },
        VS3
    }.

construct_payment_flow({instant, _}, _CreatedAt, _Terms, VS, _Revision) ->
    {
        ?invoice_payment_flow_instant(),
        VS#{flow => instant}
    };
construct_payment_flow({hold, Params}, CreatedAt, Terms, VS, Revision) ->
    OnHoldExpiration = Params#payproc_InvoicePaymentParamsFlowHold.on_hold_expiration,
    Lifetime = ?hold_lifetime(Seconds) = validate_hold_lifetime(Terms, VS, Revision),
    HeldUntil = hg_datetime:format_ts(hg_datetime:parse_ts(CreatedAt) + Seconds),
    {
        ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil),
        VS#{flow => {hold, Lifetime}}
    }.

get_predefined_route(?customer_payer(_, _, RecPaymentToolID, _, _) = Payer) ->
    case get_rec_payment_tool(RecPaymentToolID) of
        {ok, #payproc_RecurrentPaymentTool{
            route = Route
        }} when Route =/= undefined ->
            {ok, Route};
        _ ->
            % TODO more elegant error
            error({'Can\'t get route for customer payer', Payer})
    end;
get_predefined_route(?payment_resource_payer(_, _)) ->
    undefined.

validate_hold_lifetime(
    #domain_PaymentHoldsServiceTerms{
        payment_methods = PMs,
        lifetime = LifetimeSelector
    },
    VS,
    Revision
) ->
    PaymentTool = genlib_map:get(payment_tool, VS),
    _ = validate_payment_tool(PaymentTool, PMs, VS, Revision),
    reduce_selector(hold_lifetime, LifetimeSelector, VS, Revision);
validate_hold_lifetime(undefined, _VS, _Revision) ->
    throw_invalid_request(<<"Holds are not available">>).

validate_payment_tool(PaymentTool, PaymentMethodSelector, VS, Revision) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    VS#{payment_tool => PaymentTool}.

validate_payment_cost(Cost, CashLimitSelector, VS, Revision) ->
    Limit = reduce_selector(cash_limit, CashLimitSelector, VS, Revision),
    ok = validate_limit(Cost, Limit),
    VS#{cost => Cost}.

validate_limit(Cash, CashRange) ->
    case hg_condition:test_cash_range(Cash, CashRange) of
        within ->
            ok;
        {exceeds, lower} ->
            throw_invalid_request(<<"Invalid amount, less than allowed minumum">>);
        {exceeds, upper} ->
            throw_invalid_request(<<"Invalid amount, more than allowed maximum">>)
    end.

validate_risk_score(RiskScore, VS) when RiskScore == low; RiskScore == high ->
    {RiskScore, VS#{risk_score => RiskScore}};
validate_risk_score(fatal, _VS) ->
    throw_invalid_request(<<"Fatal error">>).

validate_route(Route = #domain_PaymentRoute{}, _Payment) ->
    Route;
validate_route(undefined, Payment) ->
    error({misconfiguration, {'No route found for a payment', Payment}}).

collect_varset(St, Opts) ->
    collect_varset(get_party(Opts), get_shop(Opts), get_payment(St), #{}).

collect_varset(Party, Shop = #domain_Shop{
    category = Category,
    account = #domain_ShopAccount{currency = Currency}
}, VS) ->
    VS#{
        party    => Party,
        shop     => Shop,
        category => Category,
        currency => Currency
    }.

collect_varset(Party, Shop, Payment, VS) ->
    VS0 = collect_varset(Party, Shop, VS),
    VS0#{
        cost         => get_payment_cost(Payment),
        payment_tool => get_payment_tool(Payment)
    }.

%%

collect_cashflow(
    #domain_PaymentsServiceTerms{fees = MerchantCashflowSelector},
    #domain_PaymentsProvisionTerms{cash_flow = ProviderCashflowSelector},
    VS,
    Revision
) ->
    MerchantCashflow = reduce_selector(merchant_payment_fees     , MerchantCashflowSelector, VS, Revision),
    ProviderCashflow = reduce_selector(provider_payment_cash_flow, ProviderCashflowSelector, VS, Revision),
    MerchantCashflow ++ ProviderCashflow.

construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision) ->
    hg_cashflow:finalize(
        Cashflow,
        collect_cash_flow_context(Payment),
        collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision)
    ).

construct_final_cashflow(Cashflow, Context, AccountMap) ->
    hg_cashflow:finalize(Cashflow, Context, AccountMap).

collect_cash_flow_context(
    #domain_InvoicePayment{cost = Cost}
) ->
    #{
        payment_amount => Cost
    }.

collect_account_map(
    Payment,
    #domain_Shop{account = MerchantAccount},
    #domain_PaymentInstitution{system_account_set = SystemAccountSetSelector},
    #domain_Provider{accounts = ProviderAccounts},
    VS,
    Revision
) ->
    Currency = get_currency(get_payment_cost(Payment)),
    ProviderAccount = choose_provider_account(Currency, ProviderAccounts),
    SystemAccount = choose_system_account(SystemAccountSetSelector, Currency, VS, Revision),
    M = #{
        {merchant , settlement} => MerchantAccount#domain_ShopAccount.settlement     ,
        {merchant , guarantee } => MerchantAccount#domain_ShopAccount.guarantee      ,
        {provider , settlement} => ProviderAccount#domain_ProviderAccount.settlement ,
        {system   , settlement} => SystemAccount#domain_SystemAccount.settlement
    },
    % External account probably can be optional for some payments
    case choose_external_account(Currency, VS, Revision) of
        #domain_ExternalAccount{income = Income, outcome = Outcome} ->
            M#{
                {external, income} => Income,
                {external, outcome} => Outcome
            };
        undefined ->
            M
    end.

choose_provider_account(Currency, Accounts) ->
    choose_account(provider, Currency, Accounts).

choose_system_account(SystemAccountSetSelector, Currency, VS, Revision) ->
    SystemAccountSetRef = reduce_selector(system_account_set, SystemAccountSetSelector, VS, Revision),
    SystemAccountSet = hg_domain:get(Revision, {system_account_set, SystemAccountSetRef}),
    choose_account(
        system,
        Currency,
        SystemAccountSet#domain_SystemAccountSet.accounts
    ).

choose_external_account(Currency, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    ExternalAccountSetSelector = Globals#domain_Globals.external_account_set,
    case hg_selector:reduce(ExternalAccountSetSelector, VS, Revision) of
        {value, ExternalAccountSetRef} ->
            ExternalAccountSet = hg_domain:get(Revision, {external_account_set, ExternalAccountSetRef}),
            genlib_map:get(
                Currency,
                ExternalAccountSet#domain_ExternalAccountSet.accounts
            );
        _ ->
            undefined
    end.

choose_account(Name, Currency, Accounts) ->
    case maps:find(Currency, Accounts) of
        {ok, Account} ->
            Account;
        error ->
            error({misconfiguration, {'No account for a given currency', {Name, Currency}}})
    end.

get_account_state(AccountType, AccountMap, Accounts) ->
    % FIXME move me closer to hg_accounting
    case AccountMap of
        #{AccountType := AccountID} ->
            #{AccountID := AccountState} = Accounts,
            AccountState;
        #{} ->
            undefined
    end.

get_available_amount(#{min_available_amount := V}) ->
    V.

construct_payment_plan_id(St) ->
    construct_payment_plan_id(get_invoice(get_opts(St)), get_payment(St)).

construct_payment_plan_id(Invoice, Payment) ->
    hg_utils:construct_complex_id([
        get_invoice_id(Invoice),
        get_payment_id(Payment)
    ]).

reduce_selector(Name, Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

%%

-spec start_session(target()) ->
    {ok, result()}.

start_session(Target) ->
    Events = [?session_ev(Target, ?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

-spec capture(st(), atom()) -> {ok, result()}.

capture(St, Reason) ->
    do_payment(St, ?captured_with_reason(hg_utils:format_reason(Reason))).

-spec cancel(st(), atom()) -> {ok, result()}.

cancel(St, Reason) ->
    do_payment(St, ?cancelled_with_reason(hg_utils:format_reason(Reason))).

do_payment(St, Target) ->
    Payment = get_payment(St),
    _ = assert_payment_status(processed, Payment),
    _ = assert_payment_flow(hold, Payment),
    start_session(Target).

-spec refund(refund_params(), st(), opts()) ->
    {refund(), result()}.

refund(Params, St0, Opts) ->
    St = St0#st{opts = Opts},
    Revision = hg_domain:head(),
    Payment = get_payment(St),
    Route = get_route(St),
    Shop = get_shop(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Provider = get_route_provider(Route, Revision),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_refund_pending(St),
    ID = construct_refund_id(St),
    VS0 = collect_varset(St, Opts),
    Refund = #domain_InvoicePaymentRefund{
        id              = ID,
        created_at      = hg_datetime:format_now(),
        domain_revision = Revision,
        status          = ?refund_pending(),
        reason          = Params#payproc_InvoicePaymentRefundParams.reason
    },
    MerchantTerms = get_merchant_refunds_terms(get_merchant_payments_terms(Opts, Revision)),
    VS1 = validate_refund(Payment, MerchantTerms, VS0, Revision),
    ProviderTerms = get_provider_refunds_terms(get_provider_payments_terms(Route, Revision), Payment),
    Cashflow = collect_refund_cashflow(MerchantTerms, ProviderTerms, VS1, Revision),
    % TODO specific cashflow context needed, with defined `refund_amount` for instance
    AccountMap = collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS1, Revision),
    FinalCashflow = construct_final_cashflow(Cashflow, collect_cash_flow_context(Payment), AccountMap),
    Changes = [
        ?refund_created(Refund, FinalCashflow),
        ?session_ev(?refunded(), ?session_started())
    ],
    RefundSt = collapse_refund_changes(Changes),
    AffectedAccounts = prepare_refund_cashflow(RefundSt, St),
    % NOTE we assume that posting involving merchant settlement account MUST be present in the cashflow
    case get_available_amount(get_account_state({merchant, settlement}, AccountMap, AffectedAccounts)) of
        % TODO we must pull this rule out of refund terms
        Available when Available >= 0 ->
            Action = hg_machine_action:instant(),
            {Refund, {[?refund_ev(ID, C) || C <- Changes], Action}};
        Available when Available < 0 ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt, St),
            throw(#payproc_InsufficientAccountBalance{})
    end.

construct_refund_id(#st{}) ->
    % FIXME we employ unique id in order not to reuse plan id occasionally
    %       should track sequence with some aux state instead
    hg_utils:unique_id().

assert_no_refund_pending(#st{refunds = Rs}) ->
    genlib_map:foreach(fun (ID, R) -> assert_refund_finished(ID, get_refund(R)) end, Rs).

assert_refund_finished(ID, #domain_InvoicePaymentRefund{status = ?refund_pending()}) ->
    throw(#payproc_InvoicePaymentRefundPending{id = ID});
assert_refund_finished(_ID, #domain_InvoicePaymentRefund{}) ->
    ok.

get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = Terms}) when Terms /= undefined ->
    Terms;
get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = undefined}) ->
    throw(#payproc_OperationNotPermitted{}).

get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = Terms}, _Payment) when Terms /= undefined ->
    Terms;
get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = undefined}, Payment) ->
    error({misconfiguration, {'No refund terms for a payment', Payment}}).

validate_refund(Payment, RefundTerms, VS0, Revision) ->
    VS1 = validate_payment_tool(
        get_payment_tool(Payment),
        RefundTerms#domain_PaymentRefundsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS1.

collect_refund_cashflow(
    #domain_PaymentRefundsServiceTerms{fees = MerchantCashflowSelector},
    #domain_PaymentRefundsProvisionTerms{cash_flow = ProviderCashflowSelector},
    VS,
    Revision
) ->
    MerchantCashflow = reduce_selector(merchant_refund_fees     , MerchantCashflowSelector, VS, Revision),
    ProviderCashflow = reduce_selector(provider_refund_cash_flow, ProviderCashflowSelector, VS, Revision),
    MerchantCashflow ++ ProviderCashflow.

prepare_refund_cashflow(RefundSt, St) ->
    hg_accounting:plan(construct_refund_plan_id(RefundSt, St), get_refund_cashflow_plan(RefundSt)).

commit_refund_cashflow(RefundSt, St) ->
    hg_accounting:commit(construct_refund_plan_id(RefundSt, St), [get_refund_cashflow_plan(RefundSt)]).

rollback_refund_cashflow(RefundSt, St) ->
    hg_accounting:rollback(construct_refund_plan_id(RefundSt, St), [get_refund_cashflow_plan(RefundSt)]).

construct_refund_plan_id(RefundSt, St) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(get_opts(St))),
        get_payment_id(get_payment(St)),
        {refund, get_refund_id(get_refund(RefundSt))}
    ]).

get_refund_cashflow_plan(RefundSt) ->
    {1, get_refund_cashflow(RefundSt)}.

%%

-spec create_adjustment(adjustment_params(), st(), opts()) ->
    {adjustment(), result()}.

create_adjustment(Params, St, Opts) ->
    Payment = get_payment(St),
    Revision = get_adjustment_revision(Params),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_adjustment_pending(St),
    Shop = get_shop(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    MerchantTerms = get_merchant_payments_terms(Opts, Revision),
    Route = get_route(St),
    Provider = get_route_provider(Route, Revision),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    VS = collect_varset(St, Opts),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS, Revision),
    FinalCashflow = construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision),
    ID = construct_adjustment_id(St),
    Adjustment = #domain_InvoicePaymentAdjustment{
        id                    = ID,
        status                = ?adjustment_pending(),
        created_at            = hg_datetime:format_now(),
        domain_revision       = Revision,
        reason                = Params#payproc_InvoicePaymentAdjustmentParams.reason,
        old_cash_flow_inverse = hg_cashflow:revert(get_cashflow(St)),
        new_cash_flow         = FinalCashflow
    },
    _AffectedAccounts = prepare_adjustment_cashflow(Adjustment, St, Opts),
    Event = ?adjustment_ev(ID, ?adjustment_created(Adjustment)),
    {Adjustment, {[Event], hg_machine_action:new()}}.

get_adjustment_revision(Params) ->
    hg_utils:select_defined(
        Params#payproc_InvoicePaymentAdjustmentParams.domain_revision,
        hg_domain:head()
    ).

construct_adjustment_id(#st{adjustments = As}) ->
    integer_to_binary(length(As) + 1).

assert_payment_status(Status, #domain_InvoicePayment{status = {Status, _}}) ->
    ok;
assert_payment_status(_, #domain_InvoicePayment{status = Status}) ->
    throw(#payproc_InvalidPaymentStatus{status = Status}).

assert_no_adjustment_pending(#st{adjustments = As}) ->
    lists:foreach(fun assert_adjustment_finalized/1, As).

assert_adjustment_finalized(#domain_InvoicePaymentAdjustment{id = ID, status = {pending, _}}) ->
    throw(#payproc_InvoicePaymentAdjustmentPending{id = ID});
assert_adjustment_finalized(_) ->
    ok.

assert_payment_flow(hold, #domain_InvoicePayment{flow = ?invoice_payment_flow_hold(_, _)}) ->
    ok;
assert_payment_flow(_, _) ->
    throw(#payproc_OperationNotPermitted{}).

-spec capture_adjustment(adjustment_id(), st(), opts()) ->
    {ok, result()}.

capture_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, capture, St, Options).

-spec cancel_adjustment(adjustment_id(), st(), opts()) ->
    {ok, result()}.

cancel_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, cancel, St, Options).

finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(pending, Adjustment),
    _AffectedAccounts = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
    Status = case Intent of
        capture ->
            ?adjustment_captured(hg_datetime:format_now());
        cancel ->
            ?adjustment_cancelled(hg_datetime:format_now())
    end,
    Event = ?adjustment_ev(ID, ?adjustment_status_changed(Status)),
    {ok, {[Event], hg_machine_action:new()}}.

prepare_adjustment_cashflow(Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    hg_accounting:plan(PlanID, Plan).

finalize_adjustment_cashflow(Intent, Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    case Intent of
        capture ->
            hg_accounting:commit(PlanID, Plan);
        cancel ->
            hg_accounting:rollback(PlanID, Plan)
    end.

get_adjustment_cashflow_plan(#domain_InvoicePaymentAdjustment{
    old_cash_flow_inverse = CashflowInverse,
    new_cash_flow         = Cashflow
}) ->
    [
        {1, CashflowInverse},
        {2, Cashflow}
    ].

assert_adjustment_status(Status, #domain_InvoicePaymentAdjustment{status = {Status, _}}) ->
    ok;
assert_adjustment_status(_, #domain_InvoicePaymentAdjustment{status = Status}) ->
    throw(#payproc_InvalidPaymentAdjustmentStatus{status = Status}).

construct_adjustment_plan_id(Adjustment, St, Options) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(Options)),
        get_payment_id(get_payment(St)),
        {adj, get_adjustment_id(Adjustment)}
    ]).

get_adjustment_id(#domain_InvoicePaymentAdjustment{id = ID}) ->
    ID.

get_adjustment_status(#domain_InvoicePaymentAdjustment{status = Status}) ->
    Status.

get_adjustment_cashflow(#domain_InvoicePaymentAdjustment{new_cash_flow = Cashflow}) ->
    Cashflow.

%%

-spec process_signal(timeout, st(), opts()) ->
    {next | done, result()}.

process_signal(timeout, St, Options) ->
    scoper:scope(
        payment,
        get_st_meta(St),
        fun() -> process_timeout(St#st{opts = Options}) end
    ).

process_timeout(St) ->
    Action = hg_machine_action:new(),
    case get_active_session(St) of
        Session when Session /= undefined ->
            case get_session_status(Session) of
                active ->
                    process(Action, St);
                suspended ->
                    process_callback_timeout(Action, St)
            end;
        undefined ->
            process_finished_session(St)
    end.

-spec process_call({callback, tag(), _}, st(), opts()) ->
    {_, {next | done, result()}}. % FIXME

process_call({callback, Tag, Payload}, St, Options) ->
    scoper:scope(
        payment,
        get_st_meta(St),
        fun() -> process_callback(Tag, Payload, St#st{opts = Options}) end
    ).

process_callback(Tag, Payload, St) ->
    Action = hg_machine_action:new(),
    Session = get_active_session(St),
    process_callback(Tag, Payload, Action, Session, St).

process_callback(Tag, Payload, Action, Session, St) when Session /= undefined ->
    case {get_session_status(Session), get_session_tags(Session)} of
        {suspended, [Tag | _]} ->
            handle_callback(Payload, Action, St);
        _ ->
            throw(invalid_callback)
    end;

process_callback(_Tag, _Payload, _Action, undefined, _St) ->
    throw(invalid_callback).

process_callback_timeout(Action, St) ->
    Session = get_active_session(St),
    Result = handle_proxy_callback_timeout(Action, Session),
    finish_processing(Result, St).

process(Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, ProxyResult} = issue_process_call(ProxyContext, St),
    Result = handle_proxy_result(ProxyResult, Action, get_active_session(St)),
    finish_processing(Result, St).

process_finished_session(St) ->
    Target = case get_payment_flow(get_payment(St)) of
        ?invoice_payment_flow_instant() ->
            ?captured();
        ?invoice_payment_flow_hold(OnHoldExpiration, _) ->
            case OnHoldExpiration of
                cancel ->
                    ?cancelled();
                capture ->
                    ?captured()
            end
    end,
    {ok, Result} = start_session(Target),
    {done, Result}.

handle_callback(Payload, Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, CallbackResult} = issue_callback_call(Payload, ProxyContext, St),
    {Response, Result} = handle_callback_result(CallbackResult, Action, get_active_session(St)),
    {Response, finish_processing(Result, St)}.

finish_processing(Result, St) ->
    finish_processing(get_activity(St), Result, St).

finish_processing(payment, {Events, Action}, St) ->
    Target = get_target(St),
    St1 = collapse_changes(Events, St),
    case get_session(Target, St1) of
        #{status := finished, result := ?session_succeeded(), target := Target} ->
            _AffectedAccounts = case Target of
                ?captured() ->
                    commit_payment_cashflow(St);
                ?cancelled() ->
                    rollback_payment_cashflow(St);
                ?processed() ->
                    undefined
            end,
            NewAction = get_action(Target, Action, St),
            {done, {Events ++ [?payment_status_changed(Target)], NewAction}};
        #{status := finished, result := ?session_failed(Failure)} ->
            % TODO is it always rollback?
            _AffectedAccounts = rollback_payment_cashflow(St),
            {done, {Events ++ [?payment_status_changed(?failed(Failure))], Action}};
        #{} ->
            {next, {Events, Action}}
    end;

finish_processing({refund, ID}, {Events, Action}, St) ->
    Events1 = [?refund_ev(ID, Ev) || Ev <- Events],
    St1 = collapse_changes(Events1, St),
    RefundSt1 = try_get_refund_state(ID, St1),
    case get_refund_session(RefundSt1) of
        #{status := finished, result := ?session_succeeded()} ->
            _AffectedAccounts = commit_refund_cashflow(RefundSt1, St1),
            Events2 = [
                ?refund_ev(ID, ?refund_status_changed(?refund_succeeded())),
                ?payment_status_changed(?refunded())
            ],
            {done, {Events1 ++ Events2, Action}};
        #{status := finished, result := ?session_failed(Failure)} ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt1, St1),
            Events2 = [
                ?refund_ev(ID, ?refund_status_changed(?refund_failed(Failure)))
            ],
            {done, {Events1 ++ Events2, Action}};
        #{} ->
            {next, {Events1, Action}}
    end.

get_action({processed, _}, Action, St) ->
    case get_payment_flow(get_payment(St)) of
        ?invoice_payment_flow_instant() ->
            hg_machine_action:set_timeout(0, Action);
        ?invoice_payment_flow_hold(_, HeldUntil) ->
            hg_machine_action:set_deadline(HeldUntil, Action)
    end;
get_action(_, Action, _) ->
    Action.

handle_proxy_result(
    #prxprv_PaymentProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, Action0),
    {wrap_session_events(Events1 ++ Events2 ++ Events3, Session), Action}.

handle_callback_result(
    #prxprv_PaymentCallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0)),
    {wrap_session_events([?session_activated()] ++ Events1 ++ Events2 ++ Events3, Session), Action};
handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {wrap_session_events(Events1 ++ Events2, Session), Action0}.

handle_proxy_callback_timeout(Action, Session) ->
    Events = [?session_finished(?session_failed(?operation_timeout()))],
    {wrap_session_events(Events, Session), Action}.

wrap_session_events(SessionEvents, #{target := Target}) ->
    [?session_ev(Target, Ev) || Ev <- SessionEvents].

update_proxy_state(undefined) ->
    [];
update_proxy_state(ProxyState) ->
    [?proxy_st_changed(ProxyState)].

handle_proxy_intent(#'prxprv_FinishIntent'{status = {success, _}}, Action) ->
    Events = [?session_finished(?session_succeeded())],
    {Events, Action};

handle_proxy_intent(#'prxprv_FinishIntent'{status = {failure, Failure}}, Action) ->
    Events = [?session_finished(?session_failed({failure, Failure}))],
    {Events, Action};

handle_proxy_intent(#'prxprv_SleepIntent'{timer = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = try_request_interaction(UserInteraction),
    {Events, Action};

handle_proxy_intent(#'prxprv_SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended(Tag) | try_request_interaction(UserInteraction)],
    {Events, Action}.

set_timer(Timer, Action) ->
    hg_machine_action:set_timer(Timer, Action).

try_request_interaction(undefined) ->
    [];
try_request_interaction(UserInteraction) ->
    [?interaction_requested(UserInteraction)].

commit_payment_cashflow(St) ->
    hg_accounting:commit(construct_payment_plan_id(St), get_cashflow_plan(St)).

rollback_payment_cashflow(St) ->
    hg_accounting:rollback(construct_payment_plan_id(St), get_cashflow_plan(St)).

get_cashflow_plan(St) ->
    [{1, get_cashflow(St)}].

%%

-type payment_info() :: dmsl_proxy_provider_thrift:'PaymentInfo'().

-spec construct_payment_info(st(), opts()) ->
    payment_info().

construct_payment_info(St, Opts) ->
    construct_payment_info(
        get_activity(St),
        St,
        #prxprv_PaymentInfo{
            shop = construct_proxy_shop(get_shop(Opts)),
            invoice = construct_proxy_invoice(get_invoice(Opts)),
            payment = construct_proxy_payment(get_payment(St), get_trx(St))
        }
    ).

construct_proxy_context(St) ->
    #prxprv_PaymentContext{
        session      = construct_session(get_active_session(St)),
        payment_info = construct_payment_info(St, get_opts(St)),
        options      = collect_proxy_options(St)
    }.

construct_session(Session = #{target := Target}) ->
    #prxprv_Session{
        target = Target,
        state = maps:get(proxy_state, Session, undefined)
    }.

construct_payment_info(payment, _St, PaymentInfo) ->
    PaymentInfo;
construct_payment_info({refund, ID}, St, PaymentInfo) ->
    PaymentInfo#prxprv_PaymentInfo{
        refund = construct_proxy_refund(try_get_refund_state(ID, St))
    }.

construct_proxy_payment(
    #domain_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    Trx
) ->
    ContactInfo = get_contact_info(Payer),
    #prxprv_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        trx = Trx,
        payment_resource = get_payment_resource(Payer),
        cost = construct_proxy_cash(Cost),
        contact_info = ContactInfo
    }.

get_payment_resource(?payment_resource_payer(Resource, _)) ->
    {disposable_payment_resource, Resource};
get_payment_resource(?customer_payer(_, _, RecPaymentToolID, _, _) = Payer) ->
    case get_rec_payment_tool(RecPaymentToolID) of
        {ok, #payproc_RecurrentPaymentTool{
            payment_resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool
            },
            rec_token = RecToken
        }} when RecToken =/= undefined ->
            {recurrent_payment_resource, #prxprv_RecurrentPaymentResource{
                payment_tool = PaymentTool,
                rec_token = RecToken
            }};
        _ ->
            % TODO more elegant error
            error({'Can\'t get rec_token for customer payer', Payer})
    end.

get_contact_info(?payment_resource_payer(_, ContactInfo)) ->
    ContactInfo;
get_contact_info(?customer_payer(_, _, _, _, ContactInfo)) ->
    ContactInfo.

construct_proxy_invoice(
    #domain_Invoice{
        id = InvoiceID,
        created_at = CreatedAt,
        due = Due,
        details = Details,
        cost = Cost
    }
) ->
    #prxprv_Invoice{
        id = InvoiceID,
        created_at =  CreatedAt,
        due =  Due,
        details = Details,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_shop(
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        location = Location,
        category = ShopCategoryRef
    }
) ->
    ShopCategory = hg_domain:get(
        hg_domain:head(),
        {category, ShopCategoryRef}
    ),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails,
        location = Location
    }.

construct_proxy_cash(#domain_Cash{
    amount = Amount,
    currency = CurrencyRef
}) ->
    Revision = hg_domain:head(),
    #prxprv_Cash{
        amount = Amount,
        currency = hg_domain:get(Revision, {currency, CurrencyRef})
    }.

construct_proxy_refund(#refund_st{
    refund  = #domain_InvoicePaymentRefund{id = ID, created_at = CreatedAt},
    session = Session
}) ->
    #prxprv_InvoicePaymentRefund{
        id         = ID,
        created_at = CreatedAt,
        trx        = get_session_trx(Session)
    }.

collect_proxy_options(
    #st{
        route = #domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef}
    }
) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    lists:foldl(
        fun
            (undefined, M) ->
                M;
            (M1, M) ->
                maps:merge(M1, M)
        end,
        #{},
        [
            Terminal#domain_Terminal.options,
            Proxy#domain_Proxy.additional,
            ProxyDef#domain_ProxyDefinition.options
        ]
    ).

%%

get_party(#{party := Party}) ->
    Party.

get_shop(#{party := Party, invoice := Invoice}) ->
    hg_party:get_shop(get_invoice_shop_id(Invoice), Party).

get_contract(#{party := Party, invoice := Invoice}) ->
    Shop = hg_party:get_shop(get_invoice_shop_id(Invoice), Party),
    hg_party:get_contract(Shop#domain_Shop.contract_id, Party).

get_payment_institution(Opts, Revision) ->
    Contract = get_contract(Opts),
    PaymentInstitutionRef = Contract#domain_Contract.payment_institution,
    hg_domain:get(Revision, {payment_institution, PaymentInstitutionRef}).

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_invoice_cost(#domain_Invoice{cost = Cost}) ->
    Cost.

get_invoice_shop_id(#domain_Invoice{shop_id = ShopID}) ->
    ShopID.

get_invoice_created_at(#domain_Invoice{created_at = Dt}) ->
    Dt.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

get_payment_cost(#domain_InvoicePayment{cost = Cost}) ->
    Cost.

get_payment_flow(#domain_InvoicePayment{flow = Flow}) ->
    Flow.

get_payment_tool(#domain_InvoicePayment{payer = Payer}) ->
    get_payer_payment_tool(Payer).

get_payer_payment_tool(?payment_resource_payer(PaymentResource, _ContactInfo)) ->
    get_resource_payment_tool(PaymentResource);
get_payer_payment_tool(?customer_payer(_CustomerID, _, _, PaymentTool, _)) ->
    PaymentTool.

get_currency(#domain_Cash{currency = Currency}) ->
    Currency.

get_resource_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.
%%

-spec throw_invalid_request(binary()) -> no_return().

throw_invalid_request(Why) ->
    throw(#'InvalidRequest'{errors = [Why]}).

%%

-spec merge_change(change(), st() | undefined) -> st().

merge_change(Event, undefined) ->
    merge_change(Event, #st{});

merge_change(?payment_started(Payment, RiskScore, Route, Cashflow), St) ->
    St#st{
        activity   = payment,
        payment    = Payment,
        risk_score = RiskScore,
        route      = Route,
        cash_flow  = Cashflow
    };
merge_change(?payment_status_changed(Status), St = #st{payment = Payment}) ->
    St1 = St#st{payment = Payment#domain_InvoicePayment{status = Status}},
    case Status of
        {S, _} when S == captured; S == cancelled; S == failed ->
            St1#st{activity = undefined};
        _ ->
            St1
    end;
merge_change(?refund_ev(ID, Event), St) ->
    St1 = St#st{activity = {refund, ID}},
    RefundSt = merge_refund_change(Event, try_get_refund_state(ID, St1)),
    St2 = set_refund_state(ID, RefundSt, St1),
    case get_refund_status(get_refund(RefundSt)) of
        {S, _} when S == succeeded; S == failed ->
            St2#st{activity = undefined};
        _ ->
            St2
    end;
merge_change(?adjustment_ev(ID, Event), St) ->
    Adjustment = merge_adjustment_change(Event, try_get_adjustment(ID, St)),
    St1 = set_adjustment(ID, Adjustment, St),
    % TODO new cashflow imposed implicitly on the payment state? rough
    case get_adjustment_status(Adjustment) of
        ?adjustment_captured(_) ->
            set_cashflow(get_adjustment_cashflow(Adjustment), St1);
        _ ->
            St1
    end;
merge_change(?session_ev(Target, ?session_started()), St) ->
    % FIXME why the hell dedicated handling
    set_session(Target, create_session(Target, get_trx(St)), St#st{target = Target});
merge_change(?session_ev(Target, Event), St) ->
    Session = merge_session_change(Event, get_session(Target, St)),
    St1 = set_session(Target, Session, St),
    % FIXME leaky transactions
    St2 = set_trx(get_session_trx(Session), St1),
    case get_session_status(Session) of
        finished ->
            St2#st{target = undefined};
        _ ->
            St2
    end.

collapse_refund_changes(Changes) ->
    lists:foldl(fun merge_refund_change/2, undefined, Changes).

merge_refund_change(?refund_created(Refund, Cashflow), undefined) ->
    #refund_st{refund = Refund, cash_flow = Cashflow};
merge_refund_change(?refund_status_changed(Status), RefundSt) ->
    set_refund(set_refund_status(Status, get_refund(RefundSt)), RefundSt);
merge_refund_change(?session_ev(?refunded(), ?session_started()), St) ->
    set_refund_session(create_session(?refunded(), undefined), St);
merge_refund_change(?session_ev(?refunded(), Change), St) ->
    set_refund_session(merge_session_change(Change, get_refund_session(St)), St).

merge_adjustment_change(?adjustment_created(Adjustment), undefined) ->
    Adjustment;
merge_adjustment_change(?adjustment_status_changed(Status), Adjustment) ->
    Adjustment#domain_InvoicePaymentAdjustment{status = Status}.

get_cashflow(#st{cash_flow = FinalCashflow}) ->
    FinalCashflow.

set_cashflow(Cashflow, St = #st{}) ->
    St#st{cash_flow = Cashflow}.

get_trx(#st{trx = Trx}) ->
    Trx.

set_trx(Trx, St = #st{}) ->
    St#st{trx = Trx}.

try_get_refund_state(ID, #st{refunds = Rs}) ->
    case Rs of
        #{ID := RefundSt} ->
            RefundSt;
        #{} ->
            undefined
    end.

set_refund_state(ID, RefundSt, St = #st{refunds = Rs}) ->
    St#st{refunds = Rs#{ID => RefundSt}}.

get_refund_session(#refund_st{session = Session}) ->
    Session.

set_refund_session(Session, St = #refund_st{}) ->
    St#refund_st{session = Session}.

get_refund(#refund_st{refund = Refund}) ->
    Refund.

set_refund(Refund, RefundSt = #refund_st{}) ->
    RefundSt#refund_st{refund = Refund}.

get_refund_id(#domain_InvoicePaymentRefund{id = ID}) ->
    ID.

get_refund_status(#domain_InvoicePaymentRefund{status = Status}) ->
    Status.

set_refund_status(Status, Refund = #domain_InvoicePaymentRefund{}) ->
    Refund#domain_InvoicePaymentRefund{status = Status}.

get_refund_cashflow(#refund_st{cash_flow = CashFlow}) ->
    CashFlow.

try_get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoicePaymentAdjustment.id, As) of
        V = #domain_InvoicePaymentAdjustment{} ->
            V;
        false ->
            undefined
    end.

set_adjustment(ID, Adjustment, St = #st{adjustments = As}) ->
    St#st{adjustments = lists:keystore(ID, #domain_InvoicePaymentAdjustment.id, As, Adjustment)}.

merge_session_change(?session_finished(Result), Session) ->
    Session#{status := finished, result => Result};
merge_session_change(?session_activated(), Session) ->
    Session#{status := active};
merge_session_change(?session_suspended(undefined), Session) ->
    Session#{status := suspended};
merge_session_change(?session_suspended(Tag), Session) ->
    Session#{status := suspended, tags := [Tag | get_session_tags(Session)]};
merge_session_change(?trx_bound(Trx), Session) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session) ->
    Session.

create_session(Target, Trx) ->
    #{
        target => Target,
        status => active,
        trx    => Trx,
        tags   => []
    }.

get_session(Target, #st{sessions = Sessions}) ->
    maps:get(Target, Sessions, undefined).

set_session(Target, Session, St = #st{sessions = Sessions}) ->
    St#st{sessions = Sessions#{Target => Session}}.

get_session_status(#{status := Status}) ->
    Status.

get_session_trx(#{trx := Trx}) ->
    Trx.

get_session_tags(#{tags := Tags}) ->
    Tags.

get_target(#st{target = Target}) ->
    Target.

get_opts(#st{opts = Opts}) ->
    Opts.

%%

get_active_session(St) ->
    get_active_session(get_activity(St), St).

get_active_session(payment, St) ->
    get_session(get_target(St), St);
get_active_session({refund, ID}, St) ->
    RefundSt = try_get_refund_state(ID, St),
    RefundSt#refund_st.session.

%%

collapse_changes(Changes) ->
    collapse_changes(Changes, undefined).

collapse_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

%%

get_rec_payment_tool(RecPaymentToolID) ->
    hg_woody_wrapper:call(recurrent_paytool, 'Get', [RecPaymentToolID]).

get_customer(CustomerID) ->
    case issue_customer_call('Get', [CustomerID]) of
        {ok, Customer} ->
            Customer;
        {exception, #payproc_CustomerNotFound{}} ->
            throw_invalid_request(<<"Customer not found">>);
        {exception, Error} ->
            error({<<"Can't get customer">>, Error})
    end.

issue_process_call(ProxyContext, St) ->
    issue_proxy_call('ProcessPayment', [ProxyContext], St).

issue_callback_call(Payload, ProxyContext, St) ->
    issue_proxy_call('HandlePaymentCallback', [Payload, ProxyContext], St).

issue_proxy_call(Func, Args, St) ->
    CallOpts = get_call_options(St),
    hg_woody_wrapper:call(proxy_provider, Func, Args, CallOpts).

get_call_options(St) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider_ref(get_route(St))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#st{route = Route}) ->
    Route.

get_route_provider_ref(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

get_route_provider(Route, Revision) ->
    hg_domain:get(Revision, {provider, get_route_provider_ref(Route)}).

inspect(Payment = #domain_InvoicePayment{domain_revision = Revision}, PaymentInstitution, VS, Opts) ->
    InspectorSelector = PaymentInstitution#domain_PaymentInstitution.inspector,
    InspectorRef = reduce_selector(inspector, InspectorSelector, VS, Revision),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    hg_inspector:inspect(get_shop(Opts), get_invoice(Opts), Payment, Inspector).

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.

issue_customer_call(Func, Args) ->
    hg_woody_wrapper:call(customer_management, Func, Args).

%% Business metrics logging

-spec get_log_params(change(), st()) ->
    {ok, #{type := invoice_payment_event, params := list(), message := string()}} | undefined.

get_log_params(?payment_started(Payment, _, _, Cashflow), _) ->
    Params = #{
        payment => Payment,
        cashflow => Cashflow,
        event_type => invoice_payment_started
    },
    make_log_params(Params);
get_log_params(?payment_status_changed({Status, _}), State) ->
    Payment = get_payment(State),
    Cashflow = get_cashflow(State),
    Params = #{
        status => Status,
        payment => Payment,
        cashflow => Cashflow,
        event_type => invoice_payment_status_changed
    },
    make_log_params(Params);
get_log_params(_, _) ->
    undefined.

make_log_params(Params) ->
    LogParams = maps:fold(
        fun(K, V, Acc) ->
            Acc ++ make_log_params(K, V)
        end,
        [],
        Params
    ),
    Message = get_message(maps:get(event_type, Params)),
    {ok, #{
        type => invoice_payment_event,
        params => LogParams,
        message => Message
    }}.

make_log_params(
    payment,
    #domain_InvoicePayment{
        id = ID,
        cost = Cost,
        flow = Flow
    }
) ->
    [{id, ID}, {cost, make_log_params(cash, Cost)}, {flow, make_log_params(flow, Flow)}];
make_log_params(cash, ?cash(Amount, SymbolicCode)) ->
    [{amount, Amount}, {currency, SymbolicCode}];
make_log_params(flow, ?invoice_payment_flow_instant()) ->
    [{type, instant}];
make_log_params(flow, ?invoice_payment_flow_hold(OnHoldExpiration, _)) ->
    [{type, hold}, {on_hold_expiration, OnHoldExpiration}];
make_log_params(cashflow, CashFlow) ->
    Reminders = maps:to_list(hg_cashflow:get_partial_remainders(CashFlow)),
    Accounts = lists:map(
        fun ({Account, ?cash(Amount, SymbolicCode)}) ->
            Remainder = [{remainder, [{amount, Amount}, {currency, SymbolicCode}]}],
            {get_account_key(Account), Remainder}
        end,
        Reminders
    ),
    [{accounts, Accounts}];
make_log_params(status, Status) ->
    [{status, Status}];
make_log_params(event_type, EventType) ->
    [{type, EventType}].

get_account_key({AccountParty, AccountType}) ->
    list_to_binary(lists:concat([atom_to_list(AccountParty), ".", atom_to_list(AccountType)])).

get_message(invoice_payment_started) ->
    "Invoice payment is started";
get_message(invoice_payment_status_changed) ->
    "Invoice payment status is changed".

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(change()) ->
    hg_msgpack_marshalling:value().

marshal(Change) ->
    marshal(change, Change).

%% Changes

marshal(change, ?payment_started(Payment, RiskScore, Route, Cashflow)) ->
    [2, #{
        <<"change">>        => <<"started">>,
        <<"payment">>       => marshal(payment, Payment),
        <<"risk_score">>    => marshal(risk_score, RiskScore),
        <<"route">>         => hg_routing:marshal(Route),
        <<"cash_flow">>     => hg_cashflow:marshal(Cashflow)
    }];
marshal(change, ?payment_status_changed(Status)) ->
    [2, #{
        <<"change">>        => <<"status_changed">>,
        <<"status">>        => marshal(status, Status)
    }];
marshal(change, ?session_ev(Target, Payload)) ->
    [2, #{
        <<"change">>        => <<"session_change">>,
        <<"target">>        => marshal(status, Target),
        <<"payload">>       => marshal(session_change, Payload)
    }];
marshal(change, ?adjustment_ev(AdjustmentID, Payload)) ->
    [2, #{
        <<"change">>        => <<"adjustment_change">>,
        <<"id">>            => marshal(str, AdjustmentID),
        <<"payload">>       => marshal(adjustment_change, Payload)
    }];
marshal(change, ?refund_ev(RefundID, Payload)) ->
    [2, #{
        <<"change">>        => <<"refund">>,
        <<"id">>            => marshal(str, RefundID),
        <<"payload">>       => marshal(refund_change, Payload)
    }];

%% Payment

marshal(payment, #domain_InvoicePayment{} = Payment) ->
    genlib_map:compact(#{
        <<"id">>                => marshal(str, Payment#domain_InvoicePayment.id),
        <<"created_at">>        => marshal(str, Payment#domain_InvoicePayment.created_at),
        <<"domain_revision">>   => marshal(str, Payment#domain_InvoicePayment.domain_revision),
        <<"cost">>              => hg_cash:marshal(Payment#domain_InvoicePayment.cost),
        <<"payer">>             => marshal(payer, Payment#domain_InvoicePayment.payer),
        <<"flow">>              => marshal(flow, Payment#domain_InvoicePayment.flow),
        <<"context">>           => hg_content:marshal(Payment#domain_InvoicePayment.context)
    });

%% Flow

marshal(flow, ?invoice_payment_flow_instant()) ->
    #{<<"type">> => <<"instant">>};
marshal(flow, ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)) ->
    #{
        <<"type">>                  => <<"hold">>,
        <<"on_hold_expiration">>    => marshal(on_hold_expiration, OnHoldExpiration),
        <<"held_until">>            => marshal(str, HeldUntil)
    };

%% Payment status

marshal(status, ?pending()) ->
    <<"pending">>;
marshal(status, ?processed()) ->
    <<"processed">>;
marshal(status, ?refunded()) ->
    <<"refunded">>;
marshal(status, ?failed(Failure)) ->
    [<<"failed">>, marshal(failure, Failure)];
marshal(status, ?captured_with_reason(Reason)) ->
    [<<"captured">>, marshal(str, Reason)];
marshal(status, ?cancelled_with_reason(Reason)) ->
    [<<"cancelled">>, marshal(str, Reason)];

%% Session change

marshal(session_change, ?session_started()) ->
    [3, <<"started">>];
marshal(session_change, ?session_finished(Result)) ->
    [3, [
        <<"finished">>,
        marshal(session_status, Result)
    ]];
marshal(session_change, ?session_suspended(Tag)) ->
    [3, [
        <<"suspended">>,
        marshal(str, Tag)
    ]];
marshal(session_change, ?session_activated()) ->
    [3, <<"activated">>];
marshal(session_change, ?trx_bound(Trx)) ->
    [3, [
        <<"transaction_bound">>,
        marshal(trx, Trx)
    ]];
marshal(session_change, ?proxy_st_changed(ProxySt)) ->
    [3, [
        <<"proxy_state_changed">>,
        marshal(bin, {bin, ProxySt})
    ]];
marshal(session_change, ?interaction_requested(UserInteraction)) ->
    [3, [
        <<"interaction_requested">>,
        marshal(interaction, UserInteraction)
    ]];

marshal(session_status, ?session_succeeded()) ->
    <<"succeeded">>;
marshal(session_status, ?session_failed(PayloadFailure)) ->
    [
        <<"failed">>,
        marshal(failure, PayloadFailure)
    ];

%% Adjustment change

marshal(adjustment_change, ?adjustment_created(Adjustment)) ->
    [2, [<<"created">>, marshal(adjustment, Adjustment)]];
marshal(adjustment_change, ?adjustment_status_changed(Status)) ->
    [2, [<<"status_changed">>, marshal(adjustment_status, Status)]];

%% Refund change

marshal(refund_change, ?refund_created(Refund, Cashflow)) ->
    [2, [<<"created">>, marshal(refund, Refund), hg_cashflow:marshal(Cashflow)]];
marshal(refund_change, ?refund_status_changed(Status)) ->
    [2, [<<"status">>, marshal(refund_status, Status)]];
marshal(refund_change, ?session_ev(_Target, Payload)) ->
    [2, [<<"session">>, marshal(session_change, Payload)]];

%% Adjustment

marshal(adjustment, #domain_InvoicePaymentAdjustment{} = Adjustment) ->
    #{
        <<"id">>                    => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.id),
        <<"created_at">>            => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.created_at),
        <<"domain_revision">>       => marshal(int, Adjustment#domain_InvoicePaymentAdjustment.domain_revision),
        <<"reason">>                => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.reason),
        % FIXME
        <<"old_cash_flow_inverse">> => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.old_cash_flow_inverse),
        <<"new_cash_flow">>         => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.new_cash_flow)
    };

marshal(adjustment_status, ?adjustment_pending()) ->
    <<"pending">>;
marshal(adjustment_status, ?adjustment_captured(At)) ->
    [<<"captured">>, marshal(str, At)];
marshal(adjustment_status, ?adjustment_cancelled(At)) ->
    [<<"cancelled">>, marshal(str, At)];

%% Refund

marshal(refund, #domain_InvoicePaymentRefund{} = Refund) ->
    genlib_map:compact(#{
        <<"id">>         => marshal(str, Refund#domain_InvoicePaymentRefund.id),
        <<"created_at">> => marshal(str, Refund#domain_InvoicePaymentRefund.created_at),
        <<"rev">>        => marshal(int, Refund#domain_InvoicePaymentRefund.domain_revision),
        <<"reason">>     => marshal(str, Refund#domain_InvoicePaymentRefund.reason)
    });

marshal(refund_status, ?refund_pending()) ->
    <<"pending">>;
marshal(refund_status, ?refund_succeeded()) ->
    <<"succeeded">>;
marshal(refund_status, ?refund_failed(Failure)) ->
    [<<"failed">>, marshal(failure, Failure)];

%%

marshal(payer, ?payment_resource_payer(Resource, ContactInfo)) ->
    [2, #{
        <<"type">>           => <<"payment_resource_payer">>,
        <<"resource">>       => marshal(disposable_payment_resource, Resource),
        <<"contact_info">>   => marshal(contact_info, ContactInfo)
    }];

marshal(payer, ?customer_payer(CustomerID, CustomerBindingID, RecurrentPaytoolID, PaymentTool, ContactInfo)) ->
    [3, #{
        <<"type">>                  => <<"customer_payer">>,
        <<"customer_id">>           => marshal(str, CustomerID),
        <<"customer_binding_id">>   => marshal(str, CustomerBindingID),
        <<"rec_payment_tool_id">>   => marshal(str, RecurrentPaytoolID),
        <<"payment_tool">>          => hg_payment_tool:marshal(PaymentTool),
        <<"contact_info">>          => marshal(contact_info, ContactInfo)
    }];

marshal(disposable_payment_resource, #domain_DisposablePaymentResource{} = PaymentResource) ->
    #{
        <<"payment_tool">> => hg_payment_tool:marshal(PaymentResource#domain_DisposablePaymentResource.payment_tool),
        <<"payment_session_id">> => marshal(str, PaymentResource#domain_DisposablePaymentResource.payment_session_id),
        <<"client_info">> => marshal(client_info, PaymentResource#domain_DisposablePaymentResource.client_info)
    };

marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    genlib_map:compact(#{
        <<"ip_address">>    => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        <<"fingerprint">>   => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

marshal(contact_info, #domain_ContactInfo{} = ContactInfo) ->
    genlib_map:compact(#{
        <<"phone_number">>  => marshal(str, ContactInfo#domain_ContactInfo.phone_number),
        <<"email">>         => marshal(str, ContactInfo#domain_ContactInfo.email)
    });

marshal(trx, #domain_TransactionInfo{} = TransactionInfo) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, TransactionInfo#domain_TransactionInfo.id),
        <<"timestamp">>     => marshal(str, TransactionInfo#domain_TransactionInfo.timestamp),
        <<"extra">>         => marshal(map_str, TransactionInfo#domain_TransactionInfo.extra)
    });

marshal(interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    #{<<"redirect">> =>
        [
            <<"get_request">>,
            marshal(str, URI)
        ]
    };
marshal(interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    #{<<"redirect">> =>
        [
            <<"post_request">>,
            #{
                <<"uri">>   => marshal(str, URI),
                <<"form">>  => marshal(map_str, Form)
            }
        ]
    };
marshal(interaction, {payment_terminal_reciept, #'PaymentTerminalReceipt'{short_payment_id = SPID, due = DueDate}}) ->
    #{<<"payment_terminal_receipt">> =>
        #{
            <<"spid">>  => marshal(str, SPID),
            <<"due">>   => marshal(str, DueDate)
        }
    };

marshal(sub_failure, undefined) ->
    undefined;
marshal(sub_failure, #domain_SubFailure{} = SubFailure) ->
    genlib_map:compact(#{
        <<"code">> => marshal(str        , SubFailure#domain_SubFailure.code),
        <<"sub" >> => marshal(sub_failure, SubFailure#domain_SubFailure.sub )
    });

marshal(failure, {operation_timeout, _}) ->
    [3, <<"operation_timeout">>];
marshal(failure, {failure, #domain_Failure{} = Failure}) ->
    [3, [<<"failure">>, genlib_map:compact(#{
        <<"code"  >> => marshal(str        , Failure#domain_Failure.code  ),
        <<"reason">> => marshal(str        , Failure#domain_Failure.reason),
        <<"sub"   >> => marshal(sub_failure, Failure#domain_Failure.sub   )
    })]];

marshal(on_hold_expiration, cancel) ->
    <<"cancel">>;
marshal(on_hold_expiration, capture) ->
    <<"capture">>;

marshal(risk_score, low) ->
    <<"low">>;
marshal(risk_score, high) ->
    <<"high">>;
marshal(risk_score, fatal) ->
    <<"fatal">>;

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) -> change().

unmarshal(Change) ->
    unmarshal(change, Change).

%% Changes

unmarshal(change, [2, #{
    <<"change">>        := <<"started">>,
    <<"payment">>       := Payment,
    <<"risk_score">>    := RiskScore,
    <<"route">>         := Route,
    <<"cash_flow">>     := Cashflow
}]) ->
    ?payment_started(
        unmarshal(payment, Payment),
        unmarshal(risk_score, RiskScore),
        hg_routing:unmarshal(Route),
        hg_cashflow:unmarshal(Cashflow)
    );
unmarshal(change, [2, #{
    <<"change">>    := <<"status_changed">>,
    <<"status">>    := Status
}]) ->
    ?payment_status_changed(unmarshal(status, Status));
unmarshal(change, [2, #{
    <<"change">>    := <<"session_change">>,
    <<"payload">>   := Payload,
    <<"target">>    := Target
}]) ->
    ?session_ev(unmarshal(status, Target), unmarshal(session_change, Payload));
unmarshal(change, [2, #{
    <<"change">>    := <<"adjustment_change">>,
    <<"id">>        := AdjustmentID,
    <<"payload">>   := Payload
}]) ->
    ?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adjustment_change, Payload));
unmarshal(change, [2, #{
    <<"change">>    := <<"refund">>,
    <<"id">>        := RefundID,
    <<"payload">>   := Payload
}]) ->
    ?refund_ev(unmarshal(str, RefundID), unmarshal(refund_change, Payload));

unmarshal(change, [1, ?legacy_payment_started(Payment, RiskScore, Route, Cashflow)]) ->
    ?payment_started(
        unmarshal(payment, Payment),
        unmarshal(risk_score, RiskScore),
        hg_routing:unmarshal([1, Route]),
        hg_cashflow:unmarshal([1, Cashflow])
    );
unmarshal(change, [1, ?legacy_payment_status_changed(Status)]) ->
    ?payment_status_changed(unmarshal(status, Status));
unmarshal(change, [1, ?legacy_session_ev(Target, Payload)]) ->
    ?session_ev(unmarshal(status, Target), unmarshal(session_change, [1, Payload]));
unmarshal(change, [1, ?legacy_adjustment_ev(AdjustmentID, Payload)]) ->
    ?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adjustment_change, [1, Payload]));

%% Payment

unmarshal(payment, #{
    <<"id">>                := ID,
    <<"created_at">>        := CreatedAt,
    <<"domain_revision">>   := Revision,
    <<"cost">>              := Cash,
    <<"payer">>             := MarshalledPayer,
    <<"flow">>              := Flow
} = Payment) ->
    Context = maps:get(<<"context">>, Payment, undefined),
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        cost            = hg_cash:unmarshal(Cash),
        payer           = unmarshal(payer, MarshalledPayer),
        status          = ?pending(),
        flow            = unmarshal(flow, Flow),
        context         = hg_content:unmarshal(Context)
    };

unmarshal(payment,
    ?legacy_payment(ID, CreatedAt, Revision, Status, MarshalledPayer, Cash, Context)
) ->
    Payer = unmarshal(payer, MarshalledPayer),
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        status          = unmarshal(status, Status),
        cost            = hg_cash:unmarshal([1, Cash]),
        payer           = Payer,
        flow            = ?invoice_payment_flow_instant(),
        context         = hg_content:unmarshal(Context)
    };

%% Flow

unmarshal(flow, #{<<"type">> := <<"instant">>}) ->
    ?invoice_payment_flow_instant();
unmarshal(flow, #{
    <<"type">>                  := <<"hold">>,
    <<"on_hold_expiration">>    := OnHoldExpiration,
    <<"held_until">>            := HeldUntil
}) ->
    ?invoice_payment_flow_hold(
        unmarshal(on_hold_expiration, OnHoldExpiration),
        unmarshal(str, HeldUntil)
    );

%% Payment status

unmarshal(status, <<"pending">>) ->
    ?pending();
unmarshal(status, <<"processed">>) ->
    ?processed();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?failed(unmarshal(failure, Failure));
unmarshal(status, [<<"captured">>, Reason]) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(status, [<<"cancelled">>, Reason]) ->
    ?cancelled_with_reason(unmarshal(str, Reason));
unmarshal(status, <<"refunded">>) ->
    ?refunded();

unmarshal(status, ?legacy_pending()) ->
    ?pending();
unmarshal(status, ?legacy_processed()) ->
    ?processed();
unmarshal(status, ?legacy_failed(Failure)) ->
    ?failed(unmarshal(failure, [1, Failure]));
unmarshal(status, ?legacy_captured()) ->
    ?captured();
unmarshal(status, ?legacy_cancelled()) ->
    ?cancelled();
unmarshal(status, ?legacy_captured(Reason)) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(status, ?legacy_cancelled(Reason)) ->
    ?cancelled_with_reason(unmarshal(str, Reason));

%% Session change

unmarshal(session_change, [3, [<<"suspended">>, Tag]]) ->
    ?session_suspended(unmarshal(str, Tag));
unmarshal(session_change, [3, Change]) ->
    unmarshal(session_change, [2, Change]);

unmarshal(session_change, [2, <<"started">>]) ->
    ?session_started();
unmarshal(session_change, [2, [<<"finished">>, Result]]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [2, <<"suspended">>]) ->
    ?session_suspended(undefined);
unmarshal(session_change, [2, <<"activated">>]) ->
    ?session_activated();
unmarshal(session_change, [2, [<<"transaction_bound">>, Trx]]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [2, [<<"proxy_state_changed">>, {bin, ProxySt}]]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [2, [<<"interaction_requested">>, UserInteraction]]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(session_change, [1, ?legacy_session_started()]) ->
    ?session_started();
unmarshal(session_change, [1, ?legacy_session_finished(Result)]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [1, ?legacy_session_suspended()]) ->
    ?session_suspended(undefined);
unmarshal(session_change, [1, ?legacy_session_activated()]) ->
    ?session_activated();
unmarshal(session_change, [1, ?legacy_trx_bound(Trx)]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [1, ?legacy_proxy_st_changed(ProxySt)]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [1, ?legacy_interaction_requested(UserInteraction)]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

%% Session status

unmarshal(session_status, <<"succeeded">>) ->
    ?session_succeeded();
unmarshal(session_status, [<<"failed">>, Failure]) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(session_status, ?legacy_session_succeeded()) ->
    ?session_succeeded();
unmarshal(session_status, ?legacy_session_failed(Failure)) ->
    ?session_failed(unmarshal(failure, Failure));

%% Adjustment change

unmarshal(adjustment_change, [2, [<<"created">>, Adjustment]]) ->
    ?adjustment_created(unmarshal(adjustment, Adjustment));
unmarshal(adjustment_change, [2, [<<"status_changed">>, Status]]) ->
    ?adjustment_status_changed(unmarshal(adjustment_status, Status));

unmarshal(adjustment_change, [1, ?legacy_adjustment_created(Adjustment)]) ->
    ?adjustment_created(unmarshal(adjustment, Adjustment));
unmarshal(adjustment_change, [1, ?legacy_adjustment_status_changed(Status)]) ->
    ?adjustment_status_changed(unmarshal(adjustment_status, Status));

%% Refund change

unmarshal(refund_change, [2, [<<"created">>, Refund, Cashflow]]) ->
    ?refund_created(unmarshal(refund, Refund), hg_cashflow:unmarshal(Cashflow));
unmarshal(refund_change, [2, [<<"status">>, Status]]) ->
    ?refund_status_changed(unmarshal(refund_status, Status));
unmarshal(refund_change, [2, [<<"session">>, Payload]]) ->
    ?session_ev(?refunded(), unmarshal(session_change, Payload));

%% Adjustment

unmarshal(adjustment, #{
    <<"id">>                    := ID,
    <<"created_at">>            := CreatedAt,
    <<"domain_revision">>       := Revision,
    <<"reason">>                := Reason,
    <<"old_cash_flow_inverse">> := OldCashFlowInverse,
    <<"new_cash_flow">>         := NewCashFlow
}) ->
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = ?adjustment_pending(),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal(OldCashFlowInverse),
        new_cash_flow         = hg_cashflow:unmarshal(NewCashFlow)
    };

unmarshal(adjustment,
    ?legacy_adjustment(ID, Status, CreatedAt, Revision, Reason, NewCashFlow, OldCashFlowInverse)
) ->
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = unmarshal(adjustment_status, Status),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal([1, OldCashFlowInverse]),
        new_cash_flow         = hg_cashflow:unmarshal([1, NewCashFlow])
    };

%% Adjustment status

unmarshal(adjustment_status, <<"pending">>) ->
    ?adjustment_pending();
unmarshal(adjustment_status, [<<"captured">>, At]) ->
    ?adjustment_captured(At);
unmarshal(adjustment_status, [<<"cancelled">>, At]) ->
    ?adjustment_cancelled(At);

unmarshal(adjustment_status, ?legacy_adjustment_pending()) ->
    ?adjustment_pending();
unmarshal(adjustment_status, ?legacy_adjustment_captured(At)) ->
    ?adjustment_captured(At);
unmarshal(adjustment_status, ?legacy_adjustment_cancelled(At)) ->
    ?adjustment_cancelled(At);

%% Refund

unmarshal(refund, #{
    <<"id">>         := ID,
    <<"created_at">> := CreatedAt,
    <<"rev">>        := Rev
} = V) ->
    #domain_InvoicePaymentRefund{
        id              = unmarshal(str, ID),
        status          = ?refund_pending(),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Rev),
        reason          = genlib_map:get(<<"reason">>, V)
    };

unmarshal(refund_status, <<"pending">>) ->
    ?refund_pending();
unmarshal(refund_status, <<"succeeded">>) ->
    ?refund_succeeded();
unmarshal(refund_status, [<<"failed">>, Failure]) ->
    ?refund_failed(unmarshal(failure, Failure));

%% Payer

unmarshal(payer, [3, #{
    <<"type">>                := <<"customer_payer">>,
    <<"customer_id">>         := CustomerID,
    <<"customer_binding_id">> := CustomerBindingID,
    <<"rec_payment_tool_id">> := RecurrentPaytoolID,
    <<"payment_tool">>        := PaymentTool,
    <<"contact_info">>        := ContactInfo
}]) ->
    ?customer_payer(
        unmarshal(str, CustomerID),
        unmarshal(str, CustomerBindingID),
        unmarshal(str, RecurrentPaytoolID),
        hg_payment_tool:unmarshal(PaymentTool),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, [2, #{
    <<"type">>           := <<"payment_resource_payer">>,
    <<"resource">>       := Resource,
    <<"contact_info">>   := ContactInfo
}]) ->
    ?payment_resource_payer(
        unmarshal(disposable_payment_resource, Resource),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, [2, #{
    <<"type">>                  := <<"customer_payer">>,
    <<"customer_id">>           := CustomerID,
    <<"customer_binding_id">>   := CustomerBindingID,
    <<"rec_payment_tool_id">>   := RecurrentPaytoolID,
    <<"payment_tool">>          := PaymentTool
}]) ->
    ?customer_payer(
        unmarshal(str, CustomerID),
        unmarshal(str, CustomerBindingID),
        unmarshal(str, RecurrentPaytoolID),
        hg_payment_tool:unmarshal(PaymentTool),
        get_customer_contact_info(get_customer(unmarshal(str, CustomerID)))
    );

unmarshal(payer, #{
    <<"payment_tool">>  := PaymentTool,
    <<"session_id">>    := SessionId,
    <<"client_info">>   := ClientInfo,
    <<"contact_info">>  := ContactInfo
}) ->
    Resource = #{
        <<"payment_tool">>         => PaymentTool,
        <<"payment_session_id">>   => SessionId,
        <<"client_info">>          => ClientInfo
    },
    ?payment_resource_payer(
        unmarshal(disposable_payment_resource, Resource),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, ?legacy_payer(PaymentTool, SessionId, ClientInfo, ContactInfo)) ->
    Resource = #{
        <<"payment_tool">>         => PaymentTool,
        <<"payment_session_id">>   => SessionId,
        <<"client_info">>          => ClientInfo
    },
    ?payment_resource_payer(
        unmarshal(disposable_payment_resource, Resource),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(disposable_payment_resource, #{
    <<"payment_tool">> := PaymentTool,
    <<"payment_session_id">> := PaymentSessionId,
    <<"client_info">> := ClientInfo
}) ->
    #domain_DisposablePaymentResource{
        payment_tool = hg_payment_tool:unmarshal(PaymentTool),
        payment_session_id = unmarshal(str, PaymentSessionId),
        client_info = unmarshal(client_info, ClientInfo)
    };


%% Client info

unmarshal(client_info, ?legacy_client_info(IpAddress, Fingerprint)) ->
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

unmarshal(client_info, ClientInfo) ->
    IpAddress = maps:get(<<"ip_address">>, ClientInfo, undefined),
    Fingerprint = maps:get(<<"fingerprint">>, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

%% Contract info

unmarshal(contact_info, ?legacy_contract_info(PhoneNumber, Email)) ->
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(contact_info, ContractInfo) ->
    PhoneNumber = maps:get(<<"phone_number">>, ContractInfo, undefined),
    Email = maps:get(<<"email">>, ContractInfo, undefined),
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(trx, #{
    <<"id">>    := ID,
    <<"extra">> := Extra
} = TRX) ->
    Timestamp = maps:get(<<"timestamp">>, TRX, undefined),
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(trx, ?legacy_trx(ID, Timestamp, Extra)) ->
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(interaction, #{<<"redirect">> := [<<"get_request">>, URI]}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, #{<<"redirect">> := [<<"post_request">>, #{
    <<"uri">>   := URI,
    <<"form">>  := Form
}]}) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};
unmarshal(interaction, #{<<"payment_terminal_receipt">> := #{
    <<"spid">>  := SPID,
    <<"due">>   := DueDate
}}) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(interaction, ?legacy_get_request(URI)) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, ?legacy_post_request(URI, Form)) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};
unmarshal(interaction, ?legacy_payment_terminal_reciept(SPID, DueDate)) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(sub_failure, undefined) ->
    undefined;
unmarshal(sub_failure, #{<<"code">> := Code} = SubFailure) ->
    #domain_SubFailure{
        code   = unmarshal(str        , Code),
        sub    = unmarshal(sub_failure, maps:get(<<"sub">>, SubFailure, undefined))
    };

unmarshal(failure, [3, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [3, [<<"failure">>, #{<<"code">> := Code} = Failure]]) ->
    {failure, #domain_Failure{
        code   = unmarshal(str        , Code),
        reason = unmarshal(str        , maps:get(<<"reason">>, Failure, undefined)),
        sub    = unmarshal(sub_failure, maps:get(<<"sub"   >>, Failure, undefined))
    }};

unmarshal(failure, [2, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [2, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {failure, #domain_Failure{
        code   = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};

unmarshal(failure, [1, ?legacy_operation_timeout()]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, ?legacy_external_failure(Code, Description)]) ->
    {failure, #domain_Failure{
        code   = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};

unmarshal(on_hold_expiration, <<"cancel">>) ->
    cancel;
unmarshal(on_hold_expiration, <<"capture">>) ->
    capture;

unmarshal(on_hold_expiration, OnHoldExpiration) when is_atom(OnHoldExpiration) ->
    OnHoldExpiration;

unmarshal(risk_score, <<"low">>) ->
    low;
unmarshal(risk_score, <<"high">>) ->
    high;
unmarshal(risk_score, <<"fatal">>) ->
    fatal;

unmarshal(risk_score, RiskScore) when is_atom(RiskScore) ->
    RiskScore;

unmarshal(_, Other) ->
    Other.
