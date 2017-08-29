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

-module(hg_invoice_payment).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).
-export([get_refunds/1]).
-export([get_refund/2]).
-export([get_adjustments/1]).
-export([get_adjustment/2]).

-export([get_activity/1]).

%% Business logic

-export([start_session/1]).

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
-type route()             :: dmsl_domain_thrift:'InvoicePaymentRoute'().
-type cash_flow()         :: dmsl_domain_thrift:'FinalCashFlow'().
-type trx_info()          :: dmsl_domain_thrift:'TransactionInfo'().
-type proxy_state()       :: dmsl_proxy_thrift:'ProxyState'().
-type session_result()    :: dmsl_payment_processing_thrift:'SessionResult'().

-type session() :: #{
    target      := target(),
    status      := active | suspended | finished,
    trx         := trx_info(),
    result      => session_result(),
    proxy_state => proxy_state()
}.

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

%%

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

-spec init(payment_id(), _, opts()) ->
    {payment(), hg_machine:result()}.

init(PaymentID, PaymentParams, Opts) ->
    hg_log_scope:scope(
        payment,
        fun() -> init_(PaymentID, PaymentParams, Opts) end,
        #{
            id => PaymentID
        }
    ).

-spec init_(payment_id(), _, opts()) ->
    {st(), hg_machine:result()}.

init_(PaymentID, PaymentParams, Opts) ->
    Party = get_party(Opts),
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Cost = get_invoice_cost(Invoice),
    Revision = hg_domain:head(),
    MerchantTerms = get_merchant_payments_terms(Invoice, Party),
    Payment = construct_payment(PaymentID, Cost, PaymentParams, Revision),
    VS0 = collect_varset(Party, Shop, #{}),
    VS1 = validate_payment(Payment, MerchantTerms, VS0, Revision),
    {RiskScore, VS2} = case inspect(Shop, Invoice, Payment, VS1) of
        {RS, _} = Result when RS == low; RS == high ->
            Result;
        {fatal, _} ->
            throw_invalid_request(<<"Fatal error">>)
    end,
    Route = validate_route(Payment, hg_routing:choose(VS2, Revision)),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    Terminal = get_terminal(Route, Revision),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS2, Revision),
    FinalCashflow = construct_final_cashflow(Payment, Shop, Terminal, Cashflow, VS2, Revision),
    _AccountsState = hg_accounting:plan(
        construct_payment_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Events = [?payment_started(Payment, RiskScore, Route, FinalCashflow)],
    {collapse_changes(Events), {Events, hg_machine_action:new()}}.

get_merchant_payments_terms(Opts) ->
    get_merchant_payments_terms(get_invoice(Opts), get_party(Opts)).

get_merchant_payments_terms(Invoice, Party) ->
    hg_party:get_payments_service_terms(
        get_invoice_shop_id(Invoice),
        Party,
        get_invoice_created_at(Invoice)
    ).

get_provider_payments_terms(Route, Revision) ->
    hg_routing:get_payments_terms(Route, Revision).

construct_payment(PaymentID, Cost, PaymentParams, Revision) ->
    #domain_InvoicePayment{
        id              = PaymentID,
        created_at      = hg_datetime:format_now(),
        domain_revision = Revision,
        status          = ?pending(),
        cost            = Cost,
        payer           = PaymentParams#payproc_InvoicePaymentParams.payer
    }.

validate_payment(Payment, PaymentTerms, VS0, Revision) ->
    VS1 = validate_payment_tool(
        get_payment_tool(Payment),
        PaymentTerms#domain_PaymentsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS2 = validate_payment_cost(
        get_payment_cost(Payment),
        PaymentTerms#domain_PaymentsServiceTerms.cash_limit,
        VS1,
        Revision
    ),
    VS2.

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

validate_route(_Payment, Route = #domain_InvoicePaymentRoute{}) ->
    Route;
validate_route(Payment, undefined) ->
    error({misconfiguration, {'No route found for a payment', Payment}}).

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

construct_final_cashflow(Payment, Shop, Terminal, Cashflow, VS, Revision) ->
    hg_cashflow:finalize(
        Cashflow,
        collect_cash_flow_context(Payment),
        collect_account_map(Payment, Shop, Terminal, VS, Revision)
    ).

collect_cash_flow_context(
    #domain_InvoicePayment{cost = Cost}
) ->
    #{
        payment_amount => Cost
    }.

collect_account_map(
    Payment,
    #domain_Shop{account = MerchantAccount},
    #domain_Terminal{account = ProviderAccount},
    VS,
    Revision
) ->
    Currency = get_currency(get_payment_cost(Payment)),
    SystemAccount = choose_system_account(Currency, VS, Revision),
    M = #{
        {merchant , settlement} => MerchantAccount#domain_ShopAccount.settlement     ,
        {merchant , guarantee } => MerchantAccount#domain_ShopAccount.guarantee      ,
        {provider , settlement} => ProviderAccount#domain_TerminalAccount.settlement ,
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

choose_system_account(Currency, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    SystemAccountSetSelector = Globals#domain_Globals.system_account_set,
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
    {ok, hg_machine:result()}.

start_session(Target) ->
    Events = [?session_ev(Target, ?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

%%

-spec refund(refund_params(), st(), opts()) ->
    {refund(), hg_machine:result()}.

refund(Params, St0, Opts) ->
    St = St0#st{opts = Opts},
    Revision = hg_domain:head(),
    Payment = get_payment(St),
    Route = get_route(St),
    Shop = get_shop(Opts),
    Terminal = get_terminal(Route, Revision),
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
    MerchantTerms = get_merchant_refunds_terms(get_merchant_payments_terms(Opts)),
    VS1 = validate_refund(Refund, Payment, MerchantTerms, VS0, Revision),
    ProviderTerms = get_provider_refunds_terms(get_provider_payments_terms(Route, Revision), Payment),
    Cashflow = collect_refund_cashflow(MerchantTerms, ProviderTerms, VS1, Revision),
    % TODO specific cashflow context needed, with defined `refund_amount` for instance
    FinalCashflow = construct_final_cashflow(Payment, Shop, Terminal, Cashflow, VS1, Revision),
    Changes = [
        ?refund_created(Refund, FinalCashflow),
        ?session_ev(?refunded(), ?session_started())
    ],
    RefundSt = collapse_refund_changes(Changes),
    _AccountsState = prepare_refund_cashflow(RefundSt, St),
    Action = hg_machine_action:instant(),
    {Refund, {[?refund_ev(ID, C) || C <- Changes], Action}}.

construct_refund_id(#st{refunds = Rs}) ->
    integer_to_binary(maps:size(Rs) + 1).

assert_no_refund_pending(#st{refunds = Rs}) ->
    genlib_map:foreach(fun (ID, R) -> assert_refund_finished(ID, get_refund(R)) end, Rs).

assert_refund_finished(ID, #domain_InvoicePaymentRefund{status = ?refund_pending()}) ->
    throw(#payproc_InvoicePaymentRefundPending{id = ID});
assert_refund_finished(_ID, #domain_InvoicePaymentRefund{}) ->
    ok.

collect_varset(St, Opts) ->
    collect_varset(get_party(Opts), get_shop(Opts), get_payment(St), #{}).

get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = Terms}) when Terms /= undefined ->
    Terms;
get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = undefined}) ->
    throw(#payproc_OperationNotPermitted{}).

get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = Terms}, _Payment) when Terms /= undefined ->
    Terms;
get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = undefined}, Payment) ->
    error({misconfiguration, {'No refund terms for a payment', Payment}}).

validate_refund(_Refund, Payment, RefundTerms, VS0, Revision) ->
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
    {adjustment(), hg_machine:result()}.

create_adjustment(Params, St, Opts) ->
    Payment = get_payment(St),
    Revision = get_adjustment_revision(Params),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_adjustment_pending(St),
    Shop = get_shop(Opts),
    MerchantTerms = get_merchant_payments_terms(Opts),
    Route = get_route(St),
    Terminal = get_terminal(Route, Revision),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    VS = collect_varset(St, Opts),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS, Revision),
    FinalCashflow = construct_final_cashflow(Payment, Shop, Terminal, Cashflow, VS, Revision),
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
    _AccountsState = prepare_adjustment_cashflow(Adjustment, St, Opts),
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

-spec capture_adjustment(adjustment_id(), st(), opts()) ->
    {ok, hg_machine:result()}.

capture_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, capture, St, Options).

-spec cancel_adjustment(adjustment_id(), st(), opts()) ->
    {ok, hg_machine:result()}.

cancel_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, cancel, St, Options).

finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(pending, Adjustment),
    _AccountsState = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
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
    {next | done, hg_machine:result()}.

process_signal(timeout, St, Options) ->
    hg_log_scope:scope(
        payment,
        fun() -> process_timeout(St#st{opts = Options}) end,
        get_st_meta(St)
    ).

process_timeout(St) ->
    Action = hg_machine_action:new(),
    case get_session_status(get_active_session(St)) of
        active ->
            process(Action, St);
        suspended ->
            process_callback_timeout(Action, St)
    end.

-spec process_call({callback, _}, st(), opts()) ->
    {_, {next | done, hg_machine:result()}}. % FIXME

process_call({callback, Payload}, St, Options) ->
    hg_log_scope:scope(
        payment,
        fun() -> process_callback(Payload, St#st{opts = Options}) end,
        get_st_meta(St)
    ).

process_callback(Payload, St) ->
    Action = hg_machine_action:new(),
    case get_session_status(get_active_session(St)) of
        suspended ->
            handle_callback(Payload, Action, St);
        active ->
            % there's ultimately no way how we could end up here
            error(invalid_session_status)
    end.

process_callback_timeout(Action, St) ->
    Session = get_active_session(St),
    Result = handle_proxy_callback_timeout(Action, Session),
    finish_processing(Result, St).

process(Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, ProxyResult} = issue_process_call(ProxyContext, St),
    Result = handle_proxy_result(ProxyResult, Action, get_active_session(St)),
    finish_processing(Result, St).

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
            _AccountsState = case Target of
                ?captured() ->
                    commit_payment_cashflow(St);
                ?cancelled() ->
                    rollback_payment_cashflow(St);
                ?processed() ->
                    undefined
            end,
            {done, {Events ++ [?payment_status_changed(Target)], Action}};
        #{status := finished, result := ?session_failed(Failure)} ->
            % TODO is it always rollback?
            _AccountsState = rollback_payment_cashflow(St),
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
            _AccountsState = commit_refund_cashflow(RefundSt1, St1),
            Events2 = [
                ?refund_ev(ID, ?refund_status_changed(?refund_succeeded())),
                ?payment_status_changed(?refunded())
            ],
            {done, {Events1 ++ Events2, Action}};
        #{status := finished, result := ?session_failed(Failure)} ->
            _AccountsState = rollback_refund_cashflow(RefundSt1, St1),
            Events2 = [
                ?refund_ev(ID, ?refund_status_changed(?refund_failed(Failure)))
            ],
            {done, {Events1 ++ Events2, Action}};
        #{} ->
            {next, {Events1, Action}}
    end.

handle_proxy_result(
    #prxprv_ProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, Action0),
    {wrap_session_events(Events1 ++ Events2 ++ Events3, Session), Action}.

handle_callback_result(
    #prxprv_CallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

handle_proxy_callback_result(
    #prxprv_CallbackProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0)),
    {wrap_session_events([?session_activated()] ++ Events1 ++ Events2 ++ Events3, Session), Action};
handle_proxy_callback_result(
    #prxprv_CallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {wrap_session_events(Events1 ++ Events2, Session), Action0}.

handle_proxy_callback_timeout(Action, Session) ->
    Events = [?session_finished(?session_failed(?operation_timeout()))],
    {wrap_session_events(Events, Session), Action}.

wrap_session_events(SessionEvents, #{target := Target}) ->
    [?session_ev(Target, Ev) || Ev <- SessionEvents].

bind_transaction(undefined, _Session) ->
    % no transaction yet
    [];
bind_transaction(Trx, #{trx := undefined}) ->
    % got transaction, nothing bound so far
    [?trx_bound(Trx)];
bind_transaction(Trx, #{trx := Trx}) ->
    % got the same transaction as one which has been bound previously
    [];
bind_transaction(Trx, #{trx := TrxWas}) ->
    % got transaction which differs from the bound one
    % verify against proxy contracts
    case Trx#domain_TransactionInfo.id of
        ID when ID =:= TrxWas#domain_TransactionInfo.id ->
            [?trx_bound(Trx)];
        _ ->
            error(proxy_contract_violated)
    end.

update_proxy_state(undefined) ->
    [];
update_proxy_state(ProxyState) ->
    [?proxy_st_changed(ProxyState)].

handle_proxy_intent(#'FinishIntent'{status = {success, _}}, Action) ->
    Events = [?session_finished(?session_succeeded())],
    {Events, Action};

handle_proxy_intent(#'FinishIntent'{status = {failure, Failure}}, Action) ->
    Events = [?session_finished(?session_failed(convert_failure(Failure)))],
    {Events, Action};

handle_proxy_intent(#'SleepIntent'{timer = Timer}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = [],
    {Events, Action};

handle_proxy_intent(#'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended() | try_request_interaction(UserInteraction)],
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

construct_proxy_context(St) ->
    #prxprv_Context{
        session      = construct_session(get_active_session(St)),
        payment_info = construct_payment_info(St),
        options      = collect_proxy_options(St)
    }.

construct_session(Session = #{target := Target}) ->
    #prxprv_Session{
        target = Target,
        state = maps:get(proxy_state, Session, undefined)
    }.

construct_payment_info(St) ->
    Payment = get_payment(St),
    Trx = get_trx(St),
    construct_payment_info(
        get_activity(St),
        St,
        #prxprv_PaymentInfo{
            shop = construct_proxy_shop(get_shop(get_opts(St))),
            invoice = construct_proxy_invoice(get_invoice(get_opts(St))),
            payment = construct_proxy_payment(Payment, Trx)
        }
    ).

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
    #prxprv_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        trx = Trx,
        payer = Payer,
        cost = construct_proxy_cash(Cost)
    }.

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
        route = #domain_InvoicePaymentRoute{provider = ProviderRef, terminal = TerminalRef}
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

convert_failure(#'Failure'{code = Code, description = Description}) ->
    ?external_failure(Code, Description).

%%

get_party(#{party := Party}) ->
    Party.

get_shop(#{party := Party, invoice := Invoice}) ->
    ShopID = Invoice#domain_Invoice.shop_id,
    Shops = Party#domain_Party.shops,
    maps:get(ShopID, Shops).

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

get_payment_tool(#domain_InvoicePayment{payer = #domain_Payer{payment_tool = PaymentTool}}) ->
    PaymentTool.

get_currency(#domain_Cash{currency = Currency}) ->
    Currency.

get_terminal(#domain_InvoicePaymentRoute{terminal = TerminalRef}, Revision) ->
    hg_domain:get(Revision, {terminal, TerminalRef}).

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
    St2 = set_trx(get_session_trx(Session), St1),
    case get_session_status(Session) of
        finished ->
            % FIXME leaky transactions
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
merge_session_change(?session_suspended(), Session) ->
    Session#{status := suspended};
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
        trx    => Trx
    }.

get_session(Target, #st{sessions = Sessions}) ->
    maps:get(Target, Sessions, undefined).

set_session(Target, Session, St = #st{sessions = Sessions}) ->
    St#st{sessions = Sessions#{Target => Session}}.

get_session_status(#{status := Status}) ->
    Status.

get_session_trx(#{trx := Trx}) ->
    Trx.

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

issue_process_call(ProxyContext, St) ->
    issue_call('ProcessPayment', [ProxyContext], St).

issue_callback_call(Payload, ProxyContext, St) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], St).

issue_call(Func, Args, St) ->
    CallOpts = get_call_options(St),
    hg_woody_wrapper:call('ProviderProxy', Func, Args, CallOpts).

get_call_options(St) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider(get_route(St))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#st{route = Route}) ->
    Route.

get_route_provider(#domain_InvoicePaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

inspect(Shop, Invoice, Payment = #domain_InvoicePayment{domain_revision = Revision}, VS) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    InspectorSelector = Globals#domain_Globals.inspector,
    InspectorRef = reduce_selector(inspector, InspectorSelector, VS, Revision),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    RiskScore = hg_inspector:inspect(Shop, Invoice, Payment, Inspector),
    {RiskScore, VS#{risk_score => RiskScore}}.

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.

%%

-spec get_log_params(change(), st()) ->
    {ok, #{type := invoice_payment_event, params := list(), message := string()}} | undefined.

get_log_params(?payment_started(Payment, _, _, Cashflow), _) ->
    Params = [{accounts, get_partial_remainders(Cashflow)}],
    make_log_params(invoice_payment_started, Payment, Params);
get_log_params(?payment_status_changed({Status, _}), State) ->
    Payment = get_payment(State),
    Cashflow = get_cashflow(State),
    Params = [{status, Status}, {accounts, get_partial_remainders(Cashflow)}],
    make_log_params(invoice_payment_status_changed, Payment, Params);
get_log_params(_, _) ->
    undefined.

make_log_params(EventType, Payment, Params) ->
    #domain_InvoicePayment{
        id = ID,
        cost = ?cash(Amount, Currency)
    } = Payment,
    Result = #{
        type => invoice_payment_event,
        params => [{type, EventType}, {id, ID}, {cost, [{amount, Amount}, {currency, Currency}]} | Params],
        message => get_message(EventType)
    },
    {ok, Result}.

get_partial_remainders(CashFlow) ->
    Remainders = maps:to_list(hg_cashflow:get_partial_remainders(CashFlow)),
    lists:map(
        fun ({Account, ?cash(Amount, Currency)}) ->
            Remainder = [{remainder, [{amount, Amount}, {currency, Currency}]}],
            {get_account_key(Account), Remainder}
        end,
        Remainders
    ).

get_account_key({AccountParty, AccountType}) ->
    list_to_binary(lists:concat([atom_to_list(AccountParty), ".", atom_to_list(AccountType)])).

get_message(invoice_payment_started) ->
    "Invoice payment is started";
get_message(invoice_payment_status_changed) ->
    "Invoice payment status is changed".
