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
-export([get_adjustment/2]).

%% Machine like

-export([init/3]).

-export([process_signal/3]).
-export([process_call/3]).

-export([start_session/1]).

-export([create_adjustment/3]).
-export([capture_adjustment/3]).
-export([cancel_adjustment/3]).

-export([merge_event/2]).

%%

-record(st, {
    payment          :: undefined | payment(),
    session          :: undefined | session(),
    adjustments = [] :: [adjustment()]
}).

-type st() :: #st{}.
-export_type([st/0]).

-type party()             :: dmsl_domain_thrift:'Party'().
-type invoice()           :: dmsl_domain_thrift:'Invoice'().
-type payment()           :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()        :: dmsl_domain_thrift:'InvoicePaymentID'().
-type adjustment()        :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()     :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type target()            :: dmsl_proxy_provider_thrift:'TargetInvoicePaymentStatus'().
-type proxy_state()       :: dmsl_proxy_thrift:'ProxyState'().

-type session() :: #{
    target      => target(),
    status      => active | suspended,
    proxy_state => proxy_state() | undefined
}.

%%

-include("domain.hrl").
-include("invoice_events.hrl").

-type ev() ::
    {invoice_payment_event, dmsl_payment_processing_thrift:'InvoicePaymentEvent'()} |
    {session_event, session_ev()}.

-type session_ev() ::
    {started, target()} |
    {proxy_state_changed, proxy_state()} |
    suspended |
    activated.

-define(session_ev(E), {session_event, E}).

%%

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

-spec get_adjustment(adjustment_id(), st()) -> adjustment() | no_return().

get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoicePaymentAdjustment.id, As) of
        Adjustment = #domain_InvoicePaymentAdjustment{} ->
            Adjustment;
        false ->
            throw(#payproc_InvoicePaymentAdjustmentNotFound{})
    end.

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
    {payment(), hg_machine:result()}.

init_(PaymentID, PaymentParams, #{party := Party} = Opts) ->
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Revision = hg_domain:head(),
    PaymentTerms = get_merchant_payment_terms(Opts),
    VS0 = collect_varset(Party, Shop, #{}),
    VS1 = validate_payment_params(PaymentParams, {Revision, PaymentTerms}, VS0),
    VS2 = validate_payment_cost(Invoice, {Revision, PaymentTerms}, VS1),
    Payment0 = construct_payment(PaymentID, Invoice, PaymentParams, Revision),
    {Payment, VS3} = inspect(Shop, Invoice, Payment0, VS2),
    Route = validate_route(Payment, hg_routing:choose(VS3, Revision)),
    FinalCashflow = construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS3, Revision),
    _AccountsState = hg_accounting:plan(
        construct_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Event = ?payment_ev(?payment_started(Payment, Route, FinalCashflow)),
    {Payment, {[Event], hg_machine_action:new()}}.

get_merchant_payment_terms(Opts) ->
    Invoice = get_invoice(Opts),
    hg_party:get_payments_service_terms(
        get_shop_id(Invoice),
        get_party(Opts),
        get_created_at(Invoice)
    ).

construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS, Revision) ->
    hg_cashflow:finalize(
        collect_cash_flow({Revision, PaymentTerms}, Route, VS),
        collect_cash_flow_context(Invoice, Payment),
        collect_account_map(Invoice, Shop, Route, VS, Revision)
    ).

construct_payment(PaymentID, Invoice, PaymentParams, Revision) ->
    #domain_InvoicePayment{
        id              = PaymentID,
        created_at      = hg_datetime:format_now(),
        domain_revision = Revision,
        status          = ?pending(),
        cost            = Invoice#domain_Invoice.cost,
        payer           = PaymentParams#payproc_InvoicePaymentParams.payer
    }.

validate_payment_params(
    #payproc_InvoicePaymentParams{payer = #domain_Payer{payment_tool = PaymentTool}},
    Terms,
    VS
) ->
    VS1 = validate_payment_tool(PaymentTool, Terms, VS),
    VS1#{payment_tool => PaymentTool}.

validate_payment_tool(
    PaymentTool,
    {Revision, #domain_PaymentsServiceTerms{payment_methods = PaymentMethodSelector}},
    VS
) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    VS.

validate_payment_cost(
    #domain_Invoice{cost = Cash},
    {Revision, #domain_PaymentsServiceTerms{cash_limit = LimitSelector}},
    VS
) ->
    Limit = reduce_selector(cash_limit, LimitSelector, VS, Revision),
    ok = validate_limit(Cash, Limit),
    VS#{cost => Cash}.

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

collect_varset(Party, Shop, #domain_InvoicePayment{
    cost = Cash,
    payer = #domain_Payer{payment_tool = PaymentTool}
}, VS) ->
    VS0 = collect_varset(Party, Shop, VS),
    VS0#{
        cost         => Cash,
        payment_tool => PaymentTool
    }.

%%

collect_cash_flow(
    {Revision, #domain_PaymentsServiceTerms{fees = MerchantCashFlowSelector}},
    #domain_InvoicePaymentRoute{terminal = TerminalRef},
    VS
) ->
    #domain_Terminal{cash_flow = ProviderCashFlow} = hg_domain:get(Revision, {terminal, TerminalRef}),
    MerchantCashFlow = reduce_selector(merchant_cash_flow, MerchantCashFlowSelector, VS, Revision),
    MerchantCashFlow ++ ProviderCashFlow.

collect_cash_flow_context(
    #domain_Invoice{cost = InvoiceCost},
    #domain_InvoicePayment{cost = PaymentCost}
) ->
    #{
        invoice_amount => InvoiceCost,
        payment_amount => PaymentCost
    }.

collect_account_map(Invoice, Shop, Route, VS, Revision) ->
    TerminalRef = Route#domain_InvoicePaymentRoute.terminal,
    #domain_Terminal{account = ProviderAccount} = hg_domain:get(Revision, {terminal, TerminalRef}),
    #domain_Shop{account = MerchantAccount} = Shop,
    Currency = get_invoice_currency(Invoice),
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

construct_plan_id(
    #domain_Invoice{id = InvoiceID},
    #domain_InvoicePayment{id = PaymentID}
) ->
    <<InvoiceID/binary, ".", PaymentID/binary>>.

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
    Events = [?session_ev({started, Target})],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

%%

-spec create_adjustment(adjustment_params(), st(), opts()) ->
    {adjustment(), hg_machine:result()}.

create_adjustment(Params, St, Opts) ->
    Payment = get_payment(St),
    Revision = get_adjustment_revision(Params),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_adjustment_pending(St),
    Party = get_party(Opts),
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    PaymentTerms = get_merchant_payment_terms(Opts),
    Route = get_route(Payment),
    VS = collect_varset(Party, Shop, Payment, #{}),
    FinalCashflow = construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS, Revision),
    Adjustment = #domain_InvoicePaymentAdjustment{
        id              = construct_id(adjustment, St),
        status          = ?adjustment_pending(),
        created_at      = hg_datetime:format_now(),
        domain_revision = Revision,
        reason          = Params#payproc_InvoicePaymentAdjustmentParams.reason,
        cash_flow       = FinalCashflow
    },
    _AccountsState = prepare_adjustment_cashflow(Adjustment, St, Opts),
    Event = ?payment_ev(?adjustment_ev(?adjustment_created(get_payment_id(St), Adjustment))),
    {Adjustment, {[Event], hg_machine_action:new()}}.

get_adjustment_revision(Params) ->
    hg_utils:select_defined(
        Params#payproc_InvoicePaymentAdjustmentParams.domain_revision,
        hg_domain:head()
    ).

construct_id(adjustment, #st{adjustments = As}) ->
    integer_to_binary(length(As) + 1).

assert_payment_status(Status, #domain_InvoicePayment{status = {Status, _}}) ->
    ok;
assert_payment_status(_, #domain_InvoicePayment{status = Status}) ->
    throw(#payproc_InvalidPaymentStatus{status = Status}).

assert_no_adjustment_pending(#st{adjustments = [Adjustment | _]}) ->
    assert_adjustment_finalized(Adjustment);
assert_no_adjustment_pending(#st{adjustments = []}) ->
    ok.

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
    Event = ?payment_ev(?adjustment_ev(?adjustment_status_changed(get_payment_id(St), ID, Status))),
    {ok, {[Event], hg_machine_action:new()}}.

prepare_adjustment_cashflow(Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment, St),
    hg_accounting:plan(PlanID, Plan).

finalize_adjustment_cashflow(Intent, Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment, St),
    case Intent of
        capture ->
            hg_accounting:commit(PlanID, Plan);
        cancel ->
            hg_accounting:rollback(PlanID, Plan)
    end.

get_adjustment_cashflow_plan(Adjustment, St) ->
    [
        {1, hg_cashflow:revert(get_cashflow(get_payment(St)))},
        {2, get_adjustment_cashflow(Adjustment)}
    ].

assert_adjustment_status(Status, #domain_InvoicePaymentAdjustment{status = {Status, _}}) ->
    ok;
assert_adjustment_status(_, #domain_InvoicePaymentAdjustment{status = Status}) ->
    throw(#payproc_InvalidPaymentAdjustmentStatus{status = Status}).

construct_adjustment_plan_id(Adjustment, St, Options) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(Options)),
        get_payment_id(St),
        {adj, get_adjustment_id(Adjustment)}
    ]).

get_adjustment_id(#domain_InvoicePaymentAdjustment{id = ID}) ->
    ID.

get_adjustment_cashflow(#domain_InvoicePaymentAdjustment{cash_flow = Cashflow}) ->
    Cashflow.

%%

-spec process_signal(timeout, st(), opts()) ->
    {next | done, hg_machine:result()}.

process_signal(timeout, St, Options) ->
    Action = hg_machine_action:new(),
    hg_log_scope:scope(payment, fun() ->
        case get_status(St) of
            active ->
                process(Action, St, Options);
            suspended ->
                fail(construct_failure(<<"provider_timeout">>), Action, St)
        end
    end, get_st_meta(St)).

-spec process_call({callback, _}, st(), opts()) ->
    {_, {next | done, hg_machine:result()}}. % FIXME

process_call({callback, Payload}, St, Options) ->
    hg_log_scope:scope(payment, fun() ->
        case get_status(St) of
            suspended ->
                Action = hg_machine_action:unset_timer(),
                handle_callback(Payload, Action, St, Options);
            active ->
                % there's ultimately no way how we could end up here
                error(invalid_session_status)
        end
    end, get_st_meta(St)).

process(Action0, St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    {ok, ProxyResult} = issue_process_call(ProxyContext, St, Options),
    handle_proxy_result(ProxyResult, Action0, St, Options).

handle_callback(Payload, Action0, St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    {ok, CallbackResult} = issue_callback_call(Payload, ProxyContext, St, Options),
    handle_callback_result(CallbackResult, Action0, St, Options).

handle_callback_result(
    #prxprv_CallbackResult{result = ProxyResult, response = Response},
    Action0, St, Options
) ->
    {What, {Events, Action}} = handle_proxy_result(ProxyResult, Action0, St, Options),
    {Response, {What, {[?session_ev(activated) | Events], Action}}}.

handle_proxy_result(
    #prxprv_ProxyResult{intent = {_, Intent}, trx = Trx, next_state = ProxyState},
    Action0, St, Options
) ->
    Events1 = bind_transaction(Trx, St),
    {What, {Events2, Action}} = handle_proxy_intent(Intent, ProxyState, Action0, St, Options),
    {What, {Events1 ++ Events2, Action}}.

bind_transaction(undefined, _St) ->
    % no transaction yet
    [];
bind_transaction(Trx, #st{payment = #domain_InvoicePayment{id = PaymentID, trx = undefined}}) ->
    % got transaction, nothing bound so far
    [?payment_ev(?payment_bound(PaymentID, Trx))];
bind_transaction(Trx, #st{payment = #domain_InvoicePayment{trx = Trx}}) ->
    % got the same transaction as one which has been bound previously
    [];
bind_transaction(Trx, #st{payment = #domain_InvoicePayment{id = PaymentID, trx = TrxWas}}) ->
    % got transaction which differs from the bound one
    % verify against proxy contracts
    case Trx#domain_TransactionInfo.id of
        ID when ID =:= TrxWas#domain_TransactionInfo.id ->
            [?payment_ev(?payment_bound(PaymentID, Trx))];
        _ ->
            error(proxy_contract_violated)
    end.

handle_proxy_intent(#'FinishIntent'{status = {success, _}}, _ProxyState, Action, St, Options) ->
    PaymentID = get_payment_id(St),
    Target = get_target(St),
    case get_target(St) of
        {captured, _} ->
            _AccountsState = commit_plan(St, Options);
        {cancelled, _} ->
            _AccountsState = rollback_plan(St, Options);
        {processed, _} ->
            ok
    end,
    Events = [?payment_ev(?payment_status_changed(PaymentID, Target))],
    {done, {Events, Action}};

handle_proxy_intent(#'FinishIntent'{status = {failure, Failure}}, _ProxyState, Action0, St, Options) ->
    _AccountsState = rollback_plan(St, Options),
    fail(convert_failure(Failure), Action0, St);

handle_proxy_intent(#'SleepIntent'{timer = Timer}, ProxyState, Action0, _St, _Options) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = [?session_ev({proxy_state_changed, ProxyState})],
    {next, {Events, Action}};

handle_proxy_intent(
    #'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction},
    ProxyState, Action0, St, _Options
) ->
    Action = try_set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [
        ?session_ev({proxy_state_changed, ProxyState}),
        ?session_ev(suspended)
        | try_emit_interaction_event(UserInteraction, St)
    ],
    {next, {Events, Action}}.

try_set_timer(undefined, Action) ->
    Action;
try_set_timer(Timer, Action) ->
    hg_machine_action:set_timer(Timer, Action).

try_emit_interaction_event(undefined, _St) ->
    [];
try_emit_interaction_event(UserInteraction, St) ->
    [?payment_ev(?payment_interaction_requested(get_payment_id(St), UserInteraction))].

fail(Error, Action, St) ->
    Events = [?payment_ev(?payment_status_changed(get_payment_id(St), ?failed(Error)))],
    {done, {Events, Action}}.

commit_plan(St, Options) ->
    finalize_plan(fun hg_accounting:commit/2, St, Options).

rollback_plan(St, Options) ->
    finalize_plan(fun hg_accounting:rollback/2, St, Options).

finalize_plan(Finalizer, St, Options) ->
    PlanID = construct_plan_id(get_invoice(Options), get_payment(St)),
    FinalCashflow = get_cashflow(get_payment(St)),
    Finalizer(PlanID, [{1, FinalCashflow}]).

get_cashflow(#domain_InvoicePayment{cash_flow = FinalCashflow}) ->
    FinalCashflow.

%%

construct_proxy_context(#st{payment = Payment, session = Session}, Options) ->
    #prxprv_Context{
        session = construct_session(Session),
        payment_info = construct_payment_info(Payment, Options),
        options = collect_proxy_options(Payment)
    }.

construct_session(#{target := Target, proxy_state := ProxyState}) ->
    #prxprv_Session{
        target = Target,
        state = ProxyState
    }.

construct_payment_info(Payment, Options) ->
    #prxprv_PaymentInfo{
        shop = construct_proxy_shop(Options),
        invoice = construct_proxy_invoice(Options),
        payment = construct_proxy_payment(Payment)
    }.

construct_proxy_payment(#domain_InvoicePayment{
    id = ID,
    created_at = CreatedAt,
    trx = Trx,
    payer = Payer,
    cost = Cost
}) ->
    #prxprv_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        trx = Trx,
        payer = Payer,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_invoice(#{invoice := #domain_Invoice{
    id = InvoiceID,
    created_at = CreatedAt,
    due = Due,
    details = Details,
    cost = Cost
}}) ->
    #prxprv_Invoice{
        id = InvoiceID,
        created_at =  CreatedAt,
        due =  Due,
        details = Details,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_shop(Options) ->
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        category = ShopCategoryRef
    } = get_shop(Options),
    ShopCategory = hg_domain:get(
        hg_domain:head(),
        {category, ShopCategoryRef}
    ),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails
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

collect_proxy_options(
    #domain_InvoicePayment{
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

construct_failure(Code) when is_binary(Code) ->
    #domain_OperationFailure{code = Code}.

convert_failure(#'Failure'{code = Code, description = Description}) ->
    #domain_OperationFailure{code = Code, description = Description}.

%%

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_shop_id(#domain_Invoice{shop_id = ShopID}) ->
    ShopID.

get_created_at(#domain_Invoice{created_at = Dt}) ->
    Dt.

get_party(#{party := Party}) ->
    Party.

get_shop(#{party := Party, invoice := Invoice}) ->
    ShopID = Invoice#domain_Invoice.shop_id,
    Shops = Party#domain_Party.shops,
    maps:get(ShopID, Shops).

get_invoice_currency(#domain_Invoice{cost = #domain_Cash{currency = Currency}}) ->
    Currency.

%%

get_payment_id(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    ID.

get_status(#st{session = #{status := Status}}) ->
    Status.

get_target(#st{session =  #{target := Target}}) ->
    Target.

%%

-spec throw_invalid_request(binary()) -> no_return().

throw_invalid_request(Why) ->
    throw(#'InvalidRequest'{errors = [Why]}).

%%

-spec merge_event(ev(), st() | undefined) -> st().

merge_event(Event, undefined) ->
    merge_event(Event, #st{});

merge_event(?payment_ev(Event), St) ->
    merge_public_event(Event, St);
merge_event(?session_ev(Event), St) ->
    merge_session_event(Event, St).

merge_public_event(?payment_started(Payment, Route, Cashflow), St) ->
    St#st{payment = Payment#domain_InvoicePayment{route = Route, cash_flow = Cashflow}};
merge_public_event(?payment_bound(_, Trx), St = #st{payment = Payment}) ->
    St#st{payment = Payment#domain_InvoicePayment{trx = Trx}};
merge_public_event(?payment_status_changed(_, Status), St = #st{payment = Payment}) ->
    St#st{payment = Payment#domain_InvoicePayment{status = Status}};
merge_public_event(?payment_interaction_requested(_, _), St) ->
    St;

merge_public_event(?adjustment_ev(Event), St) ->
    merge_adjustment_event(Event, St).

merge_adjustment_event(?adjustment_created(_, Adjustment), St = #st{adjustments = As}) ->
    St#st{adjustments = [Adjustment | As]};
merge_adjustment_event(?adjustment_status_changed(_, ID, Status), St0) ->
    Adjustment = get_adjustment(ID, St0),
    St1 = set_adjustment(Adjustment#domain_InvoicePaymentAdjustment{status = Status}, St0),
    case Status of
        ?adjustment_captured(_) ->
            set_payment(set_cashflow(get_adjustment_cashflow(Adjustment), get_payment(St1)), St1);
        ?adjustment_cancelled(_) ->
            St1
    end.

set_payment(Payment, St = #st{}) ->
    St#st{payment = Payment}.

set_cashflow(Cashflow, Payment = #domain_InvoicePayment{}) ->
    Payment#domain_InvoicePayment{cash_flow = Cashflow}.

set_adjustment(Adjustment, St = #st{adjustments = As}) ->
    ID = get_adjustment_id(Adjustment),
    St#st{adjustments = lists:keyreplace(ID, #domain_InvoicePaymentAdjustment.id, As, Adjustment)}.

%% TODO session_finished?
merge_session_event({started, Target}, St) ->
    St#st{session = create_session(Target)};
merge_session_event({proxy_state_changed, ProxyState}, St = #st{session = Session}) ->
    St#st{session = Session#{proxy_state => ProxyState}};
merge_session_event(activated, St = #st{session = Session}) ->
    St#st{session = Session#{status => active}};
merge_session_event(suspended, St = #st{session = Session}) ->
    St#st{session = Session#{status => suspended}}.

create_session(Target) ->
    #{
        target => Target,
        status => active,
        proxy_state => undefined
    }.

%%

issue_process_call(ProxyContext, St, Opts) ->
    issue_call('ProcessPayment', [ProxyContext], St, Opts).

issue_callback_call(Payload, ProxyContext, St, Opts) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], St, Opts).

issue_call(Func, Args, St, Opts) ->
    CallOpts = get_call_options(St, Opts),
    hg_woody_wrapper:call('ProviderProxy', Func, Args, CallOpts).

get_call_options(St, _Opts) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider(get_route(get_payment(St)))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#domain_InvoicePayment{route = Route}) ->
    Route.

get_route_provider(#domain_InvoicePaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

inspect(Shop, Invoice, Payment = #domain_InvoicePayment{domain_revision = Revision}, VS) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    InspectorSelector = Globals#domain_Globals.inspector,
    InspectorRef = reduce_selector(inspector, InspectorSelector, VS, Revision),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    RiskScore = hg_inspector:inspect(Shop, Invoice, Payment, Inspector),
    {
        Payment#domain_InvoicePayment{risk_score = RiskScore},
        VS#{risk_score => RiskScore}
    }.

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.
