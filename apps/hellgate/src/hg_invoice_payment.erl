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
%%%  - proper exception interface instead of dirtily copied `raise`

-module(hg_invoice_payment).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).

%% Machine like

-export([init/3]).
-export([start_session/1]).

-export([process_signal/3]).
-export([process_call/3]).

-export([merge_event/2]).

%%

-record(st, {
    payment  :: payment(),
    route    :: route(),
    cashflow :: cashflow(),
    session  :: session()
}).

-type st() :: #st{}.
-export_type([st/0]).

-type party()       :: dmsl_domain_thrift:'Party'().
-type invoice()     :: dmsl_domain_thrift:'Invoice'().
-type payment()     :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()  :: dmsl_domain_thrift:'InvoicePaymentID'().
-type route()       :: dmsl_domain_thrift:'InvoicePaymentRoute'().
-type cashflow()    :: dmsl_domain_thrift:'FinalCashFlow'().
-type target()      :: dmsl_proxy_provider_thrift:'TargetInvoicePaymentStatus'().
-type proxy_state() :: dmsl_proxy_thrift:'ProxyState'().

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

-define(BATCH_ID, 1).

%%

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

%%

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

-spec init(payment_id(), _, opts()) ->
    hg_machine:result().

init(PaymentID, PaymentParams, Opts) ->
    hg_log_scope:scope(
        payment,
        fun() -> init_(PaymentID, PaymentParams, Opts) end,
        #{
            id => PaymentID
        }
    ).

-spec init_(payment_id(), _, opts()) ->
    hg_machine:result().

init_(PaymentID, PaymentParams, #{party := Party} = Opts) ->
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Revision = hg_domain:head(),
    PaymentTerms = hg_party:get_payments_service_terms(
        Shop#domain_Shop.id,
        Party,
        Invoice#domain_Invoice.created_at
    ),
    VS0 = collect_varset(Shop, #{}),

    VS1 = validate_payment_params(PaymentParams, {Revision, PaymentTerms}, VS0),
    VS2 = validate_payment_cost(Invoice, {Revision, PaymentTerms}, VS1),
    Payment0 = construct_payment(PaymentID, Invoice, PaymentParams),
    RiskScore = inspect(Shop, Invoice, Payment0, Revision),

    Payment = Payment0#domain_InvoicePayment{risk_score = RiskScore},

    VS3 = VS2#{risk_score => RiskScore},
    Route = validate_route(hg_routing:choose(VS3, Revision)),
    FinalCashflow = hg_cashflow:finalize(
        collect_cash_flow({Revision, PaymentTerms}, Route, VS3),
        collect_cash_flow_context(Invoice, Payment),
        collect_account_map(Invoice, Shop, Route, VS3, Revision)
    ),
    _AccountsState = hg_accounting:plan(construct_plan_id(Invoice, Payment), {?BATCH_ID, FinalCashflow}),
    Events = [
        ?payment_ev(?payment_started(Payment, Route, FinalCashflow))
    ],
    Action = hg_machine_action:new(),
    {Events, Action}.

construct_payment(PaymentID, Invoice, PaymentParams) ->
    #domain_InvoicePayment{
        id           = PaymentID,
        created_at   = hg_datetime:format_now(),
        status       = ?pending(),
        cost         = Invoice#domain_Invoice.cost,
        payer        = PaymentParams#payproc_InvoicePaymentParams.payer
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
    {value, PMs} = hg_selector:reduce(PaymentMethodSelector, VS, Revision), % FIXME
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        raise_invalid_request(<<"Invalid payment method">>),
    VS.

validate_payment_cost(
    #domain_Invoice{cost = Cash},
    {Revision, #domain_PaymentsServiceTerms{cash_limit = LimitSelector}},
    VS
) ->
    {value, Limit} = hg_selector:reduce(LimitSelector, VS, Revision), % FIXME
    _ = validate_limit(Cash, Limit),
    VS#{cost => Cash}.

validate_limit(Cash, #domain_CashLimit{min = Min, max = Max}) ->
    _ = validate_bound(min, Min, Cash),
    _ = validate_bound(max, Max, Cash),
    ok.

validate_bound(_, {inclusive, V}, V) ->
    ok;
validate_bound(min, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A > Am orelse raise_invalid_request(<<"Limit exceeded">>);
validate_bound(max, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A < Am orelse raise_invalid_request(<<"Limit exceeded">>).

validate_route(Route = #domain_InvoicePaymentRoute{}) ->
    Route.

collect_varset(#domain_Shop{
    category = Category,
    account = #domain_ShopAccount{currency = Currency}
}, VS) ->
    VS#{
        category => Category,
        currency => Currency
    }.

%%

collect_cash_flow(
    {Revision, #domain_PaymentsServiceTerms{fees = MerchantCashFlowSelector}},
    #domain_InvoicePaymentRoute{terminal = TerminalRef},
    VS
) ->
    #domain_Terminal{cash_flow = ProviderCashFlow} = hg_domain:get(Revision, {terminal, TerminalRef}),
    {value, MerchantCashFlow} = hg_selector:reduce(MerchantCashFlowSelector, VS, Revision),
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
    SystemAccount = choose_system_account(Invoice, VS, Revision),
    #{
        {merchant , settlement} => MerchantAccount#domain_ShopAccount.settlement     ,
        {merchant , guarantee } => MerchantAccount#domain_ShopAccount.guarantee      ,
        {provider , settlement} => ProviderAccount#domain_TerminalAccount.settlement ,
        {system   , settlement} => SystemAccount#domain_SystemAccount.settlement
    }.

choose_system_account(Invoice, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    SystemAccountSetSelector = Globals#domain_Globals.system_account_set,
    {value, SystemAccountSetRef} = hg_selector:reduce(SystemAccountSetSelector, VS, Revision), % FIXME
    SystemAccountSet = hg_domain:get(Revision, {system_account_set, SystemAccountSetRef}),
    Currency = get_invoice_currency(Invoice),
    maps:get( % FIXME
        Currency,
        SystemAccountSet#domain_SystemAccountSet.accounts
    ).

construct_plan_id(
    #domain_Invoice{id = InvoiceID},
    #domain_InvoicePayment{id = PaymentID}
) ->
    <<InvoiceID/binary, ".", PaymentID/binary>>.

%%

-spec start_session(target()) ->
    hg_machine:result().

start_session(Target) ->
    Events = [?session_ev({started, Target})],
    Action = hg_machine_action:instant(),
    {Events, Action}.

%%

-spec process_signal(timeout, st(), opts()) ->
    {next | done, hg_machine:result()}.

process_signal(timeout, St, Options) ->
    hg_log_scope:scope(payment, fun() ->
        case get_status(St) of
            active ->
                process(St, Options);
            suspended ->
                fail(construct_failure(<<"provider_timeout">>), St)
        end
    end, get_st_meta(St)).

-spec process_call({callback, _}, st(), opts()) ->
    {_, {next | done, hg_machine:result()}}. % FIXME

process_call({callback, Payload}, St, Options) ->
    hg_log_scope:scope(payment, fun() ->
        case get_status(St) of
            suspended ->
                handle_callback(Payload, St, Options);
            active ->
                % there's ultimately no way how we could end up here
                error(invalid_session_status)
        end
    end, get_st_meta(St)).

process(St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    {ok, ProxyResult} = issue_process_call(ProxyContext, Options, St),
    handle_proxy_result(ProxyResult, St, Options).

handle_callback(Payload, St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    {ok, CallbackResult} = issue_callback_call(Payload, ProxyContext, Options, St),
    handle_callback_result(CallbackResult, Options, St).

handle_callback_result(#prxprv_CallbackResult{result = ProxyResult, response = Response}, Options, St) ->
    {What, {Events, Action}} = handle_proxy_result(ProxyResult, St, Options),
    {Response, {What, {[?session_ev(activated) | Events], Action}}}.

handle_proxy_result(
    #prxprv_ProxyResult{intent = {_, Intent}, trx = Trx, next_state = ProxyState},
    St, Options
) ->
    Events1 = bind_transaction(Trx, St),
    {What, {Events2, Action}} = handle_proxy_intent(Intent, ProxyState, St, Options),
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

handle_proxy_intent(#'FinishIntent'{status = {success, _}}, _ProxyState, St, Options) ->
    PaymentID = get_payment_id(St),
    Target = get_target(St),
    case get_target(St) of
        ?captured() ->
            _AccountsState = commit_plan(St, Options);
        ?cancelled(_) ->
            _AccountsState = rollback_plan(St, Options);
        ?processed() ->
            ok
    end,
    Events = [?payment_ev(?payment_status_changed(PaymentID, Target))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}};

handle_proxy_intent(#'FinishIntent'{status = {failure, Failure}}, _ProxyState, St, Options) ->
    _AccountsState = rollback_plan(St, Options),
    fail(convert_failure(Failure), St);

handle_proxy_intent(#'SleepIntent'{timer = Timer}, ProxyState, _St, _Options) ->
    Action = hg_machine_action:set_timer(Timer),
    Events = [?session_ev({proxy_state_changed, ProxyState})],
    {next, {Events, Action}};

handle_proxy_intent(
    #'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction},
    ProxyState, St, _Options
) ->
    Action = try_set_timer(Timer, hg_machine_action:set_tag(Tag)),
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

fail(Error, St) ->
    Events = [?payment_ev(?payment_status_changed(get_payment_id(St), ?failed(Error)))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}}.

commit_plan(St, Options) ->
    finalize_plan(fun hg_accounting:commit/2, St, Options).

rollback_plan(St, Options) ->
    finalize_plan(fun hg_accounting:rollback/2, St, Options).

finalize_plan(Finalizer, St, Options) ->
    PlanID = construct_plan_id(get_invoice(Options), get_payment(St)),
    FinalCashflow = get_final_cashflow(St),
    Finalizer(PlanID, [{?BATCH_ID, FinalCashflow}]).

get_final_cashflow(#st{cashflow = FinalCashflow}) ->
    FinalCashflow.

%%

construct_proxy_context(#st{payment = Payment, route = Route, session = Session}, Options) ->
    #prxprv_Context{
        session = construct_session(Session),
        payment = construct_payment_info(Payment, Options),
        options = collect_proxy_options(Route)
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

collect_proxy_options(#domain_InvoicePaymentRoute{provider = ProviderRef, terminal = TerminalRef}) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    lists:foldl(
        fun maps:merge/2,
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

-spec raise(term()) -> no_return().

raise(What) ->
    throw({exception, What}).

-spec raise_invalid_request(binary()) -> no_return().

raise_invalid_request(Why) ->
    raise(#'InvalidRequest'{errors = [Why]}).

%%

-spec merge_event(ev(), st()) -> st().

merge_event(?payment_ev(Event), St) ->
    merge_public_event(Event, St);
merge_event(?session_ev(Event), St) ->
    merge_session_event(Event, St).

merge_public_event(?payment_started(Payment, Route, Cashflow), undefined) ->
    #st{payment = Payment, route = Route, cashflow = Cashflow};
merge_public_event(?payment_bound(_, Trx), St = #st{payment = Payment}) ->
    St#st{payment = Payment#domain_InvoicePayment{trx = Trx}};
merge_public_event(?payment_status_changed(_, Status), St = #st{payment = Payment}) ->
    St#st{payment = Payment#domain_InvoicePayment{status = Status}};
merge_public_event(?payment_interaction_requested(_, _), St) ->
    St.

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

issue_process_call(ProxyContext, Opts, St) ->
    issue_call('ProcessPayment', [ProxyContext], Opts, St).

issue_callback_call(Payload, ProxyContext, Opts, St) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], Opts, St).

issue_call(Func, Args, Opts, St) ->
    CallOpts = get_call_options(St, Opts),
    hg_woody_wrapper:call('ProviderProxy', Func, Args, CallOpts).

get_call_options(#st{route = #domain_InvoicePaymentRoute{provider = ProviderRef}}, _Opts) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    #{url => ProxyDef#domain_ProxyDefinition.url}.

inspect(Shop, Invoice, Payment, Revision) ->
    #domain_Globals{inspector = InspectorRef} = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    hg_inspector:inspect(Shop, Invoice, Payment, Inspector, Revision).

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.
