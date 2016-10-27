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
%%%     - remove ability to throw `TryLater` from `HandlePaymentCallback`
%%%     - drop `TryLater` completely (?)
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
-type cashflow()    :: dmsl_domain_thrift:'InvoicePaymentCashFlow'().
-type target()      :: dmsl_proxy_provider_thrift:'Target'().
-type proxy_state() :: dmsl_proxy_thrift:'ProxyState'().

-type session() :: #{
    target      => target(),
    status      => active | suspended,
    proxy_state => proxy_state() | undefined,
    retry       => genlib_retry:strategy()
}.

%%

-include("invoice_events.hrl").

-type ev() ::
    {invoice_payment_event, dmsl_payment_processing_thrift:'InvoicePaymentEvent'()} |
    {session_event, session_ev()}.

-type session_ev() ::
    {started, target()} |
    {proxy_state_changed, proxy_state()} |
    {proxy_retry_changed, genlib_retry:strategy()} |
    suspended |
    activated.

-define(session_ev(E), {session_event, E}).

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
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Revision = get_invoice_revision(Invoice),
    Terms = get_payments_service_terms(Shop),
    VS0 = collect_varset(Shop, #{}),
    VS1 = validate_payment_params(PaymentParams, Terms, VS0),
    VS2 = validate_payment_amount(Invoice, Terms, VS1),
    Payment = construct_payment(PaymentID, Invoice, PaymentParams),
    Route = validate_route(hg_routing:choose(VS2, Revision)),
    Computed = hg_cashflow:compute( % FIXME
        collect_cash_flow(Terms, Route, VS2, Revision),
        get_invoice_currency(Invoice),
        collect_cash_flow_context(Invoice, Payment)
    ),
    AccountMap = collect_account_map(Computed, Shop, Route, VS2, Revision),
    _AccountsState = hg_accounting:plan(
        construct_plan_id(Invoice, Payment),
        Computed,
        AccountMap
    ),
    Cashflow = construct_payment_cash_flow(Computed, AccountMap),
    Events = [?payment_ev(?payment_started(Payment, Route, Cashflow))],
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

construct_payment_cash_flow(Computed, AccountMap) ->
    #domain_InvoicePaymentCashFlow{
        account_map = AccountMap,
        final_cash_flow = [
            #domain_CashFlowPosting{
                source = S,
                destination = D,
                volume = {fixed, #domain_CashVolumeFixed{amount = A}}
            } ||
                {S, D, A, _} <- Computed
        ]
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

validate_payment_amount(
    #domain_Invoice{cost = #domain_Cash{amount = Amount}},
    {Revision, #domain_PaymentsServiceTerms{limits = LimitSelector}},
    VS
) ->
    {value, Limit} = hg_selector:reduce(LimitSelector, VS, Revision), % FIXME
    _ = validate_limit(Amount, Limit),
    VS#{amount => Amount}.

validate_limit(Amount, #domain_AmountLimit{min = Min, max = Max}) ->
    _ = validate_bound(min, Min, Amount),
    _ = validate_bound(max, Max, Amount),
    ok.

validate_bound(_, {inclusive, V}, V) ->
    ok;
validate_bound(min, {_, B}, V) ->
    V > B orelse raise_invalid_request(<<"Limit exceeded">>);
validate_bound(max, {_, B}, V) ->
    V < B orelse raise_invalid_request(<<"Limit exceeded">>).

validate_route(Route = #domain_InvoicePaymentRoute{}) ->
    Route.

collect_varset(#domain_Shop{
    category = Category,
    accounts = #domain_ShopAccountSet{currency = Currency}
}, VS) ->
    VS#{
        category => Category,
        currency => Currency
    }.

%%

collect_cash_flow(
    {_Revision, #domain_PaymentsServiceTerms{fees = MerchantCashFlowSelector}},
    #domain_InvoicePaymentRoute{terminal = TerminalRef},
    VS,
    Revision
) ->
    #domain_Terminal{cash_flow = ProviderCashFlow} = hg_domain:get(Revision, {terminal, TerminalRef}),
    {value, MerchantCashFlow} = hg_selector:reduce(MerchantCashFlowSelector, VS, Revision),
    MerchantCashFlow ++ ProviderCashFlow.

collect_cash_flow_context(
    #domain_Invoice{cost = #domain_Cash{amount = InvoiceAmount}},
    #domain_InvoicePayment{cost = #domain_Cash{amount = PaymentAmount}}
) ->
    #{
        invoice_amount => InvoiceAmount,
        payment_amount => PaymentAmount
    }.

collect_account_map(Computed, Shop, Route, VS, Revision) ->
    Accounts = collect_accounts(Computed),
    TerminalRef = Route#domain_InvoicePaymentRoute.terminal,
    #domain_Terminal{accounts = ProviderAccountSet} = hg_domain:get(Revision, {terminal, TerminalRef}),
    #domain_Shop{accounts = MerchantAccountSet} = Shop,
    SystemAccountSet = choose_system_account(VS, Revision),
    lists:foldl(
        fun
            (A = #domain_CashFlowAccount{party = merchant, designation = <<"general">>}, M) ->
                M#{A => MerchantAccountSet#domain_ShopAccountSet.general};
            (A = #domain_CashFlowAccount{party = merchant, designation = <<"guarantee">>}, M) ->
                M#{A => MerchantAccountSet#domain_ShopAccountSet.guarantee};
            (A = #domain_CashFlowAccount{party = provider, designation = <<"receipt">>}, M) ->
                M#{A => ProviderAccountSet#domain_TerminalAccountSet.receipt};
            (A = #domain_CashFlowAccount{party = provider, designation = <<"compensation">>}, M) ->
                M#{A => ProviderAccountSet#domain_TerminalAccountSet.compensation};
            (A = #domain_CashFlowAccount{party = system, designation = <<"compensation">>}, M) ->
                M#{A => SystemAccountSet#domain_SystemAccountSet.compensation}
        end,
        #{},
        Accounts
    ).

collect_accounts(Computed) ->
    lists:usort(lists:foldl(fun ({S, D, _, _}, Acc) -> [S, D | Acc] end, [], Computed)).

choose_system_account(VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    SystemAccountSetSelector = Globals#domain_Globals.system_accounts,
    {value, [SystemAccountSetRef | _]} = hg_selector:reduce(SystemAccountSetSelector, VS, Revision), % FIXME
    hg_domain:get(Revision, {system_account_set, SystemAccountSetRef}).

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
    case get_status(St) of
        active ->
            process(St, Options);
        suspended ->
            fail(construct_error(<<"provider_timeout">>), St)
    end.

-spec process_call({callback, _}, st(), opts()) ->
    {_, {next | done, hg_machine:result()}}. % FIXME

process_call({callback, Payload}, St, Options) ->
    case get_status(St) of
        suspended ->
            handle_callback(Payload, St, Options);
        active ->
            % there's ultimately no way how we could end up here
            error(invalid_session_status)
    end.

process(St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_process_result(issue_process_call(ProxyContext, Options, St), Options, St).

handle_process_result(Result, Options, St) ->
    case Result of
        ProxyResult = #prxprv_ProxyResult{} ->
            handle_proxy_result(ProxyResult, St, Options);
        {exception, Exception} ->
            handle_exception(Exception, St);
        {error, Error} ->
            error(Error)
    end.

handle_callback(Payload, St, Options) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_callback_result(issue_callback_call(Payload, ProxyContext, Options, St), Options, St).

handle_callback_result(Result, Options, St) ->
    case Result of
        #prxprv_CallbackResult{result = ProxyResult, response = Response} ->
            {What, {Events, Action}} = handle_proxy_result(ProxyResult, St, Options),
            {Response, {What, {[?session_ev(activated) | Events], Action}}};
        {error, Error} ->
            error(Error)
    end.

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

handle_proxy_intent(#'FinishIntent'{status = {ok, _}}, _ProxyState, St, Options) ->
    PaymentID = get_payment_id(St),
    Target = get_target(St),
    case get_target(St) of
        ?captured() ->
            _AccountsState = commit_plan(St, Options);
        ?cancelled(_) ->
            _AccountsState = rollback_plan(St, Options);
        ?processed() ->
            ok % FIXME dont know what is this
    end,
    Events = [?payment_ev(?payment_status_changed(PaymentID, Target))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}};

handle_proxy_intent(#'FinishIntent'{status = {failure, Error}}, _ProxyState, St, Options) ->
    _AccountsState = rollback_plan(St, Options),
    fail(construct_error(Error), St);

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

handle_exception(#'TryLater'{e = Error}, St) ->
    case retry(St) of
        {wait, Timeout, Events} ->
            Action = hg_machine_action:set_timeout(Timeout),
            {next, {Events, Action}};
        finish ->
            fail(construct_error(Error), St)
    end.

retry(#st{session = #{retry := Retry}}) ->
    case genlib_retry:next_step(Retry) of
        {wait, Timeout, RetryNext} ->
            {wait, Timeout div 1000, [?session_ev({proxy_retry_changed, RetryNext})]};
        finish ->
            finish
    end.

fail(Error, St) ->
    Events = [?payment_ev(?payment_status_changed(get_payment_id(St), ?failed(Error)))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}}.

construct_retry_strategy(_Target) ->
    Timecap = 30000,
    Timeout = 10000,
    genlib_retry:timecap(Timecap, genlib_retry:linear(infinity, Timeout)).

commit_plan(St, Options) ->
    finalize_plan(fun hg_accounting:commit/3, St, Options).

rollback_plan(St, Options) ->
    finalize_plan(fun hg_accounting:rollback/3, St, Options).

finalize_plan(Finalizer, St, Options) ->
    PlanID = construct_plan_id(get_invoice(Options), get_payment(St)),
    Computed = get_computed_cashflow(Options, St),
    AccountMap = get_account_map(St),
    Finalizer(PlanID, Computed, AccountMap).

get_account_map(#st{cashflow = #domain_InvoicePaymentCashFlow{account_map = V}}) ->
    V.

get_computed_cashflow(Options, #st{cashflow = #domain_InvoicePaymentCashFlow{final_cash_flow = V}}) ->
    Currency = get_invoice_currency(get_invoice(Options)),
    CurrencyCode = Currency#domain_CurrencyRef.symbolic_code,
    [
        {S, D, A, CurrencyCode} ||
            #domain_CashFlowPosting{
                source = S,
                destination = D,
                volume = {fixed, #domain_CashVolumeFixed{amount = A}}
            } <- V
    ].

%%

construct_proxy_context(#st{payment = Payment, route = Route, session = Session}, Options) ->
    #prxprv_Context{
        session = construct_session(Session),
        payment = construct_payment_info(Payment, Options),
        options = collect_proxy_options(Route, Options)
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
    product = Product,
    description = Description,
    cost = Cost
}}) ->
    #prxprv_Invoice{
        id = InvoiceID,
        created_at =  CreatedAt,
        due =  Due,
        product = Product,
        description =  Description,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_shop(Options = #{invoice := Invoice}) ->
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        category = ShopCategoryRef
    } = get_shop(Options),
    ShopCategory = hg_domain:get(
        get_invoice_revision(Invoice),
        {category, ShopCategoryRef}
    ),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails
    }.

construct_proxy_cash(#domain_Cash{
    amount = Amount,
    currency = Currency
}) ->
    #prxprv_Cash{
        amount = Amount,
        currency = Currency
    }.

collect_proxy_options(
    #domain_InvoicePaymentRoute{provider = ProviderRef, terminal = TerminalRef},
    Options
) ->
    Revision = get_invoice_revision(get_invoice(Options)),
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

construct_error(#'Error'{code = Code, description = Description}) ->
    construct_error(Code, Description);
construct_error(Code) when is_binary(Code) ->
    construct_error(Code, undefined).

construct_error(Code, Description) ->
    #'Error'{code = Code, description = Description}.

%%

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_shop(#{party := Party, invoice := Invoice}) ->
    ShopID = Invoice#domain_Invoice.shop_id,
    Shops = Party#domain_Party.shops,
    maps:get(ShopID, Shops).

get_invoice_revision(#domain_Invoice{domain_revision = Revision}) ->
    Revision.

get_invoice_currency(#domain_Invoice{cost = #domain_Cash{currency = Currency}}) ->
    #domain_CurrencyRef{symbolic_code = Currency#domain_Currency.symbolic_code}.

get_payments_service_terms(
    #domain_Shop{
        services = #domain_ShopServices{
            payments = #domain_PaymentsService{domain_revision = Revision, terms = Ref}
        }
    }
) ->
    {Revision, hg_domain:get(Revision, {payments_service_terms, Ref})}.

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
merge_session_event({proxy_retry_changed, Retry}, St = #st{session = Session}) ->
    St#st{session = Session#{retry => Retry}};
merge_session_event(activated, St = #st{session = Session}) ->
    St#st{session = Session#{status => active}};
merge_session_event(suspended, St = #st{session = Session}) ->
    St#st{session = Session#{status => suspended}}.

create_session(Target) ->
    #{
        target => Target,
        status => active,
        proxy_state => undefined,
        retry => construct_retry_strategy(Target)
    }.

%%

issue_process_call(ProxyContext, Opts, St) ->
    issue_call('ProcessPayment', [ProxyContext], Opts, St).

issue_callback_call(Payload, ProxyContext, Opts, St) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], Opts, St).

issue_call(Func, Args, Opts, St) ->
    CallOpts = get_call_options(St, Opts),
    hg_woody_wrapper:call_safe('ProviderProxy', Func, Args, CallOpts).

get_call_options(#st{route = #domain_InvoicePaymentRoute{provider = ProviderRef}}, Opts) ->
    Revision = get_invoice_revision(get_invoice(Opts)),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    #{url => ProxyDef#domain_ProxyDefinition.url}.
