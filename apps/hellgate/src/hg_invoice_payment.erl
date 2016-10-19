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

-export([init/4]).
-export([start_session/2]).

-export([process_signal/4]).
-export([process_call/4]).

-export([merge_event/2]).

%%

-record(st, {
    payment :: payment(),
    route   :: route(),
    session :: session()
}).

-type st() :: #st{}.
-export_type([st/0]).

-type party()       :: dmsl_domain_thrift:'Party'().
-type invoice()     :: dmsl_domain_thrift:'Invoice'().
-type payment()     :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()  :: dmsl_domain_thrift:'InvoicePaymentID'().
-type route()       :: dmsl_domain_thrift:'InvoicePaymentRoute'().
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
    {started, session()} |
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

-spec init(payment_id(), _, opts(), hg_machine:context()) ->
    {hg_machine:result(), hg_machine:context()}.

init(PaymentID, PaymentParams, Opts, Context) ->
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Terms = get_payments_service_terms(Shop),
    VS0 = collect_varset(Shop, #{}),
    VS1 = validate_payment_params(PaymentParams, Terms, VS0),
    VS2 = validate_payment_amount(Invoice, Terms, VS1),
    Payment = construct_payment(PaymentID, Invoice, PaymentParams),
    Route = validate_route(hg_routing:choose(get_invoice_revision(Invoice), VS2)),
    CashFlow = compute_cash_flow(
        collect_cash_flow(Terms, Route, VS2, get_invoice_revision(Invoice)),
        collect_cash_flow_context(Invoice, Payment)
    ),
    Events = [?payment_ev(?payment_started(Payment, Route, CashFlow))],
    Action = hg_machine_action:new(),
    {{Events, Action}, Context}.

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
    hg_cashflow:join(MerchantCashFlow, ProviderCashFlow).


collect_cash_flow_context(
    #domain_Invoice{cost = #domain_Cash{amount = InvoiceAmount}},
    #domain_InvoicePayment{cost = #domain_Cash{amount = PaymentAmount}}
) ->
    #{
        invoice_amount => InvoiceAmount,
        payment_amount => PaymentAmount
    }.

compute_cash_flow(CashFlow, Context) ->
    {ok, Computed} = hg_cashflow:compute(CashFlow, Context), % FIXME
    #domain_InvoicePaymentCashFlow{
        final_cash_flow = Computed,
        account_map = #{}
    }.

%%

-spec start_session(target(), hg_machine:context()) ->
    {hg_machine:result(), hg_machine:context()}.

start_session(Target, Context) ->
    Events = [?session_ev({started, Target})],
    Action = hg_machine_action:instant(),
    {{Events, Action}, Context}.

%%

-spec process_signal(timeout, st(), opts(), hg_machine:context()) ->
    {{next | done, hg_machine:result()}, hg_machine:context()}.

process_signal(timeout, St, Options, Context) ->
    case get_status(St) of
        active ->
            process(St, Options, Context);
        suspended ->
            {fail(construct_error(<<"provider_timeout">>), St), Context}
    end.

-spec process_call({callback, _}, st(), opts(), hg_machine:context()) ->
    {{_, {next | done, hg_machine:result()}}, hg_machine:context()}. % FIXME

process_call({callback, Payload}, St, Options, Context) ->
    case get_status(St) of
        suspended ->
            handle_callback(Payload, St, Options, Context);
        active ->
            % there's ultimately no way how we could end up here
            error(invalid_session_status)
    end.

process(St, Options, Context) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_process_result(issue_process_call(ProxyContext, St, Options, Context), St).

handle_process_result({Result, Context}, St) ->
    case Result of
        ProxyResult = #'ProxyResult'{} ->
            {handle_proxy_result(ProxyResult, St), Context};
        {exception, Exception} ->
            {handle_exception(Exception, St), Context};
        {error, Error} ->
            error(Error)
    end.

handle_callback(Payload, St, Options, Context) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_callback_result(issue_callback_call(Payload, ProxyContext, St, Options, Context), St).

handle_callback_result({Result, Context}, St) ->
    case Result of
        #'CallbackResult'{result = ProxyResult, response = Response} ->
            {What, {Events, Action}} = handle_proxy_result(ProxyResult, St),
            {{Response, {What, {[?session_ev(activated) | Events], Action}}}, Context};
        {error, _} = Error ->
            error({Error, Context})
    end.

handle_proxy_result(#'ProxyResult'{intent = {_, Intent}, trx = Trx, next_state = ProxyState}, St) ->
    Events1 = bind_transaction(Trx, St),
    {What, {Events2, Action}} = handle_proxy_intent(Intent, ProxyState, St),
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

handle_proxy_intent(#'FinishIntent'{status = {ok, _}}, _ProxyState, St) ->
    PaymentID = get_payment_id(St),
    Target = get_target(St),
    Events = [?payment_ev(?payment_status_changed(PaymentID, Target))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}};

handle_proxy_intent(#'FinishIntent'{status = {failure, Error}}, _ProxyState, St) ->
    fail(construct_error(Error), St);

handle_proxy_intent(#'SleepIntent'{timer = Timer}, ProxyState, _St) ->
    Action = hg_machine_action:set_timer(Timer),
    Events = [?session_ev({proxy_state_changed, ProxyState})],
    {next, {Events, Action}};

handle_proxy_intent(
    #'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction},
    ProxyState, St
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

construct_proxy_context(#st{payment = Payment, route = Route, session = Session}, Options) ->
    #'Context'{
        session = construct_session(Session),
        payment = construct_payment_info(Payment, Options),
        options = collect_proxy_options(Route, Options)
    }.

construct_session(#{target := Target, proxy_state := ProxyState}) ->
    #'Session'{
        target = Target,
        state = ProxyState
    }.

construct_payment_info(Payment, #{invoice := Invoice}) ->
    #'PaymentInfo'{
        invoice = Invoice,
        payment = Payment
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

merge_public_event(?payment_started(Payment, Route, _), undefined) ->
    #st{payment = Payment, route = Route};
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

-define(SERVICE, {dmsl_proxy_provider_thrift, 'ProviderProxy'}).

issue_process_call(ProxyContext, St, Opts, Context) ->
    issue_call({?SERVICE, 'ProcessPayment', [ProxyContext]}, St, Opts, Context).

issue_callback_call(Payload, ProxyContext, St, Opts, Context) ->
    issue_call({?SERVICE, 'HandlePaymentCallback', [Payload, ProxyContext]}, St, Opts, Context).

issue_call(Call, St, Opts, Context = #{client_context := ClientContext}) ->
    CallOpts = get_call_options(St, Opts),
    {Result, ClientContext1} = woody_client:call_safe(ClientContext, Call, CallOpts),
    {Result, Context#{client_context := ClientContext1}}.

get_call_options(#st{route = #domain_InvoicePaymentRoute{provider = ProviderRef}}, Opts) ->
    Revision = get_invoice_revision(get_invoice(Opts)),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    #{url => ProxyDef#domain_ProxyDefinition.url}.
