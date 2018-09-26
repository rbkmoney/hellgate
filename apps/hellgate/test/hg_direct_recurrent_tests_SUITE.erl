-module(hg_direct_recurrent_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("hg_ct_domain.hrl").
-include("hg_ct_json.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include("invoice_events.hrl").
-include("payment_events.hrl").
-include("customer_events.hrl").

-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([first_recurrent_payment_success_test/1]).
-export([second_recurrent_payment_success_test/1]).
-export([another_shop_test/1]).
-export([not_recurring_first_test/1]).
-export([customer_paytools_as_first_test/1]).
-export([cancelled_first_payment_test/1]).
-export([not_permitted_recurrent_test/1]).
-export([not_exists_invoice_test/1]).
-export([not_exists_payment_test/1]).

%% Internal types

-type config()           :: hg_ct_helper:config().
-type test_case_name()   :: hg_ct_helper:test_case_name().
-type group_name()       :: hg_ct_helper:group_name().
-type test_result()      :: any() | no_return().

%% Macro helpers

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(customer(ID), #payproc_Customer{id = ID}).
-define(adjustment(ID), #domain_InvoicePaymentAdjustment{id = ID}).
-define(adjustment(ID, Status), #domain_InvoicePaymentAdjustment{id = ID, status = Status}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(invoice_state(Invoice, Payments), #payproc_Invoice{invoice = Invoice, payments = Payments}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).
-define(evp(Pattern), fun(EvpPattern) -> case EvpPattern of Pattern -> true; (_) -> false end end).

%% Supervisor callbacks

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%% Common tests callbacks

-spec all() -> [test_case_name()].
all() ->
    [
        {group, basic_operations},
        {group, domain_affecting_operations}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {basic_operations, [parallel], [
            first_recurrent_payment_success_test,
            second_recurrent_payment_success_test,
            another_shop_test,
            not_recurring_first_test,
            customer_paytools_as_first_test,
            cancelled_first_payment_test,
            not_exists_invoice_test,
            not_exists_payment_test,
            recurrent_risk_score_always_high
        ]},
        {domain_affecting_operations, [], [
            not_permitted_recurrent_test
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({woody_client, '_', '_'}, x),
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, scoper, dmt_client, hellgate, {cowboy, CowboySpec}]),
    ok = hg_domain:insert(construct_domain_fixture(construct_term_set_w_recurrent_paytools())),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    CustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID)),
    Shop1ID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    Shop2ID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    C1 = [
        {apps, Apps},
        {root_url, RootUrl},
        {party_client, PartyClient},
        {customer_client, CustomerClient},
        {party_id, PartyID},
        {shop_id, Shop1ID},
        {another_shop_id, Shop2ID},
        {test_sup, SupPid}
        | C
    ],
    ok = start_proxies([{hg_dummy_provider, 1, C1}, {hg_dummy_inspector, 2, C1}]),
    C1.

-spec end_per_suite(config()) -> config().
end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

-spec init_per_group(group_name(), config()) -> config().
init_per_group(_Name, C) ->
    C.

-spec end_per_group(group_name(), config()) -> ok.
end_per_group(_Name, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    TraceID = hg_ct_helper:make_trace_id(Name),
    ApiClient = hg_ct_helper:create_client(cfg(root_url, C), cfg(party_id, C)),
    Client = hg_client_invoicing:start_link(ApiClient),
    [
        {test_case_name, genlib:to_binary(Name)},
        {trace_id, TraceID},
        {client, Client}
        | C
    ].

-spec end_per_testcase(test_case_name(), config()) -> ok.
end_per_testcase(_Name, _C) ->
    ok.

%% Tests

-spec first_recurrent_payment_success_test(config()) -> test_result().
first_recurrent_payment_success_test(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    {ok, PaymentID} = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec second_recurrent_payment_success_test(config()) -> test_result().
second_recurrent_payment_success_test(C) ->
    Client = cfg(client, C),
    Invoice1ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    %% first payment in recurrent session
    Payment1Params = make_payment_params(),
    {ok, Payment1ID} = start_payment(Invoice1ID, Payment1Params, Client),
    Payment1ID = await_payment_capture(Invoice1ID, Payment1ID, Client),
    %% second recurrent payment
    Invoice2ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(Invoice1ID, Payment1ID),
    Payment2Params = make_recurrent_payment_params(true, RecurrentParent),
    {ok, Payment2ID} = start_payment(Invoice2ID, Payment2Params, Client),
    Payment2ID = await_payment_capture(Invoice2ID, Payment2ID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(Payment2ID, ?captured()))]
    ) = hg_client_invoicing:get(Invoice2ID, Client).

-spec another_shop_test(config()) -> test_result().
another_shop_test(C) ->
    Client = cfg(client, C),
    Invoice1ID = start_invoice(cfg(another_shop_id, C), <<"rubberduck">>, make_due_date(10), 42000, C),
    %% first payment in recurrent session
    Payment1Params = make_payment_params(),
    {ok, Payment1ID} = start_payment(Invoice1ID, Payment1Params, Client),
    Payment1ID = await_payment_capture(Invoice1ID, Payment1ID, Client),
    %% second recurrent payment
    Invoice2ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(Invoice1ID, Payment1ID),
    Payment2Params = make_recurrent_payment_params(true, RecurrentParent),
    ExpectedError = #payproc_InvalidRecurrentParentPayment{details = <<"Parent payment refer to another shop">>},
    {error, ExpectedError} = start_payment(Invoice2ID, Payment2Params, Client).

-spec not_recurring_first_test(config()) -> test_result().
not_recurring_first_test(C) ->
    Client = cfg(client, C),
    Invoice1ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    %% first payment in recurrent session
    Payment1Params = make_payment_params(false, undefined),
    {ok, Payment1ID} = start_payment(Invoice1ID, Payment1Params, Client),
    Payment1ID = await_payment_capture(Invoice1ID, Payment1ID, Client),
    %% second recurrent payment
    Invoice2ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(Invoice1ID, Payment1ID),
    Payment2Params = make_recurrent_payment_params(true, RecurrentParent),
    ExpectedError = #payproc_InvalidRecurrentParentPayment{details = <<"Parent payment has no recurrent token">>},
    {error, ExpectedError} = start_payment(Invoice2ID, Payment2Params, Client).

-spec customer_paytools_as_first_test(config()) -> test_result().
customer_paytools_as_first_test(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    CustomerID = make_customer_w_rec_tool(cfg(party_id, C), ShopID, cfg(customer_client, C)),
    PaymentParams = make_customer_payment_params(CustomerID),
    ExpectedError = #'InvalidRequest'{errors = [<<"Invalid payer">>]},
    {error, ExpectedError} = start_payment(InvoiceID, PaymentParams, Client).

-spec cancelled_first_payment_test(config()) -> test_result().
cancelled_first_payment_test(C) ->
    Client = cfg(client, C),
    Invoice1ID = start_invoice(<<"rubberduck">>, make_due_date(1), 42000, C),
    %% first payment in recurrent session
    Payment1Params = make_payment_params({hold, cancel}, true, undefined),
    {ok, Payment1ID} = start_payment(Invoice1ID, Payment1Params, Client),
    Payment1ID = await_payment_cancel(Invoice1ID, Payment1ID, undefined, Client),
    %% second recurrent payment
    Invoice2ID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(Invoice1ID, Payment1ID),
    Payment2Params = make_recurrent_payment_params(true, RecurrentParent),
    {ok, Payment2ID} = start_payment(Invoice2ID, Payment2Params, Client),
    Payment2ID = await_payment_capture(Invoice2ID, Payment2ID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(Payment2ID, ?captured()))]
    ) = hg_client_invoicing:get(Invoice2ID, Client).

-spec not_permitted_recurrent_test(config()) -> test_result().
not_permitted_recurrent_test(C) ->
    ok = hg_domain:upsert(construct_domain_fixture(construct_simple_term_set())),
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    ExpectedError = #payproc_OperationNotPermitted{},
    {error, ExpectedError} = start_payment(InvoiceID, PaymentParams, Client).

-spec not_exists_invoice_test(config()) -> test_result().
not_exists_invoice_test(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(<<"not_exists">>, <<"not_exists">>),
    PaymentParams = make_payment_params(true, RecurrentParent),
    ExpectedError = #payproc_InvalidRecurrentParentPayment{details = <<"Parent invoice not found">>},
    {error, ExpectedError} = start_payment(InvoiceID, PaymentParams, Client).

-spec not_exists_payment_test(config()) -> test_result().
not_exists_payment_test(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    RecurrentParent = ?recurrent_parent(InvoiceID, <<"not_exists">>),
    PaymentParams = make_payment_params(true, RecurrentParent),
    ExpectedError = #payproc_InvalidRecurrentParentPayment{details = <<"Parent payment not found">>},
    {error, ExpectedError} = start_payment(InvoiceID, PaymentParams, Client).

-spec recurrent_risk_score_always_high(config()) -> test_result().
recurrent_risk_score_always_high(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    {ok, PaymentID} = start_payment(InvoiceID, PaymentParams, Client),
    Pattern = [
        ?evp(?payment_ev(PaymentID, ?risk_score_changed(_)))
    ],
    {ok, [?payment_ev(PaymentID, ?risk_score_changed(Score))]} = await_events(InvoiceID, Pattern, Client),
    high = Score.

%% Internal functions

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

start_proxies(Proxies) ->
    setup_proxies(lists:map(
        fun
            Mapper({Module, ProxyID, Context}) ->
                Mapper({Module, ProxyID, #{}, Context});
            Mapper({Module, ProxyID, ProxyOpts, Context}) ->
                construct_proxy(ProxyID, start_service_handler(Module, Context, #{}), ProxyOpts)
        end,
        Proxies
    )).

setup_proxies(Proxies) ->
    ok = hg_domain:upsert(Proxies).

start_service_handler(Module, C, HandlerOpts) ->
    start_service_handler(Module, Module, C, HandlerOpts).

start_service_handler(Name, Module, C, HandlerOpts) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = maps:merge(HandlerOpts, #{hellgate_root_url => cfg(root_url, C)}),
    ChildSpec = hg_test_proxy:get_child_spec(Name, Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(cfg(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = ?prx(ID),
        data = #domain_ProxyDefinition{
            name              = Url,
            description       = Url,
            url               = Url,
            options           = Options
        }
    }}.

make_payment_params() ->
    make_payment_params(true, undefined).

make_payment_params(MakeRecurrent, RecurrentParent) ->
    make_payment_params(instant, MakeRecurrent, RecurrentParent).

make_payment_params(FlowType, MakeRecurrent, RecurrentParent) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    make_payment_params(PaymentTool, Session, FlowType, MakeRecurrent, RecurrentParent).

make_payment_params(PaymentTool, Session, FlowType, MakeRecurrent, RecurrentParent) ->
    make_payment_params(#domain_ClientInfo{}, PaymentTool, Session, FlowType, MakeRecurrent, RecurrentParent).

make_payment_params(ClientInfo, PaymentTool, Session, FlowType, MakeRecurrent, RecurrentParent) ->
    Flow = case FlowType of
        instant ->
            {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
        {hold, OnHoldExpiration} ->
            {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
    end,
    Payer = make_payer_params(PaymentTool, Session, ClientInfo, RecurrentParent),
    #payproc_InvoicePaymentParams{
        payer = Payer,
        flow = Flow,
        make_recurrent = MakeRecurrent
    }.

make_payer_params(PaymentTool, Session, ClientInfo, undefined = _RecurrentParent) ->
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = Session,
            client_info = ClientInfo
        },
        contact_info = #domain_ContactInfo{}
    }};
make_payer_params(_PaymentTool, _Session, _ClientInfo, RecurrentParent) ->
    {recurrent, #payproc_RecurrentPayerParams{
        recurrent_parent = RecurrentParent,
        contact_info = #domain_ContactInfo{}
    }}.

make_recurrent_payment_params(MakeRecurrent, RecurrentParent) ->
    make_recurrent_payment_params(instant, MakeRecurrent, RecurrentParent).

make_recurrent_payment_params(FlowType, MakeRecurrent, RecurrentParent) ->
    {PaymentTool, _Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    make_payment_params(undefined, PaymentTool, undefined, FlowType, MakeRecurrent, RecurrentParent).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

start_invoice(Product, Due, Amount, C) ->
    start_invoice(cfg(shop_id, C), Product, Due, Amount, C).

start_invoice(ShopID, Product, Due, Amount, C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    InvoiceID = create_invoice(InvoiceParams, Client),
    _Events = await_events(InvoiceID, [?evp(?invoice_created(?invoice_w_status(?invoice_unpaid())))], Client),
    InvoiceID.

start_payment(InvoiceID, PaymentParams, Client) ->
    case hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client) of
        ?payment_state(?payment(PaymentID)) ->
            {ok, PaymentID};
        {exception, Exception} ->
            {error, Exception}
    end.

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

create_invoice(InvoiceParams, Client) ->
    ?invoice_state(?invoice(InvoiceID)) = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

await_payment_capture(InvoiceID, PaymentID, Client) ->
    await_payment_capture(InvoiceID, PaymentID, undefined, Client).

await_payment_capture(InvoiceID, PaymentID, Reason, Client) ->
    Pattern = [
        ?evp(?payment_ev(PaymentID, ?payment_status_changed(?captured_with_reason(Reason))))
    ],
    {ok, _Events} = await_events(InvoiceID, Pattern, Client),
    PaymentID.

await_payment_cancel(InvoiceID, PaymentID, Reason, Client) ->
    Pattern = [
        ?evp(?payment_ev(PaymentID, ?payment_status_changed(?cancelled_with_reason(Reason))))
    ],
    {ok, _Events} = await_events(InvoiceID, Pattern, Client),
    PaymentID.

make_customer_w_rec_tool(PartyID, ShopID, Client) ->
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, <<"InvoicingTests">>),
    #payproc_Customer{id = CustomerID} =
        hg_client_customer:create(CustomerParams, Client),
    #payproc_CustomerBinding{id = BindingID} =
        hg_client_customer:start_binding(
            CustomerID,
            hg_ct_helper:make_customer_binding_params(hg_dummy_provider:make_payment_tool(no_preauth)),
            Client
        ),
    ok = wait_for_binding_success(CustomerID, BindingID, Client),
    CustomerID.

wait_for_binding_success(CustomerID, BindingID, Client) ->
    wait_for_binding_success(CustomerID, BindingID, 5000, Client).

wait_for_binding_success(CustomerID, BindingID, TimeLeft, Client) when TimeLeft > 0 ->
    Target = ?customer_binding_changed(BindingID, ?customer_binding_status_changed(?customer_binding_succeeded())),
    Started = genlib_time:ticks(),
    Event = hg_client_customer:pull_event(CustomerID, Client),
    R = case Event of
        {ok, ?customer_event(Changes)} ->
            lists:member(Target, Changes);
        _ ->
            false
    end,
    case R of
        true ->
            ok;
        false ->
            timer:sleep(200),
            Now = genlib_time:ticks(),
            TimeLeftNext = TimeLeft - (Now - Started) div 1000,
            wait_for_binding_success(CustomerID, BindingID, TimeLeftNext, Client)
    end;
wait_for_binding_success(_, _, _, _) ->
    timeout.

make_customer_payment_params(CustomerID) ->
    make_customer_payment_params(CustomerID, true).

make_customer_payment_params(CustomerID, MakeRecurrent) ->
    #payproc_InvoicePaymentParams{
        payer = {customer, #payproc_CustomerPayerParams{
            customer_id = CustomerID
        }},
        flow = {instant, #payproc_InvoicePaymentParamsFlowInstant{}},
        make_recurrent = MakeRecurrent
    }.

%% Event helpers

await_events(InvoiceID, Filters, Client) ->
    await_events(InvoiceID, Filters, 12000, Client).

await_events(InvoiceID, Filters, Timeout, Client) ->
    do_await_events(InvoiceID, Filters, Timeout, Client, [], []).

do_await_events(_InvoiceID, [], _Timeout, _Client, _NotProcessedEvents, MatchedEvents) ->
    {ok, lists:reverse(MatchedEvents)};
do_await_events(_InvoiceID, Filters, _Timeout, _Client, timeout, MatchedEvents) ->
    {error, {timeout, Filters, MatchedEvents}};
do_await_events(InvoiceID, Filters, Timeout, Client, [], MatchedEvents) ->
    NewEvents = next_event(InvoiceID, Timeout, Client),
    do_await_events(InvoiceID, Filters, Timeout, Client, NewEvents, MatchedEvents);
do_await_events(InvoiceID, [FilterFn | FTail] = Filters, Timeout, Client, [Ev | EvTail], MatchedEvents) ->
    case FilterFn(Ev) of
        true ->
            do_await_events(InvoiceID, FTail, Timeout, Client, EvTail, [Ev | MatchedEvents]);
        false ->
            do_await_events(InvoiceID, Filters, Timeout, Client, EvTail, MatchedEvents)
    end.

next_event(InvoiceID, Timeout, Client) ->
    case hg_client_invoicing:pull_event(InvoiceID, Timeout, Client) of
        {ok, ?invoice_ev(Changes)} ->
            Changes;
        Result ->
            Result
    end.

%% Domain helper

-spec construct_term_set_w_recurrent_paytools() -> term().
construct_term_set_w_recurrent_paytools() ->
    TermSet = construct_simple_term_set(),
    TermSet#domain_TermSet{recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa)
            ])}
        }
    }.

-spec construct_simple_term_set() -> term().
construct_simple_term_set() ->
    #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(1)
            ])},
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(     1000, <<"RUB">>)},
                        upper = {exclusive, ?cash(420000000, <<"RUB">>)}
                    }}
                }
            ]},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(45, 1000, operation_amount)
                )
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 10}}
                    }
                ]}
            }
        }
    }.

-spec construct_domain_fixture(term()) -> [hg_domain:object()].
construct_domain_fixture(TermSet) ->
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([
                    ?prv(1)
                ])},
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, ?insp(1)}
                    }
                ]},
                residences = [],
                realm = test
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, ?eas(1)},
                payment_institutions = ?ordset([?pinst(1)])
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(18, 1000, operation_amount)
                        )
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                    definition = {payment_system_is, visa}
                                }}}},
                                then_ = {value, ?hold_lifetime(12)}
                            }
                        ]}
                    }
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                risk_coverage = high
            }
        }}
    ].
