-module(hg_memory_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

-include("hg_ct_domain.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([start_payment/1]).

-type config()         :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name()     :: hg_ct_helper:group_name().
-type test_return()    :: _ | no_return().

-include("invoice_events.hrl").
-include("payment_events.hrl").
-include("customer_events.hrl").

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(payment(ID, Revision), #domain_InvoicePayment{id = ID, party_revision = Revision}).
-define(customer(ID), #payproc_Customer{id = ID}).
-define(adjustment(ID), #domain_InvoicePaymentAdjustment{id = ID}).
-define(adjustment(ID, Status), #domain_InvoicePaymentAdjustment{id = ID, status = Status}).
-define(adjustment_revision(Revision), #domain_InvoicePaymentAdjustment{party_revision = Revision}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(invoice_state(Invoice, Payments), #payproc_Invoice{invoice = Invoice, payments = Payments}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(invoice_w_revision(Revision), #domain_Invoice{party_revision = Revision}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).
-define(trx_info(ID, Extra), #domain_TransactionInfo{id = ID, extra = Extra}).

%%% supervisor

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%% CT

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        {group, payments}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        % {payments, [parallel], n(start_payment, 50)}
        {payments, [], n(start_payment, 1000)}
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    application:set_env(hellgate, transport_opts, [{max_connections, 10000}]),
    {Apps, Ret} = hg_ct_helper:start_apps([
        lager, woody, scoper, dmt_client, party_client, hellgate, {cowboy, CowboySpec}
    ]),
    Config = get_domain_config("../../lib/hellgate/test/hg_domain_config.data"),
    try
        ok = hg_domain:insert(Config)
    catch
        throw:_ ->
            ok
    end,
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    CustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID)),
    AnotherPartyID = hg_utils:unique_id(),
    AnotherPartyClient = hg_client_party:start(AnotherPartyID, hg_ct_helper:create_client(RootUrl, AnotherPartyID)),
    AnotherCustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, AnotherPartyID)),
    timer:sleep(2000),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    AnotherShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), AnotherPartyClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    ok = hg_invoice_tests_SUITE:start_kv_store(SupPid),
    NewC = [
        {party_id, PartyID},
        {party_client, PartyClient},
        {shop_id, ShopID},
        {customer_client, CustomerClient},
        {another_party_id, AnotherPartyID},
        {another_party_client, AnotherPartyClient},
        {another_shop_id, AnotherShopID},
        {another_customer_client, AnotherCustomerClient},
        {root_url, RootUrl},
        {apps, Apps},
        {test_sup, SupPid}
        | C
    ],
    % ok = start_proxies([{hg_dummy_provider, 1, NewC}, {hg_dummy_inspector, 2, NewC}]),
    NewC.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

-spec init_per_group(group_name(), config()) -> config().

init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    ApiClient = hg_ct_helper:create_client(cfg(root_url, C), cfg(party_id, C)),
    Client = hg_client_invoicing:start_link(ApiClient),
    ClientTpl = hg_client_invoice_templating:start_link(ApiClient),
    [{client, Client}, {client_tpl, ClientTpl} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    C.

%%% Tests

-spec start_payment(config()) -> test_return().

start_payment(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams0 = make_payment_params(),
    PaymentID1 = <<"1">>,
    ExternalID = undefined,
    PaymentParams1 = PaymentParams0#payproc_InvoicePaymentParams{
        id = PaymentID1,
        external_id = ExternalID
    },
    ?payment_state(#domain_InvoicePayment{
        id = PaymentID1,
        external_id = ExternalID
    }) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams1, Client),
    PaymentParams2 = PaymentParams0#payproc_InvoicePaymentParams{id = <<"2">>},
    {exception, #payproc_InvoicePaymentPending{id = PaymentID1}} =
        hg_client_invoicing:start_payment(InvoiceID, PaymentParams2, Client).
    % PaymentID1 = process_payment(InvoiceID, PaymentParams1, Client),
    % PaymentID1 = await_payment_capture(InvoiceID, PaymentID1, Client),
    % ?invoice_state(
    %     ?invoice_w_status(?invoice_paid()),
    %     [?payment_state(?payment_w_status(PaymentID1, ?captured()))]
    % ) = hg_client_invoicing:get(InvoiceID, Client).

%%% Internal functions

n(T, N) ->
    lists:duplicate(N, T).

get_domain_config(Filename) ->
    {ok, [Snapshot]} = file:consult(Filename),
    #'Snapshot'{domain = Domain} = Snapshot,
    maps:values(Domain).

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

start_invoice(Product, Due, Amount, C) ->
    start_invoice(cfg(shop_id, C), Product, Due, Amount, C).

make_payment_params() ->
    make_payment_params(instant).

make_payment_params(FlowType) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    make_payment_params(PaymentTool, Session, FlowType).

% make_payment_params(PaymentTool, Session) ->
%     make_payment_params(PaymentTool, Session, instant).

make_payment_params(PaymentTool, Session, FlowType) ->
    Flow = case FlowType of
        instant ->
            {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
        {hold, OnHoldExpiration} ->
            {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
    end,
    #payproc_InvoicePaymentParams{
        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
            resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = Session,
                client_info = #domain_ClientInfo{}
            },
            contact_info = #domain_ContactInfo{}
        }},
        flow = Flow
    }.

start_invoice(ShopID, Product, Due, Amount, C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    InvoiceID.

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

create_invoice(InvoiceParams, Client) ->
    ?invoice_state(?invoice(InvoiceID)) = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

next_event(InvoiceID, Client) ->
    %% timeout should be at least as large as hold expiration in construct_domain_fixture/0
    next_event(InvoiceID, 12000, Client).

next_event(InvoiceID, Timeout, Client) ->
    case hg_client_invoicing:pull_event(InvoiceID, Timeout, Client) of
        {ok, ?invoice_ev(Changes)} ->
            case filter_changes(Changes) of
                L when length(L) > 0 ->
                    L;
                [] ->
                    next_event(InvoiceID, Timeout, Client)
            end;
        Result ->
            Result
    end.

filter_changes(Changes) ->
    lists:filtermap(fun filter_change/1, Changes).

filter_change(?payment_ev(_, C)) ->
    filter_change(C);
filter_change(?refund_ev(_, C)) ->
    filter_change(C);
filter_change(?session_ev(_, ?proxy_st_changed(_))) ->
    false;
filter_change(?session_ev(_, ?session_suspended(_))) ->
    false;
filter_change(?session_ev(_, ?session_activated())) ->
    false;
filter_change(_) ->
    true.

% process_payment(InvoiceID, PaymentParams, Client) ->
%     process_payment(InvoiceID, PaymentParams, Client, 0).

% process_payment(InvoiceID, PaymentParams, Client, Restarts) ->
%     PaymentID = start_payment(InvoiceID, PaymentParams, Client),
%     PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
%     PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client, Restarts).

% start_payment(InvoiceID, PaymentParams, Client) ->
%     ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
%     [
%         ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
%     ] = next_event(InvoiceID, Client),
%     [
%         ?payment_ev(PaymentID, ?risk_score_changed(_)),
%         ?payment_ev(PaymentID, ?route_changed(_)),
%         ?payment_ev(PaymentID, ?cash_flow_changed(_))
%     ] = next_event(InvoiceID, Client),
%     PaymentID.

% await_payment_session_started(InvoiceID, PaymentID, Client, Target) ->
%     [
%         ?payment_ev(PaymentID, ?session_ev(Target, ?session_started()))
%     ] = next_event(InvoiceID, Client),
%     PaymentID.

% await_payment_process_finish(InvoiceID, PaymentID, Client, Restarts) ->
%     PaymentID = await_sessions_restarts(PaymentID, ?processed(), InvoiceID, Client, Restarts),
%     [
%         ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(_)))),
%         ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
%     ] = next_event(InvoiceID, Client),
%     [
%         ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
%     ] = next_event(InvoiceID, Client),
%     PaymentID.

% await_sessions_restarts(PaymentID, _Target, _InvoiceID, _Client, 0) ->
%     PaymentID;
% await_sessions_restarts(PaymentID, ?refunded() = Target, InvoiceID, Client, Restarts) when Restarts > 0 ->
%     [
%         ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(Target, ?session_finished(?session_failed(_))))),
%         ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(Target, ?session_started())))
%     ] = next_event(InvoiceID, Client),
%     await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts - 1);
% await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts) when Restarts > 0 ->
%     [
%         ?payment_ev(PaymentID, ?session_ev(Target, ?session_finished(?session_failed(_)))),
%         ?payment_ev(PaymentID, ?session_ev(Target, ?session_started()))
%     ] = next_event(InvoiceID, Client),
%     await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts - 1).

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
