-module(hg_invoice_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invoice_cancellation/1]).
-export([overdue_invoice_cancelled/1]).
-export([invoice_cancelled_after_payment_timeout/1]).
-export([payment_success/1]).
-export([payment_success_on_second_try/1]).
-export([invoice_success_on_third_payment/1]).
-export([consistent_history/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%

-define(c(Key, C), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].

all() ->
    [
        invoice_cancellation,
        overdue_invoice_cancelled,
        invoice_cancelled_after_payment_timeout,
        payment_success,
        payment_success_on_second_try,
        invoice_success_on_third_payment,

        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(RootUrl)),
    ShopID = hg_ct_helper:create_party_and_shop(Client),
    [
        {party_id, PartyID},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps} | C
    ].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?c(apps, C)].

%% tests

-include("invoice_events.hrl").

-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-define(invalid_invoice_status(Status),
    {exception, #payproc_InvalidInvoiceStatus{status = Status}}).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    PartyID = ?c(party_id, C),
    Client = hg_client_invoicing:start_link(make_userinfo(PartyID), hg_client_api:new(?c(root_url, C))),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    [{client, Client}, {test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    _ = unlink(?c(test_sup, C)),
    _ = application:set_env(hellgate, provider_proxy_url, undefined),
    exit(?c(test_sup, C), shutdown).

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    {ok, InvoiceID} = hg_client_invoicing:create(InvoiceParams, Client),
    ?invalid_invoice_status(_) = hg_client_invoicing:fulfill(InvoiceID, <<"perfect">>, Client),
    ok = hg_client_invoicing:rescind(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client = ?c(client, C),
    {ok, InvoiceID} = start_invoice(C, <<"rubberduck">>, make_due_date(1), 10000),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, Client).

-spec invoice_cancelled_after_payment_timeout(config()) -> _ | no_return().

invoice_cancelled_after_payment_timeout(C) ->
    Client = ?c(client, C),
    InvoiceParams = make_invoice_params(<<"rubberdusk">>, make_due_date(7), 1000),
    {ok, InvoiceID} = start_invoice(C, InvoiceParams),
    PaymentParams = make_3ds_payment_params(),
    {ok, PaymentID} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout
    ?payment_status_changed(PaymentID, ?failed(_)) = next_event(InvoiceID, 5000, Client),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, 5000, Client).

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = ?c(client, C),
    {ok, InvoiceID} = start_invoice(C, <<"rubberduck">>, make_due_date(10), 42000),
    PaymentParams = make_payment_params(),
    {ok, PaymentID} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_status_changed(PaymentID, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 1000, Client).

-spec payment_success_on_second_try(config()) -> _ | no_return().

payment_success_on_second_try(C) ->
    Client = ?c(client, C),
    {ok, InvoiceID} = start_invoice(C, <<"rubberdick">>, make_due_date(20), 42000),
    PaymentParams = make_3ds_payment_params(),
    {ok, PaymentID} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID, UserInteraction) = next_event(InvoiceID, Client),
    {'redirect', {'get_request', #'BrowserGetRequest'{uri = GoodTag}}} = UserInteraction,
    %% simulate user interaction
    BadTag = <<"666">>,
    assert_call_failed(hg_client_api:call(
         proxy_host_provider, 'ProcessCallback', [BadTag, <<"payload">>],
         hg_client_api:new( ?c(root_url, C))
    )),
    assert_call_succeed(hg_client_api:call(
         proxy_host_provider, 'ProcessCallback', [GoodTag, <<"payload">>],
         hg_client_api:new( ?c(root_url, C))
    )),
    ?payment_status_changed(PaymentID, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 1000, Client).

-spec invoice_success_on_third_payment(config()) -> _ | no_return().

invoice_success_on_third_payment(C) ->
    Client = ?c(client, C),
    {ok, InvoiceID} = start_invoice(C, <<"rubberdock">>, make_due_date(60), 42000),
    PaymentParams = make_3ds_payment_params(),
    {ok, PaymentID_1} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID_1, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout and start new one after
    ?payment_status_changed(PaymentID_1, ?failed(_)) = next_event(InvoiceID, 5000, Client),
    {ok, PaymentID_2} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID_2, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout and start new one after
    ?payment_status_changed(PaymentID_2, ?failed(_)) = next_event(InvoiceID, 5000, Client),
    {ok, PaymentID_3} = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID_3, UserInteraction) = next_event(InvoiceID, Client),
    {'redirect', {'get_request', #'BrowserGetRequest'{uri = GoodTag}}} = UserInteraction,
    %% simulate user interaction FTW!
    assert_call_succeed(hg_client_api:call(
         proxy_host_provider, 'ProcessCallback', [GoodTag, <<"payload">>],
         hg_client_api:new( ?c(root_url, C))
    )),
    ?payment_status_changed(PaymentID_3, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 1000, Client).
%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(?config(root_url, C))),
    {ok, Events} = hg_client_eventsink:pull_events(5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

%%

next_event(InvoiceID, Client) ->
    next_event(InvoiceID, 3000, Client).

next_event(InvoiceID, Timeout, Client) ->
    case hg_client_invoicing:pull_event(InvoiceID, Timeout, Client) of
        {ok, Event} ->
            unwrap_event(Event);
        Result ->
            Result
    end.

unwrap_event(?invoice_ev(E)) ->
    unwrap_event(E);
unwrap_event(?payment_ev(E)) ->
    unwrap_event(E);
unwrap_event(E) ->
    E.

%%

start_service_handler(Module, C) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = #{hellgate_root_url => ?c(root_url, C)},
    ChildSpec = hg_test_proxy:get_child_spec(Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(?c(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

%%

make_userinfo(PartyID) ->
    #payproc_UserInfo{id = PartyID}.

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Cost).

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

make_3ds_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_3ds_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_simple_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params(PaymentTool, Session) ->
    #payproc_InvoicePaymentParams{
        payer = #domain_Payer{
            payment_tool = PaymentTool,
            session = Session,
            client_info = #domain_ClientInfo{},
            contact_info = #domain_ContactInfo{}
        }
    }.

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

assert_call_succeed({ok, _}) ->
    ok;
assert_call_succeed({{ok, _}, _}) ->
    ok;
assert_call_succeed({Error, _}) ->
    error(Error).

assert_call_failed({{exception, _}, _}) ->
    ok;
assert_call_failed({{error, _}, _}) ->
    ok;
assert_call_failed({Success, _}) ->
    error(Success).

start_invoice(C, Product, Due, Amount) ->
    ProxyUrl = start_service_handler(hg_dummy_provider, C),
    ok = application:set_env(hellgate, provider_proxy_url, ProxyUrl),
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    ok = hg_domain:update(hg_ct_helper:domain_fixture(proxy)),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    {ok, InvoiceID} = hg_client_invoicing:create(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    {ok, InvoiceID}.

attach_payment(InvoiceID, PaymentParams, Client) ->
    {ok, PaymentID} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    ?payment_started(?payment_w_status(?pending())) = next_event(InvoiceID, Client),
    ?payment_bound(PaymentID, ?trx_info(PaymentID)) = next_event(InvoiceID, Client),
    ?payment_status_changed(PaymentID, ?processed()) = next_event(InvoiceID, Client),
    {ok, PaymentID}.