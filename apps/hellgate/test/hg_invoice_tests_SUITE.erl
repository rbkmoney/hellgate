-module(hg_invoice_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invalid_invoice_amount/1]).
-export([invalid_invoice_currency/1]).
-export([invoice_cancellation/1]).
-export([overdue_invoice_cancelled/1]).
-export([invoice_cancelled_after_payment_timeout/1]).
-export([payment_success/1]).
-export([payment_success_on_second_try/1]).
-export([payment_risk_score_check/1]).
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
        invalid_invoice_amount,
        invalid_invoice_currency,
        invoice_cancellation,
        overdue_invoice_cancelled,
        invoice_cancelled_after_payment_timeout,
        payment_success,
        payment_success_on_second_try,
        invoice_success_on_third_payment,

        payment_risk_score_check,

        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate, {cowboy, CowboySpec}]),
    ok = hg_domain:insert(hg_ct_helper:construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(RootUrl)),
    ShopID = hg_ct_helper:create_party_and_shop(Client),
    [
        {party_id, PartyID},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps}
        | C
    ].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- ?c(apps, C)].

%% tests

-include("invoice_events.hrl").

-define(invoice_state(Invoice, Payments), #payproc_InvoiceState{invoice = Invoice, payments = Payments}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
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
    exit(?c(test_sup, C), shutdown).

-spec invalid_invoice_amount(config()) -> _ | no_return().

invalid_invoice_amount(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, -10000),
    {exception, #'InvalidRequest'{}} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_invoice_currency(config()) -> _ | no_return().

invalid_invoice_currency(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100, <<"KEK">>}),
    {exception, #'InvalidRequest'{}} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    InvoiceID = create_invoice(InvoiceParams, Client),
    ?invalid_invoice_status(_) = hg_client_invoicing:fulfill(InvoiceID, <<"perfect">>, Client),
    ok = hg_client_invoicing:rescind(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client = ?c(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(1), 10000, C),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, Client).

-spec invoice_cancelled_after_payment_timeout(config()) -> _ | no_return().

invoice_cancelled_after_payment_timeout(C) ->
    Client = ?c(client, C),
    ok = start_handler(hg_dummy_provider, 1, #{}, C),
    ok = start_handler(hg_dummy_inspector, 2, #{<<"risk_score">> => <<"high">>}, C),
    InvoiceID = start_invoice(<<"rubberdusk">>, make_due_date(7), 1000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout
    ?payment_status_changed(PaymentID, ?failed(_)) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, Client).

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = ?c(client, C),
    ok = start_handler(hg_dummy_provider, 1, #{}, C),
    ok = start_handler(hg_dummy_inspector, 2, #{<<"risk_score">> => <<"high">>}, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_status_changed(PaymentID, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client),
    ?invoice_state(
        ?invoice_w_status(?paid()),
        [?payment_w_status(PaymentID, ?captured())]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_success_on_second_try(config()) -> _ | no_return().

payment_success_on_second_try(C) ->
    Client = ?c(client, C),
    ok = start_handler(hg_dummy_provider, 1, #{}, C),
    ok = start_handler(hg_dummy_inspector, 2, #{<<"risk_score">> => <<"high">>}, C),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID, UserInteraction) = next_event(InvoiceID, Client),
    %% simulate user interaction
    {URL, GoodForm} = get_post_request(UserInteraction),
    BadForm = #{<<"tag">> => <<"666">>},
    _ = assert_failed_post_request({URL, BadForm}),
    _ = assert_success_post_request({URL, GoodForm}),
    ?payment_status_changed(PaymentID, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client).

-spec invoice_success_on_third_payment(config()) -> _ | no_return().

invoice_success_on_third_payment(C) ->
    Client = ?c(client, C),
    ok = start_handler(hg_dummy_provider, 1, #{}, C),
    ok = start_handler(hg_dummy_inspector, 2, #{<<"risk_score">> => <<"high">>}, C),
    InvoiceID = start_invoice(<<"rubberdock">>, make_due_date(60), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID1 = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID1, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout and start new one after
    ?payment_status_changed(PaymentID1, ?failed(_)) = next_event(InvoiceID, Client),
    PaymentID2 = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID2, _) = next_event(InvoiceID, Client),
    %% wait for payment timeout and start new one after
    ?payment_status_changed(PaymentID2, ?failed(_)) = next_event(InvoiceID, Client),
    PaymentID3 = attach_payment(InvoiceID, PaymentParams, Client),
    ?payment_interaction_requested(PaymentID3, UserInteraction) = next_event(InvoiceID, Client),
    GoodPost = get_post_request(UserInteraction),
    %% simulate user interaction FTW!
    _ = assert_success_post_request(GoodPost),
    ?payment_status_changed(PaymentID3, ?captured()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, Client).


%% @TODO modify this test by failures of inspector in case of wrong terminal choice
-spec payment_risk_score_check(config()) -> _ | no_return().

payment_risk_score_check(C) ->
    Client = ?c(client, C),
    ok = start_handler(hg_dummy_provider, 1, #{}, C),
    ok = start_handler(hg_dummy_inspector, 2, #{<<"risk_score">> => <<"low">>}, C),
    PaymentParams = make_tds_payment_params(),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    ?payment_started(_, Route, _) = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRoute{terminal = TermRef} = Route,
    #domain_Terminal{risk_coverage = low} = hg_domain:get(hg_domain:head(), {terminal, TermRef}).
%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(?config(root_url, C))),
    Events = hg_client_eventsink:pull_events(5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

%%

next_event(InvoiceID, Client) ->
    next_event(InvoiceID, 5000, Client).

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

start_service_handler(Module, C, Additional) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = maps:merge(Additional, #{hellgate_root_url => ?c(root_url, C)}),
    ChildSpec = hg_test_proxy:get_child_spec(Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(?c(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy_ref(ID) ->
    #domain_ProxyRef{id = ID}.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = construct_proxy_ref(ID),
        data = #domain_ProxyDefinition{
            name        = Url,
            description = Url,
            url         = Url,
            options     = Options
        }
    }}.

%%

make_userinfo(PartyID) ->
    #payproc_UserInfo{id = PartyID}.

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Cost).

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

make_tds_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_tds_payment_tool(),
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

create_invoice(InvoiceParams, Client) ->
    InvoiceID = <<_/binary>> = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

start_invoice(Product, Due, Amount, C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    PartyID = ?c(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    InvoiceID = create_invoice(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    InvoiceID.

attach_payment(InvoiceID, PaymentParams, Client) ->
    PaymentID = <<_/binary>> = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    ?payment_started(?payment_w_status(?pending())) = next_event(InvoiceID, Client),
    ?payment_bound(PaymentID, ?trx_info(PaymentID)) = next_event(InvoiceID, Client),
    ?payment_status_changed(PaymentID, ?processed()) = next_event(InvoiceID, Client),
    PaymentID.

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

assert_failed_post_request(Req) ->
    {ok, 500, _RespHeaders, _ClientRef} = post_request(Req).

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form}.

start_handler(Module, ProxyID, ProxyOpts, Context) ->
    start_handler(Module, ProxyID, ProxyOpts, Context, #{}).

start_handler(Module, ProxyID, ProxyOpts, Context, HandlerOpts) ->
    ProxyUrl = start_service_handler(Module, Context, HandlerOpts),
    ok = hg_domain:update(construct_proxy(ProxyID, ProxyUrl, ProxyOpts)).
