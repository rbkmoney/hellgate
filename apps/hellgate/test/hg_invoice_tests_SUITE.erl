-module(hg_invoice_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invoice_cancellation/1]).
-export([overdue_invoice_cancelled/1]).
-export([payment_success/1]).
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
        payment_success,

        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?c(apps, C)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("invoice_events.hrl").

-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    Client = hg_client_invoicing:start_link(make_userinfo(), hg_client_api:new(?c(root_url, C))),
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
    InvoiceParams = make_invoice_params(<<"rubberduck">>, 10000),
    {ok, InvoiceID} = hg_client_invoicing:create(InvoiceParams, Client),
    {exception, #payproc_InvalidInvoiceStatus{}} = hg_client_invoicing:fulfill(InvoiceID, <<"perfect">>, Client),
    ok = hg_client_invoicing:rescind(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client = ?c(client, C),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(1), 10000),
    {ok, InvoiceID} = hg_client_invoicing:create(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, Client).

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = ?c(client, C),
    ProxyUrl = start_service_handler(hg_dummy_provider, C),
    ok = application:set_env(hellgate, provider_proxy_url, ProxyUrl),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(2), 42000),
    PaymentParams = make_payment_params(),
    {ok, InvoiceID} = hg_client_invoicing:create(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    {ok, PaymentID} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    ?payment_started(?payment_w_status(?pending())) = next_event(InvoiceID, Client),
    ?payment_bound(PaymentID, ?trx_info(PaymentID)) = next_event(InvoiceID, Client),
    ?payment_status_changed(PaymentID, ?succeeded()) = next_event(InvoiceID, Client),
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
    Host = "localhost",
    Port = get_random_port(),
    ChildSpec = hg_test_proxy:get_child_spec(Module, Host, Port),
    {ok, _} = supervisor:start_child(?c(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, Host, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

%%

make_userinfo() ->
    #payproc_UserInfo{id = <<?MODULE_STRING>>}.

make_invoice_params(Product, Cost) ->
    hg_ct_helper:make_invoice_params(<<?MODULE_STRING>>, Product, Cost).

make_invoice_params(Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(<<?MODULE_STRING>>, Product, Due, Cost).

make_payment_params() ->
    {PaymentTool, Session} = make_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params(PaymentTool, Session) ->
    #payproc_InvoicePaymentParams{
        payer = #domain_Payer{
            payment_tool = PaymentTool,
            session = Session,
            client_info = #domain_ClientInfo{}
        }
    }.

make_payment_tool() ->
    {
        {bank_card, #domain_BankCard{
            token          = <<"TOKEN42">>,
            payment_system = visa,
            bin            = <<"424242">>,
            masked_pan     = <<"4242">>
        }},
        <<"SESSION42">>
    }.

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.
