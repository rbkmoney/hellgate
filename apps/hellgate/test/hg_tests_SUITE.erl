-module(hg_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invoice_cancellation/1]).
-export([overdue_invoice_cancelled/1]).
-export([payment_success/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%

-define(config(Key), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].

all() ->
    [
        invoice_cancellation,
        overdue_invoice_cancelled,
        payment_success
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    Host = "localhost",
    % Port = rand:uniform(32768) + 32767,
    Port = 8042,
    RootUrl = "http://" ++ Host ++ ":" ++ integer_to_list(Port),
    Apps =
        hg_ct_helper:start_app(lager) ++
        hg_ct_helper:start_app(woody) ++
        hg_ct_helper:start_app(hellgate, [
            {host, Host},
            {port, Port},
            % FIXME:
            %   You will need up and running mgun reachable at the following url,
            %   properly configured to serve incoming requests and talk back to
            %   the test hg instance.
            {automaton_service_url, <<"http://localhost:8022/v1/automaton_service">>}
        ]),
    [{root_url, RootUrl}, {apps, lists:reverse(Apps)} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-define(ev_invoice_status(Status),
    #'InvoiceStatusChanged'{invoice = #'Invoice'{status = Status}}).
-define(ev_invoice_status(Status, Details),
    #'InvoiceStatusChanged'{invoice = #'Invoice'{status = Status, details = Details}}).
-define(ev_payment_status(PaymentID, Status),
    #'InvoicePaymentStatusChanged'{payment = #'InvoicePayment'{id = PaymentID, status = Status}}).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    Client = hg_client:new(?config(root_url), make_userinfo()),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    [{client, Client}, {test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    _ = unlink(?config(test_sup)),
    _ = application:set_env(hellgate, provider_proxy_url, undefined),
    exit(?config(test_sup), shutdown).

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client = ?config(client),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, 10000),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    {exception, #'InvalidInvoiceStatus'{}} = hg_client:fulfill_invoice(InvoiceID, <<"perfect">>, Client),
    ok = hg_client:void_invoice(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client = ?config(client),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(1), 10000),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    {ok, ?ev_invoice_status(unpaid)} = hg_client:get_next_event(InvoiceID, 3000, Client),
    {ok, ?ev_invoice_status(cancelled, <<"overdue">>)} = hg_client:get_next_event(InvoiceID, 3000, Client).

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = ?config(client),
    ProxyUrl = start_service_handler(hg_dummy_provider, C),
    ok = application:set_env(hellgate, provider_proxy_url, ProxyUrl),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(5), 42000),
    PaymentParams = make_payment_params(),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    {ok, ?ev_invoice_status(unpaid)} = hg_client:get_next_event(InvoiceID, 3000, Client),
    {ok, PaymentID} = hg_client:start_payment(InvoiceID, PaymentParams, Client),
    {ok, ?ev_payment_status(PaymentID, pending)} = hg_client:get_next_event(InvoiceID, 3000, Client),
    {ok, ?ev_payment_status(PaymentID, succeeded)} = hg_client:get_next_event(InvoiceID, 3000, Client),
    % FIXME: will fail when eventlist feature lands in mg
    timeout = hg_client:get_next_event(InvoiceID, 3000, Client).

%%

start_service_handler(Module, C) ->
    Host = "localhost",
    Port = get_random_port(),
    ChildSpec = hg_test_proxy:get_child_spec(Module, Host, Port),
    {ok, _} = supervisor:start_child(?config(test_sup), ChildSpec),
    hg_test_proxy:get_url(Module, Host, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

%%

make_userinfo() ->
    #'UserInfo'{id = <<?MODULE_STRING>>}.

make_invoice_params(Product, Cost) ->
    make_invoice_params(Product, make_due_date(), Cost).

make_invoice_params(Product, Due, Cost) ->
    make_invoice_params(Product, Due, Cost, []).

make_invoice_params(Product, Due, Amount, Context) when is_integer(Amount) ->
    make_invoice_params(Product, Due, {Amount, <<"RUB">>}, Context);
make_invoice_params(Product, Due, {Amount, Currency}, Context) ->
    #'InvoiceParams'{
        product  = Product,
        amount   = Amount,
        due      = format_datetime(Due),
        currency = #'CurrencyRef'{symbolic_code = Currency},
        context  = term_to_binary(Context)
    }.

make_payment_params() ->
    {PaymentTool, Session} = make_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params(PaymentTool, Session) ->
    #'InvoicePaymentParams'{
        payer = #'Payer'{},
        payment_tool = PaymentTool,
        session = Session
    }.

make_payment_tool() ->
    {
        {bank_card, #'BankCard'{
            token          = <<"TOKEN42">>,
            payment_system = visa,
            bin            = <<"424242">>,
            masked_pan     = <<"4242">>
        }},
        <<"SESSION42">>
    }.

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

format_datetime(Datetime = {_, _}) ->
    genlib_format:format_datetime_iso8601(Datetime);
format_datetime(Timestamp) when is_integer(Timestamp) ->
    format_datetime(genlib_time:unixtime_to_daytime(Timestamp)).
