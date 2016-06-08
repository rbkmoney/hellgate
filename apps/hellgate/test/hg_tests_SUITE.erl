-module(hg_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).

-export([invoice_cancellation/1]).
-export([overdue_invoice_cancelled/1]).

%%

-define(config(Key), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].

all() ->
    [
        invoice_cancellation,
        overdue_invoice_cancelled
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
            % FIXME
            {automaton_service_url, <<"http://localhost:8022/v1/automaton_service">>}
        ]),
    [{root_url, RootUrl}, {apps, lists:reverse(Apps)} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    Client = hg_client:new(?config(root_url)),
    [{client, Client} | C].

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client0 = ?config(client),
    UserInfo = make_userinfo(),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, {10000, <<"RUB">>}),
    {{ok, InvoiceID}, Client1} =
        hg_client:create_invoice(UserInfo, InvoiceParams, Client0),
    {{exception, #'InvalidInvoiceStatus'{}}, Client2} =
        hg_client:fulfill_invoice(UserInfo, InvoiceID, <<"perfect">>, Client1),
    {ok, _Client3} =
        hg_client:void_invoice(UserInfo, InvoiceID, <<"whynot">>, Client2).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client0 = ?config(client),
    UserInfo = make_userinfo(),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(3), {10000, <<"RUB">>}, []),
    {{ok, InvoiceID}, Client1} =
        hg_client:create_invoice(UserInfo, InvoiceParams, Client0),
    {{ok, #'InvoiceStatusChanged'{invoice = Invoice0}}, Client2} =
        hg_client:get_next_event(UserInfo, InvoiceID, 5000, Client1),
    #'Invoice'{status = unpaid} = Invoice0,
    {{ok, #'InvoiceStatusChanged'{invoice = Invoice1}}, _Client3} =
        hg_client:get_next_event(UserInfo, InvoiceID, 5000, Client2),
    #'Invoice'{status = cancelled, details = <<"overdue">>} = Invoice1.

%%

make_userinfo() ->
    #'UserInfo'{id = <<?MODULE_STRING>>}.

make_invoice_params(Product, Cost) ->
    make_invoice_params(Product, Cost, []).

make_invoice_params(Product, Cost, Context) ->
    make_invoice_params(Product, make_due_date(), Cost, Context).

make_invoice_params(Product, Due, {Amount, Currency}, Context) ->
    #'InvoiceParams'{
        product = Product,
        amount = Amount,
        due = format_datetime(Due),
        currency = #'CurrencyRef'{symbolic_code = Currency},
        context = term_to_binary(Context)
    }.

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

format_datetime(Datetime = {_, _}) ->
    genlib_format:format_datetime_iso8601(Datetime);
format_datetime(Timestamp) when is_integer(Timestamp) ->
    format_datetime(genlib_time:unixtime_to_daytime(Timestamp)).
