-module(hg_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([no_events/1]).
-export([events_observed/1]).
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

-define(config(Key), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].

all() ->
    [
        no_events,
        events_observed,
        invoice_cancellation,
        overdue_invoice_cancelled,
        payment_success,
        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    Host = "hellgate",
    Port = 8022,
    RootUrl = "http://" ++ Host ++ ":" ++ integer_to_list(Port),
    Apps =
        hg_ct_helper:start_app(lager) ++
        hg_ct_helper:start_app(woody) ++
        hg_ct_helper:start_app(hellgate, [
            {host, Host},
            {port, Port},
            {automaton_service_url, <<"http://machinegun:8022/v1/automaton_service">>},
            {eventsink_service_url, <<"http://machinegun:8024/v1/eventsink_service">>}
        ]),
    [{root_url, RootUrl}, {apps, lists:reverse(Apps)} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("events.hrl").

-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-define(event(ID, InvoiceID, Seq, Payload),
    #payproc_Event{
        id = ID,
        source = {invoice, InvoiceID},
        sequence = Seq,
        payload = Payload
    }
).

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

-spec no_events(config()) -> _ | no_return().

no_events(C) ->
    Client = ?config(client),
    none = hg_client:get_last_event_id(Client).

-spec events_observed(config()) -> _ | no_return().

events_observed(C) ->
    Client = ?config(client),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, 10000),
    {ok, InvoiceID1} = hg_client:create_invoice(InvoiceParams, Client),
    ok = hg_client:rescind_invoice(InvoiceID1, <<"die">>, Client),
    {ok, InvoiceID2} = hg_client:create_invoice(InvoiceParams, Client),
    ok = hg_client:rescind_invoice(InvoiceID2, <<"noway">>, Client),
    {ok, Events1} = hg_client:pull_events(2, 3000, Client),
    {ok, Events2} = hg_client:pull_events(2, 3000, Client),
    [
        ?event(ID1, InvoiceID1, 1, ?invoice_ev(?invoice_created(?invoice_w_status(?unpaid())))),
        ?event(ID2, InvoiceID1, 2, ?invoice_ev(?invoice_status_changed(?cancelled(_))))
    ] = Events1,
    [
        ?event(ID3, InvoiceID2, 1, ?invoice_ev(?invoice_created(?invoice_w_status(?unpaid())))),
        ?event(ID4, InvoiceID2, 2, ?invoice_ev(?invoice_status_changed(?cancelled(_))))
    ] = Events2,
    IDs = [ID1, ID2, ID3, ID4],
    IDs = lists:sort(IDs).

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client = ?config(client),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, 10000),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    {exception, #payproc_InvalidInvoiceStatus{}} = hg_client:fulfill_invoice(InvoiceID, <<"perfect">>, Client),
    ok = hg_client:rescind_invoice(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancelled(config()) -> _ | no_return().

overdue_invoice_cancelled(C) ->
    Client = ?config(client),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(1), 10000),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?cancelled(<<"overdue">>)) = next_event(InvoiceID, Client).

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = ?config(client),
    ProxyUrl = start_service_handler(hg_dummy_provider, C),
    ok = application:set_env(hellgate, provider_proxy_url, ProxyUrl),
    InvoiceParams = make_invoice_params(<<"rubberduck">>, make_due_date(5), 42000),
    PaymentParams = make_payment_params(),
    {ok, InvoiceID} = hg_client:create_invoice(InvoiceParams, Client),
    ?invoice_created(?invoice_w_status(?unpaid())) = next_event(InvoiceID, Client),
    {ok, PaymentID} = hg_client:start_payment(InvoiceID, PaymentParams, Client),
    ?payment_started(?payment_w_status(?pending())) = next_event(InvoiceID, Client),
    ?payment_bound(PaymentID, ?trx_info(PaymentID)) = next_event(InvoiceID, Client),
    ?payment_status_changed(PaymentID, ?succeeded()) = next_event(InvoiceID, Client),
    ?invoice_status_changed(?paid()) = next_event(InvoiceID, 1000, Client),
    timeout = next_event(InvoiceID, 2000, Client).

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = ?config(client),
    {ok, Events} = hg_client:pull_events(_N = 1000, 1000, Client),
    InvoiceSeqs = orddict:to_list(lists:foldl(
        fun (?event(_ID, InvoiceID, Seq, _), Acc) ->
            orddict:update(InvoiceID, fun (Seqs) -> Seqs ++ [Seq] end, [Seq], Acc)
        end,
        orddict:new(),
        Events
    )),
    ok = lists:foreach(
        fun (E = {InvoiceID, Seqs}) ->
            E = {InvoiceID, lists:seq(1, length(Seqs))}
        end,
        InvoiceSeqs
    ).

%%

next_event(InvoiceID, Client) ->
    next_event(InvoiceID, 3000, Client).

next_event(InvoiceID, Timeout, Client) ->
    case hg_client:pull_invoice_event(InvoiceID, Timeout, Client) of
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
    {ok, _} = supervisor:start_child(?config(test_sup), ChildSpec),
    hg_test_proxy:get_url(Module, Host, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

%%

make_userinfo() ->
    #payproc_UserInfo{id = <<?MODULE_STRING>>}.

make_invoice_params(Product, Cost) ->
    make_invoice_params(Product, make_due_date(), Cost).

make_invoice_params(Product, Due, Cost) ->
    make_invoice_params(Product, Due, Cost, []).

make_invoice_params(Product, Due, Amount, Context) when is_integer(Amount) ->
    make_invoice_params(Product, Due, {Amount, <<"RUB">>}, Context);
make_invoice_params(Product, Due, {Amount, Currency}, Context) ->
    #payproc_InvoiceParams{
        product  = Product,
        amount   = Amount,
        due      = format_datetime(Due),
        currency = #domain_CurrencyRef{symbolic_code = Currency},
        context  = term_to_binary(Context)
    }.

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

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

format_datetime(Datetime = {_, _}) ->
    genlib_format:format_datetime_iso8601(Datetime);
format_datetime(Timestamp) when is_integer(Timestamp) ->
    format_datetime(genlib_time:unixtime_to_daytime(Timestamp)).
