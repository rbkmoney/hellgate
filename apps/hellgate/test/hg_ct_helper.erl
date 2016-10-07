-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([make_invoice_params/3]).
-export([make_invoice_params/4]).
-export([make_invoice_params/5]).

%%

-type app_name() :: atom().

-spec start_app(app_name()) -> [app_name()].

start_app(lager = AppName) ->
    {start_app(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, [debug, false]}
        ]}
    ]), #{}};

start_app(woody = AppName) ->
    {start_app(AppName, [
        {acceptors_pool_size, 4}
    ]), #{}};

start_app(hellgate = AppName) ->
    Host = "hellgate",
    Port = 8022,
    RootUrl = "http://" ++ Host ++ ":" ++ integer_to_list(Port),
    {start_app(AppName, [
        {host, Host},
        {port, Port},
        {automaton_service_url, <<"http://machinegun:8022/v1/automaton">>},
        {eventsink_service_url, <<"http://machinegun:8022/v1/event_sink">>},
        {eventsink_id         , <<"payproc">>}
    ]), #{
        hellgate_root_url => RootUrl
    }};

start_app(AppName) ->
    {genlib_app:start_application(AppName), #{}}.

-spec start_app(app_name(), list()) -> [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_apps([app_name() | {app_name(), list()}]) -> [app_name()].

start_apps(Apps) ->
    lists:foldl(
        fun
            ({AppName, Env}, {AppsAcc, RetAcc}) ->
                {lists:reverse(start_app(AppName, Env)) ++ AppsAcc, RetAcc};
            (AppName, {AppsAcc, RetAcc}) ->
                {Apps0, Ret0} = start_app(AppName),
                {lists:reverse(Apps0) ++ AppsAcc, maps:merge(Ret0, RetAcc)}
        end,
        {[], #{}},
        Apps
    ).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-type shop_id()        :: hg_domain_thrift:'ShopID'().
-type cost()           :: integer() | {integer(), binary()}.
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type timestamp()      :: integer().

-spec make_invoice_params(shop_id(), binary(), cost()) ->
    invoice_params().

make_invoice_params(ShopID, Product, Cost) ->
    make_invoice_params(ShopID, Product, make_due_date(), Cost).

-spec make_invoice_params(shop_id(), binary(), timestamp(), cost()) ->
    invoice_params().

make_invoice_params(ShopID, Product, Due, Cost) ->
    make_invoice_params(ShopID, Product, Due, Cost, []).

-spec make_invoice_params(shop_id(), binary(), timestamp(), cost(), term()) ->
    invoice_params().

make_invoice_params(ShopID, Product, Due, Amount, Context) when is_integer(Amount) ->
    make_invoice_params(ShopID, Product, Due, {Amount, <<"RUB">>}, Context);
make_invoice_params(ShopID, Product, Due, {Amount, Currency}, Context) ->
    #payproc_InvoiceParams{
        shop_id  = ShopID,
        product  = Product,
        amount   = Amount,
        due      = hg_datetime:format_ts(Due),
        currency = #domain_CurrencyRef{symbolic_code = Currency},
        context  = term_to_binary(Context)
    }.

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.
