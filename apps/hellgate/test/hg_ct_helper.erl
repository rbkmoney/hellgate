-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([create_party_and_shop/1]).

-export([make_invoice_params/4]).
-export([make_invoice_params/5]).
-export([make_invoice_params/6]).

-export([make_category_ref/1]).
-export([make_shop_details/1]).
-export([make_shop_details/2]).

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
        {accounter_service_url, <<"http://shumway:8022/accounter">>}
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

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/party_events.hrl").

-type party_id()       :: dmsl_domain_thrift:'PartyID'().
-type shop_id()        :: dmsl_domain_thrift:'ShopID'().
-type shop()           :: dmsl_domain_thrift:'Shop'().
-type cost()           :: integer() | {integer(), binary()}.
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type timestamp()      :: integer().

-spec create_party_and_shop(Client :: pid()) ->
    shop().

create_party_and_shop(Client) ->
    ok = hg_client_party:create(Client),
    {ok, #payproc_ClaimResult{id = ClaimID, status = ?pending()}} =
        hg_client_party:create_shop(make_shop_params(42, <<"THRIFT SHOP">>), Client),
    ok = hg_client_party:accept_claim(ClaimID, Client),
    {ok, #payproc_PartyState{party = #domain_Party{shops = Shops}}} =
        hg_client_party:get(Client),
    [{ShopID, _Shop} | _] = maps:to_list(Shops),
    {ok, #payproc_ClaimResult{status = ?accepted(_)}} =
        hg_client_party:activate_shop(ShopID, Client),
    ShopID.

make_shop_params(CategoryID, Name) ->
    #payproc_ShopParams{
        category = make_category_ref(CategoryID),
        details  = make_shop_details(Name)
    }.

-spec make_invoice_params(party_id(), shop_id(), binary(), cost()) ->
    invoice_params().

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    make_invoice_params(PartyID, ShopID, Product, make_due_date(), Cost).

-spec make_invoice_params(party_id(), shop_id(), binary(), timestamp(), cost()) ->
    invoice_params().

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    make_invoice_params(PartyID, ShopID, Product, Due, Cost, []).

-spec make_invoice_params(party_id(), shop_id(), binary(), timestamp(), cost(), term()) ->
    invoice_params().

make_invoice_params(PartyID, ShopID, Product, Due, Amount, Context) when is_integer(Amount) ->
    make_invoice_params(PartyID, ShopID, Product, Due, {Amount, <<"RUB">>}, Context);
make_invoice_params(PartyID, ShopID, Product, Due, {Amount, Currency}, Context) ->
    #payproc_InvoiceParams{
        party_id = PartyID,
        shop_id  = ShopID,
        product  = Product,
        amount   = Amount,
        due      = hg_datetime:format_ts(Due),
        currency = #domain_CurrencyRef{symbolic_code = Currency},
        context  = #'Content'{
            type = <<"application/octet-stream">>,
            data = term_to_binary(Context)
        }
    }.

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

-spec make_category_ref(dmsl_domain_thrift:'ObjectID'()) ->
    dmsl_domain_thrift:'CategoryRef'().

make_category_ref(ID) ->
    #domain_CategoryRef{id = ID}.

-spec make_shop_details(binary()) ->
    dmsl_domain_thrift:'ShopDetails'().

make_shop_details(Name) ->
    make_shop_details(Name, undefined).

-spec make_shop_details(binary(), binary()) ->
    dmsl_domain_thrift:'ShopDetails'().

make_shop_details(Name, Description) ->
    #domain_ShopDetails{
        name        = Name,
        description = Description
    }.
