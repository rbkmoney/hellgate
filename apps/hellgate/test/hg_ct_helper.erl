-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([cfg/2]).

-export([create_party_and_shop/1]).
-export([get_account/1]).
-export([get_first_contract_id/1]).
-export([get_first_battle_ready_contract_id/1]).
-export([get_first_payout_tool_id/2]).

-export([make_battle_ready_contract_params/0]).
-export([make_battle_ready_contract_params/1]).
-export([make_battle_ready_payout_tool_params/0]).

-export([make_userinfo/1]).

-export([make_invoice_params/4]).
-export([make_invoice_params/5]).

-export([make_invoice_params_tpl/1]).
-export([make_invoice_params_tpl/2]).
-export([make_invoice_params_tpl/3]).

-export([make_invoice_tpl_create_params/3]).
-export([make_invoice_tpl_create_params/4]).
-export([make_invoice_tpl_create_params/5]).
-export([make_invoice_tpl_create_params/6]).

-export([make_invoice_tpl_update_params/1]).

-export([make_invoice_context/0]).
-export([make_invoice_context/1]).

-export([make_shop_details/1]).
-export([make_shop_details/2]).

-export([make_cash/2]).

-export([make_lifetime/3]).
-export([make_invoice_tpl_cost/3]).
-export([make_invoice_details/1]).
-export([make_invoice_details/2]).

-export([bank_card_tds_token/0]).
-export([bank_card_simple_token/0]).
-export([make_tds_payment_tool/0]).
-export([make_simple_payment_tool/0]).
-export([get_hellgate_url/0]).


-include("hg_ct_domain.hrl").
-include_lib("hellgate/include/domain.hrl").
-include_lib("dmsl/include/dmsl_base_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export_type([config/0]).
-export_type([test_case_name/0]).
-export_type([group_name/0]).

%%

-define(HELLGATE_HOST, "hellgate").
-define(HELLGATE_PORT, 8022).


-type app_name() :: atom().

-spec start_app(app_name()) -> [app_name()].

start_app(lager = AppName) ->
    {start_app(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, warning}
        ]}
    ]), #{}};

start_app(woody = AppName) ->
    {start_app(AppName, [
        {acceptors_pool_size, 4}
    ]), #{}};

start_app(dmt_client = AppName) ->
    {start_app(AppName, [
        {cache_update_interval, 5000}, % milliseconds
        {max_cache_size, #{
            elements => 20,
            memory => 52428800 % 50Mb
        }},
        {service_urls, #{
            'Repository' => <<"dominant:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"dominant:8022/v1/domain/repository_client">>
        }}
    ]), #{}};

start_app(hellgate = AppName) ->
    {start_app(AppName, [
        {host, ?HELLGATE_HOST},
        {port, ?HELLGATE_PORT},
        {service_urls, #{
            'Automaton' => <<"http://machinegun:8022/v1/automaton">>,
            'EventSink' => <<"http://machinegun:8022/v1/event_sink">>,
            'Accounter' => <<"http://shumway:8022/accounter">>
        }},
        {proxy_opts, #{
            transport_opts => #{
                connect_timeout => 1000,
                recv_timeout    => 1000
            }
        }}
    ]), #{
        hellgate_root_url => get_hellgate_url()
    }};

start_app(AppName) ->
    {genlib_app:start_application(AppName), #{}}.

-spec start_app(app_name(), list()) -> [app_name()].

start_app(cowboy = AppName, Env) ->
    #{
        listener_ref := Ref,
        acceptors_count := Count,
        transport_opts := TransOpt,
        proto_opts := ProtoOpt
    } = Env,
    cowboy:start_http(Ref, Count, TransOpt, ProtoOpt),
    [AppName];

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


-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-spec cfg(atom(), config()) -> term().

cfg(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        _        -> undefined
    end.

%%

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/party_events.hrl").

-type user_info()                 :: dmsl_payment_processing_thrift:'UserInfo'().
-type party_id()                  :: dmsl_domain_thrift:'PartyID'().
-type account_id()                :: dmsl_domain_thrift:'AccountID'().
-type account()                   :: map().
-type contract_id()               :: dmsl_domain_thrift:'ContractID'().
-type shop_id()                   :: dmsl_domain_thrift:'ShopID'().
-type cost()                      :: integer() | {integer(), binary()}.
-type cash()                      :: dmsl_domain_thrift:'Cash'().
-type invoice_tpl_id()            :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_params()            :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type invoice_params_tpl()        :: dmsl_payment_processing_thrift:'InvoiceWithTemplateParams'().
-type timestamp()                 :: integer().
-type context()                   :: dmsl_base_thrift:'Content'().
-type lifetime_interval()         :: dmsl_domain_thrift:'LifetimeInterval'().
-type invoice_details()           :: dmsl_domain_thrift:'InvoiceDetails'().
-type invoice_tpl_cost()          :: dmsl_domain_thrift:'InvoiceTemplateCost'().
-type currency()                  :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type invoice_tpl_create_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateCreateParams'().
-type invoice_tpl_update_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateUpdateParams'().

-spec create_party_and_shop(Client :: pid()) ->
    shop_id().

create_party_and_shop(Client) ->
    _ = hg_client_party:create(make_party_params(), Client),
    #domain_Party{shops = Shops} = hg_client_party:get(Client),
    [{ShopID, _Shop}] = maps:to_list(Shops),
    ShopID.

make_party_params() ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = <<?MODULE_STRING>>
        }
    }.

-spec get_first_contract_id(Client :: pid()) ->
    contract_id().

get_first_contract_id(Client) ->
    #domain_Party{contracts = Contracts} = hg_client_party:get(Client),
    lists:min(maps:keys(Contracts)).

-spec get_first_battle_ready_contract_id(Client :: pid()) ->
    contract_id().

get_first_battle_ready_contract_id(Client) ->
    #domain_Party{contracts = Contracts} = hg_client_party:get(Client),
    IDs = lists:foldl(fun({ID, Contract}, Acc) ->
            case Contract of
                #domain_Contract{
                    contractor = {legal_entity, _},
                    payout_tools = [#domain_PayoutTool{} | _]
                } ->
                    [ID | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        maps:to_list(Contracts)
    ),
    case IDs of
        [_ | _] ->
            lists:min(IDs);
        [] ->
            error(not_found)
    end.

-spec get_account(account_id()) -> account().

get_account(AccountID) ->
    % TODO we sure need to proxy this through the hellgate interfaces
    _ = hg_context:set(woody_context:new()),
    Account = hg_accounting:get_account(AccountID),
    _ = hg_context:cleanup(),
    Account.

-spec get_first_payout_tool_id(contract_id(), Client :: pid()) ->
    dmsl_domain_thrift:'PayoutToolID'().

get_first_payout_tool_id(ContractID, Client) ->
    #domain_Contract{payout_tools = PayoutTools} = hg_client_party:get_contract(ContractID, Client),
    case PayoutTools of
        [Tool | _] ->
            Tool#domain_PayoutTool.id;
        [] ->
            error(not_found)
    end.

-spec make_battle_ready_contract_params() ->
    dmsl_payment_processing_thrift:'ContractParams'().

make_battle_ready_contract_params() ->
    make_battle_ready_contract_params(undefined).

-spec make_battle_ready_contract_params(dmsl_domain_thrift:'ContractTemplateRef'()) ->
    dmsl_payment_processing_thrift:'ContractParams'().

make_battle_ready_contract_params(TemplateRef) ->
    BankAccount = #domain_BankAccount{
        account = <<"4276300010908312893">>,
        bank_name = <<"SomeBank">>,
        bank_post_account = <<"123129876">>,
        bank_bik = <<"66642666">>
    },
    Contractor = {legal_entity,
        {russian_legal_entity, #domain_RussianLegalEntity {
            registered_name = <<"Hoofs & Horns OJSC">>,
            registered_number = <<"1234509876">>,
            inn = <<"1213456789012">>,
            actual_address = <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,
            post_address = <<"NaN">>,
            representative_position = <<"Director">>,
            representative_full_name = <<"Someone">>,
            representative_document = <<"100$ banknote">>,
            bank_account = BankAccount
        }}
    },
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef
    }.

-spec make_battle_ready_payout_tool_params() ->
    dmsl_payment_processing_thrift:'PayoutToolParams'().

make_battle_ready_payout_tool_params() ->
    #payproc_PayoutToolParams{
        currency = ?cur(<<"RUB">>),
        tool_info = {bank_account, #domain_BankAccount{
            account = <<"4276300010908312893">>,
            bank_name = <<"SomeBank">>,
            bank_post_account = <<"123129876">>,
            bank_bik = <<"66642666">>
        }}
    }.

-spec make_userinfo(party_id()) ->
    user_info().
make_userinfo(PartyID) ->
    #payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}}.

-spec make_invoice_params(party_id(), shop_id(), binary(), cost()) ->
    invoice_params().

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    make_invoice_params(PartyID, ShopID, Product, make_due_date(), Cost).

-spec make_invoice_params(party_id(), shop_id(), binary(), timestamp(), cost()) ->
    invoice_params().

make_invoice_params(PartyID, ShopID, Product, Due, Amount) when is_integer(Amount) ->
    make_invoice_params(PartyID, ShopID, Product, Due, {Amount, <<"RUB">>});
make_invoice_params(PartyID, ShopID, Product, Due, {Amount, Currency}) ->
    #payproc_InvoiceParams{
        party_id = PartyID,
        shop_id  = ShopID,
        details  = make_invoice_details(Product),
        due      = hg_datetime:format_ts(Due),
        cost     = make_cash(Amount, Currency),
        context  = make_invoice_context()
    }.

-spec make_invoice_params_tpl(invoice_tpl_id()) ->
    invoice_params_tpl().

make_invoice_params_tpl(TplID) ->
    make_invoice_params_tpl(TplID, undefined).

-spec make_invoice_params_tpl(invoice_tpl_id(), cost()) ->
    invoice_params_tpl().

make_invoice_params_tpl(TplID, Cost) ->
    make_invoice_params_tpl(TplID, Cost, undefined).

-spec make_invoice_params_tpl(invoice_tpl_id(), cost(), context()) ->
    invoice_params_tpl().

make_invoice_params_tpl(TplID, Cost, Context) ->
    #payproc_InvoiceWithTemplateParams{
       template_id = TplID,
       cost        = Cost,
       context     = Context
    }.

-spec make_invoice_tpl_create_params(party_id(), shop_id(), binary()) ->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Product) ->
    make_invoice_tpl_create_params(PartyID, ShopID, Product, make_lifetime()).

-spec make_invoice_tpl_create_params(party_id(), shop_id(), binary(),
    lifetime_interval())
->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime) ->
    make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, make_invoice_tpl_cost()).
-spec make_invoice_tpl_create_params(party_id(), shop_id(), binary(),
    lifetime_interval(), invoice_tpl_cost())
->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost) ->
    make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost, make_invoice_context()).

-spec make_invoice_tpl_create_params(party_id(), shop_id(), binary(),
    lifetime_interval(), invoice_tpl_cost(), context())
->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost, Context) ->
    #payproc_InvoiceTemplateCreateParams{
        party_id         = PartyID,
        shop_id          = ShopID,
        details          = make_invoice_details(Product),
        invoice_lifetime = Lifetime,
        cost             = Cost,
        context          = Context
    }.

-spec make_invoice_tpl_update_params(map()) -> invoice_tpl_update_params().

make_invoice_tpl_update_params(Diff) ->
    maps:fold(fun update_field/3, #payproc_InvoiceTemplateUpdateParams{}, Diff).

update_field(party_id, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{party_id = V};
update_field(shop_id, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{shop_id = V};
update_field(details, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{details = V};
update_field(invoice_lifetime, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{invoice_lifetime = V};
update_field(cost, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{cost = V};
update_field(context, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{context = V}.



-spec make_lifetime() -> lifetime_interval().

make_lifetime() ->
    make_lifetime(0, 0, 1).

-spec make_lifetime(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    lifetime_interval().

make_lifetime(Y, M, D) ->
    #domain_LifetimeInterval{
        days   = D,
        months = M,
        years  = Y
    }.

-spec make_invoice_details(binary()) -> invoice_details().

make_invoice_details(Product) ->
    make_invoice_details(Product, undefined).

-spec make_invoice_details(binary(), binary() | undefined) -> invoice_details().

make_invoice_details(Product, Description) ->
    #domain_InvoiceDetails{
        product = Product,
        description = Description
    }.

-spec make_invoice_tpl_cost() -> invoice_tpl_cost().

make_invoice_tpl_cost() ->
    make_invoice_tpl_cost(fixed, 10000, <<"RUB">>).

-type cash_bound() :: {inclusive | exclusive, non_neg_integer(), currency()}.

-spec make_invoice_tpl_cost
    (fixed, non_neg_integer(), currency()) -> invoice_tpl_cost();
    (range, cash_bound(), cash_bound())    -> invoice_tpl_cost();
    (unlim, _, _)                          -> invoice_tpl_cost().

make_invoice_tpl_cost(fixed, Amount, Currency) ->
    {fixed, make_cash(Amount, Currency)};
make_invoice_tpl_cost(range, {LowerType, LowerAm, LowerCur}, {UpperType, UpperAm, UpperCur}) ->
    {range, #domain_CashRange{
        upper = make_cash_bound(UpperType, UpperAm, UpperCur),
        lower = make_cash_bound(LowerType, LowerAm, LowerCur)
    }};
make_invoice_tpl_cost(unlim, _, _) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}}.

-spec make_cash(non_neg_integer(), currency()) -> cash().

make_cash(Amount, Currency) ->
    #domain_Cash{
        amount   = Amount,
        currency = ?cur(Currency)
    }.

make_cash_bound(Type, Amount, Currency) when Type =:= inclusive orelse Type =:= exclusive ->
    {Type, make_cash(Amount, Currency)}.

-spec make_invoice_context() -> context().

make_invoice_context() ->
    make_invoice_context(<<"some_merchant_specific_data">>).

-spec make_invoice_context(binary()) -> context().

make_invoice_context(Data) ->
    #'Content'{
        type = <<"application/octet-stream">>,
        data = Data
    }.

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

-spec bank_card_tds_token() -> string().

bank_card_tds_token() ->
    <<"TOKEN666">>.

-spec bank_card_simple_token() -> string().

bank_card_simple_token() ->
    <<"TOKEN42">>.

-spec make_tds_payment_tool() -> hg_domain_thrift:'PaymentTool'().

make_tds_payment_tool() ->
    {
        {bank_card, #domain_BankCard{
            token          = bank_card_tds_token(),
            payment_system = visa,
            bin            = <<"666666">>,
            masked_pan     = <<"666">>
        }},
        <<"SESSION666">>
    }.

-spec make_simple_payment_tool() -> hg_domain_thrift:'PaymentTool'().

make_simple_payment_tool() ->
    {
        {bank_card, #domain_BankCard{
            token          = bank_card_simple_token(),
            payment_system = visa,
            bin            = <<"424242">>,
            masked_pan     = <<"4242">>
        }},
        <<"SESSION42">>
    }.

-spec get_hellgate_url() -> string().

get_hellgate_url() ->
    "http://" ++ ?HELLGATE_HOST ++ ":" ++ integer_to_list(?HELLGATE_PORT).

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.
