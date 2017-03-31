-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([create_party_and_shop/1]).
-export([create_contract/2]).
-export([create_shop/4]).
-export([create_shop/5]).
-export([set_shop_proxy/4]).
-export([get_first_contract_id/1]).
-export([get_first_battle_ready_contract_id/1]).
-export([get_first_payout_tool_id/2]).

-export([make_battle_ready_contract_params/0]).
-export([make_battle_ready_contract_params/1]).

-export([make_invoice_params/4]).
-export([make_invoice_params/5]).

-export([make_shop_details/1]).
-export([make_shop_details/2]).

-export([bank_card_tds_token/0]).
-export([bank_card_simple_token/0]).
-export([make_tds_payment_tool/0]).
-export([make_simple_payment_tool/0]).
-export([get_hellgate_url/0]).


-include("hg_ct_domain.hrl").
-include_lib("hellgate/include/domain.hrl").
-include_lib("dmsl/include/dmsl_base_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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
            {lager_common_test_backend, debug}
        ]}
    ]), #{}};

start_app(woody = AppName) ->
    {start_app(AppName, [
        {acceptors_pool_size, 4}
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

%%

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/party_events.hrl").

-type party_id()       :: dmsl_domain_thrift:'PartyID'().
-type contract_id()    :: dmsl_domain_thrift:'ContractID'().
-type shop_id()        :: dmsl_domain_thrift:'ShopID'().
-type category()       :: dmsl_domain_thrift:'CategoryRef'().
-type cost()           :: integer() | {integer(), binary()}.
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type proxy_ref()      :: dmsl_domain_thrift:'ProxyRef'().
-type proxy_options()  :: dmsl_domain_thrift:'ProxyOptions'().
-type timestamp()      :: integer().

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

-spec create_contract(dmsl_payment_processing_thrift:'ContractParams'(), Client :: pid()) ->
    contract_id().

create_contract(_, _) -> ok.
% create_contract(#payproc_ContractParams{} = Params, Client) ->
%     #payproc_ClaimResult{id = ClaimID} = hg_client_party:create_contract(Params, Client),
%     #payproc_Claim{changeset = Changeset} = hg_client_party:get_claim(ClaimID, Client),
%     {_, #domain_Contract{id = ContractID}} = lists:keyfind(contract_creation, 1, Changeset),
%     ok = hg_client_party:accept_claim(ClaimID, Client),
%     ContractID.

-spec create_shop(contract_id(), category(), binary(), Client :: pid()) ->
    shop_id().

create_shop(ContractID, Category, Name, Client) ->
    create_shop(ContractID, Category, Name, undefined, Client).

-spec create_shop(contract_id(), category(), binary(), binary(), Client :: pid()) ->
    shop_id().

create_shop(_, _, _, _, _) -> ok.
% create_shop(ContractID, Category, Name, Description, Client) ->
%     PayoutToolID = hg_ct_helper:get_first_payout_tool_id(ContractID, Client),
%     Params = #payproc_ShopParams{
%         contract_id       = ContractID,
%         payout_tool_id    = PayoutToolID,
%         category          = Category,
%         details           = make_shop_details(Name, Description)
%     },
%     #payproc_ClaimResult{id = ClaimID} = hg_client_party:create_shop(Params, Client),
%     #payproc_Claim{changeset = Changeset} = hg_client_party:get_claim(ClaimID, Client),
%     {_, #domain_Shop{id = ShopID}} = lists:keyfind(shop_creation, 1, Changeset),
%     ok = hg_client_party:accept_claim(ClaimID, Client),
%     #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, Client),
%     ok = flush_events(Client),
%     ShopID.

-spec set_shop_proxy(shop_id(), proxy_ref(), proxy_options(), Client :: pid()) ->
    ok.

set_shop_proxy(_, _, _, _) ->
% set_shop_proxy(ShopID, ProxyRef, ProxyOptions, Client) ->
    % Proxy = #domain_Proxy{ref = ProxyRef, additional = ProxyOptions},
    % Update = #payproc_ShopUpdate{proxy = Proxy},
    % #payproc_ClaimResult{status = ?accepted(_, _)} = hg_client_party:update_shop(ShopID, Update, Client),
    % ok = flush_events(Client),
    ok.

% flush_events(Client) ->
%     case hg_client_party:pull_event(500, Client) of
%         timeout ->
%             ok;
%         _Event ->
%             flush_events(Client)
%     end.

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
    _PayoutToolParams = #payproc_PayoutToolParams{
        currency = ?cur(<<"RUB">>),
        tool_info = {bank_account, BankAccount}
    },
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef
        % payout_tool_params = PayoutToolParams
    }.

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
        details  = #domain_InvoiceDetails{product = Product},
        due      = hg_datetime:format_ts(Due),
        cost     = #domain_Cash{
            amount   = Amount,
            currency = ?cur(Currency)
        },
        context  = #'Content'{
            type = <<"application/octet-stream">>,
            data = <<"some_merchant_specific_data">>
        }
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
