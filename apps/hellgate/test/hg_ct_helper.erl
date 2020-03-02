-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([cfg/2]).

-export([create_client/2]).
-export([create_client/3]).

-export([create_party_and_shop/5]).
-export([create_battle_ready_shop/5]).
-export([get_account/1]).
-export([get_balance/1]).
-export([get_first_contract_id/1]).
-export([get_first_battle_ready_contract_id/1]).
-export([get_first_payout_tool_id/2]).
-export([adjust_contract/3]).

-export([make_battle_ready_contract_params/2]).
-export([make_battle_ready_contractor/0]).
-export([make_battle_ready_payout_tool_params/0]).

-export([make_userinfo/1]).

-export([make_invoice_params/4]).
-export([make_invoice_params/5]).

-export([make_invoice_params_tpl/1]).
-export([make_invoice_params_tpl/2]).
-export([make_invoice_params_tpl/3]).

-export([make_invoice_tpl_create_params/5]).
-export([make_invoice_tpl_create_params/6]).
-export([make_invoice_tpl_details/2]).

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

-export([make_disposable_payment_resource/1]).
-export([make_customer_params/3]).
-export([make_customer_binding_params/1]).

-export([make_meta_ns/0]).
-export([make_meta_data/0]).
-export([make_meta_data/1]).
-export([get_hellgate_url/0]).

-export([make_trace_id/1]).


-include("hg_ct_domain.hrl").
-include("hg_ct_json.hrl").
-include_lib("hellgate/include/domain.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export_type([config/0]).
-export_type([test_case_name/0]).
-export_type([group_name/0]).

%%

-define(HELLGATE_HOST, "hellgate").
-define(HELLGATE_PORT, 8022).


-type app_name() :: atom().

-spec start_app(app_name()) -> [app_name()].

start_app(scoper = AppName) ->
    {start_app(AppName, [
        {storage, scoper_storage_logger}
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
        {woody_event_handlers, [
            {scoper_woody_event_handler, #{
                event_handler_opts => #{
                    formatter_opts => #{
                        max_length => 1000
                    }
                }
            }}
        ]},
        {service_urls, #{
            'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
        }}
    ]), #{}};

start_app(hellgate = AppName) ->
    {start_app(AppName, [
        {host, ?HELLGATE_HOST},
        {port, ?HELLGATE_PORT},
        {default_woody_handling_timeout, 30000},
        {transport_opts, #{
            max_connections => 8096
        }},
        {scoper_event_handler_options, #{
            event_handler_opts => #{
                formatter_opts => #{
                    max_length => 1000
                }
        }}},
        {services, #{
            accounter           => <<"http://shumway:8022/shumpune">>,
            automaton           => <<"http://machinegun:8022/v1/automaton">>,
            customer_management => #{
                url => <<"http://hellgate:8022/v1/processing/customer_management">>,
                transport_opts => #{
                    pool => customer_management,
                    max_connections => 300
                }
            },
            eventsink           => <<"http://machinegun:8022/v1/event_sink">>,
            fault_detector      => <<"http://127.0.0.1:20001/">>,
            invoice_templating  => #{
                url => <<"http://hellgate:8022/v1/processing/invoice_templating">>,
                transport_opts => #{
                    pool => invoice_templating,
                    max_connections => 300
                }
            },
            invoicing           => #{
                url => <<"http://hellgate:8022/v1/processing/invoicing">>,
                transport_opts => #{
                    pool => invoicing,
                    max_connections => 300
                }
            },
            party_management    => #{
                url => <<"http://hellgate:8022/v1/processing/partymgmt">>,
                transport_opts => #{
                    pool => party_management,
                    max_connections => 300
                }
            },
            recurrent_paytool   => #{
                url => <<"http://hellgate:8022/v1/processing/recpaytool">>,
                transport_opts => #{
                    pool => recurrent_paytool,
                    max_connections => 300
                }
            },
            proxy_host_provider => #{
                url => <<"http://hellgate:8022/v1/proxyhost/provider">>,
                transport_opts => #{
                    pool => proxy_host_provider,
                    max_connections => 300
                }
            },
            payment_processing_eventsink => #{
                url => <<"http://hellgate:8022/v1/processing/eventsink">>,
                transport_opts => #{
                    pool => payment_processing_eventsink,
                    max_connections => 300
                }
            },
            recurrent_paytool_eventsink => #{
                url => <<"http://hellgate:8022/v1/processing/recpaytool/eventsink">>,
                transport_opts => #{
                    pool => recurrent_paytool_eventsink,
                    max_connections => 300
                }
            }
        }},
        {proxy_opts, #{
            transport_opts => #{
                max_connections => 300
            }
        }},
        {payment_retry_policy, #{
            processed => {intervals, [1, 1, 1]},
            captured  => {intervals, [1, 1, 1]},
            refunded  => {intervals, [1, 1, 1]}
        }},
        {inspect_timeout, 1000},
        {fault_detector, #{
            timeout => 20, % very low to speed up tests
            availability => #{
                critical_fail_rate   => 0.7,
                sliding_window       => 60000,
                operation_time_limit => 10000,
                pre_aggregation_size => 2
            },
            conversion => #{
                critical_fail_rate   => 0.7,
                sliding_window       => 6000000,
                operation_time_limit => 1200000,
                pre_aggregation_size => 2
            }
        }}
    ]), #{
        hellgate_root_url => get_hellgate_url()
    }};

start_app(party_management = AppName) ->
    {start_app(AppName, [
        {scoper_event_handler_options, #{
            event_handler_opts => #{
                formatter_opts => #{
                    max_length => 1000
                }
            }}},
        {services, #{
            accounter           => <<"http://shumway:8022/shumpune">>,
            automaton           => <<"http://machinegun:8022/v1/automaton">>,
            party_management    => #{
                url => <<"http://hellgate:8022/v1/processing/partymgmt">>,
                transport_opts => #{
                    pool => party_management,
                    max_connections => 300
                }
            }
        }}
    ]), #{}};

start_app(party_client = AppName) ->
    {start_app(AppName, [
        {services, #{
            party_management => "http://hellgate:8022/v1/processing/partymgmt"
        }},
        {woody, #{
            cache_mode => safe,  % disabled | safe | aggressive
            options => #{
                woody_client => #{
                    event_handler => {scoper_woody_event_handler, #{
                        event_handler_opts => #{
                            formatter_opts => #{
                                max_length => 1000
                            }
                        }
                    }}
                }
            }
        }}
    ]), #{}};

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
    cowboy:start_clear(Ref, [{num_acceptors, Count} | TransOpt], ProtoOpt),
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


-spec create_client(woody:url(), woody_user_identity:id()) ->
    hg_client_api:t().

create_client(RootUrl, UserID) ->
    create_client_w_context(RootUrl, UserID, woody_context:new()).

-spec create_client(woody:url(), woody_user_identity:id(), woody:trace_id()) ->
    hg_client_api:t().

create_client(RootUrl, UserID, TraceID) ->
    create_client_w_context(RootUrl, UserID, woody_context:new(TraceID)).

create_client_w_context(RootUrl, UserID, WoodyCtx) ->
    hg_client_api:new(RootUrl, woody_user_identity:put(make_user_identity(UserID), WoodyCtx)).

make_user_identity(UserID) ->
    #{id => genlib:to_binary(UserID), realm => <<"external">>}.

%%

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/party_events.hrl").

-type party_id()                  :: dmsl_domain_thrift:'PartyID'().
-type user_info()                 :: dmsl_payment_processing_thrift:'UserInfo'().
-type account_id()                :: dmsl_domain_thrift:'AccountID'().
-type account()                   :: map().
-type balance()                   :: map().
-type contract_id()               :: dmsl_domain_thrift:'ContractID'().
-type contract_tpl()              :: dmsl_domain_thrift:'ContractTemplateRef'().
-type shop_id()                   :: dmsl_domain_thrift:'ShopID'().
-type cost()                      :: integer() | {integer(), binary()}.
-type category()                  :: dmsl_domain_thrift:'CategoryRef'().
-type cash()                      :: dmsl_domain_thrift:'Cash'().
-type invoice_tpl_id()            :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type invoice_params()            :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type invoice_params_tpl()        :: dmsl_payment_processing_thrift:'InvoiceWithTemplateParams'().
-type timestamp()                 :: integer().
-type context()                   :: dmsl_base_thrift:'Content'().
-type lifetime_interval()         :: dmsl_domain_thrift:'LifetimeInterval'().
-type invoice_details()           :: dmsl_domain_thrift:'InvoiceDetails'().
-type invoice_tpl_details()       :: dmsl_domain_thrift:'InvoiceTemplateDetails'().
-type invoice_tpl_cost()          :: dmsl_domain_thrift:'InvoiceTemplateCost'().
-type currency()                  :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type invoice_tpl_create_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateCreateParams'().
-type invoice_tpl_update_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateUpdateParams'().

-spec create_party_and_shop(
    category(),
    currency(),
    contract_tpl(),
    dmsl_domain_thrift:'PaymentInstitutionRef'(),
    Client :: pid()
) ->
    shop_id().

create_party_and_shop(Category, Currency, TemplateRef, PaymentInstitutionRef, Client) ->
    _ = hg_client_party:create(make_party_params(), Client),
    #domain_Party{} = hg_client_party:get(Client),
    create_battle_ready_shop(Category, Currency, TemplateRef, PaymentInstitutionRef, Client).

make_party_params() ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = <<?MODULE_STRING>>
        }
    }.

-spec create_battle_ready_shop(
    category(),
    currency(),
    contract_tpl(),
    dmsl_domain_thrift:'PaymentInstitutionRef'(),
    Client :: pid()
) ->
    shop_id().

create_battle_ready_shop(Category, Currency, TemplateRef, PaymentInstitutionRef, Client) ->
    ContractID = hg_utils:unique_id(),
    ContractParams = make_battle_ready_contract_params(TemplateRef, PaymentInstitutionRef),
    PayoutToolID = hg_utils:unique_id(),
    PayoutToolParams = make_battle_ready_payout_tool_params(),
    ShopID = hg_utils:unique_id(),
    ShopParams = #payproc_ShopParams{
        category = Category,
        location = {url, <<>>},
        details = make_shop_details(<<"Battle Ready Shop">>),
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    },
    ShopAccountParams = #payproc_ShopAccountParams{currency = ?cur(Currency)},
    Changeset = [
        {contract_modification, #payproc_ContractModificationUnit{
            id           = ContractID,
            modification = {creation, ContractParams}
        }},
        {contract_modification, #payproc_ContractModificationUnit{
            id           = ContractID,
            modification = {payout_tool_modification, #payproc_PayoutToolModificationUnit{
                payout_tool_id = PayoutToolID,
                modification   = {creation, PayoutToolParams}
            }}
        }},
        ?shop_modification(ShopID, {creation, ShopParams}),
        ?shop_modification(ShopID, {shop_account_creation, ShopAccountParams})
    ],
    ok = ensure_claim_accepted(hg_client_party:create_claim(Changeset, Client), Client),
    _Shop = hg_client_party:get_shop(ShopID, Client),
    ShopID.

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

-spec adjust_contract(contract_id(), contract_tpl(), Client :: pid()) -> ok.

adjust_contract(ContractID, TemplateRef, Client) ->
    ensure_claim_accepted(hg_client_party:create_claim([
        {contract_modification, #payproc_ContractModificationUnit{
            id           = ContractID,
            modification = {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
                adjustment_id = hg_utils:unique_id(),
                modification  = {creation, #payproc_ContractAdjustmentParams{
                    template = TemplateRef
                }}
            }}
        }}
    ], Client), Client).

ensure_claim_accepted(#payproc_Claim{id = ClaimID, revision = ClaimRevision, status = Status}, Client) ->
    case Status of
        {accepted, _} ->
            ok;
        _ ->
            ok = hg_client_party:accept_claim(ClaimID, ClaimRevision, Client)
    end.

-spec get_account(account_id()) -> account().

get_account(AccountID) ->
    % TODO we sure need to proxy this through the hellgate interfaces
    hg_accounting:get_account(AccountID).

-spec get_balance(account_id()) -> balance().

get_balance(AccountID) ->
    % TODO we sure need to proxy this through the hellgate interfaces
    hg_accounting:get_balance(AccountID).

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

-spec make_battle_ready_contract_params(
    dmsl_domain_thrift:'ContractTemplateRef'() | undefined,
    dmsl_domain_thrift:'PaymentInstitutionRef'()
) ->
    dmsl_payment_processing_thrift:'ContractParams'().

make_battle_ready_contract_params(TemplateRef, PaymentInstitutionRef) ->
    #payproc_ContractParams{
        contractor = make_battle_ready_contractor(),
        template = TemplateRef,
        payment_institution = PaymentInstitutionRef
    }.

-spec make_battle_ready_contractor() ->
    dmsl_payment_processing_thrift:'Contractor'().

make_battle_ready_contractor() ->
    BankAccount = #domain_RussianBankAccount{
        account = <<"4276300010908312893">>,
        bank_name = <<"SomeBank">>,
        bank_post_account = <<"123129876">>,
        bank_bik = <<"66642666">>
    },
    {legal_entity,
        {russian_legal_entity, #domain_RussianLegalEntity {
            registered_name = <<"Hoofs & Horns OJSC">>,
            registered_number = <<"1234509876">>,
            inn = <<"1213456789012">>,
            actual_address = <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,
            post_address = <<"NaN">>,
            representative_position = <<"Director">>,
            representative_full_name = <<"Someone">>,
            representative_document = <<"100$ banknote">>,
            russian_bank_account = BankAccount
        }}
    }.

-spec make_battle_ready_payout_tool_params() ->
    dmsl_payment_processing_thrift:'PayoutToolParams'().

make_battle_ready_payout_tool_params() ->
    #payproc_PayoutToolParams{
        currency = ?cur(<<"RUB">>),
        tool_info = {russian_bank_account, #domain_RussianBankAccount{
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

-spec make_invoice_tpl_create_params(party_id(), shop_id(), lifetime_interval(), binary(), invoice_tpl_details()) ->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details) ->
    make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details, make_invoice_context()).

-spec make_invoice_tpl_create_params(
    party_id(),
    shop_id(),
    lifetime_interval(),
    binary(),
    invoice_tpl_details(),
    context()
) ->
    invoice_tpl_create_params().

make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details, Context) ->
    #payproc_InvoiceTemplateCreateParams{
        party_id         = PartyID,
        shop_id          = ShopID,
        invoice_lifetime = Lifetime,
        product          = Product,
        details          = Details,
        context          = Context
    }.

-spec make_invoice_tpl_details(binary(), cost()) ->
    invoice_tpl_details().

make_invoice_tpl_details(Product, Price) ->
    {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Price,
        metadata = #{}
    }}.

-spec make_invoice_tpl_update_params(map()) -> invoice_tpl_update_params().

make_invoice_tpl_update_params(Diff) ->
    maps:fold(fun update_field/3, #payproc_InvoiceTemplateUpdateParams{}, Diff).

update_field(details, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{details = V};
update_field(invoice_lifetime, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{invoice_lifetime = V};
update_field(product, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{product = V};
update_field(description, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{description = V};
update_field(context, V, Params) ->
    Params#payproc_InvoiceTemplateUpdateParams{context = V}.

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

-spec make_disposable_payment_resource({dmsl_domain_thrift:'PaymentTool'(), dmsl_domain_thrift:'SessionID'()}) ->
    hg_domain_thrift:'DisposablePaymentResource'().

make_disposable_payment_resource({PaymentTool, SessionID}) ->
    #domain_DisposablePaymentResource{
        payment_tool = PaymentTool,
        payment_session_id = SessionID,
        client_info = #domain_ClientInfo{}
    }.

-spec make_meta_ns() -> dmsl_domain_thrift:'PartyMetaNamespace'().

make_meta_ns() ->
    list_to_binary(lists:concat(["NS-", erlang:system_time()])).

-spec make_meta_data() -> dmsl_domain_thrift:'PartyMetaData'().

make_meta_data() ->
    make_meta_data(<<"NS-0">>).

-spec make_meta_data(dmsl_domain_thrift:'PartyMetaNamespace'()) -> dmsl_domain_thrift:'PartyMetaData'().

make_meta_data(NS) ->
    {obj, #{
        {str, <<"NS">>} => {str, NS},
        {i, 42} => {str, <<"42">>},
        {str, <<"STRING!">>} => {arr, []}
    }}.

-spec get_hellgate_url() -> string().

get_hellgate_url() ->
    "http://" ++ ?HELLGATE_HOST ++ ":" ++ integer_to_list(?HELLGATE_PORT).

-spec make_customer_params(party_id(), shop_id(), binary()) -> dmsl_payment_processing_thrift:'CustomerParams'().

make_customer_params(PartyID, ShopID, EMail) ->
    #payproc_CustomerParams{
        party_id     = PartyID,
        shop_id      = ShopID,
        contact_info = ?contact_info(EMail),
        metadata     = ?null()
    }.

-spec make_customer_binding_params({dmsl_domain_thrift:'PaymentTool'(), dmsl_domain_thrift:'SessionID'()}) ->
    dmsl_payment_processing_thrift:'CustomerBindingParams'().

make_customer_binding_params(PaymentToolSession) ->
    #payproc_CustomerBindingParams{
        payment_resource = make_disposable_payment_resource(PaymentToolSession)
    }.

%%

make_due_date() ->
    make_due_date(24 * 60 * 60).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

%%

-spec make_trace_id(term()) -> woody:trace_id().

make_trace_id(Prefix) ->
    B = genlib:to_binary(Prefix),
    iolist_to_binary([binary:part(B, 0, min(byte_size(B), 20)), $., hg_utils:unique_id()]).
