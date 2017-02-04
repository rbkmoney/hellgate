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

-export([make_invoice_params/4]).
-export([make_invoice_params/5]).

-export([make_category_ref/1]).
-export([make_shop_details/1]).
-export([make_shop_details/2]).

-export([bank_card_tds_token/0]).
-export([bank_card_simple_token/0]).
-export([make_tds_payment_tool/0]).
-export([make_simple_payment_tool/0]).
-export([get_hellgate_url/0]).

-export([construct_domain_fixture/0]).

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
            {lager_common_test_backend, info}
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

create_contract(#payproc_ContractParams{} = Params, Client) ->
    #payproc_ClaimResult{id = ClaimID} = hg_client_party:create_contract(Params, Client),
    #payproc_Claim{changeset = Changeset} = hg_client_party:get_claim(ClaimID, Client),
    {_, #domain_Contract{id = ContractID}} = lists:keyfind(contract_creation, 1, Changeset),
    ok = hg_client_party:accept_claim(ClaimID, Client),
    ContractID.

-spec create_shop(contract_id(), category(), binary(), Client :: pid()) ->
    shop_id().

create_shop(ContractID, Category, Name, Client) ->
    create_shop(ContractID, Category, Name, undefined, Client).

-spec create_shop(contract_id(), category(), binary(), binary(), Client :: pid()) ->
    shop_id().

create_shop(ContractID, Category, Name, Description, Client) ->
    PayoutToolID = hg_ct_helper:get_first_payout_tool_id(ContractID, Client),
    Params = #payproc_ShopParams{
        contract_id       = ContractID,
        payout_tool_id    = PayoutToolID,
        category          = Category,
        details           = make_shop_details(Name, Description)
    },
    #payproc_ClaimResult{id = ClaimID} = hg_client_party:create_shop(Params, Client),
    #payproc_Claim{changeset = Changeset} = hg_client_party:get_claim(ClaimID, Client),
    {_, #domain_Shop{id = ShopID}} = lists:keyfind(shop_creation, 1, Changeset),
    ok = hg_client_party:accept_claim(ClaimID, Client),
    #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, Client),
    ok = flush_events(Client),
    ShopID.

-spec set_shop_proxy(shop_id(), proxy_ref(), proxy_options(), Client :: pid()) ->
    ok.

set_shop_proxy(ShopID, ProxyRef, ProxyOptions, Client) ->
    Proxy = #domain_Proxy{ref = ProxyRef, additional = ProxyOptions},
    Update = #payproc_ShopUpdate{proxy = Proxy},
    #payproc_ClaimResult{status = ?accepted(_)} = hg_client_party:update_shop(ShopID, Update, Client),
    ok = flush_events(Client),
    ok.

flush_events(Client) ->
    case hg_client_party:pull_event(500, Client) of
        timeout ->
            ok;
        _Event ->
            flush_events(Client)
    end.

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
                    contractor = #domain_Contractor{},
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
    BankAccount = #domain_BankAccount{
        account = <<"4276300010908312893">>,
        bank_name = <<"SomeBank">>,
        bank_post_account = <<"123129876">>,
        bank_bik = <<"66642666">>
    },
    Contractor = #domain_Contractor{
        entity = {russian_legal_entity, #domain_RussianLegalEntity {
            registered_name = <<"Hoofs & Horns OJSC">>,
            registered_number = <<"1234509876">>,
            inn = <<"1213456789012">>,
            actual_address = <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,
            post_address = <<"NaN">>,
            representative_position = <<"Director">>,
            representative_full_name = <<"Someone">>,
            representative_document = <<"100$ banknote">>
        }},
        bank_account = BankAccount
    },
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
        tool_info = {bank_account, BankAccount}
    },
    #payproc_ContractParams{
        contractor = Contractor,
        payout_tool_params = PayoutToolParams
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
            currency = #domain_CurrencyRef{symbolic_code = Currency}
        },
        context  = #'Content'{
            type = <<"application/octet-stream">>,
            data = <<"some_merchant_specific_data">>
        }
    }.

-spec make_category_ref(dmsl_domain_thrift:'ObjectID'()) ->
    category().

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

%%

-spec construct_domain_fixture() -> [hg_domain:object()].

-include_lib("hellgate/include/domain.hrl").

-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).

-define(trmacc(Cur, Stl),
    #domain_TerminalAccount{currency = ?cur(Cur), settlement = Stl}).
-define(partycond(ID, Def),
    {condition, {party, #domain_PartyCondition{id = ID, definition = Def}}}).

-define(cfpost(A1, A2, V),
    #domain_CashFlowPosting{
        source      = A1,
        destination = A2,
        volume      = V
    }
).

-define(fixed(A),
    {fixed, #domain_CashVolumeFixed{amount = A}}).
-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C}}).

construct_domain_fixture() ->
    _ = hg_context:set(woody_context:new()),
    Accounts = lists:foldl(
        fun ({N, CurrencyCode}, M) ->
            AccountID = hg_accounting:create_account(CurrencyCode),
            M#{N => AccountID}
        end,
        #{},
        [
            {system_settlement       , <<"RUB">>},
            {external_income         , <<"RUB">>},
            {external_outcome        , <<"RUB">>},
            {terminal_1_settlement   , <<"USD">>},
            {terminal_2_settlement   , <<"RUB">>},
            {terminal_3_settlement   , <<"RUB">>}
        ]
    ),
    hg_context:cleanup(),
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(<<"DEPRIVED ONE">>, {shop_is, 1}),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(     1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(420000000, ?cur(<<"RUB">>))}
                    }}
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(      200, ?cur(<<"USD">>))},
                        upper = {exclusive, ?cash(   313370, ?cur(<<"USD">>))}
                    }}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, payment_amount)
                        )
                    ]}
                }
            ]}
        }
    },
    [
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, [?prv(1), ?prv(2)]},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(1),
                common_merchant_proxy = ?prx(3),
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, #domain_CashRange{
                                    lower = {inclusive, ?cash(        0, ?cur(<<"RUB">>))},
                                    upper = {exclusive, ?cash(   500000, ?cur(<<"RUB">>))}
                                }}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, #domain_CashRange{
                                    lower = {inclusive, ?cash(   500000, ?cur(<<"RUB">>))},
                                    upper = {exclusive, ?cash(100000000, ?cur(<<"RUB">>))}
                                }}},
                                then_ = {value, ?insp(2)}
                            }
                        ]}
                    }
                ]}
            }
        }},
        {system_account_set, #domain_SystemAccountSetObject{
            ref = ?sas(1),
            data = #domain_SystemAccountSet{
                name = <<"Primaries">>,
                description = <<"Primaries">>,
                accounts = #{
                    ?cur(<<"RUB">>) => #domain_SystemAccount{
                        settlement = maps:get(system_settlement, Accounts)
                    }
                }
            }
        }},
        {external_account_set, #domain_ExternalAccountSetObject{
            ref = ?eas(1),
            data = #domain_ExternalAccountSet{
                name = <<"Primaries">>,
                description = <<"Primaries">>,
                accounts = #{
                    ?cur(<<"RUB">>) => #domain_ExternalAccount{
                        income  = maps:get(external_income , Accounts),
                        outcome = maps:get(external_outcome, Accounts)
                    }
                }
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = #domain_PartyPrototypeRef{id = 42},
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    }
                },
                %% FIXME create test template with test categories only
                test_contract_template = ?tmpl(1)
            }
        }},
        {inspector, #domain_InspectorObject{
            ref = #domain_InspectorRef{id = 1},
            data = #domain_Inspector{
                name = <<"Kovalsky">>,
                description = <<"World famous inspector Kovalsky at your service!">>,
                proxy = #domain_Proxy{
                    ref = ?prx(2),
                    additional = #{<<"risk_score">> => <<"low">>}
                }
            }
        }},
        {inspector, #domain_InspectorObject{
            ref = #domain_InspectorRef{id = 2},
            data = #domain_Inspector{
                name = <<"Skipper">>,
                description = <<"World famous inspector Skipper at your service!">>,
                proxy = #domain_Proxy{
                    ref = ?prx(2),
                    additional = #{<<"risk_score">> => <<"high">>}
                }
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }},
        {contract_template, #domain_ContractTemplateObject{
            ref = ?tmpl(1),
            data = #domain_ContractTemplate{terms = ?trms(1)}
        }},
        {currency, #domain_CurrencyObject{
            ref = ?cur(<<"RUB">>),
            data = #domain_Currency{
                name = <<"Russian rubles">>,
                numeric_code = 643,
                symbolic_code = <<"RUB">>,
                exponent = 2
            }
        }},
        {currency, #domain_CurrencyObject{
            ref = ?cur(<<"USD">>),
            data = #domain_Currency{
                name = <<"US Dollars">>,
                numeric_code = 840,
                symbolic_code = <<"USD">>,
                exponent = 2
            }
        }},
        {category, #domain_CategoryObject{
            ref = ?cat(1),
            data = #domain_Category{
                name = <<"Categories">>,
                description = <<"Goods sold by category providers">>
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1), ?trm(2), ?trm(3), ?trm(4)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"USD">>,
                    maps:get(terminal_1_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 1">>
                },
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(2),
            data = #domain_Terminal{
                name = <<"Brominal 2">>,
                description = <<"Brominal 2">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"USD">>,
                    maps:get(terminal_1_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 2">>
                },
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(3),
            data = #domain_Terminal{
                name = <<"Brominal 3">>,
                description = <<"Brominal 3">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_2_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 3">>
                },
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(4),
            data = #domain_Terminal{
                name = <<"Brominal 4">>,
                description = <<"Brominal 4">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_2_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Brominal 4">>
                },
                risk_coverage = low
            }
        }},
        {provider, #domain_ProviderObject{
            ref = #domain_ProviderRef{id = 2},
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?trm(5), ?trm(6)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(5),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Drominal 1">>
                },
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    )
                ],
                account = ?trmacc(
                    <<"RUB">>,
                    maps:get(terminal_3_settlement, Accounts)
                ),
                options = #{
                    <<"override">> => <<"Drominal 1">>
                },
                risk_coverage = low
            }
        }},
        {payment_method, #domain_PaymentMethodObject{
            ref = ?pmt(bank_card, visa),
            data = #domain_PaymentMethodDefinition{
                name = <<"Visa bank card">>,
                description = <<"Visa is a major brand of cards issued by Visa">>
            }
        }},
        {payment_method, #domain_PaymentMethodObject{
            ref =  ?pmt(bank_card, mastercard),
            data = #domain_PaymentMethodDefinition{
                name = <<"Mastercard bank card">>,
                description = <<"For everything else, there's MasterCard.">>
            }
        }},
        {proxy, #domain_ProxyObject{
            ref = ?prx(1),
            data = #domain_ProxyDefinition{
                name        = <<"Dummy proxy">>,
                description = <<"Dummy proxy, what else to say">>,
                url         = <<>>,
                options     = #{}
            }
        }},
        {proxy, #domain_ProxyObject{
            ref = ?prx(2),
            data = #domain_ProxyDefinition{
                name        = <<"Inspector proxy">>,
                description = <<"Inspector proxy that hates your mom">>,
                url         = <<>>,
                options     = #{}
            }
        }},
        {proxy, #domain_ProxyObject{
            ref = ?prx(3),
            data = #domain_ProxyDefinition{
                name        = <<"Merchant proxy">>,
                description = <<"Merchant proxy that noone cares about">>,
                url         = <<>>,
                options     = #{}
            }
        }}
    ].
