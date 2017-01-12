-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).
-export([start_apps/1]).

-export([create_party_and_shop/1]).

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
-export([construct_context/0]).

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
    % FIXME ASAP! tests fail due to lag between docker start and service in container start
    timer:sleep(5000),
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
    #domain_Party{shops = Shops} = hg_client_party:get(Client),
    [{ShopID, _Shop}] = maps:to_list(Shops),
    ShopID.

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
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(trmacc(Cur, Stl),
    #domain_TerminalAccount{currency = ?cur(Cur), settlement = Stl}).

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
    Context = construct_context(),
    hg_context:set(Context),
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
    [
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, [?prv(1), ?prv(2)]},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(1),
                inspector = ?insp(1)
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
                description = <<"Wold famous inspector Kovalsky at your service!">>,
                proxy = #domain_Proxy{
                    ref = ?prx(2),
                    additional = #{}
                }
            }
        }},
        {contract_template, #domain_ContractTemplateObject{
            ref = ?tmpl(1),
            data = #domain_ContractTemplate{
                parent_template = undefined,
                valid_since = undefined,
                valid_until = undefined,
                terms = #domain_Terms{
                    payments = #domain_PaymentsServiceTerms{
                        payment_methods = {value, ordsets:from_list([
                            ?pmt(bank_card, visa),
                            ?pmt(bank_card, mastercard)
                        ])},
                        cash_limit = {decisions, [
                            #domain_CashLimitDecision{
                                if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                then_ = {value, #domain_CashLimit{
                                    min = {inclusive, ?cash(1000, ?cur(<<"RUB">>))},
                                    max = {exclusive, ?cash(4200000, ?cur(<<"RUB">>))}
                                }}
                            },
                            #domain_CashLimitDecision{
                                if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                                then_ = {value, #domain_CashLimit{
                                    min = {inclusive, ?cash(200, ?cur(<<"USD">>))},
                                    max = {exclusive, ?cash(313370, ?cur(<<"USD">>))}
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
                }
            }
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
                }
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
                }
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
                url     = <<>>,
                options = #{}
            }
        }}
    ].

-spec construct_context() -> term().

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, hg_client_api).
