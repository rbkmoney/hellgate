-module(hg_customer_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([all/0]).
-export([groups/0]).

-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invalid_user/1]).
-export([invalid_party/1]).
-export([invalid_shop/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%

-type config()           :: hg_ct_helper:config().
-type test_case_name()   :: hg_ct_helper:test_case_name().
-type group_name()       :: hg_ct_helper:group_name().
-type test_case_result() :: _ | no_return().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, dmt_client, hellgate, {cowboy, CowboySpec}]),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(PartyClient),
    [
        {apps, Apps},
        {root_url, RootUrl},
        {party_client, PartyClient},
        {party_id, PartyID},
        {shop_id, ShopID}
        | C
    ].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

-spec all() -> [test_case_name()].

all() ->
    [
        {group, invalid_customer_params}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {invalid_customer_params, [sequence], [
            invalid_user,
            invalid_party,
            invalid_shop
        ]}
    ].

%%

-spec invalid_user(config()) -> test_case_result().
-spec invalid_party(config()) -> test_case_result().
-spec invalid_shop(config()) -> test_case_result().

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    RootUrl = cfg(root_url, C),
    PartyID = cfg(party_id, C),
    TraceID = make_trace_id(Name),
    Client = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID, TraceID)),
    [
        {test_case_name, genlib:to_binary(Name)},
        {trace_id, TraceID},
        {client, Client}
        | C
    ].

make_trace_id(Prefix) ->
    iolist_to_binary([genlib:to_binary(Prefix), $., hg_utils:unique_id()]).

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-include("hg_ct_domain.hrl").
-include("hg_ct_json.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

invalid_user(C) ->
    Client = cfg(client, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Params = make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_InvalidUser{}} = hg_client_customer:create(Params, Client).

invalid_party(C) ->
    RootUrl = cfg(root_url, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Client = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID, cfg(trace_id, C))),
    Params = make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_PartyNotFound{}} = hg_client_customer:create(Params, Client).

invalid_shop(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = hg_utils:unique_id(),
    Params = make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_ShopNotFound{}} = hg_client_customer:create(Params, Client).

make_customer_params(PartyID, ShopID, EMail) ->
    make_customer_params(PartyID, ShopID, ?contact_info(EMail), ?null()).

make_customer_params(PartyID, ShopID, ContactInfo, Metadata) ->
    #payproc_CustomerParams{
        party_id     = PartyID,
        shop_id      = ShopID,
        contact_info = ContactInfo,
        metadata     = Metadata
    }.

%%

-spec construct_domain_fixture() -> [hg_domain:object()].

construct_domain_fixture() ->
    AccountRUB = hg_ct_fixture:construct_terminal_account(?cur(<<"RUB">>)),
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(1)
            ])},
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(     1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(420000000, ?cur(<<"RUB">>))}
                    }}
                }
            ]},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(45, 1000, payment_amount)
                )
            ]}
        }
    },
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = ?partyproto(1),
                providers = {value, ordsets:from_list([
                    ?prv(1)
                ])},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(1),
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, ?insp(1)}
                    }
                ]}
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = ?partyproto(1),
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    shop_id = <<"TESTSHOP">>,
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    },
                    location = {url, <<"">>}
                },
                contract = #domain_ContractPrototype{
                    contract_id = <<"TESTCONTRACT">>,
                    test_contract_template = ?tmpl(1),
                    payout_tool = #domain_PayoutToolPrototype{
                        payout_tool_id = <<"TESTPAYOUTTOOL">>,
                        payout_tool_info = {bank_account, #domain_BankAccount{
                            account = <<"">>,
                            bank_name = <<"">>,
                            bank_post_account = <<"">>,
                            bank_bik = <<"">>
                        }},
                        payout_tool_currency = ?cur(<<"RUB">>)
                    }
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
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1), ?trm(2)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{}
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
                account = AccountRUB,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(2),
            data = #domain_Terminal{
                name = <<"Brominal 2">>,
                description = <<"Brominal 2">>,
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
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = AccountRUB,
                risk_coverage = low
            }
        }}
    ].
