-module(hg_recurrent_paytools_tests_SUITE).
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
-export([invalid_party_status/1]).
-export([invalid_shop_status/1]).
-export([invalid_payment_method/1]).

-export([get_recurrent_paytool/1]).
-export([recurrent_paytool_not_found/1]).
-export([recurrent_paytool_abandoned/1]).
-export([recurrent_paytool_acquirement_failed/1]).
-export([recurrent_paytool_acquired/1]).
-export([recurrent_paytool_w_tds_acquired/1]).

-export([recurrent_paytool_creation_not_permitted/1]).

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
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({woody_client, '_', '_'}, x),
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, scoper, dmt_client, hellgate, {cowboy, CowboySpec}]),
    ok = hg_domain:insert(construct_domain_fixture(construct_term_set_w_recurrent_paytools())),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(PartyClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    C1 = [
        {apps, Apps},
        {root_url, RootUrl},
        {party_client, PartyClient},
        {party_id, PartyID},
        {shop_id, ShopID},
        {test_sup, SupPid}
        | C
    ],
    ok = start_proxies([{hg_dummy_provider, 1, C1}, {hg_dummy_inspector, 2, C1}]),
    C1.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

-spec all() -> [test_case_name()].

all() ->
    [
        {group, invalid_recurrent_paytool_params},
        recurrent_paytool_not_found,
        get_recurrent_paytool,
        recurrent_paytool_acquirement_failed,
        recurrent_paytool_acquired,
        recurrent_paytool_w_tds_acquired,
        recurrent_paytool_abandoned,
        recurrent_paytool_creation_not_permitted
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {invalid_recurrent_paytool_params, [sequence], [
            invalid_user,
            invalid_party,
            invalid_shop,
            invalid_party_status,
            invalid_shop_status,
            invalid_payment_method
        ]}
    ].

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    RootUrl = cfg(root_url, C),
    PartyID = cfg(party_id, C),
    TraceID = hg_ct_helper:make_trace_id(Name),
    Client = hg_client_recurrent_paytool:start(hg_ct_helper:create_client(RootUrl, PartyID, TraceID)),
    [
        {test_case_name, genlib:to_binary(Name)},
        {trace_id, TraceID},
        {client, Client}
        | C
    ].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-include("hg_ct_domain.hrl").
-include("hg_ct_json.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/customer_events.hrl").
-include_lib("hellgate/include/recurrent_payment_tools.hrl").

-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

%% invalid_recurrent_paytool_params group

-spec invalid_user(config()) -> test_case_result().
-spec invalid_party(config()) -> test_case_result().
-spec invalid_shop(config()) -> test_case_result().
-spec invalid_party_status(config()) -> test_case_result().
-spec invalid_shop_status(config()) -> test_case_result().
-spec invalid_payment_method(config()) -> test_case_result().

invalid_user(C) ->
    Client = cfg(client, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    {exception, #payproc_InvalidUser{}} = hg_client_recurrent_paytool:create(Params, Client).

invalid_party(C) ->
    RootUrl = cfg(root_url, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Client = hg_client_recurrent_paytool:start(hg_ct_helper:create_client(RootUrl, PartyID, cfg(trace_id, C))),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    {exception, #payproc_PartyNotFound{}} = hg_client_recurrent_paytool:create(Params, Client).

invalid_shop(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = hg_utils:unique_id(),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    {exception, #payproc_ShopNotFound{}} = hg_client_recurrent_paytool:create(Params, Client).

invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    ok = hg_client_party:block(<<>>, PartyClient),
    {exception, ?invalid_party_status({blocking, _})} = hg_client_recurrent_paytool:create(Params, Client),
    ok = hg_client_party:unblock(<<>>, PartyClient),
    ok = hg_client_party:suspend(PartyClient),
    {exception, ?invalid_party_status({suspension, _})} = hg_client_recurrent_paytool:create(Params, Client),
    ok = hg_client_party:activate(PartyClient).

invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    ok = hg_client_party:block_shop(ShopID, <<>>, PartyClient),
    {exception, ?invalid_shop_status({blocking, _})} = hg_client_recurrent_paytool:create(Params, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<>>, PartyClient),
    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, ?invalid_shop_status({suspension, _})} = hg_client_recurrent_paytool:create(Params, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient).

invalid_payment_method(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    PaymentTool = {bank_card, #domain_BankCard{
        token          = <<"TOKEN">>,
        payment_system = mastercard,
        bin            = <<"666666">>,
        masked_pan     = <<"666">>
    }},
    PaymentResource = make_disposable_payment_resource(PaymentTool, <<"SESSION0">>),
    Params = #payproc_RecurrentPaymentToolParams{
        party_id = PartyID,
        shop_id = ShopID,
        payment_resource = PaymentResource
    },
    {exception, #payproc_InvalidPaymentMethod{}} = hg_client_recurrent_paytool:create(Params, Client).

%% recurrent_paytool_flow group

-spec recurrent_paytool_not_found(config()) -> test_case_result().
-spec get_recurrent_paytool(config()) -> test_case_result().
-spec recurrent_paytool_acquirement_failed(config()) -> test_case_result().
-spec recurrent_paytool_acquired(config()) -> test_case_result().
-spec recurrent_paytool_w_tds_acquired(config()) -> test_case_result().
-spec recurrent_paytool_abandoned(config()) -> test_case_result().

recurrent_paytool_not_found(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    _RecurrentPaytool = hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    {exception, #payproc_RecurrentPaymentToolNotFound{}} =
        hg_client_recurrent_paytool:get(hg_utils:unique_id(), Client).

get_recurrent_paytool(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    RecurrentPaytool = hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID} = RecurrentPaytool,
    RecurrentPaytool = hg_client_recurrent_paytool:get(RecurrentPaytoolID, Client).

recurrent_paytool_acquirement_failed(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_bad_recurrent_paytool_params(PartyID, ShopID),
    RecurrentPaytool = hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID} = RecurrentPaytool,
    [
        ?recurrent_payment_tool_has_created(_, _, _),
        ?session_ev(?session_started())
    ] = next_event(RecurrentPaytoolID, Client),
    ok = await_failure(RecurrentPaytoolID, Client).

recurrent_paytool_acquired(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    RecurrentPaytool = hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID} = RecurrentPaytool,
    ok = await_acquirement(RecurrentPaytoolID, Client).

recurrent_paytool_w_tds_acquired(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_tds_recurrent_paytool_params(PartyID, ShopID),
    RecurrentPaytool = hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID} = RecurrentPaytool,
    [
        ?recurrent_payment_tool_has_created(_, _, _),
        ?session_ev(?session_started())
    ] = next_event(RecurrentPaytoolID, Client),
    [
        ?session_ev(?interaction_requested(UserInteraction))
    ] = next_event(RecurrentPaytoolID, Client),

    {URL, GoodForm} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, GoodForm}),
    ok = await_acquirement_finish(RecurrentPaytoolID, Client).

recurrent_paytool_abandoned(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID} =
        hg_client_recurrent_paytool:create(Params, cfg(client, C)),
    {exception, #payproc_InvalidRecurrentPaymentToolStatus{status = {created, _}}} =
        hg_client_recurrent_paytool:abandon(RecurrentPaytoolID, Client),
    ok = await_acquirement(RecurrentPaytoolID, Client),
    #payproc_RecurrentPaymentTool{id = RecurrentPaytoolID, status = {abandoned, _}} =
        hg_client_recurrent_paytool:abandon(RecurrentPaytoolID, Client),
    [
        ?recurrent_payment_tool_has_abandoned()
    ] = next_event(RecurrentPaytoolID, Client).

%%

-spec recurrent_paytool_creation_not_permitted(config()) -> test_case_result().

recurrent_paytool_creation_not_permitted(C) ->
    ok = hg_domain:upsert(construct_domain_fixture(construct_simple_term_set())),
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = make_recurrent_paytool_params(PartyID, ShopID),
    {exception, #payproc_OperationNotPermitted{}} = hg_client_recurrent_paytool:create(Params, Client).

%%

make_bad_recurrent_paytool_params(PartyID, ShopID) ->
    {PaymentTool, Session} = hg_ct_helper:make_bad_payment_tool(),
    PaymentResource = make_disposable_payment_resource(PaymentTool, Session),
    #payproc_RecurrentPaymentToolParams{
        party_id = PartyID,
        shop_id = ShopID,
        payment_resource = PaymentResource
    }.

make_recurrent_paytool_params(PartyID, ShopID) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    PaymentResource = make_disposable_payment_resource(PaymentTool, Session),
    #payproc_RecurrentPaymentToolParams{
        party_id = PartyID,
        shop_id = ShopID,
        payment_resource = PaymentResource
    }.

make_tds_recurrent_paytool_params(PartyID, ShopID) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(preauth_3ds),
    PaymentResource = make_disposable_payment_resource(PaymentTool, Session),
    #payproc_RecurrentPaymentToolParams{
        party_id = PartyID,
        shop_id = ShopID,
        payment_resource = PaymentResource
    }.

make_disposable_payment_resource(PaymentTool, Session) ->
    #domain_DisposablePaymentResource{
        payment_tool = PaymentTool,
        payment_session_id = Session,
        client_info = #domain_ClientInfo{}
    }.

%%

start_proxies(Proxies) ->
    setup_proxies(lists:map(
        fun
            Mapper({Module, ProxyID, Context}) ->
                Mapper({Module, ProxyID, #{}, Context});
            Mapper({Module, ProxyID, ProxyOpts, Context}) ->
                construct_proxy(ProxyID, start_service_handler(Module, Context, #{}), ProxyOpts)
        end,
        Proxies
    )).

setup_proxies(Proxies) ->
    ok = hg_domain:upsert(Proxies).

start_service_handler(Module, C, HandlerOpts) ->
    start_service_handler(Module, Module, C, HandlerOpts).

start_service_handler(Name, Module, C, HandlerOpts) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = maps:merge(HandlerOpts, #{hellgate_root_url => cfg(root_url, C)}),
    ChildSpec = hg_test_proxy:get_child_spec(Name, Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(cfg(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = ?prx(ID),
        data = #domain_ProxyDefinition{
            name              = Url,
            description       = Url,
            url               = Url,
            options           = Options
        }
    }}.

%%

await_acquirement(RecurrentPaytoolID, Client) ->
    [
        ?recurrent_payment_tool_has_created(_, _, _),
        ?session_ev(?session_started())
    ] = next_event(RecurrentPaytoolID, Client),
    await_acquirement_finish(RecurrentPaytoolID, Client).

await_acquirement_finish(RecurrentPaytoolID, Client) ->
    [
        ?session_ev(?trx_bound(?trx_info(_))),
        ?session_ev(?session_finished(?session_succeeded())),
        ?recurrent_payment_tool_has_acquired(_)
    ] = next_event(RecurrentPaytoolID, Client),
    ok.

await_failure(RecurrentPaytoolID, Client) ->
    [
        ?session_ev(?session_finished(?session_failed(_))),
        ?recurrent_payment_tool_has_failed(_)
    ] = next_event(RecurrentPaytoolID, Client),
    ok.

next_event(RecurrentPaytoolID, Client) ->
    next_event(RecurrentPaytoolID, 5000, Client).

next_event(RecurrentPaytoolID, Timeout, Client) ->
    {ok, Changes} = hg_client_recurrent_paytool:pull_event(RecurrentPaytoolID, Timeout, Client),
    case filter_changes(Changes) of
        L when length(L) > 0 ->
            L;
        [] ->
            next_event(RecurrentPaytoolID, Timeout, Client)
    end.

filter_changes(Changes) ->
    lists:filtermap(fun filter_change/1, Changes).

filter_change(?session_ev(?proxy_st_changed(_))) ->
    false;
filter_change(?session_ev(?session_suspended())) ->
    false;
filter_change(?session_ev(?session_activated())) ->
    false;
filter_change(_) ->
    true.

%%

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

% assert_failed_post_request(Req) ->
%     {ok, 500, _RespHeaders, _ClientRef} = post_request(Req).

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form}.

%%

-spec construct_term_set_w_recurrent_paytools() -> term().

construct_term_set_w_recurrent_paytools() ->
    TermSet = construct_simple_term_set(),
    TermSet#domain_TermSet{recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa)
            ])}
        }
    }.

-spec construct_simple_term_set() -> term().

construct_simple_term_set() ->
    #domain_TermSet{
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
                        lower = {inclusive, ?cash(     1000, <<"RUB">>)},
                        upper = {exclusive, ?cash(420000000, <<"RUB">>)}
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
    }.

-spec construct_domain_fixture(term()) -> [hg_domain:object()].

construct_domain_fixture(TermSet) ->
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
                terminal = {value, [?trm(1)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
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
                    ]}
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                risk_coverage = high
            }
        }}
    ].
