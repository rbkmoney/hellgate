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
-export([invalid_party_status/1]).
-export([invalid_shop_status/1]).

-export([create_customer/1]).
-export([delete_customer/1]).
-export([start_binding_w_failure/1]).
-export([start_binding/1]).
-export([start_binding_w_tds/1]).
-export([start_two_bindings/1]).
-export([start_two_bindings_w_tds/1]).

-export([create_customer_not_permitted/1]).
-export([start_binding_not_permitted/1]).

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
        {group, invalid_customer_params},
        {group, basic_customer_methods},
        {group, not_permitted_methods}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {invalid_customer_params, [sequence], [
            invalid_user,
            invalid_party,
            invalid_shop,
            invalid_party_status,
            invalid_shop_status
        ]},
        {basic_customer_methods, [sequence], [
            create_customer,
            delete_customer,
            start_binding_w_failure,
            start_binding,
            start_binding_w_tds,
            start_two_bindings,
            start_two_bindings_w_tds
        ]},
        {not_permitted_methods, [sequence], [
            create_customer_not_permitted,
            start_binding_not_permitted
        ]}
    ].

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    RootUrl = cfg(root_url, C),
    PartyID = cfg(party_id, C),
    TraceID = hg_ct_helper:make_trace_id(Name),
    Client = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID, TraceID)),
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

%%

-spec invalid_user(config()) -> test_case_result().
-spec invalid_party(config()) -> test_case_result().
-spec invalid_shop(config()) -> test_case_result().
-spec invalid_party_status(config()) -> test_case_result().
-spec invalid_shop_status(config()) -> test_case_result().

invalid_user(C) ->
    Client = cfg(client, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Params = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_InvalidUser{}} = hg_client_customer:create(Params, Client).

invalid_party(C) ->
    RootUrl = cfg(root_url, C),
    PartyID = hg_utils:unique_id(),
    ShopID = hg_utils:unique_id(),
    Client = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID, cfg(trace_id, C))),
    Params = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_PartyNotFound{}} = hg_client_customer:create(Params, Client).

invalid_shop(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = hg_utils:unique_id(),
    Params = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_ShopNotFound{}} = hg_client_customer:create(Params, Client).

invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    ok = hg_client_party:block(<<>>, PartyClient),
    {exception, ?invalid_party_status({blocking, _})} = hg_client_customer:create(Params, Client),
    ok = hg_client_party:unblock(<<>>, PartyClient),
    ok = hg_client_party:suspend(PartyClient),
    {exception, ?invalid_party_status({suspension, _})} = hg_client_customer:create(Params, Client),
    ok = hg_client_party:activate(PartyClient).

invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    Params = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    ok = hg_client_party:block_shop(ShopID, <<>>, PartyClient),
    {exception, ?invalid_shop_status({blocking, _})} = hg_client_customer:create(Params, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<>>, PartyClient),
    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, ?invalid_shop_status({suspension, _})} = hg_client_customer:create(Params, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient).

%%

-spec create_customer(config()) -> test_case_result().
-spec delete_customer(config()) -> test_case_result().
-spec start_binding_w_failure(config()) -> test_case_result().
-spec start_binding(config()) -> test_case_result().
-spec start_binding_w_tds(config()) -> test_case_result().
-spec start_two_bindings(config()) -> test_case_result().
-spec start_two_bindings_w_tds(config()) -> test_case_result().

create_customer(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    Customer = hg_client_customer:create(CustomerParams, Client),
    #payproc_Customer{id = CustomerID} = Customer,
    Customer = hg_client_customer:get(CustomerID, Client).

delete_customer(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    Customer = hg_client_customer:create(CustomerParams, Client),
    #payproc_Customer{id = CustomerID} = Customer,
    ok = hg_client_customer:delete(CustomerID, Client),
    {exception, #'payproc_CustomerNotFound'{}} = hg_client_customer:get(CustomerID, Client).

start_binding_w_failure(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    Customer = hg_client_customer:create(CustomerParams, Client),
    #payproc_Customer{id = CustomerID} = Customer,
    {PaymentTool, Session} = hg_ct_helper:make_bad_payment_tool(),
    CustomerBindingParams = #payproc_CustomerBindingParams{
        payment_resource = make_disposable_payment_resource(PaymentTool, Session)
    },
    CustomerBinding = hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    Customer1 = hg_client_customer:get(CustomerID, Client),
    #payproc_Customer{id = CustomerID, bindings = Bindings} = Customer1,
    Bindings = [CustomerBinding],
    [
        ?customer_created(_)
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_started(_))
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_status_changed(?customer_binding_failed(_)))
    ] = next_event(CustomerID, Client).

start_binding(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    Customer = hg_client_customer:create(CustomerParams, Client),
    #payproc_Customer{id = CustomerID} = Customer,
    CustomerBindingParams = hg_ct_helper:make_customer_binding_params(),
    CustomerBinding = hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    Customer1 = hg_client_customer:get(CustomerID, Client),
    #payproc_Customer{id = CustomerID, bindings = Bindings} = Customer1,
    Bindings = [CustomerBinding],
    [
        ?customer_created(_)
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_started(_))
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_status_changed(?customer_binding_succeeded())),
        ?customer_status_changed(?customer_ready())
    ] = next_event(CustomerID, Client).

start_binding_w_tds(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    Customer = hg_client_customer:create(CustomerParams, Client),
    #payproc_Customer{id = CustomerID} = Customer,
    {PaymentTool, Session} = hg_ct_helper:make_tds_payment_tool(),
    CustomerBindingParams = #payproc_CustomerBindingParams{
        payment_resource = make_disposable_payment_resource(PaymentTool, Session)
    },
    CustomerBinding = hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    Customer1 = hg_client_customer:get(CustomerID, Client),
    #payproc_Customer{id = CustomerID, bindings = Bindings} = Customer1,
    Bindings = [CustomerBinding],
    [
        ?customer_created(_)
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_started(_))
    ] = next_event(CustomerID, Client),
    [
        ?customer_binding_changed(_, ?customer_binding_interaction_requested(UserInteraction))
    ] = next_event(CustomerID, Client),
    {URL, GoodForm} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, GoodForm}),
    [
        ?customer_binding_changed(_, ?customer_binding_status_changed(?customer_binding_succeeded())),
        ?customer_status_changed(?customer_ready())
    ] = next_event(CustomerID, Client).

start_two_bindings(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    #payproc_Customer{id = CustomerID} = hg_client_customer:create(CustomerParams, Client),
    CustomerBindingParams = hg_ct_helper:make_customer_binding_params(),
    CustomerBinding1 = #payproc_CustomerBinding{id = CustomerBindingID1} =
        hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    CustomerBinding2 = #payproc_CustomerBinding{id = CustomerBindingID2} =
        hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    #payproc_Customer{id = CustomerID, bindings = Bindings} = hg_client_customer:get(CustomerID, Client),
    true = sets:from_list(Bindings) =:= sets:from_list([CustomerBinding1, CustomerBinding2]),
    [
        ?customer_created(_Customer)
    ] = next_event(CustomerID, Client),
    StartChanges = [
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_started(CustomerBinding1)),
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_started(CustomerBinding2))
    ],
    _ = await_for_changes(StartChanges, CustomerID, Client),
    SuccessChanges = [
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_status_changed(?customer_binding_succeeded())),
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_status_changed(?customer_binding_succeeded())),
        ?customer_status_changed(?customer_ready())
    ],
    _ = await_for_changes(SuccessChanges, CustomerID, Client, 10000).

start_two_bindings_w_tds(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    #payproc_Customer{id = CustomerID} = hg_client_customer:create(CustomerParams, Client),
    {PaymentTool, Session} = hg_ct_helper:make_tds_payment_tool(),
    CustomerBindingParams = #payproc_CustomerBindingParams{
        payment_resource = make_disposable_payment_resource(PaymentTool, Session)
    },
    CustomerBinding1 = #payproc_CustomerBinding{id = CustomerBindingID1} =
        hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    CustomerBinding2 = #payproc_CustomerBinding{id = CustomerBindingID2} =
        hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client),
    #payproc_Customer{id = CustomerID, bindings = Bindings} = hg_client_customer:get(CustomerID, Client),
    true = sets:from_list(Bindings) =:= sets:from_list([CustomerBinding1, CustomerBinding2]),
    [
        ?customer_created(_Customer)
    ] = next_event(CustomerID, Client),
    StartChanges = [
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_started(CustomerBinding1)),
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_started(CustomerBinding2)),
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_interaction_requested('_')),
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_interaction_requested('_'))
    ],
    [
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_started(CustomerBinding1)),
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_started(CustomerBinding2)),
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_interaction_requested(UserInteraction1)),
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_interaction_requested(UserInteraction2))
    ] = await_for_changes(StartChanges, CustomerID, Client),
    _ = assert_success_post_request(get_post_request(UserInteraction1)),
    [
        ?customer_binding_changed(CustomerBindingID1, ?customer_binding_status_changed(?customer_binding_succeeded())),
        ?customer_status_changed(?customer_ready())
    ] = next_event(CustomerID, Client),
    _ = assert_success_post_request(get_post_request(UserInteraction2)),
    [
        ?customer_binding_changed(CustomerBindingID2, ?customer_binding_status_changed(?customer_binding_succeeded()))
    ] = next_event(CustomerID, Client).

%%

-spec create_customer_not_permitted(config()) -> test_case_result().
-spec start_binding_not_permitted(config()) -> test_case_result().

create_customer_not_permitted(C) ->
    ok = hg_domain:upsert(construct_domain_fixture(construct_simple_term_set())),
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    {exception, #payproc_OperationNotPermitted{}} = hg_client_customer:create(CustomerParams, Client).

start_binding_not_permitted(C) ->
    ok = hg_domain:upsert(construct_domain_fixture(construct_term_set_w_recurrent_paytools())),
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, cfg(test_case_name, C)),
    #payproc_Customer{id = CustomerID} = hg_client_customer:create(CustomerParams, Client),
    ok = hg_domain:upsert(construct_domain_fixture(construct_simple_term_set())),
    CustomerBindingParams = hg_ct_helper:make_customer_binding_params(),
    {exception, #payproc_OperationNotPermitted{}} =
        hg_client_customer:start_binding(CustomerID, CustomerBindingParams, Client).

%%

-define(INTERVAL, 100).
-define(DEFAULT_TIMEOUT, 5000).

await_for_changes(ChangeMatchPatterns, CustomerID, Client) ->
    await_for_changes(ChangeMatchPatterns, CustomerID, Client, ?DEFAULT_TIMEOUT).

await_for_changes(ChangeMatchPatterns, CustomerID, Client, Timeout) ->
    MatchSpecs = [ets:match_spec_compile([{MP, [], ['$_']}]) || MP <- ChangeMatchPatterns],
    MatchSpecs1 = lists:zip(lists:seq(1, length(MatchSpecs)), MatchSpecs),
    Matched = await_for_changes(MatchSpecs1, CustomerID, Client, [], Timeout),
    {_, Result} = lists:unzip(lists:keysort(1, Matched)),
    Result.

await_for_changes(MatchSpecs, CustomerID, Client, Acc, TimeLeftWas) when TimeLeftWas > 0 ->
    Started = genlib_time:ticks(),
    Changes = next_event(CustomerID, Client),
    case run_match_specs(MatchSpecs, Changes) of
        {Matched, []} ->
            Acc ++ Matched;
        {Matched, MatchSpecsLeft} ->
            ok = timer:sleep(?INTERVAL),
            TimeLeft = TimeLeftWas - (genlib_time:ticks() - Started) div 1000,
            await_for_changes(MatchSpecsLeft, CustomerID, Client, Acc ++ Matched, TimeLeft)
    end;
await_for_changes(MatchSpecs, CustomerID, _Client, _Acc, _TimeLeft) ->
    error({event_limit_exceeded, {CustomerID, MatchSpecs}}).

run_match_specs(MatchSpecs0, Changes) ->
    lists:foldl(
        fun (Change, {Acc, MatchSpecs}) ->
            case run_match_specs_(MatchSpecs, Change) of
                [{N, _} | _] ->
                    {[{N, Change} | Acc], lists:keydelete(N, 1, MatchSpecs)};
                [] ->
                    {Acc, MatchSpecs}
            end
        end,
        {[], MatchSpecs0},
        Changes
    ).

run_match_specs_(MatchSpecs, Change) ->
    lists:dropwhile(
        fun ({_N, MS}) ->
            length(ets:match_spec_run([Change], MS)) == 0
        end,
        MatchSpecs
    ).

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

next_event(CustomerID, Client) ->
    case hg_client_customer:pull_event(CustomerID, 10000, Client) of
        {ok, ?customer_event(Changes)} ->
            Changes;
        Result ->
            Result
    end.

%%

make_disposable_payment_resource(PaymentTool, Session) ->
    #domain_DisposablePaymentResource{
        payment_tool = PaymentTool,
        payment_session_id = Session,
        client_info = #domain_ClientInfo{}
    }.

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form}.

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

%%

-spec construct_term_set_w_recurrent_paytools() -> term().

construct_term_set_w_recurrent_paytools() ->
    TermSet = construct_simple_term_set(),
    TermSet#domain_TermSet{recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
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
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
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
