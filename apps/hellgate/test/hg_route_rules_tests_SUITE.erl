% Whitebox tests suite
-module(hg_route_rules_tests_SUITE).

-include("hg_ct_domain.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([no_route_found_for_payment/1]).
-export([gather_route_success/1]).
-export([rejected_by_table_prohibitions/1]).
-export([empty_candidate_ok/1]).
-export([ruleset_misconfig/1]).
-export([routes_selected_for_high_risk_score/1]).
-export([routes_selected_for_low_risk_score/1]).

-export([prefer_alive/1]).
-export([prefer_normal_conversion/1]).
-export([prefer_higher_availability/1]).
-export([prefer_higher_conversion/1]).
-export([prefer_weight_over_availability/1]).
-export([prefer_weight_over_conversion/1]).
-export([gathers_fail_rated_routes/1]).
-export([terminal_priority_for_shop/1]).

-behaviour(supervisor).

-export([init/1]).

-define(dummy_party_id, <<"dummy_party_id">>).
-define(dummy_shop_id, <<"dummy_shop_id">>).
-define(dummy_another_shop_id, <<"dummy_another_shop_id">>).
-define(assert_set_equal(S1, S2), ?assertEqual(lists:sort(S1), lists:sort(S2))).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name() :: hg_ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, base_routing_rule},
        {group, routing_with_risk_coverage_set},
        {group, routing_with_fail_rate},
        {group, terminal_priority}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base_routing_rule, [parallel], [
            gather_route_success,
            no_route_found_for_payment,
            rejected_by_table_prohibitions,
            empty_candidate_ok,
            ruleset_misconfig
        ]},
        {routing_with_risk_coverage_set, [parallel], [
            routes_selected_for_low_risk_score,
            routes_selected_for_high_risk_score
        ]},
        {routing_with_fail_rate, [parallel], [
            prefer_alive,
            prefer_normal_conversion,
            prefer_higher_availability,
            prefer_higher_conversion,
            prefer_weight_over_availability,
            prefer_weight_over_conversion,
            gathers_fail_rated_routes
        ]},
        {terminal_priority, [], [
            terminal_priority_for_shop
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, _Ret} = hg_ct_helper:start_apps([
        woody,
        scoper,
        dmt_client,
        party_client,
        hellgate,
        {cowboy, CowboySpec}
    ]),
    ok = hg_domain:insert(construct_domain_fixture()),
    PartyID = hg_utils:unique_id(),
    PartyClient = party_client:create_client(),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    {ok, _} = supervisor:start_child(SupPid, hg_dummy_fault_detector:child_spec()),
    FDConfig = genlib_app:env(hellgate, fault_detector),
    application:set_env(hellgate, fault_detector, FDConfig#{enabled => true}),
    _ = timer:sleep(5000),
    _ = unlink(SupPid),
    [
        {apps, Apps},
        {test_sup, SupPid},
        {party_client, PartyClient},
        {party_id, PartyID}
        | C
    ].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    SupPid = cfg(test_sup, C),
    ok = supervisor:terminate_child(SupPid, hg_dummy_fault_detector),
    ok = hg_domain:cleanup().

-spec init_per_group(group_name(), config()) -> config().
init_per_group(base_routing_rule, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(base_routing_rules_fixture(Revision)),
    C;
init_per_group(routing_with_risk_coverage_set, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(routing_with_risk_score_fixture(Revision, true)),
    C;
init_per_group(routing_with_fail_rate, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(routing_with_risk_score_fixture(Revision, false)),
    C;
init_per_group(terminal_priority, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(terminal_priority_fixture(Revision)),
    C;
init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_GroupName, C) ->
    case cfg(original_domain_revision, C) of
        Revision when is_integer(Revision) ->
            ok = hg_domain:reset(Revision);
        undefined ->
            ok
    end.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_, C) ->
    Ctx0 = hg_context:set_party_client(cfg(party_client, C), hg_context:create()),
    Ctx1 = hg_context:set_user_identity(
        #{
            id => cfg(party_id, C),
            realm => <<"internal">>
        },
        Ctx0
    ),
    PartyClientContext = party_client_context:create(#{}),
    Ctx2 = hg_context:set_party_client_context(PartyClientContext, Ctx1),
    ok = hg_context:save(Ctx2),
    C.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = hg_context:cleanup(),
    ok.

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec no_route_found_for_payment(config()) -> test_return().
no_route_found_for_payment(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(999, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {[], RejectContext}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    #{
        rejected_routes := [
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', cost}},
            {?prv(2), ?trm(2), {'PaymentsProvisionTerms', category}},
            {?prv(3), ?trm(3), {'PaymentsProvisionTerms', payment_tool}}
        ]
    } = RejectContext,

    VS1 = VS#{
        currency => ?cur(<<"EUR">>),
        cost => ?cash(1000, <<"EUR">>)
    },
    {ok, {[], RejectContext1}} = hg_routing:gather_routes(payment, PaymentInstitution, VS1, Revision),
    #{
        rejected_routes := [
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', currency}},
            {?prv(2), ?trm(2), {'PaymentsProvisionTerms', category}},
            {?prv(3), ?trm(3), {'PaymentsProvisionTerms', payment_tool}}
        ]
    } = RejectContext1.

-spec gather_route_success(config()) -> test_return().
gather_route_success(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => low
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {ok, {[Route], RejectContext}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    ?assertMatch(?trm(1), hg_routing:terminal_ref(Route)),
    ?assertMatch(
        #{
            rejected_routes := [
                {?prv(2), ?trm(2), {'PaymentsProvisionTerms', category}},
                {?prv(3), ?trm(3), {'PaymentsProvisionTerms', payment_tool}}
            ]
        },
        RejectContext
    ).

-spec rejected_by_table_prohibitions(config()) -> test_return().
rejected_by_table_prohibitions(_C) ->
    BankCard = #domain_BankCard{
        token = <<"bank card token">>,
        payment_system_deprecated = visa,
        bin = <<"411111">>,
        last_digits = <<"11">>
    },
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {bank_card, BankCard},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => low
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {[], RejectContext}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    #{
        rejected_routes := [
            {?prv(3), ?trm(3), {'RoutingRule', undefined}},
            {?prv(1), ?trm(1), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), ?trm(2), {'PaymentsProvisionTerms', category}}
        ]
    } = RejectContext,
    ok.

-spec empty_candidate_ok(config()) -> test_return().
empty_candidate_ok(_C) ->
    BankCard = #domain_BankCard{
        token = <<"bank card token">>,
        payment_system_deprecated = visa,
        bin = <<"411111">>,
        last_digits = <<"11">>
    },
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(101010, <<"RUB">>),
        payment_tool => {bank_card, BankCard},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(2)}),
    {ok,
        {[], #{
            varset := VS,
            rejected_routes := []
        }}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision).

-spec ruleset_misconfig(config()) -> test_return().
ruleset_misconfig(_C) ->
    VS = #{
        party_id => <<"54321">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {error, {misconfiguration, {routing_decisions, {delegates, []}}}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ).

-spec routes_selected_for_low_risk_score(config()) -> test_return().
routes_selected_for_low_risk_score(C) ->
    routes_selected_with_risk_score(C, low, [?prv(21), ?prv(22), ?prv(23)]).

-spec routes_selected_for_high_risk_score(config()) -> test_return().
routes_selected_for_high_risk_score(C) ->
    routes_selected_with_risk_score(C, high, [?prv(22), ?prv(23)]).

routes_selected_with_risk_score(_C, RiskScore, ProviderRefs) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => RiskScore
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {ok, {Routes, _}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    ?assert_set_equal(ProviderRefs, lists:map(fun hg_routing:provider_ref/1, Routes)).

-spec prefer_alive(config()) -> test_return().
prefer_alive(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {RoutesUnordered, _RejectContext}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    Alive = {alive, 0.0},
    Dead = {dead, 1.0},
    Normal = {normal, 0.0},

    ProviderStatuses0 = [{Alive, Normal}, {Dead, Normal}, {Dead, Normal}],
    ProviderStatuses1 = [{Dead, Normal}, {Alive, Normal}, {Dead, Normal}],
    ProviderStatuses2 = [{Dead, Normal}, {Dead, Normal}, {Alive, Normal}],

    FailRatedRoutes0 = lists:zip(Routes, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip(Routes, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip(Routes, ProviderStatuses2),

    Route1 = hg_routing:new(?prv(21), ?trm(21)),
    Route2 = hg_routing:new(?prv(22), ?trm(22)),
    Route3 = hg_routing:new(?prv(23), ?trm(23)),

    {Route1, Meta0} = hg_routing:choose_rated_route(FailRatedRoutes0),
    {Route2, Meta1} = hg_routing:choose_rated_route(FailRatedRoutes1),
    {Route3, Meta2} = hg_routing:choose_rated_route(FailRatedRoutes2),

    #{reject_reason := availability_condition, preferable_route := Route3} = Meta0,
    #{reject_reason := availability_condition, preferable_route := Route3} = Meta1,
    false = maps:is_key(reject_reason, Meta2).

-spec prefer_normal_conversion(config()) -> test_return().
prefer_normal_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {RoutesUnordered, _RC}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    Alive = {alive, 0.0},
    Normal = {normal, 0.0},
    Lacking = {lacking, 1.0},

    ProviderStatuses0 = [{Alive, Normal}, {Alive, Lacking}, {Alive, Lacking}],
    ProviderStatuses1 = [{Alive, Lacking}, {Alive, Normal}, {Alive, Lacking}],
    ProviderStatuses2 = [{Alive, Lacking}, {Alive, Lacking}, {Alive, Normal}],
    FailRatedRoutes0 = lists:zip(Routes, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip(Routes, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip(Routes, ProviderStatuses2),

    Route1 = hg_routing:new(?prv(21), ?trm(21)),
    Route2 = hg_routing:new(?prv(22), ?trm(22)),
    Route3 = hg_routing:new(?prv(23), ?trm(23)),

    {Route1, Meta0} = hg_routing:choose_rated_route(FailRatedRoutes0),
    {Route2, Meta1} = hg_routing:choose_rated_route(FailRatedRoutes1),
    {Route3, Meta2} = hg_routing:choose_rated_route(FailRatedRoutes2),

    #{reject_reason := conversion_condition, preferable_route := Route3} = Meta0,
    #{reject_reason := conversion_condition, preferable_route := Route3} = Meta1,
    false = maps:is_key(reject_reason, Meta2).

-spec prefer_higher_availability(config()) -> test_return().
prefer_higher_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {RoutesUnordered, _RC}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    ProviderStatuses = [{{alive, 0.5}, {normal, 0.5}}, {{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.6}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip(Routes, ProviderStatuses),

    Route1 = hg_routing:new(?prv(21), ?trm(21)),
    Route2 = hg_routing:new(?prv(23), ?trm(23)),
    Result = hg_routing:choose_rated_route(FailRatedRoutes),
    ?assertMatch({Route1, #{reject_reason := availability, preferable_route := Route2}}, Result).

-spec prefer_higher_conversion(config()) -> test_return().
prefer_higher_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {ok, {RoutesUnordered, _RC}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    ProviderStatuses = [{{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip(Routes, ProviderStatuses),
    Route1 = hg_routing:new(?prv(22), ?trm(22)),
    Route2 = hg_routing:new(?prv(23), ?trm(23)),

    Result = hg_routing:choose_rated_route(FailRatedRoutes),
    ?assertMatch({Route1, #{reject_reason := conversion, preferable_route := Route2}}, Result).

-spec prefer_weight_over_availability(config()) -> test_return().
prefer_weight_over_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {RoutesUnordered, _RC}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip(Routes, ProviderStatuses),

    Route = hg_routing:new(?prv(22), ?trm(22), 0, 1005),
    ?assertMatch({Route, _}, hg_routing:choose_rated_route(FailRatedRoutes)).

-spec prefer_weight_over_conversion(config()) -> test_return().
prefer_weight_over_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {ok, {RoutesUnordered, _RC}} = hg_routing:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    Routes = sort_routes(RoutesUnordered),
    ?assert_set_equal([?prv(21), ?prv(22), ?prv(23)], [hg_routing:provider_ref(R) || R <- RoutesUnordered]),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.5}}, {{alive, 0.3}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip(Routes, ProviderStatuses),
    Result = hg_routing:choose_rated_route(FailRatedRoutes),
    {Route, _Meta} = Result,
    ?assertMatch({?prv(22), ?trm(22)}, {hg_routing:provider_ref(Route), hg_routing:terminal_ref(Route)}).

-spec gathers_fail_rated_routes(config()) -> test_return().
gathers_fail_rated_routes(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {ok, {Routes0, _RejectContext0}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    Result = hg_routing:gather_fail_rates(Routes0),
    ?assertEqual(
        [
            {hg_routing:new(?prv(21), ?trm(21)), {{dead, 0.9}, {lacking, 0.9}}},
            {hg_routing:new(?prv(22), ?trm(22)), {{alive, 0.1}, {normal, 0.1}}},
            {hg_routing:new(?prv(23), ?trm(23)), {{alive, 0.0}, {normal, 0.0}}}
        ],
        lists:sort(Result)
    ).

sort_routes(Routes) ->
    lists:sort(
        fun(Route1, Route2) ->
            ?prv(ID1) = hg_routing:provider_ref(Route1),
            ?prv(ID2) = hg_routing:provider_ref(Route2),
            ID1 < ID2
        end,
        Routes
    ).

%%% Terminal priority tests

-spec terminal_priority_for_shop(config()) -> test_return().
terminal_priority_for_shop(C) ->
    Route1 = hg_routing:new(?prv(41), ?trm(41), 0, 10),
    Route2 = hg_routing:new(?prv(42), ?trm(42), 0, 10),
    ?assertMatch({Route1, _}, terminal_priority_for_shop(?dummy_party_id, ?dummy_shop_id, C)),
    ?assertMatch({Route2, _}, terminal_priority_for_shop(?dummy_party_id, ?dummy_another_shop_id, C)).

terminal_priority_for_shop(PartyID, ShopID, _C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = euroset}},
        party_id => PartyID,
        shop_id => ShopID,
        flow => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {ok, {Routes, _RejectContext}} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    FailRatedRoutes = hg_routing:gather_fail_rates(Routes),
    hg_routing:choose_rated_route(FailRatedRoutes).

%%% Domain config fixtures

base_routing_rules_fixture(Revision) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                payment_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(2),
                    prohibitions = ?ruleset(1)
                }
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(1),
            data = #domain_RoutingRuleset{
                name = <<"Prohibition: bank_card terminal is denied">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(3))
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(2),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {delegates, [
                        ?delegate(condition(party, <<"12345">>), ?ruleset(3))
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(3),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(1)),
                        ?candidate({constant, true}, ?trm(2)),
                        ?candidate({constant, true}, ?trm(3))
                    ]}
            }
        }},
        {terminal, ?terminal_obj(?trm(1), ?prv(1))},
        {terminal, ?terminal_obj(?trm(2), ?prv(2))},
        {terminal, ?terminal_obj(?trm(3), ?prv(3))},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    categories =
                        {value,
                            ?ordset([
                                ?cat(2)
                            ])},
                    currencies =
                        {value,
                            ?ordset([
                                ?cur(<<"RUB">>),
                                ?cur(<<"EUR">>)
                            ])}
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    payment_methods =
                        {value,
                            ?ordset([
                                ?pmt(bank_card_deprecated, visa)
                            ])},
                    currencies =
                        {value,
                            ?ordset([
                                ?cur(<<"RUB">>),
                                ?cur(<<"EUR">>)
                            ])}
                }
            })
        }}
    ].

routing_with_risk_score_fixture(Revision, AddRiskScore) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                payment_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(2),
                    prohibitions = ?ruleset(1)
                }
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(2),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {delegates, [
                        ?delegate(condition(party, <<"12345">>), ?ruleset(3)),
                        ?delegate(condition(party, <<"54321">>), ?ruleset(4))
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(3),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(21)),
                        ?candidate({constant, true}, ?trm(22)),
                        ?candidate({constant, true}, ?trm(23))
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(4),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(21)),
                        ?candidate(<<"high priority">>, {constant, true}, ?trm(22), 1005),
                        ?candidate({constant, true}, ?trm(23))
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(1),
            data = #domain_RoutingRuleset{
                name = <<"No prohibition: all candidate is allowed">>,
                decisions = {candidates, []}
            }
        }},
        {terminal, ?terminal_obj(?trm(21), ?prv(21))},
        {terminal, ?terminal_obj(?trm(22), ?prv(22))},
        {terminal, ?terminal_obj(?trm(23), ?prv(23))},
        {provider, #domain_ProviderObject{
            ref = ?prv(21),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    risk_coverage = maybe_set_risk_coverage(AddRiskScore, low)
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(22),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    risk_coverage = maybe_set_risk_coverage(AddRiskScore, high)
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(23),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms
            })
        }}
    ].

terminal_priority_fixture(Revision) ->
    PartyID = ?dummy_party_id,
    ShopID = ?dummy_shop_id,
    AnotherShopID = ?dummy_another_shop_id,
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                payment_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(2),
                    prohibitions = ?ruleset(1)
                }
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(2),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {delegates, [
                        ?delegate(condition(party_shop, PartyID, ShopID), ?ruleset(3)),
                        ?delegate(condition(party_shop, PartyID, AnotherShopID), ?ruleset(4))
                    ]}
            }
        }},

        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(3),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate(<<"high priority">>, {constant, true}, ?trm(41), 10),
                        ?candidate(<<"low priority">>, {constant, true}, ?trm(42), 5)
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(4),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate(<<"low priority">>, {constant, true}, ?trm(41), 5),
                        ?candidate(<<"high priority">>, {constant, true}, ?trm(42), 10)
                    ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(1),
            data = #domain_RoutingRuleset{
                name = <<"No prohibition: all candidate is allowed">>,
                decisions = {candidates, []}
            }
        }},

        {terminal, ?terminal_obj(?trm(41), ?prv(41))},
        {terminal, ?terminal_obj(?trm(42), ?prv(42))},
        {provider, #domain_ProviderObject{
            ref = ?prv(41),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(42),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms
            })
        }}
    ].

construct_domain_fixture() ->
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),
        hg_ct_fixture:construct_currency(?cur(<<"EUR">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, jcb)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal_deprecated, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(digital_wallet_deprecated, qiwi)),
        hg_ct_fixture:construct_payment_method(?pmt(empty_cvv_bank_card_deprecated, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(tokenized_bank_card_deprecated, ?tkz_bank_card(visa, applepay))),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_system_account_set(?sas(2)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set =
                    {decisions, [
                        #domain_ExternalAccountSetDecision{
                            if_ =
                                {condition,
                                    {party, #domain_PartyCondition{
                                        id = <<"LGBT">>
                                    }}},
                            then_ = {value, ?eas(2)}
                        },
                        #domain_ExternalAccountSetDecision{
                            if_ = {constant, true},
                            then_ = {value, ?eas(1)}
                        }
                    ]},
                payment_institutions = ?ordset([?pinst(1), ?pinst(2)])
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                term_sets = []
            }
        }},
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                % TODO do we realy need this decision hell here?
                inspector = {decisions, []},
                residences = [],
                realm = test
            }
        }},
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(1)},
                inspector = {decisions, []},
                residences = [],
                realm = live
            }
        }}
    ].

condition(party, ID) ->
    {condition, {party, #domain_PartyCondition{id = ID}}}.

condition(party_shop, PartyID, ShopID) ->
    {condition, {party, #domain_PartyCondition{id = PartyID, definition = {shop_is, ShopID}}}}.

maybe_set_risk_coverage(false, _) ->
    undefined;
maybe_set_risk_coverage(true, V) ->
    {value, V}.
