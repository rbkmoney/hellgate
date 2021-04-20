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
-export([prefer_better_risk_score/1]).
-export([routes_selected_for_high_risk_score/1]).
-export([routes_selected_for_low_risk_score/1]).

-export([prefer_alive/1]).
-export([prefer_normal_conversion/1]).
-export([prefer_higher_availability/1]).
-export([prefer_higher_conversion/1]).
-export([prefer_weight_over_availability/1]).
-export([prefer_weight_over_conversion/1]).
-export([gathers_fail_rated_routes/1]).
% -export([terminal_priority_for_shop/1]).

-behaviour(supervisor).

-export([init/1]).

-define(dummy_party_id, <<"dummy_party_id">>).
-define(dummy_shop_id, <<"dummy_shop_id">>).
-define(dummy_another_shop_id, <<"dummy_another_shop_id">>).

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
        % {group, base_routing_rule}
        {group, routing_with_risk_coverage_set}
        % {group, routing_with_fail_rate}
    % {group, terminal_priority}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {base_routing_rule, [], [
            gather_route_success,
            no_route_found_for_payment,
            rejected_by_table_prohibitions,
            empty_candidate_ok,
            ruleset_misconfig
        ]},
        {routing_with_risk_coverage_set, [], [
            routes_selected_for_low_risk_score,
            routes_selected_for_high_risk_score,
            prefer_better_risk_score
        ]},
        {routing_with_fail_rate, [], [
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
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[], RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),
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
    {[], RejectContext1} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS1, Revision),
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
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => low
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[{_, {?trm(1), _, _}}], RejectContext} = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),
    #{
        rejected_routes := [
            {?prv(2), ?trm(2), {'PaymentsProvisionTerms', category}},
            {?prv(3), ?trm(3), {'PaymentsProvisionTerms', payment_tool}}
        ]
    } = RejectContext.

-spec rejected_by_table_prohibitions(config()) -> test_return().
rejected_by_table_prohibitions(_C) ->
    BankCard = #domain_BankCard{
        token = <<"bank card token">>,
        payment_system = visa,
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

    {[], RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),

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
        payment_system = visa,
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
    {[], #{
        varset := VS,
        rejected_routes := [],
        rejected_providers := []
    }} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision).

-spec ruleset_misconfig(config()) -> test_return().
ruleset_misconfig(_C) ->
    VS = #{
        party_id => <<"54321">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {[], #{
        varset := VS,
        rejected_routes := [],
        rejected_providers := []
    }} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision).

-spec prefer_better_risk_score(config()) -> test_return().
prefer_better_risk_score(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => low
    },
    RiskScore = high,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(lists:reverse(Routes)),

    ProviderStatuses = [{{dead, 1.0}, {normal, 0.0}}],
    ct:print("ProviderRefs: ~p~n TerminalData: ~p~n", [ProviderRefs, TerminalData]),
    ct:print("Rejected ~p~n", [RC]),
    FailRatedRoutes = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(3)}, Meta} = Result,
    false = maps:is_key(reject_reason, Meta),

    ok.

-spec routes_selected_for_low_risk_score(config()) -> test_return().
routes_selected_for_low_risk_score(C) ->
    routes_selected_with_risk_score(C, low, [1, 2, 3]).

-spec routes_selected_for_high_risk_score(config()) -> test_return().
routes_selected_for_high_risk_score(C) ->
    routes_selected_with_risk_score(C, high, [2, 3]).

routes_selected_with_risk_score(_C, RiskScore, PrvIDList) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant,
        risk_score => RiskScore
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(2)}),
    {SelectedProviders, _} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),

    %% Ensure list of selected provider ID match to given
    PrvIDList = lists:sort([P || {{?prv(P), _}, _} <- SelectedProviders]),
    ok.

-spec prefer_alive(config()) -> test_return().
prefer_alive(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {
        [{{?prv(3), _}, _}, {{?prv(2), _}, _}, {{?prv(1), _}, _}] = Routes,
        RejectContext
    } = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),

    {ProviderRefs, TerminalData} = lists:unzip(lists:reverse(Routes)),

    Alive = {alive, 0.0},
    Dead = {dead, 1.0},
    Normal = {normal, 0.0},

    ProviderStatuses0 = [{Alive, Normal}, {Dead, Normal}, {Dead, Normal}],
    ProviderStatuses1 = [{Dead, Normal}, {Alive, Normal}, {Dead, Normal}],
    ProviderStatuses2 = [{Dead, Normal}, {Dead, Normal}, {Alive, Normal}],

    FailRatedRoutes0 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses2),

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RejectContext, RiskScore),
    Result1 = hg_routing:choose_route(FailRatedRoutes1, RejectContext, RiskScore),
    Result2 = hg_routing:choose_route(FailRatedRoutes2, RejectContext, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(1), terminal = ?trm(1)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(2), terminal = ?trm(2)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(3), terminal = ?trm(3)}, Meta2} = Result2,

    #{reject_reason := availability_condition, preferable_route := #{provider_ref := 3}} = Meta0,
    #{reject_reason := availability_condition, preferable_route := #{provider_ref := 3}} = Meta1,
    false = maps:is_key(reject_reason, Meta2),

    ok.

-spec prefer_normal_conversion(config()) -> test_return().
prefer_normal_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {
        [{{?prv(3), _}, _}, {{?prv(2), _}, _}, {{?prv(1), _}, _}] = Routes,
        RC
    } = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),

    {Providers, TerminalData} = lists:unzip(lists:reverse(Routes)),

    Alive = {alive, 0.0},
    Normal = {normal, 0.0},
    Lacking = {lacking, 1.0},

    ProviderStatuses0 = [{Alive, Normal}, {Alive, Lacking}, {Alive, Lacking}],
    ProviderStatuses1 = [{Alive, Lacking}, {Alive, Normal}, {Alive, Lacking}],
    ProviderStatuses2 = [{Alive, Lacking}, {Alive, Lacking}, {Alive, Normal}],
    FailRatedRoutes0 = lists:zip3(Providers, TerminalData, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip3(Providers, TerminalData, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip3(Providers, TerminalData, ProviderStatuses2),

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RC, RiskScore),
    Result1 = hg_routing:choose_route(FailRatedRoutes1, RC, RiskScore),
    Result2 = hg_routing:choose_route(FailRatedRoutes2, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(1), terminal = ?trm(1)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(2), terminal = ?trm(2)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(3), terminal = ?trm(3)}, Meta2} = Result2,

    #{reject_reason := conversion_condition, preferable_route := #{provider_ref := 3}} = Meta0,
    #{reject_reason := conversion_condition, preferable_route := #{provider_ref := 3}} = Meta1,
    false = maps:is_key(reject_reason, Meta2),

    ok.

-spec prefer_higher_availability(config()) -> test_return().
prefer_higher_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {
        [
            {{?prv(3), _}, _},
            {{?prv(2), _}, _},
            {{?prv(1), _}, _}
        ] = Routes,
        RC
    } = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),

    {ProviderRefs, TerminalData} = lists:unzip(lists:reverse(Routes)),

    ProviderStatuses = [{{alive, 0.5}, {normal, 0.5}}, {{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.6}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),


    {ok, #domain_PaymentRoute{provider = ?prv(1), terminal = ?trm(1)}, #{
        reject_reason := availability,
        preferable_route := #{provider_ref := 3}
    }} = Result,

    ok.

-spec prefer_higher_conversion(config()) -> test_return().
prefer_higher_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {
        [
            {{?prv(3), _}, _},
            {{?prv(2), _}, _},
            {{?prv(1), _}, _}
        ] = Routes,
        RC
    } = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),

    {Providers, TerminalData} = lists:unzip(lists:reverse(Routes)),

    ProviderStatuses = [{{dead, 0.8}, {lacking, 1.0}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.5}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, undefined),
    {ok, #domain_PaymentRoute{provider = ?prv(2), terminal = ?trm(2)}, #{
        reject_reason := conversion,
        preferable_route := #{provider_ref := 3}
    }} = Result,
    ok.

-spec prefer_weight_over_availability(config()) -> test_return().
prefer_weight_over_availability(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },
    RiskScore = low,

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {
        [
            {{?prv(3), _}, _},
            {{?prv(2), _}, _},
            {{?prv(1), _}, _}
        ] = Routes,
        RC
    } = hg_routing_rule:gather_routes(
        payment,
        PaymentInstitution,
        VS,
        Revision
    ),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(2), terminal = ?trm(2)}, _Meta} = Result,

    ok.

-spec prefer_weight_over_conversion(config()) -> test_return().
prefer_weight_over_conversion(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"54321">>,
        flow => instant
    },
    RiskScore = low,
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {
        [
            {{?prv(3), _}, _},
            {{?prv(2), _}, _},
            {{?prv(1), _}, _}
        ] = Routes,
        RC
    } = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.5}}, {{alive, 0.3}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, RiskScore),

    {ok, #domain_PaymentRoute{provider = ?prv(2), terminal = ?trm(2)}, _Meta} = Result,

    ok.

-spec gathers_fail_rated_routes(config()) -> test_return().
gathers_fail_rated_routes(_C) ->
    VS = #{
        category => ?cat(1),
        currency => ?cur(<<"RUB">>),
        cost => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id => <<"12345">>,
        flow => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes0, _RejectContext0} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),
    Result = hg_routing:gather_fail_rates(Routes0),
    [
        {{?prv(3), _}, _, {{alive, 0.0}, {normal, 0.0}}},
        {{?prv(2), _}, _, {{alive, 0.1}, {normal, 0.1}}},
        {{?prv(1), _}, _, {{dead, 0.9}, {lacking, 0.9}}}
    ] = Result,
    ok.

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
                decisions = {candidates, [
                    ?candidate({constant, true}, ?trm(3))
                ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(2),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions = {delegates, [
                    ?delegate(condition(party, <<"12345">>), ?ruleset(3))
                ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(3),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions = {candidates, [
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
            }
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    categories = {value,
                        ?ordset([
                            ?cat(2)
                        ])
                    },
                    currencies = {value,
                        ?ordset([
                            ?cur(<<"RUB">>),
                            ?cur(<<"EUR">>)
                        ])
                    }
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    payment_methods = {value,
                        ?ordset([
                            ?pmt(bank_card_deprecated, visa)
                        ])
                    },
                    currencies = {value,
                        ?ordset([
                            ?cur(<<"RUB">>),
                            ?cur(<<"EUR">>)
                        ])
                    }
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
                decisions = {delegates, [
                    ?delegate(condition(party, <<"12345">>), ?ruleset(3)),
                    ?delegate(condition(party, <<"54321">>), ?ruleset(4))

                ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(3),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions = {candidates, [
                    ?candidate({constant, true}, ?trm(1)),
                    ?candidate({constant, true}, ?trm(2)),
                    ?candidate({constant, true}, ?trm(3))
                ]}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(4),
            data = #domain_RoutingRuleset{
                name = <<"">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(1)),
                        ?candidate(<<"high priority">>, {constant, true}, ?trm(2), 1005),
                        ?candidate({constant, true}, ?trm(3))
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
        {terminal, ?terminal_obj(?trm(1), ?prv(1))},
        {terminal, ?terminal_obj(?trm(2), ?prv(2))},
        {terminal, ?terminal_obj(?trm(3), ?prv(3))},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    risk_coverage = maybe_set_risk_coverage(AddRiskScore, low)
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms#domain_PaymentsProvisionTerms{
                    risk_coverage = maybe_set_risk_coverage(AddRiskScore, high)
                }
            })
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = ?provider(#domain_ProvisionTermSet{
                payments = ?payment_terms
            })
        }}
    ].

construct_domain_fixture() ->
    % Prohibitions =
    %     {delegates, [
    %         ?delegate(condition(payment_terminal, euroset), ?ruleset(4))
    %     ]},
    % NoProhibitions = {candidates, []},

    % Decision2 =
    %     {delegates, [
    %         ?delegate(condition(cost_in, {0, 500000, <<"RUB">>}), ?ruleset(9))
    %     ]},
    % Decision3 =
    %     {candidates, [
    %         ?candidate({constant, true}, ?trm(10)),
    %         ?candidate({constant, true}, ?trm(11))
    %     ]},
    % Decision4 =
    %     {candidates, [
    %         ?candidate({constant, true}, ?trm(1)),
    %         ?candidate({constant, true}, ?trm(11))
    %     ]},
    % Decision9 =
    %     {candidates, [
    %         ?candidate({constant, true}, ?trm(1)),
    %         ?candidate({constant, true}, ?trm(6)),
    %         ?candidate({constant, true}, ?trm(10))
    %     ]},
    % Decision11 =
    %     {candidates, [
    %         ?candidate({constant, true}, ?trm(1)),
    %         ?candidate({constant, true}, ?trm(6)),
    %         ?candidate({constant, true}, ?trm(10)),
    %         ?candidate({constant, true}, ?trm(12))
    %     ]},
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),
        hg_ct_fixture:construct_currency(?cur(<<"EUR">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, jcb)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(digital_wallet, qiwi)),
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
                % payment_routing_rules = #domain_RoutingRules{
                %     policies = ?ruleset(1),
                %     prohibitions = ?ruleset(5)
                % },
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
                % payment_routing_rules = #domain_RoutingRules{
                %     policies = ?ruleset(7),
                %     prohibitions = ?ruleset(6)
                % },
                inspector = {decisions, []},
                residences = [],
                realm = live
            }
        }}
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(1), <<"Rule#1">>, Decision1),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(2), <<"Rule#2">>, Decision2),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(3), <<"Rule#3">>, Decision3),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(4), <<"Rule#4">>, Decision4),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(5), <<"ProhobitionRule#1">>, Prohibitions),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(6), <<"ProhobitionRule#2">>, Decision4),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(7), <<"Empty Delegates">>, {delegates, []}),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(8), <<"Empty Candidates">>, {candidates, []}),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(9), <<"Rule#9">>, Decision9),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(11), <<"Rule#11">>, Decision11),
        % hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(10), <<"No Prohobition">>, NoProhibitions),


        % {routing_rules, #domain_RoutingRulesObject{
        %     ref = ?ruleset(1),
        %     data = #domain_RoutingRuleset{
        %         name = <<"">>,
        %         decisions = {delegates, [
        %             ?delegate(condition(party, <<"12345">>), ?ruleset(2)),
        %             ?delegate(condition(party, <<"67890">>), ?ruleset(4)),
        %             ?delegate(condition(party, <<"all providers">>), ?ruleset(11)),
        %             ?delegate(predicate(true), ?ruleset(3))
        %         ]}
        % }}},





        % {provider, #domain_ProviderObject{
        %     ref = ?prv(1),
        %     data = #domain_Provider{
        %         name = <<"Brovider">>,
        %         description = <<"A provider but bro">>,
        %         proxy = #domain_Proxy{
        %             ref = ?prx(1),
        %             additional = #{
        %                 <<"override">> => <<"brovider">>
        %             }
        %         },
        %         abs_account = <<"1234567890">>,
        %         accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
        %         terms = #domain_ProvisionTermSet{
        %             payments = #domain_PaymentsProvisionTerms{
        %                 currencies =
        %                     {value,
        %                         ?ordset([
        %                             ?cur(<<"RUB">>)
        %                         ])},
        %                 categories =
        %                     {value,
        %                         ?ordset([
        %                             ?cat(1)
        %                         ])},
        %                 payment_methods =
        %                     {value,
        %                         ?ordset([
        %                             ?pmt(bank_card_deprecated, visa),
        %                             ?pmt(bank_card_deprecated, mastercard),
        %                             ?pmt(bank_card_deprecated, jcb),
        %                             ?pmt(empty_cvv_bank_card_deprecated, visa),
        %                             ?pmt(tokenized_bank_card_deprecated, ?tkz_bank_card(visa, applepay))
        %                         ])},
        %                 cash_limit =
        %                     {value,
        %                         ?cashrng(
        %                             {inclusive, ?cash(1000, <<"RUB">>)},
        %                             {exclusive, ?cash(1000000000, <<"RUB">>)}
        %                         )},
        %                 cash_flow =
        %                     {value, [
        %                         ?cfpost(
        %                             {provider, settlement},
        %                             {merchant, settlement},
        %                             ?share(1, 1, operation_amount)
        %                         )
        %                     ]}
        %             },
        %             recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
        %                 categories = {value, ?ordset([?cat(1)])},
        %                 payment_methods =
        %                     {value,
        %                         ?ordset([
        %                             ?pmt(bank_card_deprecated, visa),
        %                             ?pmt(bank_card_deprecated, mastercard)
        %                         ])},
        %                 cash_value = {value, ?cash(1000, <<"RUB">>)}
        %             }
        %         }
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(1),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(1),
        %         name = <<"Brominal 1">>,
        %         description = <<"Brominal 1">>
        %     }
        % }},

        % {provider, #domain_ProviderObject{
        %     ref = ?prv(2),
        %     data = #domain_Provider{
        %         name = <<"Drovider">>,
        %         description = <<"I'm out of ideas of what to write here">>,
        %         terminal = {value, [?prvtrm(6), ?prvtrm(7)]},
        %         proxy = #domain_Proxy{
        %             ref = ?prx(1),
        %             additional = #{
        %                 <<"override">> => <<"drovider">>
        %             }
        %         },
        %         abs_account = <<"1234567890">>,
        %         accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
        %         terms = #domain_ProvisionTermSet{
        %             payments = #domain_PaymentsProvisionTerms{
        %                 currencies =
        %                     {value,
        %                         ?ordset([
        %                             ?cur(<<"RUB">>)
        %                         ])},
        %                 categories =
        %                     {value,
        %                         ?ordset([
        %                             ?cat(2)
        %                         ])},
        %                 payment_methods =
        %                     {value,
        %                         ?ordset([
        %                             ?pmt(bank_card_deprecated, visa),
        %                             ?pmt(bank_card_deprecated, mastercard)
        %                         ])},
        %                 cash_limit =
        %                     {value,
        %                         ?cashrng(
        %                             {inclusive, ?cash(1000, <<"RUB">>)},
        %                             {exclusive, ?cash(10000000, <<"RUB">>)}
        %                         )},
        %                 cash_flow =
        %                     {value, [
        %                         ?cfpost(
        %                             {provider, settlement},
        %                             {merchant, settlement},
        %                             ?share(1, 1, operation_amount)
        %                         )
        %                     ]}
        %             }
        %         }
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(6),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(2),
        %         name = <<"Drominal 1">>,
        %         description = <<"Drominal 1">>,
        %         terms = #domain_ProvisionTermSet{
        %             payments = #domain_PaymentsProvisionTerms{
        %                 currencies =
        %                     {value,
        %                         ?ordset([
        %                             ?cur(<<"RUB">>)
        %                         ])},
        %                 categories =
        %                     {value,
        %                         ?ordset([
        %                             ?cat(2)
        %                         ])},
        %                 payment_methods =
        %                     {value,
        %                         ?ordset([
        %                             ?pmt(bank_card_deprecated, visa)
        %                         ])},
        %                 cash_limit =
        %                     {value,
        %                         ?cashrng(
        %                             {inclusive, ?cash(1000, <<"RUB">>)},
        %                             {exclusive, ?cash(5000000, <<"RUB">>)}
        %                         )},
        %                 cash_flow =
        %                     {value, [
        %                         ?cfpost(
        %                             {provider, settlement},
        %                             {merchant, settlement},
        %                             ?share(1, 1, operation_amount)
        %                         )
        %                     ]}
        %             }
        %         }
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(7),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(2),
        %         name = <<"Terminal 7">>,
        %         description = <<"Terminal 7">>
        %     }
        % }},

        % {provider, #domain_ProviderObject{
        %     ref = ?prv(3),
        %     data = #domain_Provider{
        %         name = <<"Crovider">>,
        %         description = <<"Payment terminal provider">>,
        %         terminal = {value, [?prvtrm(10), ?prvtrm(11)]},
        %         proxy = #domain_Proxy{
        %             ref = ?prx(1),
        %             additional = #{
        %                 <<"override">> => <<"crovider">>
        %             }
        %         },
        %         abs_account = <<"0987654321">>,
        %         accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
        %         terms = #domain_ProvisionTermSet{
        %             payments = #domain_PaymentsProvisionTerms{
        %                 currencies =
        %                     {value,
        %                         ?ordset([
        %                             ?cur(<<"RUB">>),
        %                             ?cur(<<"EUR">>)
        %                         ])},
        %                 categories =
        %                     {value,
        %                         ?ordset([
        %                             ?cat(1)
        %                         ])},
        %                 payment_methods =
        %                     {value,
        %                         ?ordset([
        %                             ?pmt(payment_terminal, euroset),
        %                             ?pmt(bank_card_deprecated, visa),
        %                             ?pmt(bank_card_deprecated, mastercard),
        %                             ?pmt(bank_card_deprecated, jcb),
        %                             ?pmt(digital_wallet, qiwi)
        %                         ])},
        %                 cash_limit =
        %                     {value,
        %                         ?cashrng(
        %                             {inclusive, ?cash(1000, <<"RUB">>)},
        %                             {exclusive, ?cash(10000000, <<"RUB">>)}
        %                         )},
        %                 cash_flow =
        %                     {value, [
        %                         ?cfpost(
        %                             {provider, settlement},
        %                             {merchant, settlement},
        %                             ?share(1, 1, operation_amount)
        %                         ),
        %                         ?cfpost(
        %                             {system, settlement},
        %                             {provider, settlement},
        %                             ?share(21, 1000, operation_amount)
        %                         )
        %                     ]}
        %             }
        %         }
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(10),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(3),
        %         name = <<"Payment Terminal Terminal">>,
        %         description = <<"Euroset">>
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(11),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(3),
        %         name = <<"Second Payment Terminal">>,
        %         description = <<"Euroset">>
        %     }
        % }},

        % {provider, #domain_ProviderObject{
        %     ref = ?prv(4),
        %     data = #domain_Provider{
        %         name = <<"Zrovider">>,
        %         description = <<"Non-configured provider">>,
        %         proxy = #domain_Proxy{
        %             ref = ?prx(1),
        %             additional = #{
        %                 <<"override">> => <<"zrovider">>
        %             }
        %         },
        %         abs_account = <<"0987654321">>,
        %         accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
        %         terms = #domain_ProvisionTermSet{
        %             payments = #domain_PaymentsProvisionTerms{
        %                 currencies = undefined,
        %                 categories = undefined,
        %                 payment_methods = undefined,
        %                 cash_limit = undefined,
        %                 cash_flow = undefined
        %             }
        %         }
        %     }
        % }},
        % {terminal, #domain_TerminalObject{
        %     ref = ?trm(12),
        %     data = #domain_Terminal{
        %         provider_ref = ?prv(4),
        %         name = <<"Second Payment Terminal">>,
        %         description = <<"Euroset">>
        %     }
        % }}
    ].

% predicate(Constant) when is_boolean(Constant) ->
%     {constant, Constant}.

condition(cost_in, {Min, Max, Cur}) ->
    {condition,
        {cost_in,
            ?cashrng(
                {inclusive, ?cash(Min, Cur)},
                {exclusive, ?cash(Max, Cur)}
            )}};
condition(party, ID) ->
    {condition, {party, #domain_PartyCondition{id = ID}}};
condition(payment_terminal, Provider) ->
    {condition,
        {payment_tool,
            {payment_terminal, #domain_PaymentTerminalCondition{
                definition = {provider_is, Provider}
            }}}}.

maybe_set_risk_coverage(false, _) ->
    undefined;
maybe_set_risk_coverage(true, V) ->
    {value, V}.
