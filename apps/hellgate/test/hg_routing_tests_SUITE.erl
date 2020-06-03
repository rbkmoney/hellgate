-module(hg_routing_tests_SUITE).

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

-export([gathers_fail_rated_routes/1]).
-export([no_route_found_for_payment/1]).
-export([fatal_risk_score_for_route_found/1]).
-export([prefer_alive/1]).
-export([prefer_normal_conversion/1]).
-export([prefer_better_risk_score/1]).
-export([prefer_higher_availability/1]).
-export([prefer_higher_conversion/1]).
-export([prefer_weight_over_availability/1]).
-export([prefer_weight_over_conversion/1]).
-export([handle_uncomputable_provider_terms/1]).

-export([terminal_priority_for_shop/1]).

-behaviour(supervisor).
-export([init/1]).

-define(dummy_party_id,        <<"dummy_party_id">>).
-define(dummy_shop_id,         <<"dummy_shop_id">>).
-define(dummy_another_shop_id, <<"dummy_another_shop_id">>).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-type config()         :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name()     :: hg_ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() -> [
    fatal_risk_score_for_route_found,
    no_route_found_for_payment,
    handle_uncomputable_provider_terms,
    {group, routing_with_fail_rate},
    {group, terminal_priority}
].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() -> [
    {routing_with_fail_rate, [], [
        gathers_fail_rated_routes,
        prefer_alive,
        prefer_normal_conversion,
        prefer_better_risk_score,
        prefer_higher_availability,
        prefer_higher_conversion,
        prefer_weight_over_availability,
        prefer_weight_over_conversion
    ]},
    {terminal_priority, [], [
        terminal_priority_for_shop
    ]}
].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, _Ret} = hg_ct_helper:start_apps([
        woody, scoper, dmt_client, hellgate, {cowboy, CowboySpec}
    ]),
    ok = hg_domain:insert(construct_domain_fixture()),

    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    {ok, _} = supervisor:start_child(SupPid, hg_dummy_fault_detector:child_spec()),
    _ = unlink(SupPid),
    [{apps, Apps}, {test_sup, SupPid} | C].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    SupPid = cfg(test_sup, C),
    ok = supervisor:terminate_child(SupPid, hg_dummy_fault_detector),
    ok = hg_domain:cleanup().

-spec init_per_group(group_name(), config()) -> config().
init_per_group(routing_with_fail_rate, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(routing_with_fail_rate_fixture(Revision)),
    [{original_domain_revision, Revision} | C];
init_per_group(terminal_priority, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(terminal_priority_fixture(Revision, C)),
    [{original_domain_revision, Revision} | C];
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
init_per_testcase(_, C) -> C.

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, _C) -> ok.

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec handle_uncomputable_provider_terms(config()) -> test_return().
handle_uncomputable_provider_terms(_C) ->
    ok = hg_context:save(hg_context:create()),
    VS0 = #{
        category        => ?cat(1),
        currency        => ?cur(<<"EUR">>),
        cost            => ?cash(1000, <<"EUR">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Providers0, RejectContext0} = hg_routing:gather_routes(payment, PaymentInstitution, VS0, Revision),
    {[], #{
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(3), {'Misconfiguration', _}},
            {?prv(2), {'PaymentsProvisionTerms', currency}},
            {?prv(1), {'PaymentsProvisionTerms', currency}}
        ]}
    } = {Providers0, RejectContext0}.

-spec gathers_fail_rated_routes(config()) -> test_return().
gathers_fail_rated_routes(_C) ->
    ok = hg_context:save(hg_context:create()),

    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes0, _RejectContext0} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    Result = hg_routing:gather_fail_rates(Routes0),
    [
        {{?prv(200), _}, _, {{dead,  0.9}, {lacking, 0.9}}},
        {{?prv(201), _}, _, {{alive, 0.1}, {normal, 0.1}}},
        {{?prv(202), _}, _, {{alive, 0.0}, {normal, 0.0}}}
    ] = Result,

    hg_context:cleanup(),
    ok.

-spec fatal_risk_score_for_route_found(config()) -> test_return().

fatal_risk_score_for_route_found(_C) ->
    ok = hg_context:save(hg_context:create()),
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    VS0 = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {bank_card, #domain_BankCard{}},
        party_id        => <<"12345">>,
        risk_score      => fatal,
        flow            => instant
    },
    {Routes0, RejectContext0} = hg_routing:gather_routes(payment, PaymentInstitution, VS0, Revision),
    FailRatedRoutes0 = hg_routing:gather_fail_rates(Routes0),
    Result0          = hg_routing:choose_route(FailRatedRoutes0, RejectContext0, VS0),

    {error, {no_route_found, {risk_score_is_too_high, #{
        varset := VS0,
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(3), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), {'PaymentsProvisionTerms', category}},
            {?prv(1), {'PaymentsProvisionTerms', payment_tool}}
        ],
        rejected_routes := []
    }}}} = Result0,

    VS1 = VS0#{
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}}
    },
    {Routes1, RejectContext1} = hg_routing:gather_routes(payment, PaymentInstitution, VS1, Revision),
    FailRatedRoutes1 = hg_routing:gather_fail_rates(Routes1),
    Result1          = hg_routing:choose_route(FailRatedRoutes1, RejectContext1, VS1),
    {error, {no_route_found, {risk_score_is_too_high, #{
        varset := VS1,
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(2), {'PaymentsProvisionTerms', category}},
            {?prv(1), {'PaymentsProvisionTerms', payment_tool}}
        ],
        rejected_routes := [{?prv(3), ?trm(10), {'Terminal', risk_coverage}}]}
    }}} = Result1,
    hg_context:cleanup(),
    ok.

-spec no_route_found_for_payment(config()) -> test_return().
no_route_found_for_payment(_C) ->
    ok = hg_context:save(hg_context:create()),
    VS0 = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {bank_card, #domain_BankCard{}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes0, RejectContext0} = hg_routing:gather_routes(payment, PaymentInstitution, VS0, Revision),
    {[], #{
        rejected_providers := [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(3), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), {'PaymentsProvisionTerms', category}},
            {?prv(1), {'PaymentsProvisionTerms', payment_tool}}
        ]}
    } = {Routes0, RejectContext0},

    FailRatedRoutes0 = hg_routing:gather_fail_rates(Routes0),
    [] = FailRatedRoutes0,

    Result0 = {error, {no_route_found, {unknown, #{
        varset => VS0,
        rejected_providers => [
            {?prv(4), {'PaymentsProvisionTerms', currency}},
            {?prv(3), {'PaymentsProvisionTerms', payment_tool}},
            {?prv(2), {'PaymentsProvisionTerms', category}},
            {?prv(1), {'PaymentsProvisionTerms', payment_tool}}
        ],
        rejected_routes => []
    }}}},

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RejectContext0, VS0),

    VS1 = VS0#{
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}}
    },

    {Routes1, RejectContext1} = hg_routing:gather_routes(payment, PaymentInstitution, VS1, Revision),
    FailRatedRoutes1 = hg_routing:gather_fail_rates(Routes1),

    {ok, #domain_PaymentRoute{
        provider = ?prv(3),
        terminal = ?trm(10)
    }, _Meta} = hg_routing:choose_route(FailRatedRoutes1, RejectContext1, VS1),
    hg_context:cleanup(),
    ok.

-spec prefer_alive(config()) -> test_return().
prefer_alive(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RejectContext} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(Routes),

    Alive   = {alive,   0.0},
    Dead    = {dead,    1.0},
    Normal  = {normal,  0.0},

    ProviderStatuses0 = [{Alive, Normal}, {Dead,  Normal}, {Dead, Normal}],
    ProviderStatuses1 = [{Dead,  Normal}, {Alive, Normal}, {Dead, Normal}],
    ProviderStatuses2 = [{Dead,  Normal}, {Dead, Normal},  {Alive, Normal}],

    FailRatedRoutes0  = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses0),
    FailRatedRoutes1  = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses1),
    FailRatedRoutes2  = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses2),

    Result0           = hg_routing:choose_route(FailRatedRoutes0, RejectContext, VS),
    Result1           = hg_routing:choose_route(FailRatedRoutes1, RejectContext, VS),
    Result2           = hg_routing:choose_route(FailRatedRoutes2, RejectContext, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(200)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(201)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(202)}, Meta2} = Result2,

    #{reject_reason := availability_condition} = Meta0,
    false = maps:is_key(reject_reason, Meta1),
    #{reject_reason := availability_condition} = Meta2,

    ok.

-spec prefer_normal_conversion(config()) -> test_return().
prefer_normal_conversion(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    Alive   = {alive,   0.0},
    Normal  = {normal,  0.0},
    Lacking = {lacking, 1.0},

    ProviderStatuses0   = [{Alive, Normal},  {Alive, Lacking}, {Alive, Lacking}],
    ProviderStatuses1   = [{Alive, Lacking}, {Alive, Normal},  {Alive, Lacking}],
    ProviderStatuses2   = [{Alive, Lacking}, {Alive, Lacking}, {Alive, Normal}],
    FailRatedRoutes0 = lists:zip3(Providers, TerminalData, ProviderStatuses0),
    FailRatedRoutes1 = lists:zip3(Providers, TerminalData, ProviderStatuses1),
    FailRatedRoutes2 = lists:zip3(Providers, TerminalData, ProviderStatuses2),

    Result0 = hg_routing:choose_route(FailRatedRoutes0, RC, VS),
    Result1 = hg_routing:choose_route(FailRatedRoutes1, RC, VS),
    Result2 = hg_routing:choose_route(FailRatedRoutes2, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(200)}, Meta0} = Result0,
    {ok, #domain_PaymentRoute{provider = ?prv(201)}, Meta1} = Result1,
    {ok, #domain_PaymentRoute{provider = ?prv(202)}, Meta2} = Result2,

    #{reject_reason := conversion_condition} = Meta0,
    false = maps:is_key(reject_reason, Meta1),
    #{reject_reason := conversion_condition} = Meta2,

    ok.

-spec prefer_better_risk_score(config()) -> test_return().
prefer_better_risk_score(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{dead, 1.0}, {normal, 0.0}}, {{alive, 0.3}, {normal, 0.3}}, {{alive, 0.0}, {normal, 0.0}}],
    FailRatedRoutes  = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(201)}, Meta} = Result,
    false = maps:is_key(reject_reason, Meta),

    ok.

-spec prefer_higher_availability(config()) -> test_return().
prefer_higher_availability(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {ProviderRefs, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.5}, {normal, 0.5}}, {{alive, 0.6}, {normal, 0.5}}, {{dead, 0.8}, {lacking, 1.0}}],
    FailRatedRoutes  = lists:zip3(ProviderRefs, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(200)}, #{reject_reason := availability}} = Result,

    ok.

-spec prefer_higher_conversion(config()) -> test_return().
prefer_higher_conversion(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.5}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.5}}, {{dead, 0.8}, {lacking, 1.0}}],
    FailRatedRoutes  = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(200)}, #{reject_reason := conversion}} = Result,

    ok.

-spec prefer_weight_over_availability(config()) -> test_return().
prefer_weight_over_availability(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"54321">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.3}}, {{alive, 0.5}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes  = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(201)}, _Meta} = Result,

    ok.

-spec prefer_weight_over_conversion(config()) -> test_return().
prefer_weight_over_conversion(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"54321">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {Routes, RC} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),

    {Providers, TerminalData} = lists:unzip(Routes),

    ProviderStatuses = [{{alive, 0.3}, {normal, 0.3}}, {{alive, 0.3}, {normal, 0.5}}, {{alive, 0.3}, {normal, 0.3}}],
    FailRatedRoutes  = lists:zip3(Providers, TerminalData, ProviderStatuses),

    Result = hg_routing:choose_route(FailRatedRoutes, RC, VS),

    {ok, #domain_PaymentRoute{provider = ?prv(201)}, _Meta} = Result,

    ok.

%%% Terminal priority tests

-spec terminal_priority_for_shop(config()) -> test_return().
terminal_priority_for_shop(C) ->
    ok = hg_context:save(hg_context:create()),
    {ok, #domain_PaymentRoute{
        provider = ?prv(300),
        terminal = ?trm(111)
    }, _Meta0} = terminal_priority_for_shop(?dummy_party_id, ?dummy_shop_id, C),
    {ok, #domain_PaymentRoute{
        provider = ?prv(300),
        terminal = ?trm(222)
    }, _Meta1} = terminal_priority_for_shop(?dummy_party_id, ?dummy_another_shop_id, C),
    ok = hg_context:cleanup().

terminal_priority_for_shop(PartyID, ShopID, _C) ->
    VS = #{
        category     => ?cat(1),
        currency     => ?cur(<<"RUB">>),
        cost         => ?cash(1000, <<"RUB">>),
        payment_tool => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id     => PartyID,
        shop_id      => ShopID,
        risk_score   => low,
        flow         => instant
    },
    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    {Routes, RejectContext} = hg_routing:gather_routes(payment, PaymentInstitution, VS, Revision),
    FailRatedRoutes = hg_routing:gather_fail_rates(Routes),
    hg_routing:choose_route(FailRatedRoutes, RejectContext, VS).

%%% Domain config fixtures

routing_with_fail_rate_fixture(Revision) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(200),
                    ?prv(201),
                    ?prv(202)
                ])}
            }}
        },
        {terminal, #domain_TerminalObject{
            ref = ?trm(111),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(222),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = high
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(200),
            data = #domain_Provider{
                name = <<"Biba">>,
                description = <<"Payment terminal provider">>,
                terminal = {decisions, [
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"12345">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 111} ]}
                    },
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"54321">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 111} ]}
                    }
                ]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"biba">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(201),
            data = #domain_Provider{
                name = <<"Boba">>,
                description = <<"Payment terminal provider">>,
                terminal = {decisions, [
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"12345">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 111} ]}
                    },
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"54321">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 111, priority = 1005} ]}
                    }
                ]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"biba">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(202),
            data = #domain_Provider{
                name = <<"Buba">>,
                description = <<"Payment terminal provider">>,
                terminal = {decisions, [
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"12345">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 222} ]}
                    },
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{ id = <<"54321">> }}
                        },
                        then_ = {value, [ #domain_ProviderTerminalRef{id = 111} ]}
                    }
                ]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"buba">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }}
    ].

construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ?ordset([
                ?cat(1)
            ])},
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(<<"DEPRIVED ONE">>, undefined),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ?ordset([
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = mastercard,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = jcb,
                            has_cvv = true
                        }),
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = false
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true,
                            token_provider = applepay
                        })
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     10, <<"RUB">>)},
                        {exclusive, ?cash(420000000, <<"RUB">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, operation_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = visa,
                        has_cvv = true
                    }),
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = mastercard,
                        has_cvv = true
                    })
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 10}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = visa,
                        has_cvv = true
                    }),
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = mastercard,
                        has_cvv = true
                    })
                ])},
                fees = {value, [
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?fixed(100, <<"RUB">>)
                    )
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ = {value, ?cashrng(
                                {inclusive, ?cash(      1000, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    ]}

                }
            }
        },
        recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = visa,
                    has_cvv = true
                }),
                ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = mastercard,
                    has_cvv = true
                })
            ])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ?ordset([
                ?cat(2),
                ?cat(3),
                ?cat(4),
                ?cat(5),
                ?cat(6)
            ])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = visa,
                    has_cvv = true
                }),
                ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = mastercard,
                    has_cvv = true
                })
            ])},
            cash_limit = {decisions, [
                % проверяем, что условие никогда не отрабатывает
                #domain_CashLimitDecision {
                    if_ = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                        definition = {empty_cvv_is, true}
                    }}}},
                    then_ = {value,
                        ?cashrng(
                            {inclusive, ?cash(0, <<"RUB">>)},
                            {inclusive, ?cash(0, <<"RUB">>)}
                        )
                    }
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       10, <<"RUB">>)},
                        {exclusive, ?cash(  4200000, <<"RUB">>)}
                    )}
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(      200, <<"USD">>)},
                        {exclusive, ?cash(   313370, <<"USD">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, operation_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, operation_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = visa,
                    has_cvv = true
                }),
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                    payment_system = mastercard,
                    has_cvv = true
                })
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 3}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = visa,
                        has_cvv = true
                    }),
                    ?pmt(bank_card, #domain_BankCardPaymentMethod{
                        payment_system = mastercard,
                        has_cvv = true
                    })
                ])},
                fees = {value, [
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash( 1000, <<"RUB">>)},
                        {exclusive, ?cash(40000, <<"RUB">>)}
                    )}
                }
            }
        }
    },
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),
        hg_ct_fixture:construct_currency(?cur(<<"EUR">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        hg_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),
        hg_ct_fixture:construct_category(?cat(4), <<"Offliner">>, live),
        hg_ct_fixture:construct_category(?cat(5), <<"Timeouter">>, live),
        hg_ct_fixture:construct_category(?cat(6), <<"MachineFailer">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, #domain_BankCardPaymentMethod{
            payment_system = visa,
            has_cvv = true
        })),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, #domain_BankCardPaymentMethod{
            payment_system = mastercard,
            has_cvv = true
        })),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, #domain_BankCardPaymentMethod{
            payment_system = jcb,
            has_cvv = true
        })),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(digital_wallet, qiwi)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, #domain_BankCardPaymentMethod{
            payment_system = visa,
            has_cvv = false
        })),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, #domain_BankCardPaymentMethod{
            payment_system = visa,
            has_cvv = true,
            token_provider = applepay
        })),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),
        hg_ct_fixture:construct_inspector(?insp(3), <<"Fatalist">>, ?prx(2), #{<<"risk_score">> => <<"fatal">>}),
        hg_ct_fixture:construct_inspector(?insp(4), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}, low),
        hg_ct_fixture:construct_inspector(?insp(5), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"timeout">>}, low),
        hg_ct_fixture:construct_inspector(?insp(6), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),
        hg_ct_fixture:construct_contract_template(?tmpl(3), ?trms(3)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_system_account_set(?sas(2)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3),
                    ?prv(4)
                ])},

                % TODO do we realy need this decision hell here?
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(4)}},
                                then_ = {value, ?insp(4)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(        0, <<"RUB">>)},
                                    {exclusive, ?cash(   500000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(   500000, <<"RUB">>)},
                                    {exclusive, ?cash(100000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash( 100000000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(3)}
                            }
                        ]}
                    }
                ]},
                residences = [],
                realm = test
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3)
                ])},
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(4)}},
                                then_ = {value, ?insp(4)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(5)}},
                                then_ = {value, ?insp(5)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(6)}},
                                then_ = {value, ?insp(6)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(        0, <<"RUB">>)},
                                    {exclusive, ?cash(   500000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(   500000, <<"RUB">>)},
                                    {exclusive, ?cash(100000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash( 100000000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(3)}
                            }
                        ]}
                    }
                ]},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {decisions, [
                    #domain_ExternalAccountSetDecision{
                        if_ = {condition, {party, #domain_PartyCondition{
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
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TestTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = DefaultTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = []
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?prvtrm(1)
                ])},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = mastercard,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = jcb,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = false
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true,
                            token_provider = applepay
                        })
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, visa}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(18, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, mastercard}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(19, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, jcb}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system, #domain_PaymentSystemCondition{
                                    payment_system_is = visa,
                                    token_provider_is = applepay
                                }}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        }
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                    definition = {payment_system_is, visa}
                                }}}},
                                then_ = {value, ?hold_lifetime(12)}
                            }
                        ]}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        })
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
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?prvtrm(6), ?prvtrm(7)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2),
                        ?cat(4),
                        ?cat(5),
                        ?cat(6)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        })
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        )
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                risk_coverage = low,
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        })
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash( 5000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {external, outcome},
                            ?fixed(20, <<"RUB">>),
                            <<"Assist fee">>
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(7),
            data = #domain_Terminal{
                name = <<"Terminal 7">>,
                description = <<"Terminal 7">>,
                risk_coverage = high
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = #domain_Provider{
                name = <<"Crovider">>,
                description = <<"Payment terminal provider">>,
                terminal = {value, [?prvtrm(10)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"crovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>),
                        ?cur(<<"EUR">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = visa,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = mastercard,
                            has_cvv = true
                        }),
                        ?pmt(bank_card, #domain_BankCardPaymentMethod{
                            payment_system = jcb,
                            has_cvv = true
                        }),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(10),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(4),
            data = #domain_Provider{
                name = <<"Zrovider">>,
                description = <<"Non-configured provider">>,
                terminal = {value, []},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"zrovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = undefined,
                    categories = undefined,
                    payment_methods = undefined,
                    cash_limit = undefined,
                    cash_flow = undefined
                }
            }
        }}

    ].

terminal_priority_fixture(Revision, _C) ->
    PartyID            = ?dummy_party_id,
    ShopID             = ?dummy_shop_id,
    AnotherShopID      = ?dummy_another_shop_id,
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(300)
                ])}
            }}
        },
        {terminal, #domain_TerminalObject{
            ref = ?trm(111),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(222),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(300),
            data = #domain_Provider{
                name = <<"Biba">>,
                description = <<"Payment terminal provider">>,
                terminal = {decisions, [
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{
                                id = PartyID,
                                definition = {shop_is, ShopID}}
                            }
                        },
                        then_ = {value, [
                            #domain_ProviderTerminalRef{id = 111, priority = 10},
                            #domain_ProviderTerminalRef{id = 222, priority = 5}
                        ]}
                    },
                    #domain_TerminalDecision{
                        if_ = {condition,
                            {party, #domain_PartyCondition{
                                id = PartyID,
                                definition = {shop_is, AnotherShopID}}
                            }
                        },
                        then_ = {value, [
                            #domain_ProviderTerminalRef{id = 111, priority = 5},
                            #domain_ProviderTerminalRef{id = 222, priority = 10}
                        ]}
                    },
                    #domain_TerminalDecision{
                        if_ = {constant, true},
                        then_ = {value, []}
                    }
                ]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"biba">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }}
    ].
