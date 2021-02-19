-module(hg_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).

%%
-define(const(Bool), {constant, Bool}).

-type route_predestination() :: hg_routing:route_predestination().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type non_fail_rated_route() :: hg_routing:non_fail_rated_route().
-type reject_context() :: hg_routing:reject_context().

-spec gather_routes(
    route_predestination(),
    payment_institution(),
    hg_routing:varset(),
    hg_domain:revision()
) -> {[non_fail_rated_route()], reject_context()}.
gather_routes(_, #domain_PaymentInstitution{payment_routing_rules = undefined} = PayInst, VS, _) ->
    logger:log(
        warning,
        "Payment routing rules is undefined, PaymentInstitution: ~p",
        [PayInst]
    ),
    {[], #{varset => VS, rejected_providers => [], rejected_routes => []}};
gather_routes(Predestination, PaymentInstitution, VS, Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    PaymentRouting = PaymentInstitution#domain_PaymentInstitution.payment_routing_rules,
    Candidates = get_candidates(PaymentRouting#domain_RoutingRules.policies, VS, Revision),
    RatedRoutes = collect_routes(Predestination, Candidates, VS, Revision),
    RuleSetDeny = get_rule_set(PaymentRouting#domain_RoutingRules.prohibitions, Revision),
    Prohibitions = get_table_prohibitions(RuleSetDeny, VS, Revision),
    {Accepted, RejectedRoutes} = filter_routes(RatedRoutes, Prohibitions),
    {Accepted, RejectedContext#{rejected_routes => RejectedRoutes}}.

get_candidates(RoutingRuleRef, VS, Revision) ->
    RuleSet = get_rule_set(RoutingRuleRef, Revision),
    {Candidates, RoutingRuleTrace} = reduce(RuleSet, VS, Revision, [RoutingRuleRef]),
    logger:info("Routing computation trace: ~p. Candidates: ~p", [lists:reverse(RoutingRuleTrace), Candidates]),
    Candidates.

get_table_prohibitions(RuleSetDeny, VS, Revision) ->
    {Candidates, _} = reduce(RuleSetDeny, VS, Revision, []),
    lists:foldl(
        fun(C, AccIn) ->
            AccIn#{get_terminal_ref(C) => get_description(C)}
        end,
        #{},
        Candidates
    ).

reduce(RuleSet, VS, Revision, RoutingRuleTrace) ->
    #domain_RoutingRuleset{
        decisions = Decisions
    } = RuleSet,
    reduce_decisions(Decisions, VS, Revision, RoutingRuleTrace).

reduce_decisions({_, []}, _, _, RoutingRuleTrace) ->
    {[], RoutingRuleTrace};
reduce_decisions({delegates, Delegates}, VS, Rev, Trace) ->
    reduce_delegates_decision(Delegates, VS, Rev, Trace);
reduce_decisions({candidates, C}, VS, Rev, Trace) ->
    Candidates = reduce_candidates_decision(C, VS, Rev),
    {Candidates, Trace}.

reduce_delegates_decision([], _VS, _Rev, Trace) ->
    {[], Trace};
reduce_delegates_decision([D | Delegates], VS, Rev, Trace) ->
    Predicate = D#domain_RoutingDelegate.allowed,
    RuleSetRef = D#domain_RoutingDelegate.ruleset,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_delegates_decision(Delegates, VS, Rev, Trace);
        ?const(true) ->
            reduce(get_rule_set(RuleSetRef, Rev), VS, Rev, [RuleSetRef | Trace]);
        _ ->
            logger:warning(
                "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~n Varset:~n~p",
                [Predicate, VS]
            ),
            {[], Trace}
    end.

reduce_candidates_decision(Candidates, VS, Rev) ->
    lists:foldl(
        fun(C, AccIn) ->
            Predicate = C#domain_RoutingCandidate.allowed,
            case pm_selector:reduce_predicate(Predicate, VS, Rev) of
                ?const(false) ->
                    AccIn;
                ?const(true) ->
                    [C | AccIn];
                _ ->
                    logger:warning(
                        "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~nVarset:~n~p",
                        [Predicate, VS]
                    ),
                    AccIn
            end
        end,
        [],
        Candidates
    ).

collect_routes(Predestination, Candidates, VS, Revision) ->
    lists:foldl(
        fun(Candidate, {Accepted, Rejected}) ->
            #domain_RoutingCandidate{
                terminal = TerminalRef,
                priority = Priority,
                weight = Weight
            } = Candidate,
            #domain_Terminal{
                provider_ref = ProviderRef
            } = hg_domain:get(Revision, {terminal, TerminalRef}),
            Provider = hg_domain:get(Revision, {provider, ProviderRef}),
            try
                {_, Terminal} = hg_routing:acceptable_terminal(Predestination, ProviderRef, TerminalRef, VS, Revision),
                {[{{ProviderRef, Provider}, {TerminalRef, Terminal, {Priority, Weight}}} | Accepted], Rejected}
            catch
                {rejected, Reason} ->
                    {Accepted, [{ProviderRef, TerminalRef, Reason} | Rejected]};
                error:{misconfiguration, Reason} ->
                    {Accepted, [{ProviderRef, TerminalRef, {'Misconfiguration', Reason}} | Rejected]}
            end
        end,
        {[], []},
        Candidates
    ).

filter_routes({Routes, Rejected}, Prohibitions) ->
    lists:foldl(
        fun(Route, {AccIn, RejectedIn}) ->
            {{ProviderRef, _}, {TerminalRef, _, _}} = Route,
            case maps:find(TerminalRef, Prohibitions) of
                error ->
                    {[Route | AccIn], RejectedIn};
                {ok, Description} ->
                    RejectedOut = [{ProviderRef, TerminalRef, {'RoutingRule', Description}} | RejectedIn],
                    {AccIn, RejectedOut}
            end
        end,
        {[], Rejected},
        Routes
    ).

get_rule_set(RuleSetRef, Revision) ->
    hg_domain:get(Revision, {routing_rules, RuleSetRef}).

get_terminal_ref(Candidate) ->
    Candidate#domain_RoutingCandidate.terminal.

get_description(Candidate) ->
    Candidate#domain_RoutingCandidate.description.
