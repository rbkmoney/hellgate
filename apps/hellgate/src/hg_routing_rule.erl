-module(hg_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).

%%
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
gather_routes(Predestination, #domain_PaymentInstitution{payment_routing_rules = RoutingRules}, VS, Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    #domain_RoutingRules{
        policies = Policies,
        prohibitions = Prohibitions
    } = RoutingRules,
    try
        Candidates = get_candidates(Policies, VS, Revision),
        {Accepted, RejectedRoutes} = filter_routes(
            collect_routes(Predestination, Candidates, VS, Revision),
            get_table_prohibitions(Prohibitions, VS, Revision)
        ),
        {Accepted, RejectedContext#{rejected_routes => RejectedRoutes}}
    catch
        error:{misconfiguration, Reason} ->
            {[], RejectedContext#{error => Reason}}
    end.

get_table_prohibitions(Prohibitions, VS, Revision) ->
    RuleSetDeny = compute_rule_set(Prohibitions, VS, Revision),
    lists:foldr(
        fun(#domain_RoutingCandidate{terminal = K, description = V}, AccIn) ->
            AccIn#{K => V}
        end,
        #{},
        get_decisions_candidates(RuleSetDeny)
    ).

get_candidates(RoutingRule, VS, Revision) ->
    get_decisions_candidates(
        compute_rule_set(RoutingRule, VS, Revision)
    ).

get_decisions_candidates(#domain_RoutingRuleset{decisions = Decisions}) ->
    case Decisions of
        {delegates, _Delegates} ->
            error({misconfiguration, {'PaymentRoutingDecisions couldn\'t be reduced to candidates', Decisions}});
        {candidates, Candidates} ->
            ok = validate_decisions_candidates(Candidates),
            Candidates
    end.

validate_decisions_candidates([]) ->
    ok;
validate_decisions_candidates([#domain_RoutingCandidate{allowed = {constant, true}} | Rest]) ->
    validate_decisions_candidates(Rest);
validate_decisions_candidates([Candidate | _]) ->
    error({misconfiguration, {'PaymentRoutingCandidate couldn\'t be reduced', Candidate}}).

collect_routes(Predestination, Candidates, VS, Revision) ->
    lists:foldr(
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
    lists:foldr(
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

compute_rule_set(RuleSetRef, VS, Revision) ->
    Ctx = hg_context:load(),
    {ok, RuleSet} = party_client_thrift:compute_routing_ruleset(
        RuleSetRef,
        Revision,
        hg_varset:prepare_varset(VS),
        hg_context:get_party_client(Ctx),
        hg_context:get_party_client_context(Ctx)
    ),
    RuleSet.
