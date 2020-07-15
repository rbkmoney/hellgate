-module(hg_routing_rule).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([gather_routes/4]).

%%
-define(const(Bool), {constant, Bool}).

-type route_predestination() :: hg_routing:route_predestination().
-type payment_institution()  :: dmsl_domain_thrift:'PaymentInstitution'().
-type non_fail_rated_route() :: hg_routing:non_fail_rated_route().
-type reject_context()       :: hg_routing:reject_context().

-spec gather_routes(
    route_predestination(),
    payment_institution(),
    pm_selector:varset(),
    hg_domain:revision()
) -> {[non_fail_rated_route()], reject_context()}.

gather_routes(_, #domain_PaymentInstitution{payment_routing = undefined}, VS, _) ->
    {[], #{varset => VS, rejected_providers => [], reject_routes => []}};
gather_routes(Predestination, PaymentInstitution, VS, Revision) ->
    PaymentRouting = PaymentInstitution#domain_PaymentInstitution.payment_routing,
    RuleSet = get_rule_set(PaymentRouting#domain_PaymentRouting.policies, Revision),
    RuleSetDeny = get_rule_set(PaymentRouting#domain_PaymentRouting.prohibitions, Revision),
    Candidates = reduce(RuleSet, VS, Revision),
    RatedRoutes = collect_routes(Predestination, Candidates, VS, Revision),
    Prohibitions = get_table_prohibitions(RuleSetDeny, VS, Revision),
    filter_routes(RatedRoutes, Prohibitions).

get_table_prohibitions(RuleSetDeny, VS, Revision) ->
    Candidates = reduce(RuleSetDeny, VS, Revision),
    lists:foldl(fun(C, AccIn) ->
        AccIn#{get_terminal_ref(C) => get_description(C)}
    end, #{}, Candidates).

reduce(RuleSet, VS, Revision) ->
    #domain_PaymentRoutingRuleset{
        decisions = Decisions
    } = RuleSet,
    reduce_decisions(Decisions, VS, Revision).

reduce_decisions({_, []}, _, _) ->
    [];
reduce_decisions({delegates, Delegates}, VS, Rev) ->
    reduce_delegates_decision(Delegates, VS, Rev);
reduce_decisions({candidates, Candidates}, VS, Rev) ->
    reduce_candidates_decision(Candidates, VS, Rev).

reduce_delegates_decision([D | Delegates], VS, Rev) ->
    Predicate = D#domain_PaymentRoutingDelegate.allowed,
    RuleSetRef = D#domain_PaymentRoutingDelegate.ruleset,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_delegates_decision(Delegates, VS, Rev);
        ?const(true) ->
            reduce(get_rule_set(RuleSetRef, Rev), VS, Rev);
        _ ->
            logger:warning(
                "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~n Varset:~n~p",
                [Predicate, VS]
            ),
            []
    end.

reduce_candidates_decision(Candidates, VS, Rev) ->
    lists:foldl(fun(C, AccIn) ->
        Predicate = C#domain_PaymentRoutingCandidate.allowed,
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
    end, [], Candidates).

collect_routes(Predestination, Candidates, VS, Revision) ->
    lists:foldl(
        fun(Candidate, {Accepted, Rejected}) ->
            #domain_PaymentRoutingCandidate{
                terminal = TerminalRef,
                priority = Priority,
                weight = Weight
            } = Candidate,
            {#domain_Terminal{provider_ref = ProviderRef}, Provider} = get_route(TerminalRef, Revision),
            try
                {_, Terminal} = hg_routing:acceptable_terminal(Predestination, TerminalRef, Provider, VS, Revision),
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
    lists:foldl(fun(Route, {AccIn, RejectedIn}) ->
        {{ProviderRef, _}, {TerminalRef, _, _}} = Route,
        case maps:find(TerminalRef, Prohibitions) of
            error ->
                {[Route | AccIn], RejectedIn};
            {ok, Description} ->
                RejectedOut = [{ProviderRef, TerminalRef, {'RoutingRule', Description}} | RejectedIn],
                {AccIn, RejectedOut}
        end
    end, {[], Rejected}, Routes).

get_route(TerminalRef, Revision) ->
    Terminal = #domain_Terminal{
        provider_ref = ProviderRef
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    {Terminal, hg_domain:get(Revision, {provider, ProviderRef})}.

get_rule_set(RuleSetRef, Revision) ->
    hg_domain:get(Revision, {payment_routing_rules, RuleSetRef}).

get_terminal_ref(Candidate) ->
    Candidate#domain_PaymentRoutingCandidate.terminal.

get_description(Candidate) ->
    Candidate#domain_PaymentRoutingCandidate.description.
