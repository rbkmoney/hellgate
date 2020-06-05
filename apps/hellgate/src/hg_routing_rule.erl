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
    CandidatesDeny = marshal(candidates, reduce(RuleSetDeny, VS, Revision)),
    filter_routes(Predestination, RatedRoutes, CandidatesDeny).

reduce(RuleSet, VS, Revision) ->
    #domain_PaymentRoutingRuleset{
        decisions = Decisions
    } = RuleSet,
    reduce_decisions(Decisions, VS, Revision).

reduce_decisions({_, []}, _, _) ->
    [];
reduce_decisions({delegates, [D | Delegates]}, VS, Rev) ->
    #domain_PaymentRoutingDelegate{
        allowed = Predicate,
        ruleset = RuleSetRef
    } = D,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_decisions({delegates, Delegates}, VS, Rev);
        ?const(true) ->
            case reduce(get_rule_set(RuleSetRef, Rev), VS, Rev) of
                [] ->
                   reduce_decisions({delegates, Delegates}, VS, Rev);
                Candidates ->
                    Candidates ++ reduce_decisions({delegates, Delegates}, VS, Rev)
            end;
        _ ->
            logger:warning("Misconfiguration routing rule, can't reduce decision: ~p~nVarset:~n~p", [Predicate, VS]),
            reduce_decisions({delegates, Delegates}, VS, Rev)
    end;
reduce_decisions({candidates, [C | Candidates]}, VS, Rev) ->
    #domain_PaymentRoutingCandidate{
        allowed  = Predicate
    } = C,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_decisions({candidates, Candidates}, VS, Rev);
        ?const(true) ->
            [C | reduce_decisions({candidates, Candidates}, VS, Rev)];
        _ ->
            logger:warning("Misconfiguration routing rule, can't reduce decision: ~p~nVarset:~n~p", [Predicate, VS]),
            reduce_decisions({candidates, Candidates}, VS, Rev)
    end.

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
                Terminal = hg_routing:acceptable_terminal(Predestination, TerminalRef, Provider, VS, Revision),
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

%%  Does filter for recurrent_payment only?
filter_routes(_Predestination, {Routes, Rejected}, CandidatesDeny) ->
    lists:foldl(fun(Route, {AccIn, RejectedIn}) ->
        {{ProviderRef, _}, {TerminalRef, _, _}} = Route,
        case lists:keyfind(TerminalRef, 1, CandidatesDeny) of
            false ->
                {[Route | AccIn], RejectedIn};
            {_, Description, _} ->
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
get_priority(Candidate) ->
    {
        Candidate#domain_PaymentRoutingCandidate.priority,
        Candidate#domain_PaymentRoutingCandidate.weight
    }.

marshal(candidates, Candidates) ->
    lists:foldl(fun(C, AccIn) ->
        [{
            get_terminal_ref(C),
            get_description(C),
            get_priority(C)
        } | AccIn]
    end, [], Candidates).
