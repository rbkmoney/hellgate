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
    {[], #{varset => VS, rejected_providers => [], rejected_routes => []}};
gather_routes(Predestination, PaymentInstitution, VS, Revision) ->
    RejectedContext = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    PaymentRouting = PaymentInstitution#domain_PaymentInstitution.payment_routing,
    RuleSet = compute_rule_set(PaymentRouting#domain_PaymentRouting.policies, VS, Revision),
    RuleSetDeny = compute_rule_set(PaymentRouting#domain_PaymentRouting.prohibitions, VS, Revision),
    Candidates = get_decisions_candidates(RuleSet),
    RatedRoutes = collect_routes(Predestination, Candidates, VS, Revision),
    Prohibitions = get_table_prohibitions(RuleSetDeny),
    {Accepted, RejectedRoutes} = filter_routes(RatedRoutes, Prohibitions),
    {Accepted, RejectedContext#{rejected_routes => RejectedRoutes}}.

get_table_prohibitions(RuleSetDeny) ->
    Candidates = get_decisions_candidates(RuleSetDeny),
    lists:foldr(fun(C, AccIn) ->
        AccIn#{get_terminal_ref(C) => get_description(C)}
    end, #{}, Candidates).

get_decisions_candidates(RuleSet) ->
    #domain_PaymentRoutingRuleset{
        decisions = Decisions
    } = RuleSet,
    case Decisions of
        {delegates, _Delegates} ->
            error({misconfiguration, {'PaymentRoutingDecisions couldn\'t be reduced to candidates', Decisions}});
        {candidates, Candidates} ->
            ok = validate_decisions_candidates(Candidates),
            Candidates
    end.

validate_decisions_candidates([]) ->
    ok;
validate_decisions_candidates([Candidate | Rest]) ->
    #domain_PaymentRoutingCandidate{
        allowed = Predicate
    } = Candidate,
    case Predicate of
        {constant, true} ->
            validate_decisions_candidates(Rest);
        _ ->
            error({misconfiguration, {'PaymentRoutingCandidate couldn\'t be reduced', Candidate}})
    end.

collect_routes(Predestination, Candidates, VS, Revision) ->
    lists:foldr(
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
    lists:foldr(fun(Route, {AccIn, RejectedIn}) ->
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

compute_rule_set(RuleSetRef, VS, Revision) ->
    {Client, Context} = get_party_client(),
    PreparedVarset = hg_varset:prepare_varset(VS),
    {ok, RuleSet} = party_client_thrift:compute_payment_routing_ruleset(
        RuleSetRef, Revision, PreparedVarset, Client, Context),
    RuleSet.

get_terminal_ref(Candidate) ->
    Candidate#domain_PaymentRoutingCandidate.terminal.

get_description(Candidate) ->
    Candidate#domain_PaymentRoutingCandidate.description.

get_party_client() ->
    HgContext = hg_context:load(),
    Client = hg_context:get_party_client(HgContext),
    Context = hg_context:get_party_client_context(HgContext),
    {Client, Context}.
