-module(pm_ruleset).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

%% API
-export([reduce_payment_routing_ruleset/3]).

-define(const(Bool), {constant, Bool}).

-type payment_routing_ruleset() :: dmsl_domain_thrift:'PaymentRoutingRuleset'().
-type varset()                  :: pm_selector:varset().
-type domain_revision()         :: pm_domain:revision().

-spec reduce_payment_routing_ruleset(payment_routing_ruleset(), varset(), domain_revision()) ->
    payment_routing_ruleset().

reduce_payment_routing_ruleset(RuleSet, VS, DomainRevision) ->
    RuleSet#domain_PaymentRoutingRuleset{
        decisions = reduce_payment_routing_decisions(RuleSet#domain_PaymentRoutingRuleset.decisions, VS, DomainRevision)
    }.

reduce_payment_routing_decisions({Type, []}, _, _) ->
    {Type, []};
reduce_payment_routing_decisions({delegates, Delegates}, VS, Rev) ->
    reduce_payment_routing_delegates(Delegates, VS, Rev);
reduce_payment_routing_decisions({candidates, Candidates}, VS, Rev) ->
    reduce_payment_routing_candidates(Candidates, VS, Rev).

reduce_payment_routing_delegates([D | Delegates], VS, Rev) ->
    Predicate = D#domain_PaymentRoutingDelegate.allowed,
    RuleSetRef = D#domain_PaymentRoutingDelegate.ruleset,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_payment_routing_delegates(Delegates, VS, Rev);
        ?const(true) ->
            #domain_PaymentRoutingRuleset{
                decisions = Decisions
            } = get_payment_routing_ruleset(RuleSetRef, Rev),
            reduce_payment_routing_decisions(Decisions, VS, Rev);
        _ ->
            logger:warning(
                "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~n Varset:~n~p",
                [Predicate, VS]
            ),
            {delegates, [D | Delegates]}
    end.

reduce_payment_routing_candidates(Candidates, VS, Rev) ->
    {candidates, lists:foldl(
        fun(C, AccIn) ->
            Predicate = C#domain_PaymentRoutingCandidate.allowed,
            case pm_selector:reduce_predicate(Predicate, VS, Rev) of
                ?const(false) ->
                    AccIn;
                ?const(true) = ReducedPredicate ->
                    ReducedCandidate = C#domain_PaymentRoutingCandidate{
                        allowed = ReducedPredicate
                    },
                    [ReducedCandidate | AccIn];
                _ ->
                    logger:warning(
                        "Routing rule misconfiguration, can't reduce decision. Predicate: ~p~nVarset:~n~p",
                        [Predicate, VS]
                    ),
                    [C | AccIn]
            end
        end,
        [], Candidates)}.

get_payment_routing_ruleset(RuleSetRef, DomainRevision) ->
    pm_domain:get(DomainRevision, {payment_routing_rules, RuleSetRef}).
