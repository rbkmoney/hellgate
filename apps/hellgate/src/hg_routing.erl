%%% Naïve routing oracle

-module(hg_routing).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("fault_detector_proto/include/fd_proto_fault_detector_thrift.hrl").

-export([gather_routes/4]).
-export([choose_route/1]).
-export([get_payment_terms/3]).

-export([marshal/1]).
-export([unmarshal/1]).

-export([get_logger_metadata/2]).

-export([from_payment_route/1]).
-export([new/2]).
-export([new/4]).
-export([to_payment_route/1]).
-export([provider_ref/1]).
-export([terminal_ref/1]).

-export([prepare_log_message/1]).
%% Using in ct
-export([gather_fail_rates/1]).
-export([choose_rated_route/1]).
%%

-include("domain.hrl").

-record(route, {
    provider_ref :: dmsl_domain_thrift:'ProviderRef'(),
    terminal_ref :: dmsl_domain_thrift:'TerminalRef'(),
    weight :: integer(),
    priority :: integer()
}).

-type route() :: #route{}.
-type payment_terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type payment_route() :: dmsl_domain_thrift:'PaymentRoute'().
-type route_predestination() :: payment | recurrent_paytool | recurrent_payment.

-define(rejected(Reason), {rejected, Reason}).

-type rejected_route() :: {provider_ref(), terminal_ref(), Reason :: term()}.

-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

-type fd_service_stats() :: fd_proto_fault_detector_thrift:'ServiceStatistics'().

-type terminal_priority_rating() :: integer().

-type provider_status() :: {availability_status(), conversion_status()}.

-type availability_status() :: {availability_condition(), availability_fail_rate()}.
-type conversion_status() :: {conversion_condition(), conversion_fail_rate()}.

-type availability_condition() :: alive | dead.
-type availability_fail_rate() :: float().

-type conversion_condition() :: normal | lacking.
-type conversion_fail_rate() :: float().

-type condition_score() :: 0 | 1.

-type route_groups_by_priority() :: #{{availability_condition(), terminal_priority_rating()} => [fail_rated_route()]}.

-type fail_rated_route() :: {route(), provider_status()}.

-type scored_route() :: {route_scores(), route()}.

-type route_choice_context() :: #{
    chosen_route => route(),
    preferable_route => route(),
    % Contains one of the field names defined in #route_scores{}
    reject_reason => atom()
}.

-type varset() :: hg_varset:varset().
-type revision() :: hg_domain:revision().

-record(route_scores, {
    availability_condition :: condition_score(),
    conversion_condition :: condition_score(),
    priority_rating :: terminal_priority_rating(),
    random_condition :: integer(),
    availability :: float(),
    conversion :: float()
}).

-type route_scores() :: #route_scores{}.
-type misconfiguration_error() :: {misconfiguration, {routing_decisions, _} | {routing_candidate, _}}.

-export_type([route/0]).
-export_type([route_predestination/0]).

%% Route accessors

-spec new(provider_ref(), terminal_ref()) -> route().
new(ProviderRef, TerminalRef) ->
    #route{
        provider_ref = ProviderRef,
        terminal_ref = TerminalRef,
        weight = ?DOMAIN_CANDIDATE_WEIGHT,
        priority = ?DOMAIN_CANDIDATE_PRIORITY
    }.

-spec new(provider_ref(), terminal_ref(), integer() | undefined, integer()) -> route().
new(ProviderRef, TerminalRef, undefined, Priority) ->
    new(ProviderRef, TerminalRef, ?DOMAIN_CANDIDATE_WEIGHT, Priority);
new(ProviderRef, TerminalRef, Weight, Priority) ->
    #route{
        provider_ref = ProviderRef,
        terminal_ref = TerminalRef,
        weight = Weight,
        priority = Priority
    }.

-spec provider_ref(route()) -> provider_ref().
provider_ref(#route{provider_ref = Ref}) ->
    Ref.

-spec terminal_ref(route()) -> terminal_ref().
terminal_ref(#route{terminal_ref = Ref}) ->
    Ref.

-spec priority(route()) -> integer().
priority(#route{priority = Priority}) ->
    Priority.

-spec weight(route()) -> integer().
weight(#route{weight = Weight}) ->
    Weight.

-spec from_payment_route(payment_route()) -> route().
from_payment_route(Route) ->
    ?route(ProviderRef, TerminalRef) = Route,
    #route{
        provider_ref = ProviderRef,
        terminal_ref = TerminalRef,
        weight = ?DOMAIN_CANDIDATE_WEIGHT,
        priority = ?DOMAIN_CANDIDATE_PRIORITY
    }.

-spec to_payment_route(route()) -> payment_route().
to_payment_route(#route{} = Route) ->
    ?route(provider_ref(Route), terminal_ref(Route)).

-spec set_weight(integer(), route()) -> route().
set_weight(Weight, Route) ->
    Route#route{weight = Weight}.

-spec prepare_log_message(misconfiguration_error()) -> {io:format(), [term()]}.
prepare_log_message({misconfiguration, {routing_decisions, Details}}) ->
    Message = <<"PaymentRoutingDecisions couldn\'t be reduced to candidates">>,
    {"PaymentRoutingDecisions couldn't be reduced to candidates: ~p", [Details]};
prepare_log_message({misconfiguration, {routing_candidate, Candidate}}) ->
    Message = <<"PaymentRoutingCandidate couldn\'t be reduced">>,
    {"~p, ~p", [Message, Candidate]}.

%%

-spec gather_routes(
    route_predestination(),
    payment_institution(),
    varset(),
    revision()
) ->
    {ok, {[route()], [rejected_route()]}}
    | {error, misconfiguration_error()}.
gather_routes(_, #domain_PaymentInstitution{payment_routing_rules = undefined}, _, _) ->
    {ok, {[], []}};
gather_routes(Predestination, #domain_PaymentInstitution{payment_routing_rules = RoutingRules}, VS, Revision) ->
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
        {ok, {Accepted, RejectedRoutes}}
    catch
        throw:{misconfiguration, _Reason} = Error ->
            {error, Error}
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
            throw({misconfiguration, {routing_decisions, Decisions}});
        {candidates, Candidates} ->
            ok = validate_decisions_candidates(Candidates),
            Candidates
    end.

validate_decisions_candidates([]) ->
    ok;
validate_decisions_candidates([#domain_RoutingCandidate{allowed = {constant, true}} | Rest]) ->
    validate_decisions_candidates(Rest);
validate_decisions_candidates([Candidate | _]) ->
    throw({misconfiguration, {routing_candidate, Candidate}}).

collect_routes(Predestination, Candidates, VS, Revision) ->
    lists:foldr(
        fun(Candidate, {Accepted, Rejected}) ->
            #domain_RoutingCandidate{
                terminal = TerminalRef,
                priority = Priority,
                weight = Weight
            } = Candidate,
            % Looks like overhead, we got Terminal only for provider_ref. Maybe we can remove provider_ref from route().
            % https://github.com/rbkmoney/hellgate/pull/583#discussion_r682745123
            #domain_Terminal{
                provider_ref = ProviderRef
            } = hg_domain:get(Revision, {terminal, TerminalRef}),
            try
                true = acceptable_terminal(Predestination, ProviderRef, TerminalRef, VS, Revision),
                Route = new(ProviderRef, TerminalRef, Weight, Priority),
                {[Route | Accepted], Rejected}
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
            TRef = terminal_ref(Route),
            case maps:find(TRef, Prohibitions) of
                error ->
                    {[Route | AccIn], RejectedIn};
                {ok, Description} ->
                    PRef = provider_ref(Route),
                    RejectedOut = [{PRef, TRef, {'RoutingRule', Description}} | RejectedIn],
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

-spec gather_fail_rates([route()]) -> [fail_rated_route()].
gather_fail_rates(Routes) ->
    score_routes_with_fault_detector(Routes).

-spec choose_route([route()]) -> {route(), route_choice_context()}.
choose_route(Routes) ->
    FailRatedRoutes = gather_fail_rates(Routes),
    choose_rated_route(FailRatedRoutes).

-spec choose_rated_route([fail_rated_route()]) -> {route(), route_choice_context()}.
choose_rated_route(FailRatedRoutes) ->
    BalancedRoutes = balance_routes(FailRatedRoutes),
    ScoredRoutes = score_routes(BalancedRoutes),
    {ChosenScoredRoute, IdealRoute} = find_best_routes(ScoredRoutes),
    RouteChoiceContext = get_route_choice_context(ChosenScoredRoute, IdealRoute),
    {_, Route} = ChosenScoredRoute,
    {Route, RouteChoiceContext}.

-spec find_best_routes([scored_route()]) -> {Chosen :: scored_route(), Ideal :: scored_route()}.
find_best_routes([Route]) ->
    {Route, Route};
find_best_routes([First | Rest]) ->
    lists:foldl(
        fun(RouteIn, {CurrentRouteChosen, CurrentRouteIdeal}) ->
            NewRouteIdeal = select_better_route_ideal(RouteIn, CurrentRouteIdeal),
            NewRouteChosen = select_better_route(RouteIn, CurrentRouteChosen),
            {NewRouteChosen, NewRouteIdeal}
        end,
        {First, First},
        Rest
    ).

select_better_route(Left, Right) ->
    max(Left, Right).

select_better_route_ideal(Left, Right) ->
    IdealLeft = set_ideal_score(Left),
    IdealRight = set_ideal_score(Right),
    case select_better_route(IdealLeft, IdealRight) of
        IdealLeft -> Left;
        IdealRight -> Right
    end.

set_ideal_score({RouteScores, PT}) ->
    {RouteScores#route_scores{
            availability_condition = 1,
            availability = 1.0,
            conversion_condition = 1,
            conversion = 1.0
        },
        PT}.

get_route_choice_context({_, SameRoute}, {_, SameRoute}) ->
    #{
        chosen_route => SameRoute
    };
get_route_choice_context({ChosenScores, ChosenRoute}, {IdealScores, IdealRoute}) ->
    #{
        chosen_route => ChosenRoute,
        preferable_route => IdealRoute,
        reject_reason => map_route_switch_reason(ChosenScores, IdealScores)
    }.

-spec get_logger_metadata(route_choice_context(), revision()) -> LoggerFormattedMetadata :: map().
get_logger_metadata(RouteChoiceContext, Revision) ->
    #{route_choice_metadata => format_logger_metadata(RouteChoiceContext, Revision)}.

format_logger_metadata(RouteChoiceContext, Revision) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc ++ format_logger_metadata(K, V, Revision)
        end,
        [],
        RouteChoiceContext
    ).

format_logger_metadata(reject_reason, Reason, _) ->
    [{reject_reason, Reason}];
format_logger_metadata(Meta, Route, Revision) when Meta =:= chosen_route; Meta =:= preferable_route ->
    RouteInfo = add_route_name(Route, Revision),
    [{Meta, maps:to_list(RouteInfo)}].

add_route_name(Route, Revision) ->
    ProviderRef = provider_ref(Route),
    TerminalRef = terminal_ref(Route),
    #domain_Provider{name = PName} = hg_domain:get(Revision, {provider, ProviderRef}),
    #domain_Terminal{name = TName} = hg_domain:get(Revision, {terminal, TerminalRef}),
    genlib_map:compact(#{
        provider_name => PName,
        terminal_name => TName,
        provider_ref => ProviderRef,
        terminal_ref => TerminalRef,
        priority => priority(Route),
        weight => weight(Route)
    }).

map_route_switch_reason(SameScores, SameScores) ->
    unknown;
map_route_switch_reason(RealScores, IdealScores) when
    is_record(RealScores, route_scores); is_record(IdealScores, route_scores)
->
    Zipped = lists:zip(tuple_to_list(RealScores), tuple_to_list(IdealScores)),
    DifferenceIdx = find_idx_of_difference(Zipped),
    lists:nth(DifferenceIdx, record_info(fields, route_scores)).

find_idx_of_difference(ZippedList) ->
    find_idx_of_difference(ZippedList, 0).

find_idx_of_difference([{Same, Same} | Rest], I) ->
    find_idx_of_difference(Rest, I + 1);
find_idx_of_difference(_, I) ->
    I.

-spec balance_routes([fail_rated_route()]) -> [fail_rated_route()].
balance_routes(FailRatedRoutes) ->
    FilteredRouteGroups = lists:foldl(
        fun group_routes_by_priority/2,
        #{},
        FailRatedRoutes
    ),
    balance_route_groups(FilteredRouteGroups).

-spec group_routes_by_priority(fail_rated_route(), Acc :: route_groups_by_priority()) -> route_groups_by_priority().
group_routes_by_priority(FailRatedRoute = {Route, {ProviderCondition, _}}, SortedRoutes) ->
    TerminalPriority = priority(Route),
    Key = {ProviderCondition, TerminalPriority},
    Routes = maps:get(Key, SortedRoutes, []),
    SortedRoutes#{Key => [FailRatedRoute | Routes]}.

-spec balance_route_groups(route_groups_by_priority()) -> [fail_rated_route()].
balance_route_groups(RouteGroups) ->
    maps:fold(
        fun(_Priority, Routes, Acc) ->
            NewRoutes = set_routes_random_condition(Routes),
            NewRoutes ++ Acc
        end,
        [],
        RouteGroups
    ).

set_routes_random_condition(Routes) ->
    Summary = get_summary_weight(Routes),
    Random = rand:uniform() * Summary,
    lists:reverse(calc_random_condition(0.0, Random, Routes, [])).

get_summary_weight(FailRatedRoutes) ->
    lists:foldl(
        fun({Route, _}, Acc) ->
            Weight = weight(Route),
            Acc + Weight
        end,
        0,
        FailRatedRoutes
    ).

calc_random_condition(_, _, [], Routes) ->
    Routes;
calc_random_condition(StartFrom, Random, [FailRatedRoute | Rest], Routes) ->
    {Route, Status} = FailRatedRoute,
    Weight = weight(Route),
    InRange = (Random >= StartFrom) and (Random < StartFrom + Weight),
    case InRange of
        true ->
            NewRoute = set_weight(1, Route),
            calc_random_condition(StartFrom + Weight, Random, Rest, [{NewRoute, Status} | Routes]);
        false ->
            NewRoute = set_weight(0, Route),
            calc_random_condition(StartFrom + Weight, Random, Rest, [{NewRoute, Status} | Routes])
    end.

-spec score_routes([fail_rated_route()]) -> [scored_route()].
score_routes(Routes) ->
    [{score_route(FailRatedRoute), Route} || {Route, _} = FailRatedRoute <- Routes].

score_route({Route, ProviderStatus}) ->
    PriorityRate = priority(Route),
    RandomCondition = weight(Route),
    {AvailabilityStatus, ConversionStatus} = ProviderStatus,
    {AvailabilityCondition, Availability} = get_availability_score(AvailabilityStatus),
    {ConversionCondition, Conversion} = get_conversion_score(ConversionStatus),
    #route_scores{
        availability_condition = AvailabilityCondition,
        conversion_condition = ConversionCondition,
        availability = Availability,
        conversion = Conversion,
        priority_rating = PriorityRate,
        random_condition = RandomCondition
    }.

get_availability_score({alive, FailRate}) -> {1, 1.0 - FailRate};
get_availability_score({dead, FailRate}) -> {0, 1.0 - FailRate}.

get_conversion_score({normal, FailRate}) -> {1, 1.0 - FailRate};
get_conversion_score({lacking, FailRate}) -> {0, 1.0 - FailRate}.

-spec score_routes_with_fault_detector([route()]) -> [fail_rated_route()].
score_routes_with_fault_detector([]) ->
    [];
score_routes_with_fault_detector(Routes) ->
    IDs = build_ids(Routes),
    FDStats = hg_fault_detector_client:get_statistics(IDs),
    [{R, get_provider_status(provider_ref(R), FDStats)} || R <- Routes].

-spec get_provider_status(provider_ref(), [fd_service_stats()]) -> provider_status().
get_provider_status(ProviderRef, FDStats) ->
    AvailabilityServiceID = build_fd_availability_service_id(ProviderRef),
    ConversionServiceID = build_fd_conversion_service_id(ProviderRef),
    AvailabilityStatus = get_provider_availability_status(AvailabilityServiceID, FDStats),
    ConversionStatus = get_provider_conversion_status(ConversionServiceID, FDStats),
    {AvailabilityStatus, ConversionStatus}.

get_provider_availability_status(FDID, Stats) ->
    AvailabilityConfig = genlib_app:env(hellgate, fault_detector_availability, #{}),
    CriticalFailRate = genlib_map:get(critical_fail_rate, AvailabilityConfig, 0.7),
    case lists:keysearch(FDID, #fault_detector_ServiceStatistics.service_id, Stats) of
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} when FailRate >= CriticalFailRate ->
            {dead, FailRate};
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} ->
            {alive, FailRate};
        false ->
            {alive, 0.0}
    end.

get_provider_conversion_status(FDID, Stats) ->
    ConversionConfig = genlib_app:env(hellgate, fault_detector_conversion, #{}),
    CriticalFailRate = genlib_map:get(critical_fail_rate, ConversionConfig, 0.7),
    case lists:keysearch(FDID, #fault_detector_ServiceStatistics.service_id, Stats) of
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} when FailRate >= CriticalFailRate ->
            {lacking, FailRate};
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} ->
            {normal, FailRate};
        false ->
            {normal, 0.0}
    end.

build_ids(Routes) ->
    lists:foldl(fun build_fd_ids/2, [], Routes).

build_fd_ids(Route, IDs) ->
    ProviderRef = provider_ref(Route),
    AvailabilityID = build_fd_availability_service_id(ProviderRef),
    ConversionID = build_fd_conversion_service_id(ProviderRef),
    [AvailabilityID, ConversionID | IDs].

build_fd_availability_service_id(#domain_ProviderRef{id = ID}) ->
    hg_fault_detector_client:build_service_id(adapter_availability, ID).

build_fd_conversion_service_id(#domain_ProviderRef{id = ID}) ->
    hg_fault_detector_client:build_service_id(provider_conversion, ID).

-spec get_payment_terms(payment_route(), varset(), revision()) -> payment_terms() | undefined.
get_payment_terms(?route(ProviderRef, TerminalRef), VS, Revision) ->
    PreparedVS = hg_varset:prepare_varset(VS),
    {Client, Context} = get_party_client(),
    {ok, TermsSet} = party_client_thrift:compute_provider_terminal_terms(
        ProviderRef,
        TerminalRef,
        Revision,
        PreparedVS,
        Client,
        Context
    ),
    TermsSet#domain_ProvisionTermSet.payments.

-spec acceptable_terminal(
    route_predestination(),
    provider_ref(),
    terminal_ref(),
    varset(),
    revision()
) -> true | no_return().
acceptable_terminal(Predestination, ProviderRef, TerminalRef, VS, Revision) ->
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_provider_terminal_terms(
        ProviderRef,
        TerminalRef,
        Revision,
        hg_varset:prepare_varset(VS),
        Client,
        Context
    ),
    case Result of
        {ok, ProvisionTermSet} ->
            check_terms_acceptability(Predestination, ProvisionTermSet, VS);
        {error, #payproc_ProvisionTermSetUndefined{}} ->
            throw(?rejected({'ProvisionTermSet', undefined}))
    end.

%%

get_party_client() ->
    HgContext = hg_context:load(),
    Client = hg_context:get_party_client(HgContext),
    Context = hg_context:get_party_client_context(HgContext),
    {Client, Context}.

check_terms_acceptability(payment, Terms, VS) ->
    acceptable_payment_terms(Terms#domain_ProvisionTermSet.payments, VS);
check_terms_acceptability(recurrent_paytool, Terms, VS) ->
    acceptable_recurrent_paytool_terms(Terms#domain_ProvisionTermSet.recurrent_paytools, VS);
check_terms_acceptability(recurrent_payment, Terms, VS) ->
    % Use provider check combined from recurrent_paytool and payment check
    _ = acceptable_payment_terms(Terms#domain_ProvisionTermSet.payments, VS),
    acceptable_recurrent_paytool_terms(Terms#domain_ProvisionTermSet.recurrent_paytools, VS).

acceptable_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies = CurrenciesSelector,
        categories = CategoriesSelector,
        payment_methods = PMsSelector,
        cash_limit = CashLimitSelector,
        holds = HoldsTerms,
        refunds = RefundsTerms,
        risk_coverage = RiskCoverageSelector
    },
    VS
) ->
    % TODO varsets getting mixed up
    %      it seems better to pass down here hierarchy of contexts w/ appropriate module accessors
    ParentName = 'PaymentsProvisionTerms',
    _ = try_accept_term(ParentName, currency, getv(currency, VS), CurrenciesSelector),
    _ = try_accept_term(ParentName, category, getv(category, VS), CategoriesSelector),
    _ = try_accept_term(ParentName, payment_tool, getv(payment_tool, VS), PMsSelector),
    _ = try_accept_term(ParentName, cost, getv(cost, VS), CashLimitSelector),
    _ = acceptable_holds_terms(HoldsTerms, getv(flow, VS, undefined)),
    _ = acceptable_refunds_terms(RefundsTerms, getv(refunds, VS, undefined)),
    _ = acceptable_risk(ParentName, RiskCoverageSelector, VS),
    %% TODO Check chargeback terms when there will be any
    %% _ = acceptable_chargeback_terms(...)
    true;
acceptable_payment_terms(undefined, _VS) ->
    throw(?rejected({'PaymentsProvisionTerms', undefined})).

acceptable_holds_terms(_Terms, undefined) ->
    true;
acceptable_holds_terms(_Terms, instant) ->
    true;
acceptable_holds_terms(Terms, {hold, Lifetime}) ->
    case Terms of
        #domain_PaymentHoldsProvisionTerms{lifetime = LifetimeSelector} ->
            _ = try_accept_term('PaymentHoldsProvisionTerms', lifetime, Lifetime, LifetimeSelector),
            true;
        undefined ->
            throw(?rejected({'PaymentHoldsProvisionTerms', undefined}))
    end.

acceptable_risk(_ParentName, undefined, _VS) ->
    true;
acceptable_risk(ParentName, Selector, VS) ->
    RiskCoverage = get_selector_value(risk_coverage, Selector),
    RiskScore = getv(risk_score, VS),
    hg_inspector:compare_risk_score(RiskCoverage, RiskScore) >= 0 orelse
        throw(?rejected({ParentName, risk_coverage})).

acceptable_refunds_terms(_Terms, undefined) ->
    true;
acceptable_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{
        partial_refunds = PartialRefundsTerms
    },
    RVS
) ->
    _ = acceptable_partial_refunds_terms(
        PartialRefundsTerms,
        getv(partial, RVS, undefined)
    ),
    true;
acceptable_refunds_terms(undefined, _RVS) ->
    throw(?rejected({'PaymentRefundsProvisionTerms', undefined})).

acceptable_partial_refunds_terms(_Terms, undefined) ->
    true;
acceptable_partial_refunds_terms(
    #domain_PartialRefundsProvisionTerms{cash_limit = CashLimitSelector},
    #{cash_limit := MerchantLimit}
) ->
    ProviderLimit = get_selector_value(cash_limit, CashLimitSelector),
    hg_cash_range:is_subrange(MerchantLimit, ProviderLimit) == true orelse
        throw(?rejected({'PartialRefundsProvisionTerms', cash_limit}));
acceptable_partial_refunds_terms(undefined, _RVS) ->
    throw(?rejected({'PartialRefundsProvisionTerms', undefined})).

%%

acceptable_recurrent_paytool_terms(
    #domain_RecurrentPaytoolsProvisionTerms{
        categories = CategoriesSelector,
        payment_methods = PMsSelector
    },
    VS
) ->
    _ = try_accept_term('RecurrentPaytoolsProvisionTerms', category, getv(category, VS), CategoriesSelector),
    _ = try_accept_term('RecurrentPaytoolsProvisionTerms', payment_tool, getv(payment_tool, VS), PMsSelector),
    true;
acceptable_recurrent_paytool_terms(undefined, _VS) ->
    throw(?rejected({'RecurrentPaytoolsProvisionTerms', undefined})).

try_accept_term(ParentName, Name, Value, Selector) when Selector /= undefined ->
    Values = get_selector_value(Name, Selector),
    test_term(Name, Value, Values) orelse throw(?rejected({ParentName, Name}));
try_accept_term(ParentName, Name, _Value, undefined) ->
    throw(?rejected({ParentName, Name})).

test_term(currency, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(category, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(payment_tool, PT, PMs) ->
    hg_payment_tool:has_any_payment_method(PT, PMs);
test_term(cost, Cost, CashRange) ->
    hg_cash_range:is_inside(Cost, CashRange) == within;
test_term(lifetime, ?hold_lifetime(Lifetime), ?hold_lifetime(Allowed)) ->
    Lifetime =< Allowed.

%%

get_selector_value(Name, Selector) ->
    case Selector of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

getv(Name, VS) ->
    maps:get(Name, VS).

getv(Name, VS, Default) ->
    maps:get(Name, VS, Default).

%% Marshalling

-include("legacy_structures.hrl").
-include("domain.hrl").

-spec marshal(route()) -> hg_msgpack_marshalling:value().
marshal(Route) ->
    marshal(route, Route).

marshal(route, #domain_PaymentRoute{} = Route) ->
    [
        2,
        #{
            <<"provider">> => marshal(provider_ref, Route#domain_PaymentRoute.provider),
            <<"terminal">> => marshal(terminal_ref, Route#domain_PaymentRoute.terminal)
        }
    ];
marshal(provider_ref, #domain_ProviderRef{id = ObjectID}) ->
    marshal(int, ObjectID);
marshal(terminal_ref, #domain_TerminalRef{id = ObjectID}) ->
    marshal(int, ObjectID);
marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) -> payment_route().
unmarshal(Route) ->
    unmarshal(route, Route).

unmarshal(route, [
    2,
    #{
        <<"provider">> := Provider,
        <<"terminal">> := Terminal
    }
]) ->
    #domain_PaymentRoute{
        provider = unmarshal(provider_ref, Provider),
        terminal = unmarshal(terminal_ref, Terminal)
    };
unmarshal(route, [1, ?legacy_route(Provider, Terminal)]) ->
    #domain_PaymentRoute{
        provider = unmarshal(provider_ref_legacy, Provider),
        terminal = unmarshal(terminal_ref_legacy, Terminal)
    };
unmarshal(provider_ref, ObjectID) ->
    #domain_ProviderRef{id = unmarshal(int, ObjectID)};
unmarshal(provider_ref_legacy, ?legacy_provider(ObjectID)) ->
    #domain_ProviderRef{id = unmarshal(int, ObjectID)};
unmarshal(terminal_ref, ObjectID) ->
    #domain_TerminalRef{id = unmarshal(int, ObjectID)};
unmarshal(terminal_ref_legacy, ?legacy_terminal(ObjectID)) ->
    #domain_TerminalRef{id = unmarshal(int, ObjectID)};
unmarshal(_, Other) ->
    Other.

%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).

-spec record_comparsion_test() -> _.
record_comparsion_test() ->
    Bigger =
        {#route_scores{
                availability_condition = 1,
                availability = 0.5,
                conversion_condition = 1,
                conversion = 0.5,
                priority_rating = 1,
                random_condition = 1
            },
            {42, 42}},
    Smaller =
        {#route_scores{
                availability_condition = 0,
                availability = 0.1,
                conversion_condition = 1,
                conversion = 0.5,
                priority_rating = 1,
                random_condition = 1
            },
            {99, 99}},
    Bigger = select_better_route(Bigger, Smaller).

-spec balance_routes_test() -> list().
balance_routes_test() ->
    Status = {{alive, 0.0}, {normal, 0.0}},
    WithWeight = [
        {new(?prv(1), ?trm(1), 1, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 2, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(3), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(4), ?trm(1), 1, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(5), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],

    Result1 = [
        {new(?prv(1), ?trm(1), 1, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(3), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(4), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(5), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],
    Result2 = [
        {new(?prv(1), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 1, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(3), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(4), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(5), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],
    Result3 = [
        {new(?prv(1), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(3), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(4), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(5), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],
    [
        ?assertEqual(Result1, lists:reverse(calc_random_condition(0.0, 0.2, WithWeight, []))),
        ?assertEqual(Result2, lists:reverse(calc_random_condition(0.0, 1.5, WithWeight, []))),
        ?assertEqual(Result3, lists:reverse(calc_random_condition(0.0, 4.0, WithWeight, [])))
    ].

-spec balance_routes_with_default_weight_test() -> list().
balance_routes_with_default_weight_test() ->
    Status = {{alive, 0.0}, {normal, 0.0}},
    Routes = [
        {new(?prv(1), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],
    Result = [
        {new(?prv(1), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status},
        {new(?prv(2), ?trm(1), 0, ?DOMAIN_CANDIDATE_PRIORITY), Status}
    ],
    [
        ?assertEqual(Result, set_routes_random_condition(Routes))
    ].

-endif.
