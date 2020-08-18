%%% Naïve routing oracle

-module(hg_routing).
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("fault_detector_proto/include/fd_proto_fault_detector_thrift.hrl").

-export([gather_routes/4]).
-export([gather_fail_rates/1]).
-export([choose_route/3]).

-export([get_payments_terms/2]).
-export([get_reduced_payments_terms/3]).
-export([get_rec_paytools_terms/2]).

-export([acceptable_terminal/5]).

-export([marshal/1]).
-export([unmarshal/1]).

-export([get_logger_metadata/1]).

%%

-include("domain.hrl").

-type terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'()
               | dmsl_domain_thrift:'RecurrentPaytoolsProvisionTerms'()
               | undefined.

-type payment_institution()  :: dmsl_domain_thrift:'PaymentInstitution'().
-type route()                :: dmsl_domain_thrift:'PaymentRoute'().
-type route_predestination() :: payment | recurrent_paytool | recurrent_payment.

-define(rejected(Reason), {rejected, Reason}).

-type reject_context() :: #{
    varset              := pm_selector:varset(),
    rejected_providers  := list(rejected_provider()),
    rejected_routes     := list(rejected_route())
}.

-type rejected_provider() :: {provider_ref(), Reason :: term()}.
-type rejected_route()    :: {provider_ref(), terminal_ref(), Reason :: term()}.

-type provider()     :: dmsl_domain_thrift:'Provider'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal()     :: dmsl_domain_thrift:'Terminal'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

-type provider_terminal_ref() :: dmsl_domain_thrift:'ProviderTerminalRef'().

-type fd_service_stats()    :: fd_proto_fault_detector_thrift:'ServiceStatistics'().

-type non_fail_rated_route() :: {provider_with_ref(), weighted_terminal()}.
-type fail_rated_route()     :: {provider_with_ref(), weighted_terminal(), provider_status()}.

-type provider_with_ref() :: {provider_ref(), provider()}.

-type unweighted_terminal() :: {terminal_ref(), terminal()}.
-type weighted_terminal()   :: {terminal_ref(), terminal(), terminal_priority()}.
-type terminal_priority()   :: {terminal_priority_rating(), terminal_priority_weight()}.

-type terminal_priority_rating()   :: integer().
-type terminal_priority_weight()   :: integer().

-type provider_status()         :: {availability_status(), conversion_status()}.

-type availability_status()     :: {availability_condition(), availability_fail_rate()}.
-type conversion_status()       :: {conversion_condition(), conversion_fail_rate()}.

-type availability_condition()  :: alive | dead.
-type availability_fail_rate()  :: float().

-type conversion_condition()    :: normal | lacking.
-type conversion_fail_rate()    :: float().

-type condition_score() :: 0 | 1.

-type scored_route() :: {route_scores(), non_fail_rated_route()}.

-type route_groups_by_priority() :: #{{availability_condition(), terminal_priority_rating()} => [fail_rated_route()]}.

-type route_info() :: #{
    provider_ref => integer(),
    provider_name => binary(),
    terminal_ref => integer(),
    terminal_name => binary()
}.

-type route_choice_meta() :: #{
    chosen_route => route_info(),
    preferable_route => route_info(),
    reject_reason => atom() % Contains one of the field names defined in #route_scores{}
}.

-record(route_scores, {
    availability_condition  :: condition_score(),
    conversion_condition    :: condition_score(),
    priority_rating         :: terminal_priority_rating(),
    random_condition        :: integer(),
    availability            :: float(),
    conversion              :: float()
}).

-type route_scores() :: #route_scores{}.

-export_type([route_predestination/0]).
-export_type([non_fail_rated_route/0]).
-export_type([reject_context/0]).

-spec gather_routes(
    route_predestination(),
    payment_institution(),
    pm_selector:varset(),
    hg_domain:revision()
) ->
    {[non_fail_rated_route()], reject_context()}.
gather_routes(Predestination, PaymentInstitution, VS, Revision) ->
    RejectContext0 = #{
        varset => VS,
        rejected_providers => [],
        rejected_routes => []
    },
    {Providers, RejectContext1} = select_providers(Predestination, PaymentInstitution, VS, Revision, RejectContext0),
    select_routes(Predestination, Providers, VS, Revision, RejectContext1).

-spec gather_fail_rates([non_fail_rated_route()]) ->
    [fail_rated_route()].

gather_fail_rates(Routes) ->
    score_routes_with_fault_detector(Routes).

-spec choose_route([fail_rated_route()], reject_context(), pm_selector:varset()) ->
    {ok, route(), route_choice_meta()} |
    {error, {no_route_found, {risk_score_is_too_high | unknown, reject_context()}}}.

choose_route(FailRatedRoutes, RejectContext, VS) ->
    do_choose_route(FailRatedRoutes, VS, RejectContext).

-spec select_providers(
    route_predestination(),
    payment_institution(),
    pm_selector:varset(),
    hg_domain:revision(),
    reject_context()
) ->
    {[provider_with_ref()], reject_context()}.

select_providers(Predestination, PaymentInstitution, VS, Revision, RejectContext) ->
    {value, ProviderRefs0} = PaymentInstitution#domain_PaymentInstitution.providers,
    ProviderRefs1 = ordsets:to_list(ProviderRefs0),
    {Providers, RejectReasons} = lists:foldl(
        fun (ProviderRef, {Prvs, Reasons}) ->
            try
                P = acceptable_provider(Predestination, ProviderRef, VS, Revision),
                {[P | Prvs], Reasons}
             catch
                ?rejected(Reason) ->
                    {Prvs, [{ProviderRef, Reason} | Reasons]};
                error:{misconfiguration, Reason} ->
                    {Prvs, [{ProviderRef, {'Misconfiguration', Reason}} | Reasons]}
            end
        end,
        {[], []},
        ProviderRefs1
    ),
    {Providers, RejectContext#{rejected_providers => RejectReasons}}.

-spec select_routes(
    route_predestination(),
    [provider_with_ref()],
    pm_selector:varset(),
    hg_domain:revision(),
    reject_context()
) ->
    {[route()], reject_context()}.

select_routes(Predestination, Providers, VS, Revision, RejectContext) ->
    {Accepted, Rejected} = lists:foldl(
        fun (Provider, {AcceptedTerminals, RejectedRoutes}) ->
            {Accepts, Rejects} = collect_routes_for_provider(Predestination, Provider, VS, Revision),
            {Accepts ++ AcceptedTerminals, Rejects ++ RejectedRoutes}
        end,
        {[], []},
        Providers
    ),
    {Accepted, RejectContext#{rejected_routes => Rejected}}.

-spec do_choose_route([fail_rated_route()], pm_selector:varset(), reject_context()) ->
    {ok, route(), route_choice_meta()} |
    {error, {no_route_found, {risk_score_is_too_high | unknown, reject_context()}}}.

do_choose_route(_Routes, #{risk_score := fatal}, RejectContext) ->
    {error, {no_route_found, {risk_score_is_too_high, RejectContext}}};
do_choose_route([] = _Routes, _VS, RejectContext) ->
    {error, {no_route_found, {unknown, RejectContext}}};
do_choose_route(Routes, VS, _RejectContext) ->
    BalancedRoutes = balance_routes(Routes),
    ScoredRoutes = score_routes(BalancedRoutes, VS),
    {ChosenRoute, IdealRoute} = find_best_routes(ScoredRoutes),
    RouteChoiceMeta = get_route_choice_meta(ChosenRoute, IdealRoute),
    {ok, export_route(ChosenRoute), RouteChoiceMeta}.

-spec find_best_routes([scored_route()]) ->
    {Chosen :: scored_route(), Ideal :: scored_route()}.

find_best_routes([Route]) ->
    {Route, Route};
find_best_routes([First | Rest]) ->
    lists:foldl(
        fun(RouteIn, {CurrentRouteChosen, CurrentRouteIdeal}) ->
            NewRouteIdeal = select_better_route_ideal(RouteIn, CurrentRouteIdeal),
            NewRouteChosen = select_better_route(RouteIn, CurrentRouteChosen),
            {NewRouteChosen, NewRouteIdeal}
        end,
        {First, First}, Rest
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
    }, PT}.

get_route_choice_meta({_, SameRoute}, {_, SameRoute}) ->
    #{
        chosen_route => export_route_info(SameRoute)
    };
get_route_choice_meta({ChosenScores, ChosenRoute}, {IdealScores, IdealRoute}) ->
    #{
        chosen_route => export_route_info(ChosenRoute),
        preferable_route => export_route_info(IdealRoute),
        reject_reason => map_route_switch_reason(ChosenScores, IdealScores)
    }.

-spec export_route_info(non_fail_rated_route()) ->
    route_info().

export_route_info({{ProviderRef, Provider}, {TerminalRef, Terminal, _Priority}}) ->
    #{
        provider_ref => ProviderRef#domain_ProviderRef.id,
        provider_name => Provider#domain_Provider.name,
        terminal_ref => TerminalRef#domain_TerminalRef.id,
        terminal_name => Terminal#domain_Terminal.name
    }.

-spec get_logger_metadata(route_choice_meta()) ->
    LoggerFormattedMetadata :: map().

get_logger_metadata(RouteChoiceMeta) ->
    #{route_choice_metadata => format_logger_metadata(RouteChoiceMeta)}.

format_logger_metadata(RouteChoiceMeta) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc ++ format_logger_metadata(K, V)
        end,
        [],
        RouteChoiceMeta
    ).

format_logger_metadata(reject_reason, Reason) ->
    [{reject_reason, Reason}];
format_logger_metadata(Route, RouteInfo) when
    Route =:= chosen_route;
    Route =:= preferable_route
->
    [{Route, maps:to_list(RouteInfo)}].

map_route_switch_reason(SameScores, SameScores) ->
    unknown;
map_route_switch_reason(RealScores, IdealScores) when
    is_record(RealScores, route_scores);
    is_record(IdealScores, route_scores)
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

-spec balance_routes([fail_rated_route()]) ->
    [fail_rated_route()].
balance_routes(FailRatedRoutes) ->
    FilteredRouteGroups = lists:foldl(
        fun group_routes_by_priority/2,
        #{},
        FailRatedRoutes
    ),
    balance_route_groups(FilteredRouteGroups).

-spec group_routes_by_priority(fail_rated_route(), Acc :: route_groups_by_priority()) ->
    route_groups_by_priority().

group_routes_by_priority(Route = {_, _, {ProviderCondition, _}}, SortedRoutes) ->
    TerminalPriority = get_priority_from_route(Route),
    Key = {ProviderCondition, TerminalPriority},
    Routes = maps:get(Key, SortedRoutes, []),
    SortedRoutes#{Key => [Route | Routes]}.

get_priority_from_route({_Provider, {_TerminalRef, _Terminal, Priority}, _ProviderStatus}) ->
    {PriorityRate, _Weight} = Priority,
    PriorityRate.

-spec balance_route_groups(route_groups_by_priority()) ->
    [fail_rated_route()].

balance_route_groups(RouteGroups) ->
    maps:fold(
        fun (_Priority, Routes, Acc) ->
            NewRoutes = set_routes_random_condition(Routes),
            NewRoutes ++ Acc
        end,
        [],
        RouteGroups
    ).

set_routes_random_condition(Routes) ->
    NewRoutes = lists:map(
        fun(Route) ->
            case undefined =:= get_weight_from_route(Route) of
                true ->
                    set_weight_to_route(0, Route);
                false ->
                    Route
            end
        end,
        Routes
    ),
    Summary = get_summary_weight(NewRoutes),
    Random = rand:uniform() * Summary,
    lists:reverse(calc_random_condition(0.0, Random, NewRoutes, [])).

get_weight_from_route({_Provider, {_TerminalRef, _Terminal, Priority}, _ProviderStatus}) ->
    {_PriorityRate, Weight} = Priority,
    Weight.

set_weight_to_route(Value, Route) ->
    {Provider, {TerminalRef, Terminal, Priority}, ProviderStatus} = Route,
    {PriorityRate, _Weight} = Priority,
    {Provider, {TerminalRef, Terminal, {PriorityRate, Value}}, ProviderStatus}.

set_random_condition(Value, Route) ->
    set_weight_to_route(Value, Route).

get_summary_weight(Routes) ->
    lists:foldl(
        fun (Route, Acc) ->
            Weight = get_weight_from_route(Route),
            Acc + Weight
        end,
        0,
        Routes
    ).

calc_random_condition(_, _, [], Routes) ->
    Routes;
calc_random_condition(StartFrom, Random, [Route | Rest], Routes) ->
    Weight = get_weight_from_route(Route),
    InRange = (Random >= StartFrom) and (Random < StartFrom + Weight),
    case InRange of
        true ->
            NewRoute = set_random_condition(1, Route),
            calc_random_condition(StartFrom + Weight, Random, Rest, [NewRoute | Routes]);
        false ->
            NewRoute = set_random_condition(0, Route),
            calc_random_condition(StartFrom + Weight, Random, Rest, [NewRoute | Routes])
    end.

-spec score_routes([fail_rated_route()], pm_selector:varset()) ->
    [scored_route()].

score_routes(Routes, VS) ->
    [{score_route(R, VS), {Provider, Terminal}} || {Provider, Terminal, _ProviderStatus} = R <- Routes].

score_route({_Provider, {_TerminalRef, _Terminal, Priority}, ProviderStatus}, _VS) ->
    {AvailabilityStatus,    ConversionStatus} = ProviderStatus,
    {AvailabilityCondition, Availability}     = get_availability_score(AvailabilityStatus),
    {ConversionCondition,   Conversion}       = get_conversion_score(ConversionStatus),
    {PriorityRate, RandomCondition} = Priority,
    #route_scores{
        availability_condition = AvailabilityCondition,
        conversion_condition = ConversionCondition,
        availability = Availability,
        conversion = Conversion,
        priority_rating = PriorityRate,
        random_condition = RandomCondition
    }.

get_availability_score({alive, FailRate}) -> {1, 1.0 - FailRate};
get_availability_score({dead,  FailRate}) -> {0, 1.0 - FailRate}.

get_conversion_score({normal,  FailRate}) -> {1, 1.0 - FailRate};
get_conversion_score({lacking, FailRate}) -> {0, 1.0 - FailRate}.

export_route({_Scores, {{ProviderRef, _Provider}, {TerminalRef, _Terminal, _Priority}}}) ->
    % TODO shouldn't we provide something along the lines of `get_provider_ref/1`,
    %      `get_terminal_ref/1` instead?
    ?route(ProviderRef, TerminalRef).

-spec score_routes_with_fault_detector([non_fail_rated_route()]) ->
    [fail_rated_route()].

score_routes_with_fault_detector([]) -> [];
score_routes_with_fault_detector(Routes) ->
    IDs     = build_ids(Routes),
    FDStats = hg_fault_detector_client:get_statistics(IDs),
    [{P, T, get_provider_status(PR, FDStats)} || {{PR, _} = P, T} <- Routes].

-spec get_provider_status(provider_ref(), [fd_service_stats()]) ->
    provider_status().

get_provider_status(ProviderRef, FDStats) ->
    AvailabilityServiceID = build_fd_availability_service_id(ProviderRef),
    ConversionServiceID   = build_fd_conversion_service_id(ProviderRef),
    AvailabilityStatus    = get_provider_availability_status(AvailabilityServiceID, FDStats),
    ConversionStatus      = get_provider_conversion_status(ConversionServiceID, FDStats),
    {AvailabilityStatus, ConversionStatus}.

get_provider_availability_status(FDID, Stats) ->
    AvailabilityConfig = genlib_app:env(hellgate, fault_detector_availability, #{}),
    CriticalFailRate   = genlib_map:get(critical_fail_rate, AvailabilityConfig, 0.7),
    case lists:keysearch(FDID, #fault_detector_ServiceStatistics.service_id, Stats) of
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}}
            when FailRate >= CriticalFailRate ->
            {dead,  FailRate};
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} ->
            {alive, FailRate};
        false ->
            {alive, 0.0}
    end.

get_provider_conversion_status(FDID, Stats) ->
    ConversionConfig = genlib_app:env(hellgate, fault_detector_conversion, #{}),
    CriticalFailRate = genlib_map:get(critical_fail_rate, ConversionConfig, 0.7),
    case lists:keysearch(FDID, #fault_detector_ServiceStatistics.service_id, Stats) of
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}}
            when FailRate >= CriticalFailRate ->
            {lacking, FailRate};
        {value, #fault_detector_ServiceStatistics{failure_rate = FailRate}} ->
            {normal, FailRate};
        false ->
            {normal, 0.0}
    end.

build_ids(Routes) ->
    lists:foldl(fun build_fd_ids/2, [], Routes).

build_fd_ids({{ProviderRef, _Provider}, _Terminal}, IDs) ->
    AvailabilityID = build_fd_availability_service_id(ProviderRef),
    ConversionID   = build_fd_conversion_service_id(ProviderRef),
    [AvailabilityID, ConversionID | IDs].

build_fd_availability_service_id(#domain_ProviderRef{id = ID}) ->
    hg_fault_detector_client:build_service_id(adapter_availability, ID).

build_fd_conversion_service_id(#domain_ProviderRef{id = ID}) ->
    hg_fault_detector_client:build_service_id(provider_conversion, ID).

-spec get_payments_terms(route(), hg_domain:revision()) -> terms().

get_payments_terms(?route(ProviderRef, TerminalRef), Revision) ->
    #domain_Provider{terms = Terms0} = hg_domain:get(Revision, {provider, ProviderRef}),
    #domain_Terminal{terms = Terms1} = hg_domain:get(Revision, {terminal, TerminalRef}),
    Terms = merge_terms(Terms0, Terms1),
    Terms#domain_ProvisionTermSet.payments.

-spec get_reduced_payments_terms(route(), hg_varset:varset(), hg_domain:revision()) -> terms().

get_reduced_payments_terms(?route(ProviderRef, TerminalRef), VS, Revision) ->
    PreparedVS = hg_varset:prepare_varset(VS),
    {Client, Context} = get_party_client(),
    {ok, TermsSet} = party_client_thrift:compute_provider_terminal_terms(
        ProviderRef, TerminalRef, Revision, PreparedVS, Client, Context),
    #domain_ProvisionTermSet{payments = Terms} = TermsSet,
    Terms.

-spec get_rec_paytools_terms(route(), hg_domain:revision()) -> terms().

get_rec_paytools_terms(?route(ProviderRef, _), Revision) ->
    #domain_Provider{terms = Terms} = hg_domain:get(Revision, {provider, ProviderRef}),
    Terms#domain_ProvisionTermSet.recurrent_paytools.

-spec acceptable_provider(
    route_predestination(),
    provider_ref(),
    pm_selector:varset(),
    hg_domain:revision()
) ->
    provider_with_ref() | no_return().

acceptable_provider(payment, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_provision_payment_terms(Terms, VS, Revision),
    {ProviderRef, Provider};
acceptable_provider(recurrent_paytool, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_provision_recurrent_terms(Terms, VS, Revision),
    {ProviderRef, Provider};
acceptable_provider(recurrent_payment, ProviderRef, VS, Revision) ->
    % Use provider check combined from recurrent_paytool and payment check
    Provider = #domain_Provider{
        terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_provision_payment_terms(Terms, VS, Revision),
    _ = acceptable_provision_recurrent_terms(Terms, VS, Revision),
    {ProviderRef, Provider}.

%%

-spec collect_routes_for_provider(
    route_predestination(),
    provider_with_ref(),
    pm_selector:varset(),
    hg_domain:revision()
) ->
    {[non_fail_rated_route()], [rejected_route()]}.

collect_routes_for_provider(Predestination, {ProviderRef, Provider}, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    ProviderTerminalRefs = reduce(terminal, TerminalSelector, VS, Revision),
    lists:foldl(
        fun (ProviderTerminalRef, {Accepted, Rejected}) ->
            TerminalRef = get_terminal_ref(ProviderTerminalRef),
            Priority = get_terminal_priority(ProviderTerminalRef),
            try
                {TerminalRef, Terminal} = acceptable_terminal(Predestination, TerminalRef, Provider, VS, Revision),
                {[{{ProviderRef, Provider}, {TerminalRef, Terminal, Priority}} | Accepted], Rejected}
            catch
                ?rejected(Reason) ->
                    {Accepted, [{ProviderRef, TerminalRef, Reason} | Rejected]};
                error:{misconfiguration, Reason} ->
                    {Accepted, [{ProviderRef, TerminalRef, {'Misconfiguration', Reason}} | Rejected]}
            end
        end,
        {[], []},
        ordsets:to_list(ProviderTerminalRefs)
    ).

-spec acceptable_terminal(
    route_predestination(),
    terminal_ref(),
    provider(),
    pm_selector:varset(),
    hg_domain:revision()
) ->
    unweighted_terminal() | no_return().

acceptable_terminal(payment, TerminalRef, Provider, VS, Revision) ->
    #domain_Provider{
        terms = ProviderTerms
    } = Provider,
    Terminal = #domain_Terminal{
        terms = TerminalTerms
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    % TODO the ability to override any terms makes for uncommon sense
    %      is it better to allow to override only cash flow / refunds terms?
    Terms = merge_terms(ProviderTerms, TerminalTerms),
    _ = acceptable_provision_payment_terms(Terms, VS, Revision),
    {TerminalRef, Terminal};
acceptable_terminal(recurrent_paytool, TerminalRef, Provider, VS, Revision) ->
    #domain_Provider{
        terms = Terms
    } = Provider,
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    _ = acceptable_provision_recurrent_terms(Terms, VS, Revision),
    {TerminalRef, Terminal};
acceptable_terminal(recurrent_payment, TerminalRef, Provider, VS, Revision) ->
    % Use provider check combined from recurrent_paytool and payment check
    #domain_Provider{
        terms = ProviderTerms
    } = Provider,
    Terminal = #domain_Terminal{
        terms = TerminalTerms
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    Terms = merge_terms(ProviderTerms, TerminalTerms),
    _ = acceptable_provision_payment_terms(Terms, VS, Revision),
    _ = acceptable_provision_recurrent_terms(Terms, VS, Revision),
    {TerminalRef, Terminal}.

-spec get_terminal_ref(provider_terminal_ref()) ->
    terminal_ref().

get_terminal_ref(#domain_ProviderTerminalRef{id = ID}) ->
    #domain_TerminalRef{id = ID}.

-spec get_terminal_priority(provider_terminal_ref()) ->
    terminal_priority().

get_terminal_priority(#domain_ProviderTerminalRef{
    priority = Priority,
    weight = Weight
}) when is_integer(Priority) ->
    {Priority, Weight}.

%%

acceptable_provision_payment_terms(
    #domain_ProvisionTermSet{
        payments = PaymentTerms
    },
    VS,
    Revision
) ->
    _ = acceptable_payment_terms(PaymentTerms, VS, Revision),
    true;
acceptable_provision_payment_terms(undefined, _VS, _Revision) ->
    throw(?rejected({'ProvisionTermSet', undefined})).

acceptable_provision_recurrent_terms(
    #domain_ProvisionTermSet{
        recurrent_paytools = RecurrentTerms
    },
    VS,
    Revision
) ->
    _ = acceptable_recurrent_paytool_terms(RecurrentTerms, VS, Revision),
    true;
acceptable_provision_recurrent_terms(undefined, _VS, _Revision) ->
    throw(?rejected({'ProvisionTermSet', undefined})).

acceptable_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = CurrenciesSelector,
        categories      = CategoriesSelector,
        payment_methods = PMsSelector,
        cash_limit      = CashLimitSelector,
        holds           = HoldsTerms,
        refunds         = RefundsTerms,
        chargebacks     = ChargebackTerms
    },
    VS,
    Revision
) ->
    % TODO varsets getting mixed up
    %      it seems better to pass down here hierarchy of contexts w/ appropriate module accessors
    ParentName = 'PaymentsProvisionTerms',
    _ = try_accept_term(ParentName, currency     , CurrenciesSelector , VS, Revision),
    _ = try_accept_term(ParentName, category     , CategoriesSelector , VS, Revision),
    _ = try_accept_term(ParentName, payment_tool , PMsSelector        , VS, Revision),
    _ = try_accept_term(ParentName, cost         , CashLimitSelector  , VS, Revision),
    _ = acceptable_holds_terms(HoldsTerms, getv(flow, VS, undefined), VS, Revision),
    _ = acceptable_refunds_terms(RefundsTerms, getv(refunds, VS, undefined), VS, Revision),
    _ = acceptable_chargeback_terms(ChargebackTerms, getv(chargebacks, VS, undefined), VS, Revision),
    true;
acceptable_payment_terms(undefined, _VS, _Revision) ->
    throw(?rejected({'PaymentsProvisionTerms', undefined})).

acceptable_holds_terms(_Terms, undefined, _VS, _Revision) ->
    true;
acceptable_holds_terms(_Terms, instant, _VS, _Revision) ->
    true;
acceptable_holds_terms(Terms, {hold, Lifetime}, VS, Revision) ->
    case Terms of
        #domain_PaymentHoldsProvisionTerms{lifetime = LifetimeSelector} ->
            _ = try_accept_term('PaymentHoldsProvisionTerms', lifetime, Lifetime, LifetimeSelector, VS, Revision),
            true;
        undefined ->
            throw(?rejected({'PaymentHoldsProvisionTerms', undefined}))
    end.

acceptable_refunds_terms(_Terms, undefined, _VS, _Revision) ->
    true;
acceptable_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{
        partial_refunds =  PartialRefundsTerms
    },
    RVS,
    VS,
    Revision
) ->
    _ = acceptable_partial_refunds_terms(
        PartialRefundsTerms,
        getv(partial, RVS, undefined),
        VS,
        Revision
    ),
    true;
acceptable_refunds_terms(undefined, _RVS, _VS, _Revision) ->
    throw(?rejected({'PaymentRefundsProvisionTerms', undefined})).

acceptable_partial_refunds_terms(_Terms, undefined, _VS, _Revision) ->
    true;
acceptable_partial_refunds_terms(
    #domain_PartialRefundsProvisionTerms{cash_limit = CashLimitSelector},
    #{cash_limit := MerchantLimit},
    VS,
    Revision
) ->
    ProviderLimit = reduce(cash_limit, CashLimitSelector, VS, Revision),
    hg_cash_range:is_subrange(MerchantLimit, ProviderLimit) == true
        orelse throw(?rejected({'PartialRefundsProvisionTerms', cash_limit}));

acceptable_partial_refunds_terms(undefined, _RVS, _VS, _Revision) ->
    throw(?rejected({'PartialRefundsProvisionTerms', undefined})).

acceptable_chargeback_terms(_Terms, undefined, _VS, _Revision) ->
    true;
acceptable_chargeback_terms(_Terms, #{}, _VS, _Revision) ->
    true;
acceptable_chargeback_terms(undefined, _RVS, _VS, _Revision) ->
    throw(?rejected({'PaymentChargebackProvisionTerms', undefined})).

merge_terms(
    #domain_ProvisionTermSet{
        payments           = PPayments,
        recurrent_paytools = PRecurrents
    },
    #domain_ProvisionTermSet{
        payments           = TPayments,
        recurrent_paytools = _TRecurrents  % TODO: Allow to define recurrent terms in terminal
    }
) ->
    #domain_ProvisionTermSet{
        payments           = merge_payment_terms(PPayments, TPayments),
        recurrent_paytools = PRecurrents
    };
merge_terms(ProviderTerms, TerminalTerms) ->
    hg_utils:select_defined(TerminalTerms, ProviderTerms).

-spec merge_payment_terms(terms(), terms()) ->
    terms().

merge_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = PCurrencies,
        categories      = PCategories,
        payment_methods = PPaymentMethods,
        cash_limit      = PCashLimit,
        cash_flow       = PCashflow,
        holds           = PHolds,
        refunds         = PRefunds,
        chargebacks     = PChargebacks
    },
    #domain_PaymentsProvisionTerms{
        currencies      = TCurrencies,
        categories      = TCategories,
        payment_methods = TPaymentMethods,
        cash_limit      = TCashLimit,
        cash_flow       = TCashflow,
        holds           = THolds,
        refunds         = TRefunds,
        chargebacks     = TChargebacks
    }
) ->
    #domain_PaymentsProvisionTerms{
        currencies      = hg_utils:select_defined(TCurrencies,     PCurrencies),
        categories      = hg_utils:select_defined(TCategories,     PCategories),
        payment_methods = hg_utils:select_defined(TPaymentMethods, PPaymentMethods),
        cash_limit      = hg_utils:select_defined(TCashLimit,      PCashLimit),
        cash_flow       = hg_utils:select_defined(TCashflow,       PCashflow),
        holds           = hg_utils:select_defined(THolds,          PHolds),
        refunds         = hg_utils:select_defined(TRefunds,        PRefunds),
        chargebacks     = hg_utils:select_defined(TChargebacks,    PChargebacks)
    };
merge_payment_terms(ProviderTerms, TerminalTerms) ->
    hg_utils:select_defined(TerminalTerms, ProviderTerms).

get_party_client() ->
    HgContext = hg_context:load(),
    Client = hg_context:get_party_client(HgContext),
    Context = hg_context:get_party_client_context(HgContext),
    {Client, Context}.

%%

acceptable_recurrent_paytool_terms(
    #domain_RecurrentPaytoolsProvisionTerms{
        categories      = CategoriesSelector,
        payment_methods = PMsSelector
    },
    VS,
    Revision
) ->
    _ = try_accept_term('RecurrentPaytoolsProvisionTerms', category     , CategoriesSelector , VS, Revision),
    _ = try_accept_term('RecurrentPaytoolsProvisionTerms', payment_tool , PMsSelector        , VS, Revision),
    true;
acceptable_recurrent_paytool_terms(undefined, _VS, _Revision) ->
    throw(?rejected({'RecurrentPaytoolsProvisionTerms', undefined})).

try_accept_term(ParentName, Name, Selector, VS, Revision) ->
    try_accept_term(ParentName, Name, getv(Name, VS), Selector, VS, Revision).

try_accept_term(ParentName, Name, Value, Selector, VS, Revision) when Selector /= undefined ->
    Values = reduce(Name, Selector, VS, Revision),
    test_term(Name, Value, Values) orelse throw(?rejected({ParentName, Name}));
try_accept_term(ParentName, Name, _Value, undefined, _VS, _Revision) ->
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

reduce(Name, S, VS, Revision) ->
    case pm_selector:reduce(S, VS, Revision) of
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

-spec marshal(route()) ->
    hg_msgpack_marshalling:value().

marshal(Route) ->
    marshal(route, Route).

marshal(route, #domain_PaymentRoute{} = Route) ->
    [2, #{
        <<"provider">> => marshal(provider_ref, Route#domain_PaymentRoute.provider),
        <<"terminal">> => marshal(terminal_ref, Route#domain_PaymentRoute.terminal)
    }];

marshal(provider_ref, #domain_ProviderRef{id = ObjectID}) ->
    marshal(int, ObjectID);

marshal(terminal_ref, #domain_TerminalRef{id = ObjectID}) ->
    marshal(int, ObjectID);

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    route().

unmarshal(Route) ->
    unmarshal(route, Route).

unmarshal(route, [2, #{
    <<"provider">> := Provider,
    <<"terminal">> := Terminal
}]) ->
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

-type testcase() :: {_, fun()}.

-spec record_comparsion_test() -> [testcase()].
record_comparsion_test() ->
    Bigger = {#route_scores{
        availability_condition = 1,
        availability = 0.5,
        conversion_condition = 1,
        conversion = 0.5,
        priority_rating = 1,
        random_condition = 1
    }, {42, 42}},
    Smaller = {#route_scores{
        availability_condition = 0,
        availability = 0.1,
        conversion_condition = 1,
        conversion = 0.5,
        priority_rating = 1,
        random_condition = 1
    }, {99, 99}},
    Bigger = select_better_route(Bigger, Smaller).

-spec balance_routes_test() -> [testcase()].
balance_routes_test() ->
    WithWeight = [
        {1, {test, test, {test, 1}}, test},
        {2, {test, test, {test, 2}}, test},
        {3, {test, test, {test, 0}}, test},
        {4, {test, test, {test, 1}}, test},
        {5, {test, test, {test, 0}}, test}
    ],
    Result1 = [
        {1, {test, test, {test, 1}}, test},
        {2, {test, test, {test, 0}}, test},
        {3, {test, test, {test, 0}}, test},
        {4, {test, test, {test, 0}}, test},
        {5, {test, test, {test, 0}}, test}
    ],
    Result2 = [
        {1, {test, test, {test, 0}}, test},
        {2, {test, test, {test, 1}}, test},
        {3, {test, test, {test, 0}}, test},
        {4, {test, test, {test, 0}}, test},
        {5, {test, test, {test, 0}}, test}
    ],
    Result3 = [
        {1, {test, test, {test, 0}}, test},
        {2, {test, test, {test, 0}}, test},
        {3, {test, test, {test, 0}}, test},
        {4, {test, test, {test, 0}}, test},
        {5, {test, test, {test, 0}}, test}
    ],
    [
        ?assertEqual(Result1, lists:reverse(calc_random_condition(0.0, 0.2, WithWeight, []))),
        ?assertEqual(Result2, lists:reverse(calc_random_condition(0.0, 1.5, WithWeight, []))),
        ?assertEqual(Result3, lists:reverse(calc_random_condition(0.0, 4.0, WithWeight, [])))
    ].

-spec balance_routes_without_weight_test() -> [testcase()].
balance_routes_without_weight_test() ->
    Routes = [
        {1, {test, test, {test, undefined}}, test},
        {2, {test, test, {test, undefined}}, test}
    ],
    Result = [
        {1, {test, test, {test, 0}}, test},
        {2, {test, test, {test, 0}}, test}
    ],
    [
        ?assertEqual(Result, set_routes_random_condition(Routes))
    ].

-endif.
