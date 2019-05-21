%%% Naïve routing oracle

-module(hg_routing).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([choose/4]).
-export([get_payments_terms/2]).
-export([get_rec_paytools_terms/2]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-include("domain.hrl").

-type terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'()
               | dmsl_domain_thrift:'RecurrentPaytoolsProvisionTerms'()
               | undefined.

-type route()                :: dmsl_domain_thrift:'PaymentRoute'().
-type route_predestination() :: payment | recurrent_paytool | recurrent_payment.

-define(rejected(Reason), {rejected, Reason}).

-type reject_context():: #{
    varset              := hg_selector:varset(),
    rejected_providers  := list(rejected_provider()),
    rejected_terminals  := list(rejected_terminal())
}.
-type rejected_provider() :: {provider_ref(), Reason :: term()}.
-type rejected_terminal() :: {terminal_ref(), Reason :: term()}.
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

-export_type([route_predestination/0]).

-spec choose(
    route_predestination(),
    dmsl_domain_thrift:'PaymentInstitution'(),
    hg_selector:varset(),
    hg_domain:revision()
) ->
    {ok, route()} | {error, {no_route_found, {unknown | risk_score_is_too_high, reject_context()}}}.

choose(Predestination, PaymentInstitution, VS, Revision) ->
    % TODO not the optimal strategy
    RejectContext0 = #{
        varset => VS
    },
    {Providers, RejectContext1} = collect_providers(Predestination, PaymentInstitution, VS, Revision, RejectContext0),
    {Choices, RejectContext2} = collect_routes(Predestination, Providers, VS, Revision, RejectContext1),
    choose_route(Choices, VS, RejectContext2).

collect_routes(Predestination, Providers, VS, Revision, RejectContext) ->
    {Accepted, Rejected} = lists:foldl(
        fun (Provider, {AcceptedTerminals, RejectedTerminals}) ->
            {Accepts, Rejects} = collect_routes_for_provider(Predestination, Provider, VS, Revision),
            {Accepts ++ AcceptedTerminals, Rejects ++ RejectedTerminals}
        end,
        {[], []},
        Providers
    ),
    {Accepted, RejectContext#{rejected_terminals => Rejected}}.

choose_route(Routes, VS = #{risk_score := RiskScore}, RejectContext) ->
    case lists:reverse(lists:keysort(1, score_routes(Routes, VS))) of
        [{_Score, Route} | _] ->
            {ok, export_route(Route)};
        [] when RiskScore =:= fatal ->
            {error, {no_route_found, {risk_score_is_too_high, RejectContext}}};
        [] ->
            {error, {no_route_found, {unknown, RejectContext}}}
    end.

score_routes(Routes, VS) ->
    [{score_route(R, VS), R} || R <- Routes].

export_route({ProviderRef, {TerminalRef, _Terminal}}) ->
    % TODO shouldn't we provide something along the lines of `get_provider_ref/1`,
    %      `get_terminal_ref/1` instead?
    ?route(ProviderRef, TerminalRef).

-spec get_payments_terms(route(), hg_domain:revision()) -> terms().

get_payments_terms(?route(ProviderRef, TerminalRef), Revision) ->
    #domain_Provider{payment_terms = Terms0} = hg_domain:get(Revision, {provider, ProviderRef}),
    #domain_Terminal{terms = Terms1} = hg_domain:get(Revision, {terminal, TerminalRef}),
    merge_payment_terms(Terms0, Terms1).

-spec get_rec_paytools_terms(route(), hg_domain:revision()) -> terms().

get_rec_paytools_terms(?route(ProviderRef, _), Revision) ->
    #domain_Provider{recurrent_paytool_terms = Terms} = hg_domain:get(Revision, {provider, ProviderRef}),
    Terms.

%%

%% NOTE
%% Score ∈ [0.0 .. 1.0]
%% Higher score is better, e.g. route is more likely to be chosen.

score_route(Route, VS) ->
    score_risk_coverage(Route, VS).

score_risk_coverage({_Provider, {_TerminalRef, Terminal}}, VS) ->
    RiskScore = getv(risk_score, VS),
    RiskCoverage = Terminal#domain_Terminal.risk_coverage,
    math:exp(-hg_inspector:compare_risk_score(RiskCoverage, RiskScore)).

%%

collect_providers(Predestination, PaymentInstitution, VS, Revision, RejectContext) ->
    ProviderSelector = PaymentInstitution#domain_PaymentInstitution.providers,
    ProviderRefs = reduce(provider, ProviderSelector, VS, Revision),
    {Providers, RejectReasons} = lists:foldl(
        fun (ProviderRef, {Prvs, Reasons}) ->
            try
                P = acceptable_provider(Predestination, ProviderRef, VS, Revision),
                {[P | Prvs], Reasons}
             catch
                ?rejected(Reason) ->
                    {Prvs, [{ProviderRef, Reason} | Reasons]}
            end
        end,
        {[], []},
        ordsets:to_list(ProviderRefs)
    ),
    {Providers, RejectContext#{rejected_providers => RejectReasons}}.

acceptable_provider(payment, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        payment_terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_payment_terms(Terms, VS, Revision),
    {ProviderRef, Provider};
acceptable_provider(recurrent_paytool, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        recurrent_paytool_terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_recurrent_paytool_terms(Terms, VS, Revision),
    {ProviderRef, Provider};
acceptable_provider(recurrent_payment, ProviderRef, VS, Revision) ->
    % Use provider check combined from recurrent_paytool and payment check
    Provider = #domain_Provider{
        payment_terms = PaymentTerms,
        recurrent_paytool_terms = RecurrentTerms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_payment_terms(PaymentTerms, VS, Revision),
    _ = acceptable_recurrent_paytool_terms(RecurrentTerms, VS, Revision),
    {ProviderRef, Provider}.

%%

collect_routes_for_provider(Predestination, {ProviderRef, Provider}, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    TerminalRefs = reduce(terminal, TerminalSelector, VS, Revision),
    lists:foldl(
        fun (TerminalRef, {Accepted, Rejected}) ->
            try
                Terminal = acceptable_terminal(Predestination, TerminalRef, Provider, VS, Revision),
                {[{ProviderRef, Terminal} | Accepted], Rejected}
            catch
                ?rejected(Reason) ->
                    {Accepted, [{ProviderRef, TerminalRef, Reason} | Rejected]}
            end
        end,
        {[], []},
        ordsets:to_list(TerminalRefs)
    ).

acceptable_terminal(payment, TerminalRef, #domain_Provider{payment_terms = Terms0}, VS, Revision) ->
    Terminal = #domain_Terminal{
        terms         = Terms1,
        risk_coverage = RiskCoverage
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    % TODO the ability to override any terms makes for uncommon sense
    %      is it better to allow to override only cash flow / refunds terms?
    Terms = merge_payment_terms(Terms0, Terms1),
    _ = acceptable_payment_terms(Terms, VS, Revision),
    _ = acceptable_risk(RiskCoverage, VS),
    {TerminalRef, Terminal};
acceptable_terminal(recurrent_paytool, TerminalRef, #domain_Provider{recurrent_paytool_terms = Terms}, VS, Revision) ->
    Terminal = #domain_Terminal{
        risk_coverage = RiskCoverage
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    _ = acceptable_recurrent_paytool_terms(Terms, VS, Revision),
    _ = acceptable_risk(RiskCoverage, VS),
    {TerminalRef, Terminal};
acceptable_terminal(recurrent_payment, TerminalRef, Provider, VS, Revision) ->
    % Use provider check combined from recurrent_paytool and payment check
    #domain_Provider{
        payment_terms = PaymentTerms0,
        recurrent_paytool_terms = RecurrentTerms
    } = Provider,
    Terminal = #domain_Terminal{
        terms         = TerminalTerms,
        risk_coverage = RiskCoverage
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    PaymentTerms = merge_payment_terms(PaymentTerms0, TerminalTerms),
    _ = acceptable_payment_terms(PaymentTerms, VS, Revision),
    _ = acceptable_recurrent_paytool_terms(RecurrentTerms, VS, Revision),
    _ = acceptable_risk(RiskCoverage, VS),
    {TerminalRef, Terminal}.

acceptable_risk(RiskCoverage, VS) ->
    RiskScore = getv(risk_score, VS),
    hg_inspector:compare_risk_score(RiskCoverage, RiskScore) >= 0
        orelse throw(?rejected({'Terminal', risk_coverage})).

%%

acceptable_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = CurrenciesSelector,
        categories      = CategoriesSelector,
        payment_methods = PMsSelector,
        cash_limit      = CashLimitSelector,
        holds           = HoldsTerms,
        refunds         = RefundsTerms
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

merge_payment_terms(
    #domain_PaymentsProvisionTerms{
        holds           = Holds0,
        refunds         = Refunds0
    },
    #domain_PaymentsProvisionTerms{
        currencies      = Currencies1,
        categories      = Categories1,
        payment_methods = PaymentMethods1,
        cash_limit      = CashLimit1,
        cash_flow       = Cashflow1,
        holds           = Holds1,
        refunds         = Refunds1
    }
) ->
    #domain_PaymentsProvisionTerms{
        currencies      = Currencies1,
        categories      = Categories1,
        payment_methods = PaymentMethods1,
        cash_limit      = CashLimit1,
        cash_flow       = Cashflow1,
        holds           = merge_holds_terms(Holds1, Holds0),
        refunds         = merge_refunds_terms(Refunds1, Refunds0)
    };
merge_payment_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_holds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_refunds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

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
    test_term(Name, Value, Values) orelse throw(?rejected({ParentName, Name})).

test_term(currency, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(category, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(payment_tool, PT, PMs) ->
    ordsets:is_element(hg_payment_tool:get_method(PT), PMs);
test_term(cost, Cost, CashRange) ->
    hg_cash_range:is_inside(Cost, CashRange) == within;
test_term(lifetime, ?hold_lifetime(Lifetime), ?hold_lifetime(Allowed)) ->
    Lifetime =< Allowed.

%%

reduce(Name, S, VS, Revision) ->
    case hg_selector:reduce(S, VS, Revision) of
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
