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
-type route_predestination() :: payment | recurrent_paytool.

-spec choose(
    route_predestination(),
    dmsl_domain_thrift:'PaymentInstitution'(),
    hg_selector:varset(),
    hg_domain:revision()
) ->
    route() | undefined.

choose(Predestination, PaymentInstitution, VS, Revision) ->
    % TODO not the optimal strategy
    Providers = collect_providers(Predestination, PaymentInstitution, VS, Revision),
    Choices = collect_routes(Predestination, Providers, VS, Revision),
    choose_route(Choices).

collect_routes(Predestination, Providers, VS, Revision) ->
    lists:flatmap(
        fun (Provider) ->
            Terminals = collect_terminals(Predestination, Provider, VS, Revision),
            lists:map(
                fun (Terminal) ->
                    Route = {Provider, Terminal},
                    {score_route(Route, VS), Route}
                end,
                Terminals
            )
        end,
        Providers
    ).

choose_route(Routes) ->
    case lists:reverse(lists:keysort(1, Routes)) of
        [{_Score, Route} | _] ->
            export_route(Route);
        [] ->
            undefined
    end.


export_route({{ProviderRef, _Provider}, {TerminalRef, _Terminal}}) ->
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

collect_providers(Predestination, PaymentInstitution, VS, Revision) ->
    ProviderSelector = PaymentInstitution#domain_PaymentInstitution.providers,
    ProviderRefs = reduce(provider, ProviderSelector, VS, Revision),
    lists:filtermap(
        fun (ProviderRef) ->
            try acceptable_provider(Predestination, ProviderRef, VS, Revision) catch
                false ->
                    false
            end
        end,
        ordsets:to_list(ProviderRefs)
    ).

acceptable_provider(payment, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        payment_terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_payment_terms(Terms, VS, Revision),
    {true, {ProviderRef, Provider}};
acceptable_provider(recurrent_paytool, ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        recurrent_paytool_terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_recurrent_paytool_terms(Terms, VS, Revision),
    {true, {ProviderRef, Provider}}.

%%

collect_terminals(Predestination, {_ProviderRef, Provider}, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    TerminalRefs = reduce(terminal, TerminalSelector, VS, Revision),
    lists:filtermap(
        fun (TerminalRef) ->
            try acceptable_terminal(Predestination, TerminalRef, Provider, VS, Revision) catch
                false ->
                    false
            end
        end,
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
    {true, {TerminalRef, Terminal}};
acceptable_terminal(recurrent_paytool, TerminalRef, #domain_Provider{recurrent_paytool_terms = Terms}, VS, Revision) ->
    Terminal = #domain_Terminal{
        risk_coverage = RiskCoverage
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    _ = acceptable_recurrent_paytool_terms(Terms, VS, Revision),
    _ = acceptable_risk(RiskCoverage, VS),
    {true, {TerminalRef, Terminal}}.

acceptable_risk(RiskCoverage, VS) ->
    RiskScore = getv(risk_score, VS),
    hg_inspector:compare_risk_score(RiskCoverage, RiskScore) >= 0 orelse throw(false).

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
    _ = try_accept_term(currency     , CurrenciesSelector , VS, Revision),
    _ = try_accept_term(category     , CategoriesSelector , VS, Revision),
    _ = try_accept_term(payment_tool , PMsSelector        , VS, Revision),
    _ = try_accept_term(cost         , CashLimitSelector  , VS, Revision),
    _ = acceptable_holds_terms(HoldsTerms, getv(flow, VS, undefined), VS, Revision),
    _ = acceptable_refunds_terms(RefundsTerms, getv(refunds, VS, undefined), Revision),
    true;
acceptable_payment_terms(undefined, _VS, _Revision) ->
    throw(false).

acceptable_holds_terms(_Terms, undefined, _VS, _Revision) ->
    true;
acceptable_holds_terms(_Terms, instant, _VS, _Revision) ->
    true;
acceptable_holds_terms(Terms, {hold, Lifetime}, VS, Revision) ->
    case Terms of
        #domain_PaymentHoldsProvisionTerms{lifetime = LifetimeSelector} ->
            _ = try_accept_term(lifetime, Lifetime, LifetimeSelector, VS, Revision),
            true;
        undefined ->
            throw(false)
    end.

acceptable_refunds_terms(_Terms, undefined, _Revision) ->
    true;
acceptable_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{
        partial_refunds = PRs
    },
    VS,
    Revision
) ->
    _ = acceptable_partial_refunds_terms(PRs, getv(partial_refunds, VS, undefined), Revision),
    true;
acceptable_refunds_terms(undefined, _VS, _Revision) ->
    false.

acceptable_partial_refunds_terms(
    #domain_PartialRefundsProvisionTerms{cash_limit = CashLimitSelector},
    #{cost := Cost},
    Revision
) ->
    _ = try_accept_term(cost_for_refund, CashLimitSelector, #{cost_for_refund => Cost}, Revision),
    true;
acceptable_partial_refunds_terms(_Terms, _Refunds, _Revision) ->
    true.

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
    _ = try_accept_term(category     , CategoriesSelector , VS, Revision),
    _ = try_accept_term(payment_tool , PMsSelector        , VS, Revision),
    true;
acceptable_recurrent_paytool_terms(undefined, _VS, _Revision) ->
    throw(false).

try_accept_term(Name, Selector, VS, Revision) ->
    try_accept_term(Name, getv(Name, VS), Selector, VS, Revision).

try_accept_term(Name, Value, Selector, VS, Revision) when Selector /= undefined ->
    Values = reduce(Name, Selector, VS, Revision),
    test_term(Name, Value, Values) orelse throw(false).

test_term(currency, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(category, V, Vs) ->
    ordsets:is_element(V, Vs);
test_term(payment_tool, PT, PMs) ->
    ordsets:is_element(hg_payment_tool:get_method(PT), PMs);
test_term(cost, Cost, CashRange) ->
    hg_condition:test_cash_range(Cost, CashRange) == within;
test_term(lifetime, ?hold_lifetime(Lifetime), ?hold_lifetime(Allowed)) ->
    Lifetime =< Allowed;
test_term(cost_for_refund, Cost, CashRange) ->
    Result = hg_condition:test_cash_range(Cost, CashRange),
    (Result == within) or (Result == {exceeds, upper}).

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
