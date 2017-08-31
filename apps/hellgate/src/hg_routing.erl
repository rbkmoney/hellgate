%%% Naïve routing oracle

-module(hg_routing).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([choose/2]).
-export([get_payments_terms/2]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-include("domain.hrl").

-type terms()    :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type route()    :: dmsl_domain_thrift:'InvoicePaymentRoute'().

-spec choose(hg_selector:varset(), hg_domain:revision()) ->
    route() | undefined.

choose(VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    % TODO not the optimal strategy
    Providers = collect_providers(Globals, VS, Revision),
    Choices = [{Provider, collect_terminals(Provider, VS, Revision)} || Provider <- Providers],
    choose_provider_terminal(Choices, VS).

choose_provider_terminal([{{ProviderRef, _}, [{TerminalRef, _} | _]} | _], _) ->
    ?route(ProviderRef, TerminalRef);
choose_provider_terminal([{_Provider, []} | Rest], VS) ->
    choose_provider_terminal(Rest, VS);
choose_provider_terminal([], _) ->
    undefined.

-spec get_payments_terms(route(), hg_domain:revision()) -> terms().

get_payments_terms(?route(ProviderRef, TerminalRef), Revision) ->
    #domain_Provider{terms = Terms0} = hg_domain:get(Revision, {provider, ProviderRef}),
    #domain_Terminal{terms = Terms1} = hg_domain:get(Revision, {terminal, TerminalRef}),
    merge_payment_terms(Terms0, Terms1).

%%

collect_providers(Globals, VS, Revision) ->
    ProviderSelector = Globals#domain_Globals.providers,
    ProviderRefs = reduce(provider, ProviderSelector, VS, Revision),
    lists:filtermap(
        fun (ProviderRef) ->
            try acceptable_provider(ProviderRef, VS, Revision) catch
                false ->
                    false
            end
        end,
        ordsets:to_list(ProviderRefs)
    ).

acceptable_provider(ProviderRef, VS, Revision) ->
    Provider = #domain_Provider{
        terms = Terms
    } = hg_domain:get(Revision, {provider, ProviderRef}),
    _ = acceptable_payment_terms(Terms, VS, Revision),
    {true, {ProviderRef, Provider}}.

%%

collect_terminals({_ProviderRef, Provider}, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    TerminalRefs = reduce(terminal, TerminalSelector, VS, Revision),
    lists:filtermap(
        fun (TerminalRef) ->
            try acceptable_terminal(TerminalRef, Provider, VS, Revision) catch
                false ->
                    false
            end
        end,
        ordsets:to_list(TerminalRefs)
    ).

acceptable_terminal(TerminalRef, #domain_Provider{terms = Terms0}, VS, Revision) ->
    Terminal = #domain_Terminal{
        terms         = Terms1,
        risk_coverage = RiskCoverage
    } = hg_domain:get(Revision, {terminal, TerminalRef}),
    % TODO the ability to override any terms makes for uncommon sense
    %      is it better to allow to override only cash flow / refunds terms?
    Terms = merge_payment_terms(Terms0, Terms1),
    _ = acceptable_payment_terms(Terms, VS, Revision),
    _ = acceptable_risk(RiskCoverage, VS),
    {true, {TerminalRef, Terminal}}.

acceptable_risk(RiskCoverage, VS) ->
    RiskCoverage == getv(risk_score, VS) orelse throw(false).

%%

acceptable_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = CurrenciesSelector,
        categories      = CategoriesSelector,
        payment_methods = PMsSelector,
        cash_limit      = CashLimitSelector,
        holds           = HoldsTerms
    },
    VS,
    Revision
) ->
    % TODO varsets getting mixed up
    %      it seems better to pass down here hierarchy of contexts w/ appropriate module accessors
    _ = try_accept_payment_term(currency     , CurrenciesSelector , VS, Revision),
    _ = try_accept_payment_term(category     , CategoriesSelector , VS, Revision),
    _ = try_accept_payment_term(payment_tool , PMsSelector        , VS, Revision),
    _ = try_accept_payment_term(cost         , CashLimitSelector  , VS, Revision),
    _ = acceptable_holds_terms(HoldsTerms, getv(flow, VS), VS, Revision),
    true;
acceptable_payment_terms(undefined, _VS, _Revision) ->
    throw(false).

acceptable_holds_terms(_Terms, instant, _VS, _Revision) ->
    true;
acceptable_holds_terms(Terms, {hold, Lifetime}, VS, Revision) ->
    case Terms of
        #domain_PaymentHoldsProvisionTerms{lifetime = LifetimeSelector} ->
            _ = try_accept_payment_term(lifetime, Lifetime, LifetimeSelector, VS, Revision),
            true;
        undefined ->
            throw(false)
    end.

try_accept_payment_term(Name, Selector, VS, Revision) ->
    try_accept_payment_term(Name, getv(Name, VS), Selector, VS, Revision).

try_accept_payment_term(Name, Value, Selector, VS, Revision) when Selector /= undefined ->
    Values = reduce(Name, Selector, VS, Revision),
    test_payment_term(Name, Value, Values) orelse throw(false);
try_accept_payment_term(_Name, _Value, undefined, _VS, _Revision) ->
    throw(false).

test_payment_term(currency, V, Vs) ->
    ordsets:is_element(V, Vs);
test_payment_term(category, V, Vs) ->
    ordsets:is_element(V, Vs);
test_payment_term(payment_tool, PT, PMs) ->
    ordsets:is_element(hg_payment_tool:get_method(PT), PMs);
test_payment_term(cost, Cost, CashRange) ->
    hg_condition:test_cash_range(Cost, CashRange) == within;

test_payment_term(lifetime, ?hold_lifetime(Lifetime), ?hold_lifetime(Allowed)) ->
    Lifetime < Allowed.

merge_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = Currencies0,
        categories      = Categories0,
        payment_methods = PaymentMethods0,
        cash_limit      = CashLimit0,
        cash_flow       = Cashflow0,
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
        currencies      = hg_utils:select_defined(Currencies1, Currencies0),
        categories      = hg_utils:select_defined(Categories1, Categories0),
        payment_methods = hg_utils:select_defined(PaymentMethods1, PaymentMethods0),
        cash_limit      = hg_utils:select_defined(CashLimit1, CashLimit0),
        cash_flow       = hg_utils:select_defined(Cashflow1, Cashflow0),
        holds           = merge_holds_terms(Holds1, Holds0),
        refunds         = merge_refunds_terms(Refunds1, Refunds0)
    };
merge_payment_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_holds_terms(
    #domain_PaymentHoldsProvisionTerms{lifetime = Lifetime0},
    #domain_PaymentHoldsProvisionTerms{lifetime = Lifetime1}
) ->
    #domain_PaymentHoldsProvisionTerms{
        lifetime = hg_utils:select_defined(Lifetime1, Lifetime0)
    };
merge_holds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{cash_flow = Cashflow0},
    #domain_PaymentRefundsProvisionTerms{cash_flow = Cashflow1}
) ->
    #domain_PaymentRefundsProvisionTerms{
        cash_flow = hg_utils:select_defined(Cashflow1, Cashflow0)
    };
merge_refunds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

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

%% Marshalling

-include("legacy_structures.hrl").
-include("domain.hrl").

-spec marshal(route()) ->
    hg_msgpack_marshalling:value().

marshal(Route) ->
    marshal(route, Route).

marshal(route, #domain_InvoicePaymentRoute{} = Route) ->
    [2, #{
        <<"provider">> => marshal(provider_ref, Route#domain_InvoicePaymentRoute.provider),
        <<"terminal">> => marshal(terminal_ref, Route#domain_InvoicePaymentRoute.terminal)
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
    #domain_InvoicePaymentRoute{
        provider = unmarshal(provider_ref, Provider),
        terminal = unmarshal(terminal_ref, Terminal)
    };
unmarshal(route, [1, ?legacy_route(Provider, Terminal)]) ->
    #domain_InvoicePaymentRoute{
        provider = unmarshal(provider_ref, Provider),
        terminal = unmarshal(terminal_ref, Terminal)
    };

unmarshal(provider_ref, ?legacy_provider(ObjectID)) ->
    #domain_ProviderRef{id = unmarshal(int, ObjectID)};

unmarshal(provider_ref, ObjectID) ->
    #domain_ProviderRef{id = unmarshal(int, ObjectID)};

unmarshal(terminal_ref, ?legacy_terminal(ObjectID)) ->
    #domain_TerminalRef{id = unmarshal(int, ObjectID)};

unmarshal(terminal_ref, ObjectID) ->
    #domain_TerminalRef{id = unmarshal(int, ObjectID)};

unmarshal(_, Other) ->
    Other.
