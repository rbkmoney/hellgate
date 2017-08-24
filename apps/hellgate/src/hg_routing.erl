%%% Naïve routing oracle

-module(hg_routing).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([choose/2]).
-export([get_payments_terms/2]).

%%

-include("domain.hrl").

-type terms()    :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type route()    :: dmsl_domain_thrift:'InvoicePaymentRoute'().

-type t() :: route().

-spec choose(hg_selector:varset(), hg_domain:revision()) ->
    t() | undefined.

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

-spec get_payments_terms(t(), hg_domain:revision()) -> terms().

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
        cash_limit      = CashLimitSelector
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
    true;
acceptable_payment_terms(undefined, _VS, _Revision) ->
    throw(false).

try_accept_payment_term(Name, Selector, VS, Revision) when Selector /= undefined ->
    Values = reduce(Name, Selector, VS, Revision),
    test_payment_term(Name, getv(Name, VS), Values) orelse throw(false);
try_accept_payment_term(_Name, undefined, _VS, _Revision) ->
    throw(false).

test_payment_term(cost, Cost, CashRange) ->
    hg_condition:test_cash_range(Cost, CashRange) == within;
test_payment_term(payment_tool, PT, PMs) ->
    ordsets:is_element(hg_payment_tool:get_method(PT), PMs);
test_payment_term(_Name, V, Vs) ->
    ordsets:is_element(V, Vs).

merge_payment_terms(
    Terms0 = #domain_PaymentsProvisionTerms{},
    Terms1 = #domain_PaymentsProvisionTerms{}
) ->
    #domain_PaymentsProvisionTerms{
        currencies = hg_utils:select_defined(
            Terms1#domain_PaymentsProvisionTerms.currencies,
            Terms0#domain_PaymentsProvisionTerms.currencies
        ),
        categories = hg_utils:select_defined(
            Terms1#domain_PaymentsProvisionTerms.categories,
            Terms0#domain_PaymentsProvisionTerms.categories
        ),
        payment_methods = hg_utils:select_defined(
            Terms1#domain_PaymentsProvisionTerms.payment_methods,
            Terms0#domain_PaymentsProvisionTerms.payment_methods
        ),
        cash_limit = hg_utils:select_defined(
            Terms1#domain_PaymentsProvisionTerms.cash_limit,
            Terms0#domain_PaymentsProvisionTerms.cash_limit
        ),
        cash_flow = hg_utils:select_defined(
            Terms1#domain_PaymentsProvisionTerms.cash_flow,
            Terms0#domain_PaymentsProvisionTerms.cash_flow
        ),
        refunds = merge_refund_terms(
            Terms1#domain_PaymentsProvisionTerms.refunds,
            Terms0#domain_PaymentsProvisionTerms.refunds
        )
    };
merge_payment_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_refund_terms(
    Terms0 = #domain_PaymentRefundsProvisionTerms{},
    Terms1 = #domain_PaymentRefundsProvisionTerms{}
) ->
    #domain_PaymentRefundsProvisionTerms{
        cash_flow = hg_utils:select_defined(
            Terms1#domain_PaymentRefundsProvisionTerms.cash_flow,
            Terms0#domain_PaymentRefundsProvisionTerms.cash_flow
        )
    };
merge_refund_terms(Terms0, Terms1) ->
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
