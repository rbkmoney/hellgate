%%% Naïve routing oracle

-module(hg_routing).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([choose/2]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-type t() :: dmsl_domain_thrift:'InvoicePaymentRoute'().
%%-type risk_score() :: dmsl_domain_thrift:'RiskScore'().

-spec choose(hg_selector:varset(), hg_domain:revision()) ->
    t() | undefined.

choose(VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    Providers = collect_providers(Globals, VS, Revision),
    choose_provider_terminal(Providers, VS).

choose_provider_terminal([{ProviderRef, [TerminalRef | _]} | _], _) ->
    #domain_InvoicePaymentRoute{
        provider = ProviderRef,
        terminal = TerminalRef
    };
choose_provider_terminal([{_ProviderRef, []} | Rest], VS) ->
    choose_provider_terminal(Rest, VS);
choose_provider_terminal([], _) ->
    undefined.

%%

collect_providers(Globals, VS, Revision) ->
    ProviderSelector = Globals#domain_Globals.providers,
    ProviderRefs = ordsets:to_list(reduce(provider, ProviderSelector, VS, Revision)),
    [
        {ProviderRef, collect_terminals(hg_domain:get(Revision, {provider, ProviderRef}), VS, Revision)} ||
            ProviderRef <- ProviderRefs
    ].

collect_terminals(Provider, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    TerminalRefs = ordsets:to_list(reduce(terminal, TerminalSelector, VS, Revision)),
    [
        TerminalRef ||
            TerminalRef <- TerminalRefs,
            Terminal <- [hg_domain:get(Revision, {terminal, TerminalRef})],
                filter_terminal(Terminal, VS)
    ].

filter_terminal(
    #domain_Terminal{
        category = Category,
        payment_method = PaymentMethod,
        account = #domain_TerminalAccount{currency = Currency},
        risk_coverage = RiskCoverage,
        payment_flow = PaymentFlow
    },
    VS
) ->
    Category       == maps:get(category, VS) andalso
    Currency       == maps:get(currency, VS) andalso
    PaymentMethod  == hg_payment_tool:get_method(maps:get(payment_tool, VS)) andalso
    is_risk_covered(maps:get(risk_score, VS), RiskCoverage) andalso
    is_flow_suitable(PaymentFlow, maps:get(payment_flow, VS)).

%%

reduce(Name, S, VS, Revision) ->
    case hg_selector:reduce(S, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

is_risk_covered(RiskScore, RiskCoverage) ->
    RiskScore == RiskCoverage.

-include("domain.hrl").

is_flow_suitable(PaymentFlowTerminal, PaymentFlow) ->
    case {PaymentFlowTerminal, PaymentFlow} of
        {{hold, TerminalFlowHold}, {hold, ?hold_lifetime(PaymentHoldLifetime)}} ->
            #domain_TerminalPaymentFlowHold{
                hold_lifetime = ?hold_lifetime(TerminalHoldLifetime)
            } = TerminalFlowHold,
            TerminalHoldLifetime >= PaymentHoldLifetime;
        {_, instant} ->
            true;
        {_, {hold, _}} ->
            false
    end.

-include("legacy_structures.hrl").
%% Marshalling

-spec marshal(t()) ->
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
    t().

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