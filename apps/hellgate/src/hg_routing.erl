%%% Naïve routing oracle

-module(hg_routing).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([choose/2]).

%%

-type t() :: dmsl_domain_thrift:'InvoicePaymentRoute'().
-type varset() :: #{}. % FIXME

-spec choose(varset(), hg_domain:revision()) ->
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
    ProviderRefs = ordsets:to_list(reduce(ProviderSelector, VS, Revision)),
    [
        {ProviderRef, collect_terminals(hg_domain:get(Revision, {provider, ProviderRef}), VS, Revision)} ||
            ProviderRef <- ProviderRefs
    ].

collect_terminals(Provider, VS, Revision) ->
    TerminalSelector = Provider#domain_Provider.terminal,
    TerminalRefs = reduce(TerminalSelector, VS, Revision),
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
        account = #domain_TerminalAccount{currency = Currency}
    },
    VS
) ->
    Category      == maps:get(category, VS) andalso
    Currency      == maps:get(currency, VS) andalso
    PaymentMethod == hg_payment_tool:get_method(maps:get(payment_tool, VS)).

%%

reduce(S, VS, Revision) ->
    {value, V} = hg_selector:reduce(S, VS, Revision), V. % FIXME
