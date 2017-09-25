-module(hg_proxy_provider).

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([collect_proxy_options/1]).

-export([issue_process_call/3]).
-export([issue_callback_call/4]).

-export([bind_transaction/2]).

-export([update_proxy_state/1]).

-export([handle_proxy_intent/2]).

-export([wrap_session_events/2]).

-include("domain.hrl").
-include("payment_events.hrl").

%%

-record(st, {
    route :: undefined | route()
}).
-type st() :: #st{}.

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type route() :: dmsl_domain_thrift:'PaymentRoute'().

%%

-spec collect_proxy_options(st()) ->
    term().
collect_proxy_options(
    #st{
        route = #domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef}
    }
) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    lists:foldl(
        fun
            (undefined, M) ->
                M;
            (M1, M) ->
                maps:merge(M1, M)
        end,
        #{},
        [
            Terminal#domain_Terminal.options,
            Proxy#domain_Proxy.additional,
            ProxyDef#domain_ProxyDefinition.options
        ]
    ).

%%

-spec issue_process_call(atom(), _ProxyContext, st()) ->
    term().
issue_process_call(Call, ProxyContext, St) ->
    issue_call(Call, [ProxyContext], St).

-spec issue_callback_call(atom(), _Payload, _ProxyContext, st()) ->
    term().
issue_callback_call(Callback, Payload, ProxyContext, St) ->
    issue_call(Callback, [Payload, ProxyContext], St).

-spec issue_call(woody:func(), list(), hg_woody_wrapper:client_opts()) ->
    term().
issue_call(Func, Args, St) ->
    CallOpts = get_call_options(St),
    hg_woody_wrapper:call('ProviderProxy', Func, Args, CallOpts).

get_call_options(St) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider(get_route(St))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#st{route = Route}) ->
    Route.

get_route_provider(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

%%

-spec bind_transaction(trx_info(), term()) ->
    list().
bind_transaction(undefined, _Session) ->
    % no transaction yet
    [];
bind_transaction(Trx, #{trx := undefined}) ->
    % got transaction, nothing bound so far
    [?trx_bound(Trx)];
bind_transaction(Trx, #{trx := Trx}) ->
    % got the same transaction as one which has been bound previously
    [];
bind_transaction(Trx, #{trx := TrxWas}) ->
    % got transaction which differs from the bound one
    % verify against proxy contracts
    case Trx#domain_TransactionInfo.id of
        ID when ID =:= TrxWas#domain_TransactionInfo.id ->
            [?trx_bound(Trx)];
        _ ->
            error(proxy_contract_violated)
    end.

%%

-spec update_proxy_state(term()) ->
    list().
update_proxy_state(undefined) ->
    [];
update_proxy_state(ProxyState) ->
    [?proxy_st_changed(ProxyState)].

%%

-spec handle_proxy_intent(_Intent, _Action) ->
    {list(), _Action}.
handle_proxy_intent(#'FinishIntent'{status = {success, _}}, Action) ->
    Events = [?session_finished(?session_succeeded())],
    {Events, Action};
handle_proxy_intent(#'FinishIntent'{status = {failure, Failure}}, Action) ->
    Events = [?session_finished(?session_failed(convert_failure(Failure)))],
    {Events, Action};
handle_proxy_intent(#'SleepIntent'{timer = Timer}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = [],
    {Events, Action};
handle_proxy_intent(#'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended() | try_request_interaction(UserInteraction)],
    {Events, Action}.

convert_failure(#'Failure'{code = Code, description = Description}) ->
    ?external_failure(Code, Description).

try_request_interaction(undefined) ->
    [];
try_request_interaction(UserInteraction) ->
    [?interaction_requested(UserInteraction)].

%%

-spec wrap_session_events(list(), _Action) ->
    list().
wrap_session_events(SessionEvents, #{target := Target}) ->
    [?session_ev(Target, Ev) || Ev <- SessionEvents].
