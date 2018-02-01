-module(hg_proxy_provider).

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([construct_proxy_context/3]).
-export([collect_proxy_options/1]).

-export([process_payment/2]).
-export([generate_token/2]).
-export([handle_payment_callback/3]).
-export([handle_recurrent_token_callback/3]).

-export([bind_transaction/2]).
-export([update_proxy_state/1]).
-export([handle_proxy_intent/2]).
-export([wrap_session_events/2]).

-export([handle_proxy_result/3]).
-export([handle_callback_result/3]).
-export([handle_proxy_callback_result/3]).
-export([handle_proxy_callback_timeout/2]).

-include("domain.hrl").
-include("payment_events.hrl").

%%

-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type route() :: dmsl_domain_thrift:'PaymentRoute'().
-type payment_info() :: dmsl_proxy_provider_thrift:'PaymentInfo'().
-type payment_context() :: dmsl_proxy_provider_thrift:'PaymentContext'().
-type proxy_result() :: dmsl_proxy_provider_thrift:'PaymentProxyResult'().
-type callback_result() :: dmsl_proxy_provider_thrift:'PaymentCallbackResult'().
-type callback_proxy_result() :: dmsl_proxy_provider_thrift:'PaymentCallbackProxyResult'().

%%

-spec construct_proxy_context(hg_proxy_provider_session:st(), payment_info(), route()) ->
    payment_context().

construct_proxy_context(Session, PaymentInfo, Route) ->
    #prxprv_PaymentContext{
        session      = construct_session(Session),
        payment_info = PaymentInfo,
        options      = collect_proxy_options(Route)
    }.

construct_session(Session) ->
    #prxprv_Session{
        target = hg_proxy_provider_session:get_target(Session),
        state = hg_proxy_provider_session:get_proxy_state(Session)
    }.

-spec collect_proxy_options(route()) ->
    dmsl_domain_thrift:'ProxyOptions'().

collect_proxy_options(#domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef}) ->
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

-spec process_payment(_ProxyContext, route()) ->
    term().
process_payment(ProxyContext, Route) ->
    issue_call('ProcessPayment', [ProxyContext], Route).

-spec generate_token(_ProxyContext, route()) ->
    term().
generate_token(ProxyContext, Route) ->
    issue_call('GenerateToken', [ProxyContext], Route).

-spec handle_payment_callback(_Payload, _ProxyContext, route()) ->
    term().
handle_payment_callback(Payload, ProxyContext, St) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], St).

-spec handle_recurrent_token_callback(_Payload, _ProxyContext, route()) ->
    term().
handle_recurrent_token_callback(Payload, ProxyContext, St) ->
    issue_call('HandleRecurrentTokenCallback', [Payload, ProxyContext], St).

-spec issue_call(woody:func(), list(), route()) ->
    term().
issue_call(Func, Args, Route) ->
    hg_woody_wrapper:call(proxy_provider, Func, Args, get_call_options(Route)).

get_call_options(Route) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider(Route)}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route_provider(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

%%

-spec bind_transaction(trx_info(), term()) ->
    list().
bind_transaction(undefined, _Session) ->
    % no transaction yet
    [];
bind_transaction(Trx, Session) ->
    case hg_proxy_provider_session:get_trx(Session) of
        undefined ->
            % got transaction, nothing bound so far
            [?trx_bound(Trx)];
        Trx ->
            % got the same transaction as one which has been bound previously
            [];
        TrxWas ->
            % got transaction which differs from the bound one
            % verify against proxy contracts
            case Trx#domain_TransactionInfo.id of
                ID when ID =:= TrxWas#domain_TransactionInfo.id ->
                    [?trx_bound(Trx)];
                _ ->
                    error(proxy_contract_violated)
            end
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
handle_proxy_intent(#'prxprv_RecurrentTokenFinishIntent'{status = {success, _}}, Action) ->
    Events = [?session_finished(?session_succeeded())],
    {Events, Action};
handle_proxy_intent(#'prxprv_RecurrentTokenFinishIntent'{status = {failure, Failure}}, Action) ->
    Events = [?session_finished(?session_failed(convert_failure(Failure)))],
    {Events, Action};
handle_proxy_intent(#'SleepIntent'{timer = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = try_request_interaction(UserInteraction),
    {Events, Action};
handle_proxy_intent(#'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended(Tag) | try_request_interaction(UserInteraction)],
    {Events, Action}.

convert_failure(#'Failure'{code = Code, description = Description}) ->
    ?external_failure(Code, Description).

try_request_interaction(undefined) ->
    [];
try_request_interaction(UserInteraction) ->
    [?interaction_requested(UserInteraction)].

%%

-spec handle_proxy_result(proxy_result(), _Action, hg_proxy_provider_session:st()) ->
    {list(), _Action}.

handle_proxy_result(
    #prxprv_PaymentProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, Action0),
    {wrap_session_events(Events1 ++ Events2 ++ Events3, Session), Action}.

-spec handle_callback_result(callback_result(), _Action, hg_proxy_provider_session:st()) ->
    {binary(), _Action}.

handle_callback_result(
    #prxprv_PaymentCallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

-spec handle_proxy_callback_result(callback_proxy_result(), _Action, hg_proxy_provider_session:st()) ->
    {list(), _Action}.

handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0)),
    {wrap_session_events([?session_activated()] ++ Events1 ++ Events2 ++ Events3, Session), Action};
handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {wrap_session_events(Events1 ++ Events2, Session), Action0}.

-spec handle_proxy_callback_timeout(_Action, hg_proxy_provider_session:st()) ->
    {list(), _Action}.

handle_proxy_callback_timeout(Action, Session) ->
    Events = [?session_finished(?session_failed(?operation_timeout()))],
    {wrap_session_events(Events, Session), Action}.

-spec wrap_session_events(list(), _Action) ->
    list().
wrap_session_events(SessionEvents, Session) ->
    Target = hg_proxy_provider_session:get_target(Session),
    [?session_ev(Target, Ev) || Ev <- SessionEvents].
