%%%
%%% Payment processing machine
%%%

-module(hg_payment_processing).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

-define(NS, <<"payment_processing">>).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).
-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).
-export([namespace     /0]).
-export([init          /2]).
-export([process_signal/2]).
-export([process_call  /2]).

%% Event provider callbacks

-behaviour(hg_event_provider).
-export([publish_event/2]).

%% Types
>>>>>>> HG-231: Add start_binding function

-record(st, {
    rec_payment_tool :: undefined | rec_payment_tool(),
    route            :: undefined | route(),
    session          :: undefined | session()
}).
-type st() :: #st{}.
-export_type([st/0]).

-type rec_payment_tool_id()     :: dmsl_payment_processing_thrift:'RecurrentPaymentToolID'().
-type rec_payment_tool()        :: dmsl_payment_processing_thrift:'RecurrentPaymentTool'().
-type rec_payment_tool_event()  :: dmsl_payment_processing_thrift:'RecurrentPaymentToolEvent'().

-type disposable_payment_resource() :: dmsl_domain_thrift:'DisposablePaymentResource'().

-type route() :: dmsl_domain_thrift:'PaymentRoute'().

-type session() :: #{
    status      := active | suspended | finished,
    trx         := trx_info(),
    proxy_state => proxy_state()
}.

-type proxy_state()   :: dmsl_proxy_thrift:'ProxyState'().
-type trx_info()      :: dmsl_domain_thrift:'TransactionInfo'().

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_handler:handler_opts()) ->
    term() | no_return().
handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(payment_processing,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('CreateRecurrentPaymentTool', [PaymentResource], _Opts) ->
    RecPaymentToolID = hg_utils:unique_id(),
    ok = set_meta(RecPaymentToolID),
    ok = start(RecPaymentToolID, PaymentResource),
    get_rec_tool(get_state(RecPaymentToolID)).

get_rec_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

%%

set_meta(ID) ->
    hg_log_scope:set_meta(#{rec_payment_tool_id => ID}).

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

%%

get_history(RecPaymentToolID) ->
    History = hg_machine:get_history(?NS, RecPaymentToolID),
    unmarshal_history_result(map_history_error(History)).

get_state(RecPaymentToolID) ->
    collapse_history(get_history(RecPaymentToolID)).

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Events}, St0) ->
            lists:foldl(fun apply_change/2, St0, Events)
        end,
        #st{},
        History
    ).

%%

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_CustomerNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

unmarshal_history_result({ok, Result}) ->
    {ok, unmarshal(Result)};
unmarshal_history_result(Error) ->
    Error.

-include("domain.hrl").
-include("payment_processing.hrl").

%% Event provider callbacks

-spec publish_event(rec_payment_tool_id(), rec_payment_tool_event()) ->
    hg_event_provider:public_event().
publish_event(RecPaymentToolID, Changes) when is_list(Changes) ->
    {{rec_payment_tool_id, RecPaymentToolID}, ?recurrent_payment_tool_event(unmarshal({list, changes}, Changes))}.

%% hg_machine callbacks

-spec namespace() ->
    hg_machine:ns().
namespace() ->
    ?NS.

-spec init(rec_payment_tool_id(), disposable_payment_resource()) ->
    hg_machine:result(rec_payment_tool_event()).
init(RecPaymentToolID, PaymentResource) ->
    Route = undefined,
    RecPaymentTool = create_rec_payment_tool(RecPaymentToolID, Route, PaymentResource),
    {ok, {Changes, Action}} = start_session(),
    handle_result(#{
        changes => [Changes ++ ?recurrent_payment_tool_has_created(RecPaymentTool)],
        action => Action
    }).

start_session() ->
    Events = [?session_ev(?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

-spec process_signal(hg_machine:signal(), hg_machine:history(rec_payment_tool_event())) ->
    hg_machine:result(rec_payment_tool_event()).
process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal(History)))).

handle_signal(timeout, St) ->
    process_timeout(St).

process_timeout(St) ->
    case get_session_status(St) of
        active ->
            Action = hg_machine_action:new(),
            process(Action, St);
        % suspended ->
        %     Action = hg_machine_action:new(),
        %     process_callback_timeout(Action, St, Options);
        % finished ->
        %     process_finished_session(St)
        _ ->
            not_implemented
    end.

get_session_status(St) ->
    get_status(get_session(St)).

get_session(#st{session = Session}) ->
    Session.

get_status(Session) ->
    maps:get(status, Session).

process(Action, St) ->
    Session = get_session(St),
    ProxyContext = construct_proxy_context(Session, St),
    {ok, ProxyResult} = hg_proxy_provider:issue_process_call(ProxyContext, St),
    Result = handle_proxy_result(ProxyResult, Action, Session),
    finish_processing(Result, St).

%%

construct_proxy_context(Session, St) ->
    #prxprv_RecurrentTokenGenerationContext{
        session = construct_session(Session),
        token_info = construct_token_info(St),
        options = hg_proxy_provider:collect_proxy_options(St)
    }.

construct_session(Session) ->
    #prxprv_RecurrentTokenGenerationSession{
        state = maps:get(proxy_state, Session, undefined)
    }.

construct_token_info(St) ->
    Trx = get_trx(St),
    PaymentTool = get_rec_payment_tool(St),
    #prxprv_RecurrentTokenInfo{
        payment_tool = construct_proxy_payment_tool(PaymentTool),
        trx = Trx
    }.

get_trx(#st{session = #{trx := Trx}}) ->
    Trx.

set_trx(Trx, St = #st{}) ->
    Session = get_session(St),
    St#st{session = Session#{trx => Trx}}.

get_rec_payment_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

construct_proxy_payment_tool(
    #payproc_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        rec_token = RecToken
    }
) ->
    #prxprv_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        rec_token = RecToken
    }.

%%

handle_proxy_result(
    #prxprv_RecurrentTokenGenerationProxyResult{
        intent = {_Type, Intent},
        trx = Trx,
        next_state = ProxyState,
        token = Token
    },
    Action0,
    Session
) ->
    Changes1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState),
    {Changes3, Action} = hg_proxy_provider:handle_proxy_intent(Intent, Action0),
    Changes = Changes1 ++ Changes2 ++ Changes3,
    case Intent of
        #'FinishIntent'{status = {success, _}} ->
            make_proxy_result(Changes, Session, Action, Token);
        _ ->
            make_proxy_result(Changes, Session, Action)
    end.

make_proxy_result(Changes, Session, Action) ->
    make_proxy_result(Changes, Session, Action, undefined).

make_proxy_result(Changes, Session, Action, Token) ->
    {hg_proxy_provider:wrap_session_events(Changes, Session), Action, Token}.

%%

construct_proxy_context(Session, St) ->
    #prxprv_RecurrentTokenGenerationContext{
        session = construct_session(Session),
        token_info = construct_token_info(St),
        options = hg_proxy_provider:collect_proxy_options(St)
    }.

construct_session(Session) ->
    #prxprv_RecurrentTokenGenerationSession{
        state = maps:get(proxy_state, Session, undefined)
    }.

construct_token_info(St) ->
    Trx = get_trx(St),
    PaymentTool = get_rec_payment_tool(St),
    #prxprv_RecurrentTokenInfo{
        payment_tool = construct_proxy_payment_tool(PaymentTool),
        trx = Trx
    }.

get_trx(#st{session = #{trx := Trx}}) ->
    Trx.

set_trx(Trx, St = #st{}) ->
    Session = get_session(St),
    St#st{session = Session#{trx => Trx}}.

get_rec_payment_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

construct_proxy_payment_tool(
    #payproc_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        rec_token = RecToken
    }
) ->
    #prxprv_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        rec_token = RecToken
    }.

%%

handle_proxy_result(
    #prxprv_RecurrentTokenGenerationProxyResult{
        intent = {_Type, Intent},
        trx = Trx,
        next_state = ProxyState,
        token = Token
    },
    Action0,
    Session
) ->
    Changes1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState),
    {Changes3, Action} = hg_proxy_provider:handle_proxy_intent(Intent, Action0),
    Changes = Changes1 ++ Changes2 ++ Changes3,
    case Intent of
        #'FinishIntent'{status = {success, _}} ->
            make_proxy_result(Changes, Session, Action, Token);
        _ ->
            make_proxy_result(Changes, Session, Action)
    end.

make_proxy_result(Changes, Session, Action) ->
    make_proxy_result(Changes, Session, Action, undefined).

make_proxy_result(Changes, Session, Action, Token) ->
    {hg_proxy_provider:wrap_session_events(Changes, Session), Action, Token}.

%%

finish_processing({Changes, Action, Token}, St) ->
    St1 = apply_changes(Changes, St),
    case get_session(St1) of
        #{status := finished, result := ?session_succeeded()} ->
            {done, {Changes ++ [?recurrent_payment_tool_has_acquired(Token)], Action}};
        #{status := finished, result := ?session_failed(Failure)} ->
            {done, {Changes ++ [?recurrent_payment_tool_has_failed(Failure)], Action}};
        #{} ->
            {next, {Changes, Action}}
    end.

apply_changes(Changes) ->
    apply_changes(Changes, undefined).

apply_changes(Changes, St) ->
    lists:foldl(fun apply_change/2, St, Changes).

apply_change(Event, undefined) ->
    apply_change(Event, #st{});

apply_change(?recurrent_payment_tool_has_created(RecPaymentTool), St) ->
    St#st{rec_payment_tool = RecPaymentTool};
apply_change(?recurrent_payment_tool_has_acquired(Token), St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    St#st{rec_payment_tool =
            RecPaymentTool#payproc_RecurrentPaymentTool{
                rec_token = Token,
                status = ?recurrent_payment_tool_acquired()
            }
    };
apply_change(?recurrent_payment_tool_has_abandoned(), St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    St#st{rec_payment_tool =
            RecPaymentTool#payproc_RecurrentPaymentTool{
                status = ?recurrent_payment_tool_abandoned()
            }
    };
apply_change(?session_ev(?session_started()), St) ->
    St#st{session = create_session(get_trx(St))};
apply_change(?session_ev(Event), St) ->
    Session = merge_session_change(Event, get_session(St)),
    St1 = St#st{session = Session},
    case get_session_status(Session) of
        finished ->
            set_trx(get_trx(St), St1);
        _ ->
            St1
    end.

merge_session_change(?session_finished(Result), Session) ->
    Session#{status := finished, result => Result};
merge_session_change(?session_activated(), Session) ->
    Session#{status := active};
merge_session_change(?session_suspended(), Session) ->
    Session#{status := suspended};
merge_session_change(?trx_bound(Trx), Session) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session) ->
    Session.

%%

create_session(Trx) ->
    #{
        status => active,
        trx    => Trx
    }.

-type call() :: undefined.

-spec process_call(call(), hg_machine:history(rec_payment_tool_event())) ->
    {hg_machine:response(), hg_machine:result(rec_payment_tool_event())}.
process_call(Call, History) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

handle_call(_Call, _St) ->
    not_implemented.

handle_result(#{state := _St} = Params) ->
    Changes = maps:get(changes, Params, []),
    Action = maps:get(action, Params, hg_machine_action:new()),
    case maps:get(response, Params, undefined) of
        undefined ->
            {[marshal(Changes)], Action};
        Response ->
            {{ok, Response}, {[marshal(Changes)], Action}}
    end.

%%

create_rec_payment_tool(RecPaymentToolID, Route, PaymentResource) ->
    #payproc_RecurrentPaymentTool{
        id                 = RecPaymentToolID,
        status             = ?recurrent_payment_tool_created(),
        created_at         = hg_datetime:format_now(),
        payment_resource   = PaymentResource,
        route              = Route,
        rec_token          = undefined
    }.

%%
%% Marshalling
%%

marshal(Changes) when is_list(Changes) ->
    [marshal(change, Change) || Change <- Changes].

marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, changes}, Payload)}.

unmarshal(_, Other) ->
    Other.
