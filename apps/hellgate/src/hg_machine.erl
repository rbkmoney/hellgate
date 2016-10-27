-module(hg_machine).

-type id() :: dmsl_base_thrift:'ID'().
-type ref() :: dmsl_state_processing_thrift:'Reference'().
-type ns() :: dmsl_base_thrift:'Namespace'().
-type args() :: _.

-type event() :: event(_).
-type event(T) :: {event_id(), timestamp(), T}.
-type event_id() :: dmsl_base_thrift:'EventID'().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().

-type history() :: history(_).
-type history(T) :: [event(T)].

-type result(T) :: {[T], hg_machine_action:t()}.
-type result() :: result(_).

-callback namespace() ->
    ns().

-callback init(id(), args()) ->
    result().

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history()) ->
    result().

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history()) ->
    {response(), result()}.

-type context() :: #{
    client_context => woody_client:context()
}.

-export_type([id/0]).
-export_type([ns/0]).
-export_type([event_id/0]).
-export_type([event/0]).
-export_type([event/1]).
-export_type([history/0]).
-export_type([history/1]).
-export_type([signal/0]).
-export_type([result/0]).
-export_type([result/1]).
-export_type([context/0]).

-export([start/3]).
-export([call/3]).
-export([get_history/2]).
-export([get_history/4]).

%% Dispatch

-export([get_child_spec/1]).
-export([get_service_handlers/1]).
-export([get_handler_module/1]).

%% FIXME feels like hack
-export([unwrap_event/1]).

-export([start_link/1]).
-export([init/1]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-include_lib("dmsl/include/dmsl_state_processing_thrift.hrl").


%%

-spec start(ns(), id(), term()) ->
    {ok, term()} | {error, exists | term()} | no_return().

start(Ns, ID, Args) ->
    call_automaton('Start', [Ns, ID, wrap_args(Args)]).

-spec call(ns(), ref(), term()) ->
    {ok, term()} | {error, notfound | failed} | no_return().

call(Ns, Ref, Args) ->
    case call_automaton('Call', [Ns, Ref, wrap_args(Args)]) of
        {ok, Response} when is_binary(Response) ->
            % should be specific to a processing interface already
            {ok, unmarshal_term(Response)};
        {error, _} = Error ->
            Error
    end.

-spec get_history(ns(), id()) ->
    {ok, {history(), event_id()}} | {error, notfound | failed} | no_return().

get_history(Ns, ID) ->
    get_history(Ns, ID, #'HistoryRange'{}).

-spec get_history(ns(), id(), undefined | event_id(), undefined | non_neg_integer()) ->
    {ok, {history(), event_id()}} | {error, notfound | failed} | no_return().

get_history(Ns, ID, AfterID, Limit) ->
    get_history(Ns, ID, #'HistoryRange'{'after' = AfterID, limit = Limit}).

get_history(Ns, ID, Range) ->
    LastID = #'HistoryRange'.'after',
    case call_automaton('GetHistory', [Ns, {id, ID}, Range]) of
        {ok, History} when is_list(History) ->
            {ok, unwrap_history(History, LastID)};
        Error ->
            Error
    end.

%%

call_automaton(Function, Args) ->
    Result = hg_woody_wrapper:call_safe('Automaton', Function, Args),
    case Result of
        {exception, #'MachineAlreadyExists'{}} ->
            {error, exists};
        {exception, #'MachineNotFound'{}} ->
            {error, notfound};
        {exception, #'MachineFailed'{}} ->
            {error, failed};
        {exception, Reason} ->
            throw(Reason);
        {error, Reason} ->
            error(Reason);
        _ ->
            {ok, Result}
    end.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody_server_thrift_handler:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function('ProcessSignal', {Args}, #{ns := Ns} = _Opts) ->
    _ = hg_utils:logtag_process(namespace, Ns),
    #'SignalArgs'{signal = {_Type, Signal}, history = History} = Args,
    dispatch_signal(Ns, Signal, History);

handle_function('ProcessCall', {Args}, #{ns := Ns} = _Opts) ->
    _ = hg_utils:logtag_process(namespace, Ns),
    #'CallArgs'{arg = Payload, history = History} = Args,
    dispatch_call(Ns, Payload, History).

%%

-spec dispatch_signal(ns(), Signal, hg_machine:history()) ->
    Result when
        Signal ::
            dmsl_state_processing_thrift:'InitSignal'() |
            dmsl_state_processing_thrift:'TimeoutSignal'() |
            dmsl_state_processing_thrift:'RepairSignal'(),
        Result ::
            dmsl_state_processing_thrift:'SignalResult'().

dispatch_signal(Ns, #'InitSignal'{id = ID, arg = Payload}, []) ->
    Args = unwrap_args(Payload),
    _ = lager:debug("dispatch init with id = ~s and args = ~p", [ID, Args]),
    Module = get_handler_module(Ns),
    Result = Module:init(ID, Args),
    marshal_signal_result(Result);

dispatch_signal(Ns, #'TimeoutSignal'{}, History0) ->
    History = unwrap_events(History0),
    _ = lager:debug("dispatch timeout with history = ~p", [History]),
    Module = get_handler_module(Ns),
    Result = Module:process_signal(timeout, History),
    marshal_signal_result(Result);

dispatch_signal(Ns, #'RepairSignal'{arg = Payload}, History0) ->
    Args = unwrap_args(Payload),
    History = unwrap_events(History0),
    _ = lager:debug("dispatch repair with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    Result = Module:process_signal({repair, Args}, History),
    marshal_signal_result(Result).

marshal_signal_result({Events, Action}) ->
    _ = lager:debug("signal result with events = ~p and action = ~p", [Events, Action]),
    #'SignalResult'{
        events = wrap_events(Events),
        action = Action
    }.

-spec dispatch_call(ns(), Call, hg_machine:history()) ->
    Result when
        Call :: dmsl_state_processing_thrift:'Args'(),
        Result :: dmsl_state_processing_thrift:'CallResult'().

dispatch_call(Ns, Payload, History0) ->
    Args = unwrap_args(Payload),
    History = unwrap_events(History0),
    _ = lager:debug("dispatch call with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    Result = Module:process_call(Args, History),
    marshal_call_result(Result).

marshal_call_result({Response, {Events, Action}}) ->
    _ = lager:debug("call response = ~p with event = ~p and action = ~p", [Response, Events, Action]),
    #'CallResult'{
        events = wrap_events(Events),
        action = Action,
        response = marshal_term(Response)
    }.

%%

-type service_handler() ::
    {Path :: string(), {woody_t:service(), woody_t:handler(), hg_woody_wrapper:handler_opts()}}.

-spec get_child_spec([MachineHandler :: module()]) ->
    supervisor:child_spec().

get_child_spec(MachineHandlers) ->
    #{
        id => hg_machine_dispatch,
        start => {?MODULE, start_link, [MachineHandlers]},
        type => supervisor
    }.

-spec get_service_handlers([MachineHandler :: module()]) ->
    [service_handler()].

get_service_handlers(MachineHandlers) ->
    lists:map(fun get_service_handler/1, MachineHandlers).

get_service_handler(MachineHandler) ->
    Ns = MachineHandler:namespace(),
    {Path, Service} = hg_proto:get_service_spec(processor, #{namespace => Ns}),
    {Path, {Service, hg_woody_wrapper, #{ns => Ns, handler => ?MODULE}}}.

%%

-define(TABLE, hg_machine_dispatch).

-spec start_link([module()]) ->
    {ok, pid()}.

start_link(MachineHandlers) ->
    supervisor:start_link(?MODULE, MachineHandlers).

-spec init([module()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(MachineHandlers) ->
    _ = ets:new(?TABLE, [protected, named_table, {read_concurrency, true}]),
    true = ets:insert_new(?TABLE, [{MH:namespace(), MH} || MH <- MachineHandlers]),
    {ok, {#{}, []}}.

%%

-spec get_handler_module(ns()) -> module().

get_handler_module(Ns) ->
    ets:lookup_element(?TABLE, Ns, 2).

%%

-spec unwrap_event(dmsl_state_processing_thrift:'Event'()) ->
    event().

unwrap_event(#'Event'{id = ID, created_at = Dt, event_payload = Payload}) ->
    {ID, Dt, unmarshal_term(Payload)}.

%%

unwrap_history(History, LastID) ->
    lists:mapfoldl(
        fun (E, _WasLastID) -> {ID, _, _} = V = unwrap_event(E), {V, ID} end,
        LastID,
        History
    ).

unwrap_events(History) ->
    [unwrap_event(E) || E <- History].

wrap_events(Events) ->
    [wrap_event(E) || E <- Events].

wrap_event(Event) ->
    marshal_term(Event).

wrap_args(Args) ->
    marshal_term(Args).

unwrap_args(Payload) ->
    unmarshal_term(Payload).

marshal_term(V) ->
    term_to_binary(V).

unmarshal_term(B) ->
    binary_to_term(B).
