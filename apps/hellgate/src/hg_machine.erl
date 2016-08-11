-module(hg_machine).

-type id() :: hg_base_thrift:'ID'().
-type ns() :: hg_base_thrift:'Namespace'().
-type args() :: _.

-type event() :: event(_).
-type event(T) :: {event_id(), timestamp(), T}.
-type event_id() :: hg_base_thrift:'EventID'().
-type timestamp() :: hg_base_thrift:'Timestamp'().

-type history() :: history(_).
-type history(T) :: [event(T)].

-type result(T) :: {[T], hg_machine_action:t()}.
-type result() :: result(_).

-callback namespace() ->
    ns().

-callback init(id(), args(), context()) ->
    {result(), context()}.

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history(), context()) ->
    {result(), context()}.

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history(), context()) ->
    {{response(), result()}, context()}.

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

-export([start/4]).
-export([call/4]).
-export([get_history/3]).
-export([get_history/5]).

%% Dispatch

-export([get_child_spec/1]).
-export([get_service_handlers/1]).
-export([get_handler_module/1]).

%% FIXME feels like hack
-export([unwrap_event/1]).

-export([start_link/1]).
-export([init/1]).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type opts() :: #{
    client_context => woody_client:context()
}.

%%

-spec start(ns(), id(), term(), opts()) ->
    {ok | {error, exists}, woody_client:context()}.

start(Ns, ID, Args, #{client_context := Context0}) ->
    call_automaton('Start', [Ns, ID, wrap_args(Args)], Context0).

-spec call(ns(), id(), term(), opts()) ->
    {ok | {ok, term()} | {error, notfound | failed}, woody_client:context()} |
    no_return().

call(Ns, ID, Args, #{client_context := Context0}) ->
    case call_automaton('Call', [Ns, {id, ID}, wrap_args(Args)], Context0) of
        {{ok, Response}, Context} ->
            % should be specific to a processing interface already
            case unmarshal_term(Response) of
                {ok, _} = Ok ->
                    {Ok, Context};
                {exception, Exception} ->
                    throw({Exception, Context})
            end;
        {Error, Context} ->
            {Error, Context}
    end.

-spec get_history(ns(), id(), opts()) ->
    {{history(), event_id()} | {error, notfound | failed}, woody_client:context()} | no_return().

get_history(Ns, ID, Opts) ->
    get_history(Ns, ID, #'HistoryRange'{}, Opts).

-spec get_history(ns(), id(), event_id(), undefined | non_neg_integer(), opts()) ->
    {{history(), event_id()} | {error, notfound | failed}, woody_client:context()} | no_return().

get_history(Ns, ID, AfterID, Limit, Opts) ->
    get_history(Ns, ID, #'HistoryRange'{'after' = AfterID, limit = Limit}, Opts).

get_history(Ns, ID, Range, #{client_context := Context0}) ->
    LastID = #'HistoryRange'.'after',
    case call_automaton('GetHistory', [Ns, {id, ID}, Range], Context0) of
        {{ok, History}, Context} ->
            {unwrap_history(History, LastID), Context};
        {Error, Context} ->
            {Error, Context}
    end.

%%

call_automaton(Function, Args, Context0) ->
    % TODO: hg_config module, aware of config entry semantics
    Url = genlib_app:env(hellgate, automaton_service_url),
    Service = {hg_state_processing_thrift, 'Automaton'},
    try woody_client:call(Context0, {Service, Function, Args}, #{url => Url}) catch
        {{exception, #'MachineAlreadyExists'{}}, Context} ->
            {{error, exists}, Context};
        {{exception, #'MachineNotFound'{}}, Context} ->
            {{error, notfound}, Context};
        {{exception, #'MachineFailed'{}}, Context} ->
            {{error, failed}, Context}
    end.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody_server_thrift_handler:args(), woody_client:context(), [ns()]) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('ProcessSignal', {Args}, Context0, [Ns]) ->
    _ = hg_utils:logtag_process(namespace, Ns),
    #'SignalArgs'{signal = {_Type, Signal}, history = History} = Args,
    {Result, Context} = dispatch_signal(Ns, Signal, History, Context0),
    {{ok, Result}, Context};

handle_function('ProcessCall', {Args}, Context0, [Ns]) ->
    _ = hg_utils:logtag_process(namespace, Ns),
    #'CallArgs'{arg = Payload, history = History} = Args,
    {Result, Context} = dispatch_call(Ns, Payload, History, Context0),
    {{ok, Result}, Context}.

%%

-spec dispatch_signal(ns(), Signal, hg_machine:history(), woody_client:context()) ->
    {Result, woody_client:context()} when
        Signal ::
            hg_state_processing_thrift:'InitSignal'() |
            hg_state_processing_thrift:'TimeoutSignal'() |
            hg_state_processing_thrift:'RepairSignal'(),
        Result ::
            hg_state_processing_thrift:'SignalResult'().

dispatch_signal(Ns, #'InitSignal'{id = ID, arg = Payload}, [], Context0) ->
    Args = unwrap_args(Payload),
    _ = lager:debug("dispatch init with id = ~s and args = ~p", [ID, Args]),
    Module = get_handler_module(Ns),
    {Result, #{client_context := Context}} = Module:init(ID, Args, create_context(Context0)),
    {marshal_signal_result(Result), Context};

dispatch_signal(Ns, #'TimeoutSignal'{}, History0, Context0) ->
    History = unwrap_events(History0),
    _ = lager:debug("dispatch timeout with history = ~p", [History]),
    Module = get_handler_module(Ns),
    {Result, #{client_context := Context}} = Module:process_signal(timeout, History, create_context(Context0)),
    {marshal_signal_result(Result), Context};

dispatch_signal(Ns, #'RepairSignal'{arg = Payload}, History0, Context0) ->
    Args = unwrap_args(Payload),
    History = unwrap_events(History0),
    _ = lager:debug("dispatch repair with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    {Result, #{client_context := Context}} = Module:process_signal({repair, Args}, History, create_context(Context0)),
    {marshal_signal_result(Result), Context}.

marshal_signal_result({Events, Action}) ->
    _ = lager:debug("signal result with events = ~p and action = ~p", [Events, Action]),
    #'SignalResult'{
        events = wrap_events(Events),
        action = Action
    }.

-spec dispatch_call(ns(), Call, hg_machine:history(), woody_client:context()) ->
    {Result, woody_client:context()} when
        Call :: hg_state_processing_thrift:'Args'(),
        Result :: hg_state_processing_thrift:'CallResult'().

dispatch_call(Ns, Payload, History0, Context0) ->
    Args = unwrap_args(Payload),
    History = unwrap_events(History0),
    _ = lager:debug("dispatch call with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    {Result, #{client_context := Context}} = Module:process_call(Args, History, create_context(Context0)),
    {marshal_call_result(Result), Context}.

marshal_call_result({Response, {Events, Action}}) ->
    _ = lager:debug("call response = ~p with event = ~p and action = ~p", [Response, Events, Action]),
    #'CallResult'{
        events = wrap_events(Events),
        action = Action,
        response = marshal_term(Response)
    }.

create_context(ClientContext) ->
    #{client_context => ClientContext}.

%%

-type service_handler() ::
    {Path :: string(), {woody_t:service(), woody_t:handler(), [ns()]}}.

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
    {Path, {Service, ?MODULE, [Ns]}}.

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

-spec unwrap_event(hg_state_processing_thrift:'Event'()) ->
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
