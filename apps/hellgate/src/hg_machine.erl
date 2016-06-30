-module(hg_machine).

-type id() :: binary().
-type args() :: _.

-type event() :: event(_).
-type event(T) :: {event_id(), id(), datetime(), sequence(), T}.
-type event_id() :: integer().
-type datetime() :: calendar:datetime().
-type sequence() :: pos_integer().

-type history() :: history(_).
-type history(T) :: [event(T)].

-type result(T) :: {[T], hg_machine_action:t()}.
-type result() :: result(_).

-callback init(id(), args(), context()) ->
    {{ok, result()}, context()}.

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history(), context()) ->
    {{ok, result()}, context()}.

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history(), context()) ->
    {{ok, response(), result()}, context()}.

-type context() :: #{
    client_context => woody_client:context()
}.

%% TODO not the right place for sure
-callback map_event(event()) ->
    term().

-export_type([id/0]).
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
-export([get_history/3]).
-export([get_history/5]).

%% TODO not the right place either
-export([map_history/1]).

-export([dispatch_signal/3]).
-export([dispatch_call/3]).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type opts() :: #{
    client_context => woody_client:context()
}.

%%

-spec start(module(), term(), opts()) -> {id(), woody_client:context()}.

start(Module, Args, #{client_context := Context0}) ->
    {{ok, Response}, Context} = call_automaton('start', [#'Args'{arg = wrap_args({Module, Args})}], Context0),
    #'StartResult'{id = ID} = Response,
    {ID, Context}.

-spec call(id(), term(), opts()) -> {term(), woody_client:context()} | no_return().

call(ID, Args, #{client_context := Context0}) ->
    case call_automaton('call', [{id, ID}, wrap_args(Args)], Context0) of
        {{ok, Response}, Context} ->
            % should be specific to a processing interface already
            case unmarshal_term(Response) of
                ok ->
                    {ok, Context};
                {ok, Result} ->
                    {Result, Context};
                {exception, Exception} ->
                    throw({Exception, Context})
            end;
        {{exception, Exception}, Context} ->
            throw({Exception, Context});
        {{error, Reason}, _} ->
            error(Reason)
    end.

-spec get_history(module(), id(), opts()) ->
    {history(), woody_client:context()}.

get_history(Module, ID, Opts) ->
    get_history(Module, ID, #'HistoryRange'{}, Opts).

-spec get_history(module(), id(), event_id(), undefined | non_neg_integer(), opts()) ->
    {history(), woody_client:context()}.

get_history(Module, ID, AfterID, Limit, Opts) when is_integer(AfterID) ->
    get_history(Module, ID, #'HistoryRange'{'after' = AfterID, limit = Limit}, Opts).

get_history(Module, ID, Range, #{client_context := Context0}) ->
    case call_automaton('getHistory', [{id, ID}, Range], Context0) of
        {{ok, []}, Context} ->
            {[], Context};
        {{ok, History0}, Context} ->
            {Module, History} = untag_history(unwrap_history(History0)),
            {History, Context};
        {{exception, Exception}, Context} ->
            throw({Exception, Context});
        {{error, Reason}, _} ->
            error(Reason)
    end.

%%

call_automaton(Function, Args, Context) ->
    % TODO: hg_config module, aware of config entry semantics
    Url = genlib_app:env(hellgate, automaton_service_url),
    Service = {hg_state_processing_thrift, 'Automaton'},
    woody_client:call_safe(Context, {Service, Function, Args}, #{url => Url}).

%%

-type func() :: 'processSignal' | 'processCall'.

-spec handle_function(func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('processSignal', {Args}, Context0, _Opts) ->
    #'SignalArgs'{signal = {_Type, Signal}, history = History} = Args,
    {Result, Context} = dispatch_signal(Signal, History, opts(Context0)),
    {{ok, Result}, Context};

handle_function('processCall', {Args}, Context0, _Opts) ->
    #'CallArgs'{call = Payload, history = History} = Args,
    {Result, Context} = dispatch_call(Payload, History, opts(Context0)),
    {{ok, Result}, Context}.

opts(Context) ->
    #{client_context => Context}.


create_context(ClientContext) ->
    #{client_context => ClientContext}.
%%

-spec dispatch_signal(Signal, hg_machine:history(), opts()) -> {Result, woody_client:context()} when
    Signal ::
        hg_state_processing_thrift:'InitSignal'() |
        hg_state_processing_thrift:'TimeoutSignal'() |
        hg_state_processing_thrift:'RepairSignal'(),
    Result ::
        hg_state_processing_thrift:'SignalResult'().

dispatch_signal(#'InitSignal'{id = ID, arg = Payload}, [], _Opts = #{client_context := Context0}) ->
    {Module, Args} = unwrap_args(Payload),
    _ = lager:debug("[machine] [~p] dispatch init (~p: ~p) with history: ~p", [Module, ID, Args, []]),
    {Result, #{client_context := Context}} = Module:init(ID, Args, create_context(Context0)),
    {marshal_signal_result(Result, Module, []), Context};

dispatch_signal(#'TimeoutSignal'{}, History0, _Opts = #{client_context := Context0}) ->
    % TODO: deducing module from signal payload looks more natural
    %       opaque payload in every event?
    {Module, History} = untag_history(unwrap_history(History0)),
    _ = lager:debug("[machine] [~p] dispatch timeout with history: ~p", [Module, History]),
    {Result, #{client_context := Context}} = Module:process_signal(timeout, History, create_context(Context0)),
    {marshal_signal_result(Result, Module, History), Context};

dispatch_signal(#'RepairSignal'{arg = Payload}, History0, _Opts = #{client_context := Context0}) ->
    Args = unmarshal_term(Payload),
    {Module, History} = untag_history(unwrap_history(History0)),
    _ = lager:debug("[machine] [~p] dispatch repair (~p) with history: ~p", [Module, Args, History]),
    {Result, #{client_context := Context}} = Module:process_signal({repair, Args}, History, create_context(Context0)),
    {marshal_signal_result(Result, Module, History), Context}.

marshal_signal_result({ok, {Events, Action}}, Module, History) ->
    _ = lager:debug("[machine] [~p] result with events = ~p and action = ~p", [Module, Events, Action]),
    #'SignalResult'{
        events = wrap_events(Module, History, Events),
        action = Action
    }.


-spec dispatch_call(Call, hg_machine:history(), opts()) -> {Result, woody_client:context()} when
    Call :: hg_state_processing_thrift:'Call'(),
    Result :: hg_state_processing_thrift:'CallResult'().

dispatch_call(Payload, History0, _Opts = #{client_context := Context0}) ->
    % TODO: looks suspicious
    Args = unwrap_args(Payload),
    {Module, History} = untag_history(unwrap_history(History0)),
    _ = lager:debug("[machine] [~p] dispatch call (~p) with history: ~p", [Module, Args, History]),
    {Result, #{client_context := Context}} = Module:process_call(Args, History, create_context(Context0)),
    {marshal_call_result(Result, Module, History), Context}.

%%

marshal_call_result({ok, Response, {Events, Action}}, Module, History) ->
    _ = lager:debug(
        "[machine] [~p] call response = ~p with event = ~p and action = ~p",
        [Module, Response, Events, Action]
    ),
    #'CallResult'{
        events = wrap_events(Module, History, Events),
        action = Action,
        response = marshal_term(Response)
    }.

%%

-spec map_history(hg_state_processing_thrift:'History'()) ->
    [term()].

map_history(History) ->
    [Module:map_event(E) || {Module, E} <- unwrap_history(History)].

unwrap_history(History) ->
    [unwrap_event(E) || E <- History].

untag_history(History = [{Module, _} | _]) ->
    {Module, [E || {_, E} <- History]}.

wrap_events(Module, History, Events) ->
    wrap_events_(Module, length(History) + 1, Events).

wrap_events_(Module, Seq, [Ev | Rest]) ->
    [wrap_event(Module, Seq, Ev) | wrap_events_(Module, Seq + 1, Rest)];
wrap_events_(_, _, []) ->
    [].

wrap_event(Module, Seq, EventInner) ->
    marshal_term({Module, Seq, EventInner}).

unwrap_event(#'Event'{id = ID, machine_id = Source, created_at = Dt, event_payload = Payload}) ->
    {Module, Seq, EventInner} = unmarshal_term(Payload),
    {Module, {ID, Source, Dt, Seq, EventInner}}.

wrap_args(Args) ->
    marshal_term(Args).

unwrap_args(Payload) ->
    unmarshal_term(Payload).

marshal_term(V) ->
    term_to_binary(V).

unmarshal_term(B) ->
    binary_to_term(B).
