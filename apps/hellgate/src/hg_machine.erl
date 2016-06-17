-module(hg_machine).

-type id() :: binary().
-type args() :: _.
-type event() :: _.

-type history(Event) :: [Event].
-type history() :: history(event()).

-type result(Event) :: {Event, hg_machine_action:t()}.
-type result() :: result(event()).

-callback init(id(), args(), woody_client:context()) ->
    {{ok, result()}, woody_client:context()}.

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history(), woody_client:context()) ->
    {{ok, result()}, woody_client:context()}.

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history(), woody_client:context()) ->
    {{ok, response(), result()}, woody_client:context()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([signal/0]).
-export_type([history/0]).
-export_type([history/1]).
-export_type([result/0]).
-export_type([result/1]).

-export([start/3]).
-export([call/4]).
-export([get_history/3]).

-export([dispatch_signal/3]).
-export([dispatch_call/3]).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type opts() :: #{
    context => woody_client:context()
}.

%%

-spec start(module(), term(), opts()) -> {id(), woody_client:context()}.

start(Module, Args, #{context := Context0}) ->
    {{ok, Response}, Context} = call_automaton('start', [#'Args'{arg = wrap_args(Module, Args)}], Context0),
    #'StartResult'{id = ID} = Response,
    {ID, Context}.

-spec call(module(), id(), term(), opts()) -> {term(), woody_client:context()} | no_return().

call(Module, ID, Args, #{context := Context0}) ->
    case call_automaton('call', [{id, ID}, wrap_args(Module, Args)], Context0) of
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
            % TODO: exception mapping
            throw({Exception, Context});
        {{error, Reason}, _} ->
            error(Reason)
    end.

-spec get_history(module(), id(), opts()) -> {history(), woody_client:context()}.

get_history(Module, ID, #{context := Context0}) ->
    case call_automaton('getHistory', [{id, ID}, #'HistoryRange'{}], Context0) of
        {{ok, History0}, Context} ->
            {Module, History} = unwrap_history(unmarshal_history(History0)),
            {History, Context};
        {{exception, Exception}, Context} ->
            % TODO: exception mapping
            throw({Exception, Context});
        {{error, Reason}, _Context} ->
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
    {Result, Context} = dispatch_signal(Signal, unmarshal_history(History), opts(Context0)),
    {{ok, Result}, Context};

handle_function('processCall', {Args}, Context0, _Opts) ->
    #'CallArgs'{call = Payload, history = History} = Args,
    {Result, Context} = dispatch_call(Payload, unmarshal_history(History), opts(Context0)),
    {{ok, Result}, Context}.

opts(Context) ->
    #{context => Context}.

%%

-spec dispatch_signal(Signal, hg_machine:history(), opts()) -> {Result, woody_client:context()} when
    Signal ::
        hg_state_processing_thrift:'InitSignal'() |
        hg_state_processing_thrift:'TimeoutSignal'() |
        hg_state_processing_thrift:'RepairSignal'(),
    Result ::
        hg_state_processing_thrift:'SignalResult'().

dispatch_signal(#'InitSignal'{id = ID, arg = Payload}, [], _Opts = #{context := Context0}) ->
    {Module, Args} = unwrap_args(Payload),
    _ = lager:debug("[machine] [~p] dispatch init (~p: ~p) with history: ~p", [Module, ID, Args, []]),
    {Result, Context} = Module:init(ID, Args, Context0),
    {marshal_signal_result(Result, Module), Context};

dispatch_signal(#'TimeoutSignal'{}, History0, _Opts = #{context := Context0}) ->
    % TODO: deducing module from signal payload looks more natural
    %       opaque payload in every event?
    {Module, History} = unwrap_history(History0),
    _ = lager:debug("[machine] [~p] dispatch timeout with history: ~p", [Module, History]),
    {Result, Context} = Module:process_signal(timeout, History, Context0),
    {marshal_signal_result(Result, Module), Context};

dispatch_signal(#'RepairSignal'{arg = Payload}, History0, _Opts = #{context := Context0}) ->
    {Module, History} = unwrap_history(History0),
    Args = unmarshal_term(Payload),
    _ = lager:debug("[machine] [~p] dispatch repair (~p) with history: ~p", [Module, Args, History]),
    {Result, Context} = Module:process_signal({repair, Args}, History, Context0),
    {marshal_signal_result(Result, Module), Context}.

marshal_signal_result({ok, {Event, Action}}, Module) ->
    _ = lager:debug("[machine] [~p] result with event = ~p and action = ~p", [Module, Event, Action]),
    #'SignalResult'{
        ev = wrap_event(Module, Event),
        action = Action
    }.


-spec dispatch_call(Call, hg_machine:history(), opts()) -> {Result, woody_client:context()} when
    Call :: hg_state_processing_thrift:'Call'(),
    Result :: hg_state_processing_thrift:'CallResult'().

dispatch_call(Payload, History0, _Opts = #{context := Context0}) ->
    % TODO: looks suspicious
    {Module, Args} = unwrap_args(Payload),
    {Module, History} = unwrap_history(History0),
    _ = lager:debug("[machine] [~p] dispatch call (~p) with history: ~p", [Module, Args, History]),
    {Result, Context} = Module:process_call(Args, History, Context0),
    {marshal_call_result(Result, Module), Context}.

%%

marshal_call_result({ok, Response, {Event, Action}}, Module) ->
    _ = lager:debug(
        "[machine] [~p] call response = ~p with event = ~p and action = ~p",
        [Module, Response, Event, Action]
    ),
    #'CallResult'{
        ev = wrap_event(Module, Event),
        action = Action,
        response = marshal_term(Response)
    }.

%%

unmarshal_history(undefined) ->
    [];
unmarshal_history(History) ->
    [{ID, Body} || #'Event'{id = ID, body = Body} <- History].

unwrap_history(History = [Event | _]) ->
    {_ID, {Module, _EventInner}} = unwrap_event(Event),
    {Module, [begin {ID, {_, EventInner}} = unwrap_event(E), {ID, EventInner} end || E <- History]}.

wrap_event(Module, EventInner) ->
    wrap_args(Module, EventInner).

unwrap_event({ID, Payload}) ->
    {ID, unwrap_args(Payload)}.

wrap_args(Module, Args) ->
    marshal_term({Module, Args}).

unwrap_args(Payload) ->
    unmarshal_term(Payload).

marshal_term(V) ->
    term_to_binary(V).

unmarshal_term(B) ->
    binary_to_term(B).
