-module(hg_machine).

-type id() :: binary().
-type args() :: _.
-type event() :: _.

-type history() :: [event()].
-type result() :: {event(), hg_machine_action:t()}.

-callback init(id(), args()) ->
    {ok, result()}.

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history()) ->
    {ok, result()}.

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history()) ->
    {ok, response(), result()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([history/0]).
-export_type([result/0]).

-export([start/3]).
-export([call/4]).
-export([get_history/3]).

-export([dispatch_signal/3]).
-export([dispatch_call/3]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type opts() :: #{
    context => woody_client:context()
}.

%%

-spec start(module(), term(), opts()) -> id().

start(Module, Args, #{context := Context}) ->
    {{ok, Response}, _} = call_automaton('start', [#'Args'{arg = wrap_args(Module, Args)}], Context),
    #'StartResult'{id = ID} = Response,
    ID.

-spec call(module(), id(), term(), opts()) -> term() | no_return().

call(Module, ID, Args, #{context := Context}) ->
    case call_automaton('call', [{id, ID}, wrap_args(Module, Args)], Context) of
        {{ok, Response}, _} ->
            % should be specific to a processing interface already
            case unmarshal_term(Response) of
                ok ->
                    ok;
                {ok, Result} ->
                    Result;
                {exception, Exception} ->
                    throw(Exception)
            end;
        {{exception, Exception}, _} ->
            % TODO: exception mapping
            throw(Exception);
        {{error, Reason}, _} ->
            error(Reason)
    end.

-spec get_history(module(), id(), opts()) -> history().

get_history(Module, ID, #{context := Context}) ->
    case call_automaton('getHistory', [{id, ID}, #'HistoryRange'{}], Context) of
        {{ok, History0}, _} ->
            {Module, History} = unwrap_history(unmarshal_history(History0)),
            History;
        {{exception, Exception}, _} ->
            % TODO: exception mapping
            throw(Exception);
        {{error, Reason}, _} ->
            error(Reason)
    end.

unmarshal_history(undefined) ->
    [];
unmarshal_history(History) ->
    [{ID, Body} || #'Event'{id = ID, body = Body} <- History].

%%

call_automaton(Function, Args, Context) ->
    % TODO: hg_config module, aware of config entry semantics
    Url = genlib_app:env(hellgate, automaton_service_url),
    Service = {hg_state_processing_thrift, 'Automaton'},
    woody_client:call_safe(Context, {Service, Function, Args}, #{url => Url}).

%%


-spec dispatch_signal(Signal, hg_machine:history(), opts()) -> Result when
    Signal ::
        hg_state_processing_thrift:'InitSignal'() |
        hg_state_processing_thrift:'TimeoutSignal'() |
        hg_state_processing_thrift:'RepairSignal'(),
    Result ::
        hg_state_processing_thrift:'SignalResult'().

dispatch_signal(#'InitSignal'{id = ID, arg = Payload}, [], _Opts) ->
    % TODO: do not ignore `Opts`
    {Module, Args} = unwrap_args(Payload),
    _ = lager:debug("[machine] [~p] dispatch init (~p: ~p) with history: ~p", [Module, ID, Args, []]),
    marshal_signal_result(Module:init(ID, Args), Module);

dispatch_signal(#'TimeoutSignal'{}, History0, _Opts) ->
    % TODO: do not ignore `Opts`
    % TODO: deducing module from signal payload looks more natural
    %       opaque payload in every event?
    {Module, History} = unwrap_history(History0),
    _ = lager:debug("[machine] [~p] dispatch timeout with history: ~p", [Module, History]),
    marshal_signal_result(Module:process_signal(timeout, History), Module);

dispatch_signal(#'RepairSignal'{arg = Payload}, History0, _Opts) ->
    % TODO: do not ignore `Opts`
    {Module, History} = unwrap_history(History0),
    Args = unmarshal_term(Payload),
    _ = lager:debug("[machine] [~p] dispatch repair (~p) with history: ~p", [Module, Args, History]),
    marshal_signal_result(Module:process_signal({repair, Args}, History), Module).

marshal_signal_result({ok, {Event, Action}}, Module) ->
    _ = lager:debug("[machine] [~p] result with event = ~p and action = ~p", [Module, Event, Action]),
    #'SignalResult'{
        ev = wrap_event(Module, Event),
        action = Action
    }.


-spec dispatch_call(Call, hg_machine:history(), opts()) -> Result when
    Call :: hg_state_processing_thrift:'Call'(),
    Result :: hg_state_processing_thrift:'CallResult'().

dispatch_call(Payload, History0, _Opts) ->
    % TODO: do not ignore `Opts`
    % TODO: looks suspicious
    {Module, Args} = unwrap_args(Payload),
    {Module, History} = unwrap_history(History0),
    _ = lager:debug("[machine] [~p] dispatch call (~p) with history: ~p", [Module, Args, History]),
    marshal_call_result(Module:process_call(Args, History), Module).

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
