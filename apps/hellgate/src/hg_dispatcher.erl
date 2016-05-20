-module(hg_dispatcher).

-export([start_machine/3]).
-export([call_machine/4]).

-export([dispatch_signal/3]).
-export([dispatch_call/3]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type opts() :: #{
    context => woody_client:context()
}.

%%

-spec start_machine(module(), term(), opts()) -> hg_machine:id().

start_machine(Module, Args, #{context := Context}) ->
    {{ok, Response}, _} = call_automaton('start', [wrap_args(Module, Args)], Context),
    #'StartResult'{id = ID} = Response,
    ID.

-spec call_machine(module(), hg_machine:id(), term(), opts()) -> term() | no_return().

call_machine(Module, ID, Args, #{context := Context}) ->
    case call_automaton('call', [ID, wrap_args(Module, Args)], Context) of
        {{ok, Response}, _} ->
            % should be specific to a processing interface already
            case unmarshal_term(Response) of
                ok ->
                    ok;
                {ok, Result} ->
                    {ok, Result};
                {exception, Exception} ->
                    throw(Exception)
            end;
        {{exception, Exception}, _} ->
            % TODO: exception mapping
            throw(Exception);
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

-type signal() ::
    hg_state_processing_thrift:'InitSignal'() |
    hg_state_processing_thrift:'TimeoutSignal'() |
    hg_state_processing_thrift:'RepairSignal'().

-type signal_result() ::
    hg_state_processing_thrift:'SignalResult'().

-spec dispatch_signal(signal(), hg_machine:history(), opts()) -> signal_result().

dispatch_signal(#'InitSignal'{id = ID, arg = Payload}, [], Opts) ->
    {Module, Args} = unwrap_args(Payload),
    marshal_signal_result(Module:init(ID, Args), Module);

dispatch_signal(#'TimeoutSignal'{}, History0, Opts) ->
    % TODO: deducing module from signal payload looks more natural
    %       opaque payload in every event?
    {Module, History} = unwrap_history(History0),
    marshal_signal_result(Module:process_signal(timeout, History), Module);

dispatch_signal(#'RepairSignal'{arg = Payload}, History0, Opts) ->
    {Module, History} = unwrap_history(History0),
    Args = unmarshal_term(Payload),
    marshal_signal_result(Module:process_signal({repair, Args}, History), Module).

marshal_signal_result({ok, {Event, Action}}, Module) ->
    #'SignalResult'{
        ev = {event, wrap_event(Module, Event)},
        action = Action
    }.

-type call() ::
    hg_state_processing_thrift:'Call'().

-type call_result() ::
    hg_state_processing_thrift:'CallResult'().

-spec dispatch_call(call(), hg_machine:history(), opts()) -> call_result().

dispatch_call(Payload, History0, Opts) ->
    % TODO: looks suspicious
    {Module, Args} = unwrap_args(Payload),
    {Module, History} = unwrap_history(History0),
    marshal_call_result(Module:process_call(Args, History), Module).

%%

marshal_call_result({ok, Response, {Event, Action}}, Module) ->
    #'CallResult'{
        ev = {event, wrap_event(Module, Event)},
        action = Action,
        response = marshal_term(Response)
    }.

%%

unwrap_history(History = [Event | _]) ->
    {Module, _} = unwrap_event(Event),
    {Module, [begin {_, EventInner} = unwrap_event(E), EventInner end || E <- History]}.

wrap_event(Module, EventInner) ->
    wrap_args(Module, EventInner).

unwrap_event(Payload) ->
    unwrap_args(Payload).

wrap_args(Module, Args) ->
    marshal_term({Module, Args}).

unwrap_args(Payload) ->
    unmarshal_term(Payload).

marshal_term(V) ->
    term_to_binary(V).

unmarshal_term(B) ->
    binary_to_term(B).
