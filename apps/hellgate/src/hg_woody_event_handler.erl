-module(hg_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").
-include_lib("woody/src/woody_defs.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, RawMeta, Opts) ->
    FilteredMeta = filter_meta(RawMeta),
    scoper_woody_event_handler:handle_event(Event, RpcID, FilteredMeta, Opts).

filter_meta(RawMeta) ->
    maps:map(
        fun
            (result, V) -> filter_result(V);
            (args, V) -> filter_args(V);
            (_K, V) -> V
        end,
        RawMeta).

filter_result({ok, Result}) -> {ok, filter(Result)};
filter_result({system, SystemError}) -> {system, filter(SystemError)};
filter_result({exception, Exception}) -> {exception, filter(Exception)};
filter_result(Result) -> filter(Result).

filter_args(Args) -> filter(Args).

filter(L) when is_list(L) -> [filter(E) || E <- L];
filter(M) when is_map(M) -> maps:map(fun (_K, V) -> filter(V) end, M);

%% common
filter(V) when is_integer(V) -> V;
filter(V) when is_bitstring(V) -> V;
filter(ok) -> ok;
filter(undefined) -> undefined;
filter({internal, Error, Details} = V) when is_atom(Error) and is_binary(Details) -> V;
filter({external, Error, Details} = V) when is_atom(Error) and is_binary(Details) -> V;

%% filters for mg_proto_msgpack_thrift:'Value'()
filter({nl,  _} = V) -> V;
filter({b,   _} = V) -> V;
filter({i,   _} = V) -> V;
filter({flt, _} = V) -> V;
filter({str, _} = V) -> V;
filter({bin, V}) when size(V) > 0 -> {bin, <<"...">>};
filter({bin, _} = V) -> V;
filter({obj, V}) -> {obj, filter(V)};
filter({arr, V}) -> {arr, filter(V)};

%% filters for mg_proto_state_processing_thrift:'Signal'()
filter({init,    V}) -> {init, filter(V)};
filter({timeout, V}) -> {timeout, filter(V)};
filter({repair,  V}) -> {repair, filter(V)};

% mg proto filters
filter(#mg_stateproc_Content{data = D} = V) ->
    V#mg_stateproc_Content{
        data = filter(D)
    };
filter(#mg_stateproc_Event{data = D} = V) ->
    V#mg_stateproc_Event{
        data = filter(D)
    };
filter(#mg_stateproc_Machine{history = H, aux_state = A, aux_state_legacy = AL} = V) ->
    V#mg_stateproc_Machine{
        history = filter(H),
        aux_state = filter(A),
        aux_state_legacy = filter(AL)
    };
filter(#mg_stateproc_MachineStateChange{aux_state = A, events = E, aux_state_legacy = AL, events_legacy = EL} = V) ->
    V#mg_stateproc_MachineStateChange{
        aux_state = filter(A),
        events = filter(E),
        aux_state_legacy = filter(AL),
        events_legacy = filter(EL)
    };
filter(#mg_stateproc_CallArgs{arg = A, machine = M} = V) ->
    V#mg_stateproc_CallArgs{
        arg = filter(A),
        machine = filter(M)
    };
filter(#mg_stateproc_CallResult{response = A, change = C, action = AC} = V) ->
    V#mg_stateproc_CallResult{
        response = filter(A),
        change = filter(C),
        action = filter(AC)
    };
filter(#mg_stateproc_InitSignal{arg = A} = V) ->
    V#mg_stateproc_InitSignal{
        arg = filter(A)
    };
filter(#mg_stateproc_SignalResult{action = A, change = C} = V) ->
    V#mg_stateproc_SignalResult{
        action = filter(A),
        change = filter(C)
    };
filter(#mg_stateproc_RepairSignal{arg = A} = V) ->
    V#mg_stateproc_RepairSignal{
        arg = filter(A)
    };
filter(#mg_stateproc_SignalArgs{signal = A, machine = M} = V) ->
    V#mg_stateproc_SignalArgs{
        signal = filter(A),
        machine = filter(M)
    };
filter(#mg_stateproc_MachineEvent{event = A} = V) ->
    V#mg_stateproc_MachineEvent{
        event = filter(A)
    };
filter(#mg_stateproc_ModernizeEventResult{event_payload = A} = V) ->
    V#mg_stateproc_ModernizeEventResult{
        event_payload = filter(A)
    };
filter(#mg_stateproc_SinkEvent{event = A} = V) ->
    V#mg_stateproc_SinkEvent{
        event = filter(A)
    };

%% Don't filter all other/unknown features
filter(W) ->
    W.
