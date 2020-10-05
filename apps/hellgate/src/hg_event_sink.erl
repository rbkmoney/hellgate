-module(hg_event_sink).

-export([get_events/3]).
-export([get_last_event_id/1]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type sink_event() :: {
    hg_machine:event_id(),
    hg_machine:ns(),
    hg_machine:id(),
    hg_machine:event()
}.

-export_type([sink_event/0]).

%% Internal types

-type event_sink_id() :: dmsl_base_thrift:'ID'().
-type event_id() :: dmsl_base_thrift:'EventID'().

%% API

-spec get_events(event_sink_id(), event_id(), integer()) -> {ok, [sink_event()]} | {error, event_not_found}.
get_events(EventSinkID, After, Limit) ->
    try
        {ok, get_history_range(EventSinkID, After, Limit)}
    catch
        {exception, #mg_stateproc_EventNotFound{}} ->
            {error, event_not_found}
    end.

-spec get_last_event_id(event_sink_id()) -> {ok, event_id()} | {error, no_last_event}.
get_last_event_id(EventSinkID) ->
    case get_history_range(EventSinkID, undefined, 1, backward) of
        [{ID, _, _, _}] ->
            {ok, ID};
        [] ->
            {error, no_last_event}
    end.

get_history_range(EventSinkID, After, Limit) ->
    get_history_range(EventSinkID, After, Limit, forward).

get_history_range(EventSinkID, After, Limit, Direction) ->
    HistoryRange = #mg_stateproc_HistoryRange{'after' = After, limit = Limit, direction = Direction},
    {ok, History} = call_event_sink('GetHistory', {EventSinkID, HistoryRange}),
    map_sink_events(History).

call_event_sink(Function, Args) ->
    hg_woody_wrapper:call(eventsink, Function, Args).

map_sink_events(History) ->
    [map_sink_event(Ev) || Ev <- History].

map_sink_event(#mg_stateproc_SinkEvent{id = ID, source_ns = Ns, source_id = SourceID, event = Event}) ->
    #mg_stateproc_Event{
        id = EventID,
        created_at = Dt,
        format_version = FormatVer,
        data = Payload
    } = Event,
    DecodedPayload = #{
        format_version => FormatVer,
        data => mg_msgpack_marshalling:unmarshal(Payload)
    },
    {ID, Ns, SourceID, {EventID, Dt, DecodedPayload}}.
