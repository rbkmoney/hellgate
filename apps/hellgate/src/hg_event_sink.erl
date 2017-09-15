-module(hg_event_sink).

-export([get_events/2]).
-export([get_last_event_id/0]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type event_id() :: dmsl_base_thrift:'EventID'().
-type event()    :: dmsl_payment_processing_thrift:'Event'().

-spec get_events(event_id(), integer()) ->
    {ok, [event()]} | {error, event_not_found}.

get_events(After, Limit) ->
    try
        get_public_history(After, Limit)
    catch
        {exception, #'EventNotFound'{}} ->
            {error, event_not_found}
    end.

-spec get_last_event_id() ->
    {ok, event_id()} | {error, no_last_event}.

get_last_event_id() ->
    case get_history_range(undefined, 1, backward) of
        [#'SinkEvent'{id = ID}] ->
            {ok, ID};
        [] ->
            {error, no_last_event}
    end.

get_public_history(After, Limit) ->
    History = [publish_event(Ev) || Ev <- get_history_range(After, Limit)],
    {ok, History}.

get_history_range(After, Limit) ->
    get_history_range(After, Limit, forward).

get_history_range(After, Limit, Direction) ->
    HistoryRange = #'HistoryRange'{'after' = After, limit = Limit, direction = Direction},
    {ok, History} = call_event_sink('GetHistory', [HistoryRange]),
    History.

publish_event(#'SinkEvent'{id = ID, source_ns = Ns, source_id = SourceID, event = Event}) ->
    #'Event'{id = EventID, created_at = Dt, event_payload = Payload} = Event,
    hg_event_provider:publish_event(Ns, ID, SourceID,  {EventID, Dt, hg_msgpack_marshalling:unmarshal(Payload)}).

-define(EVENTSINK_ID, <<"payproc">>).

call_event_sink(Function, Args) ->
    hg_woody_wrapper:call('EventSink', Function, [?EVENTSINK_ID | Args]).
