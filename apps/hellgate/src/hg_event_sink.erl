-module(hg_event_sink).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_state_processing_thrift.hrl").

-type event_id() :: dmsl_base_thrift:'EventID'().
-type event()    :: dmsl_payment_processing_thrift:'Event'().

-spec handle_function
    ('GetEvents', woody:args(), hg_woody_wrapper:handler_opts()) ->
        [event()] | no_return();
    ('GetLastEventID', woody:args(), hg_woody_wrapper:handler_opts()) ->
        event_id() | no_return().

handle_function('GetEvents', [#payproc_EventRange{'after' = After, limit = Limit}], _Opts) ->
    try
        get_public_history(After, Limit)
    catch
        {exception, #'EventNotFound'{}} ->
            throw(#payproc_EventNotFound{})
    end;

handle_function('GetLastEventID', [], _Opts) ->
    % TODO handle thrift exceptions here
    case get_history_range(undefined, 1, backward) of
        [#'SinkEvent'{id = ID}] ->
            ID;
        [] ->
            throw(#payproc_NoLastEvent{})
    end.

get_public_history(After, Limit) ->
    [publish_event(Ev) || Ev <- get_history_range(After, Limit)].

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
