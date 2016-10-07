-module(hg_event_sink).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type event_id() :: hg_base_thrift:'EventID'().
-type event()    :: hg_payment_processing_thrift:'Event'().

-spec handle_function
    ('GetEvents', woody_server_thrift_handler:args(), woody_client:context(), []) ->
        {{ok, [event()]}, woody_client:context()} | no_return();
    ('GetLastEventID', woody_server_thrift_handler:args(), woody_client:context(), []) ->
        {{ok, event_id()}, woody_client:context()} | no_return().

handle_function('GetEvents', {#payproc_EventRange{'after' = After, limit = Limit}}, Context0, _Opts) ->
    try
        {History, Context} = get_public_history(After, Limit, Context0),
        {{ok, History}, Context}
    catch
        {{exception, #'EventNotFound'{}}, Context1} ->
            throw({#payproc_EventNotFound{}, Context1})
    end;

handle_function('GetLastEventID', {}, Context0, _Opts) ->
    case get_history_range(undefined, 1, backward, Context0) of
        {{[#'SinkEvent'{id = ID}], _LastID}, Context} ->
            {{ok, ID}, Context};
        {{[], _LastID}, Context} ->
            throw({#payproc_NoLastEvent{}, Context})
    end.

get_public_history(After, Limit, Context) ->
    hg_history:get_public_history(
        fun get_history_range/3,
        fun publish_event/1,
        After, Limit,
        Context
    ).

get_history_range(After, Limit, Context0) ->
    get_history_range(After, Limit, forward, Context0).

get_history_range(After, Limit, Direction, Context0) ->
    HistoryRange = #'HistoryRange'{'after' = After, limit = Limit, direction = Direction},
    {{ok, History}, Context} = call_event_sink('GetHistory', [HistoryRange], Context0),
    {{History, get_history_last_id(History, After)}, Context}.

get_history_last_id([], LastID) ->
    LastID;
get_history_last_id(History, _LastID) ->
    Event = lists:last(History),
    Event#'SinkEvent'.id.

publish_event(#'SinkEvent'{id = ID, source_ns = Ns, source_id = SourceID, event = Event}) ->
    hg_event_provider:publish_event(Ns, ID, SourceID, hg_machine:unwrap_event(Event)).

call_event_sink(Function, Args, Context) ->
    Url = genlib_app:env(hellgate, eventsink_service_url),
    Service = {hg_state_processing_thrift, 'EventSink'},
    EventsinkID = genlib_app:env(hellgate, eventsink_id),
    woody_client:call(Context, {Service, Function, [EventsinkID | Args]}, #{url => Url}).

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.
