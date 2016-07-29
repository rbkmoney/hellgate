-module(hg_event_sink).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type func() :: 'GetEvents' | 'GetLastEventID'.

-spec handle_function(func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('GetEvents', {#payproc_EventRange{'after' = After, limit = Limit}}, Context0, _Opts) ->
    HistoryRange = #'HistoryRange'{'after' = After, limit = Limit},
    try
        {History, Context} = get_public_history(HistoryRange, Context0),
        {{ok, History}, Context}
    catch
        {{exception, #'EventNotFound'{}}, Context1} ->
            throw({#payproc_EventNotFound{}, Context1})
    end;

handle_function('GetLastEventID', {}, Context0, _Opts) ->
    try
        call_event_sink('GetLastEventID', [], Context0)
    catch
        {{exception, #'NoLastEvent'{}}, Context} ->
            throw({#payproc_NoLastEvent{}, Context})
    end.

get_public_history(HistoryRange = #'HistoryRange'{limit = undefined}, Context0) ->
    {History0, Context} = get_history_range(HistoryRange, Context0),
    {_LastID, History} = hg_machine:map_history(History0),
    {History, Context};

get_public_history(#'HistoryRange'{limit = 0}, Context) ->
    {[], Context};
get_public_history(HistoryRange = #'HistoryRange'{limit = N}, Context0) ->
    {History0, Context1} = get_history_range(HistoryRange, Context0),
    {LastID, History} = hg_machine:map_history(History0),
    case length(History0) of
        N when length(History) =:= N ->
            {History, Context1};
        N ->
            NextRange = #'HistoryRange'{'after' = LastID, limit = N - length(History)},
            {HistoryRest, Context2} = get_public_history(NextRange, Context1),
            {History ++ HistoryRest, Context2};
        M when M < N ->
            {History, Context1}
    end.

get_history_range(HistoryRange, Context0) ->
    {{ok, History}, Context} = call_event_sink('GetHistory', [HistoryRange], Context0),
    {History, Context}.

call_event_sink(Function, Args, Context) ->
    Url = genlib_app:env(hellgate, eventsink_service_url),
    Service = {hg_state_processing_thrift, 'EventSink'},
    woody_client:call(Context, {Service, Function, Args}, #{url => Url}).

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.
