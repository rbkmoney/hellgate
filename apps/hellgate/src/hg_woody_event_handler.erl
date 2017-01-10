-module(hg_woody_event_handler).
-behaviour(woody_event_handler).

-export([handle_event/4]).

%%

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}, _Opts) ->
    lager:error(
        construct_md(RpcID),
        "[server] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta, _Optss) ->
    lager:debug(construct_md(RpcID), "[server] ~s: ~p", [EventType, EventMeta]).

construct_md(undefined) ->
    [];
construct_md(Map = #{}) ->
    maps:to_list(Map).
