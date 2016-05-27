-module(hg_api_event_handler).
-behaviour(woody_event_handler).

-export([handle_event/3]).

%%

-spec handle_event(EventType, RpcID, EventMeta)
    -> _ when
        EventType :: woody_event_handler:event_type(),
        RpcID ::  woody_t:rpc_id(),
        EventMeta :: woody_event_handler:event_meta_type().

handle_event(EventType, RpcID, EventMeta) when
    EventType == 'thrift error';
    EventType == 'internal error'
->
    lager:error(maps:to_list(RpcID), "[server] ~s: ~p", [EventType, EventMeta]);

handle_event(EventType, RpcID, EventMeta) ->
    lager:debug(maps:to_list(RpcID), "[server] ~s: ~p", [EventType, EventMeta]).
