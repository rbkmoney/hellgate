-module(hg_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType = ?EV_CALL_SERVICE, RpcID, #{
    service  := Service,
    function := Function,
    type     := Type,
    metadata := Metadata
} =  Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            service  => Service,
            function => Function,
            type     => Type,
            metadata => Metadata
        }
    );

handle_event(EventType = ?EV_SERVICE_RESULT, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_CLIENT_SEND, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_CLIENT_RECEIVE, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_SERVER_RECEIVE, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_SERVER_SEND, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_INTERNAL_ERROR, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, Meta );

handle_event(EventType = ?EV_TRACE, RpcID, #{
    event := Event,
    role := Role
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            event => Event,
            role => Role
        }
    );

handle_event(EventType = ?EV_INVOKE_SERVICE_HANDLER, RpcID, #{
    service  := Service,
    function := Function
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                service  => Service,
                function => Function
            }
        }
    );

handle_event(EventType = ?EV_SERVICE_HANDLER_RESULT, RpcID, Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        Meta
    );


handle_event(EventType, RpcID, EventMeta, _Opts) ->
    log(EventType, RpcID, EventMeta, #{}).

log(EventType, RpcID, RawMeta, RpcMeta) ->
    {Level, {Format, Args}} = woody_event_handler:format_event(EventType, RawMeta, RpcID),
    lager:log(
        Level,
        lists:append([
            [{pid, self()}],
            construct_md(collect_meta(EventType, RpcID, RpcMeta)),
            lager:md()
        ]),
        Format,
        Args
    ).

collect_meta(EventType, RpcID, RpcMeta) ->
    Meta = lists:foldl(
        fun(M, Acc) ->
            maps:merge(M, Acc)
        end,
        #{},
        [
            #{
                event_type => EventType
            },
            RpcID,
            RpcMeta
        ]
    ),
    #{
        rpc => Meta
    }.

construct_md(Map = #{}) ->
    maps:to_list(Map).
