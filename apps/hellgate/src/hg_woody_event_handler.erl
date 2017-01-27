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
            service  => Service,
            function => Function
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
    Meta = get_prepared_meta(EventType, RpcID, RpcMeta),
    lager:log(
        Level,
        [{pid, self()} | Meta],
        Format,
        Args
    ).

get_prepared_meta(EventType, RpcID, RpcMeta) ->
    prepare_meta(lists:append([
        collect_rpc_meta(EventType, RpcID, RpcMeta),
        lager:md()
    ])).

collect_rpc_meta(EventType, RpcID, RpcMeta) ->
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
    maps:to_list(Meta).

prepare_meta(Meta) ->
    prepare_meta(Meta, []).

prepare_meta([], Acc) ->
    Acc;
prepare_meta([{Key, Value} | Rest], Acc) ->
    prepare_meta(Rest, [{Key, make_printable(Value)} | Acc]).

make_printable(Item) when is_atom(Item); is_binary(Item) ->
    Item;
make_printable(Item) when is_list(Item) ->
    [make_printable(V) || V <- Item];
make_printable(Item) when is_map(Item) ->
    maps:map(
        fun(_, V) -> make_printable(V) end,
        Item
    );
make_printable(V) ->
    genlib:format("~p", [V]).
