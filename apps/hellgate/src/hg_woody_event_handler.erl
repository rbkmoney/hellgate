-module(hg_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

%% client

handle_event(EventType = ?EV_CALL_SERVICE, RpcID, #{
    service  := Service,
    function := Function,
    type     := Type,
    metadata := Metadata
} = Meta, _Opts) ->
    ok = hg_log_scope:new('rpc.client'),
    ok = hg_log_scope:set_meta(try_append_event_metadata(Metadata, #{
        service  => Service,
        function => Function,
        type     => Type
    })),
    log(EventType, RpcID, Meta, #{}, RpcID);

handle_event(EventType = ?EV_CLIENT_SEND, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, #{}, RpcID);

handle_event(EventType = ?EV_CLIENT_RECEIVE, RpcID, Meta = #{status := Status}, _Opts) ->
    log(EventType, RpcID, Meta, #{
        status => Status
    }, RpcID);

handle_event(EventType = ?EV_SERVICE_RESULT, RpcID, Meta = #{status := Status}, _Opts) ->
    _ = log(EventType, RpcID, Meta, #{
        status => Status
    }, RpcID),
    hg_log_scope:clean();

%% server

handle_event(EventType = ?EV_SERVER_RECEIVE, RpcID, Meta, _Opts) ->
    ok = tag_process(RpcID),
    ok = hg_log_scope:new('rpc.server'),
    log(EventType, RpcID, Meta, #{});

handle_event(EventType = ?EV_INVOKE_SERVICE_HANDLER, RpcID, #{
    service  := Service,
    function := Function,
    metadata := Metadata
} = Meta, _Opts) ->
    ok = hg_log_scope:set_meta(try_append_event_metadata(Metadata, #{
        service  => Service,
        function => Function
    })),
    log(EventType, RpcID, Meta, #{});

handle_event(EventType = ?EV_SERVICE_HANDLER_RESULT, RpcID, Meta, _Opts) ->
    log(EventType, RpcID, Meta, #{});

handle_event(EventType = ?EV_SERVER_SEND, RpcID, Meta, _Opts) ->
    _ = log(EventType, RpcID, Meta, #{}),
    _ = untag_process(RpcID),
    hg_log_scope:clean();

%%

handle_event(EventType, RpcID, Meta, _Opts) when
    EventType == ?EV_INTERNAL_ERROR;
    EventType == ?EV_TRACE
->
    log(EventType, RpcID, Meta, #{}).

%%

log(EventType, RpcID, RawMeta, RpcMeta0) ->
    log(EventType, RpcID, RawMeta, RpcMeta0, #{}).

log(EventType, RpcID, RawMeta, RpcMeta0, ExtraMD) ->
    RpcMeta = RpcMeta0#{event => EventType},
    ok = hg_log_scope:set_meta(RpcMeta),
    {Level, {Format, Args}} = woody_event_handler:format_event(EventType, RawMeta, RpcID),
    _ = lager:log(Level, [{pid, self()}] ++ collect_md(ExtraMD), Format, Args),
    _ = hg_log_scope:remove_meta(maps:keys(RpcMeta)),
    ok.

tag_process(MD) ->
    lager:md(collect_md(MD)).

collect_md(MD = #{}) ->
    maps:fold(
        fun (K, V, Acc) -> lists:keystore(K, 1, Acc, {K, V}) end,
        lager:md(),
        MD
    ).

untag_process(MD = #{}) ->
    lager:md(maps:fold(
        fun (K, _, Acc) -> lists:keydelete(K, 1, Acc) end,
        lager:md(),
        MD
    )).

try_append_event_metadata(undefined, MD) ->
    MD;
try_append_event_metadata(EventMD = #{}, MD) ->
    maps:merge(EventMD, MD).
