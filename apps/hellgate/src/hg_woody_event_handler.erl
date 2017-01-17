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
        type     := Type
    } = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                service  => Service,
                function => Function,
                type     => Type
            }
        }
    );

handle_event(EventType = ?EV_SERVICE_RESULT, RpcID, #{status := Status} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                status  => Status
            }
        }
    );

handle_event(EventType = ?EV_CLIENT_SEND, RpcID, #{url := Url} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                url => Url
            }
        }
    );

handle_event(EventType = ?EV_CLIENT_RECEIVE, RpcID, #{
    status := Status,
    code   := HttpCode
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                status => Status,
                code   => HttpCode
            }
        }
    );

handle_event(EventType = ?EV_SERVER_RECEIVE, RpcID, #{
    url    := Url,
    status := Status
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                status => Status,
                url   => Url
            }
        }
    );

handle_event(EventType = ?EV_SERVER_SEND, RpcID, #{
    status := Status,
    code   := HttpCode
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                status => Status,
                code   => HttpCode
            }
        }
    );

handle_event(EventType = ?EV_INTERNAL_ERROR, RpcID, #{
    role     := Role,
    error    := Error,
    reason   := Reason
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            role     => Role,
            error    => Error,
            reason   => Reason
        }
    );

handle_event(EventType = ?EV_TRACE, RpcID, Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{}
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

handle_event(EventType = ?EV_SERVICE_HANDLER_RESULT, RpcID, #{
    status := Status,
    result := Result
} = Meta, _Opts) ->
    log(
        EventType,
        RpcID,
        Meta,
        #{
            rpc => #{
                status  => Status,
                result => Result
            }
        }
    );


handle_event(EventType, RpcID, EventMeta, _Opts) ->
    log(EventType, RpcID, EventMeta, #{}).

log(EventType, RpcID, RawMeta, FormattedMeta) ->
    {Level, {Format, Args}} = woody_event_handler:format_event(EventType, RawMeta, RpcID),
    LoggerFun = get_logger_function(Level),
    LoggerFun(
        construct_md(collect_meta(EventType, RpcID, FormattedMeta)),
        Format,
        Args
    ).

collect_meta(EventType, RpcID, AdditionalMeta) ->
    lists:foldl(
        fun(M, Acc) ->
            maps:merge(M, Acc)
        end,
        [
            #{
                event_type => EventType
            },
            RpcID,
            AdditionalMeta,
            lager:md()
        ]
    ).

get_logger_function(debug) ->
    fun(Meta, Format, Args) -> lager:debug(Meta, Format, Args) end;

get_logger_function(info) ->
    fun(Meta, Format, Args) -> lager:info(Meta, Format, Args) end;

get_logger_function(notice) ->
    fun(Meta, Format, Args) -> lager:notice(Meta, Format, Args) end;

get_logger_function(warning) ->
    fun(Meta, Format, Args) -> lager:warning(Meta, Format, Args) end;

get_logger_function(error) ->
    fun(Meta, Format, Args) -> lager:error(Meta, Format, Args) end;

get_logger_function(critical) ->
    fun(Meta, Format, Args) -> lager:critical(Meta, Format, Args) end;

get_logger_function(alert) ->
    fun(Meta, Format, Args) -> lager:alert(Meta, Format, Args) end;

get_logger_function(emergency) ->
    fun(Meta, Format, Args) -> lager:emergency(Meta, Format, Args) end.

construct_md(undefined) ->
    [];
construct_md(Map = #{}) ->
    maps:to_list(Map).
