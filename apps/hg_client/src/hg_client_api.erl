-module(hg_client_api).

-export([new/1]).
-export([new/2]).
-export([call/4]).

-export_type([t/0]).

%%

-behaviour(woody_event_handler).
-export([handle_event/4]).

%%

-type t() :: {woody:url(), woody_context:ctx()}.

-spec new(woody:url()) -> t().

new(RootUrl) ->
    new(RootUrl, construct_context()).

-spec new(woody:url(), woody_context:ctx()) -> t().

new(RootUrl, Context) ->
    {RootUrl, Context}.

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_context:new(ReqID).

-spec call(Name :: atom(), woody:func(), [any()], t()) ->
    {{ok, _Response} | {exception, _} | {error, _}, t()}.

call(ServiceName, Function, Args, {RootUrl, Context}) ->
    {Path, Service} = hg_proto:get_service_spec(ServiceName),
    Url = iolist_to_binary([RootUrl, Path]),
    Request = {Service, Function, Args},
    Result = try
        woody_client:call(
            Request,
            #{url => Url, event_handler => {hg_woody_event_handler, undefined}},
            Context
        )
    catch
        error:Error ->
            {error, Error}
    end,
    {Result, {RootUrl, Context}}.

%%

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}, _Opts) ->
    lager:error(
        maps:to_list(RpcID),
        "[client] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta, _Opts) ->
    lager:debug(maps:to_list(RpcID), "[client] ~s: ~p", [EventType, EventMeta]).
