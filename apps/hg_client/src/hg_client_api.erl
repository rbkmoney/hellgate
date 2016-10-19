-module(hg_client_api).

-export([new/1]).
-export([new/2]).
-export([call/4]).

-export_type([t/0]).

%%

-behaviour(woody_event_handler).
-export([handle_event/3]).

%%

-type t() :: {woody_t:url(), woody_client:context()}.

-spec new(woody_t:url()) -> t().

new(RootUrl) ->
    new(RootUrl, construct_context()).

-spec new(woody_t:url(), woody_client:context()) -> t().

new(RootUrl, Context) ->
    {RootUrl, Context}.

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, ?MODULE).

-spec call(Name :: atom(), woody_t:func(), [any()], t()) ->
    {ok | _Response | {exception, _} | {error, _}, t()}.

call(ServiceName, Function, Args, {RootUrl, Context}) ->
    {Path, Service} = hg_proto:get_service_spec(ServiceName),
    Url = iolist_to_binary([RootUrl, Path]),
    Request = {Service, Function, Args},
    {Result, ContextNext} = woody_client:call_safe(Context, Request, #{url => Url}),
    {Result, {RootUrl, ContextNext}}.

%%

-spec handle_event(EventType, RpcID, EventMeta)
    -> _ when
        EventType :: woody_event_handler:event_type(),
        RpcID ::  woody_t:rpc_id(),
        EventMeta :: woody_event_handler:event_meta_type().

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}) ->
    lager:error(
        maps:to_list(RpcID),
        "[client] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta) ->
    lager:debug(maps:to_list(RpcID), "[client] ~s: ~p", [EventType, EventMeta]).
