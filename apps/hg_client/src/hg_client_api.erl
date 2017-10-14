-module(hg_client_api).

-export([new/1]).
-export([new/2]).
-export([call/4]).

-export_type([t/0]).

%%

-type t() :: {woody:url(), woody_context:ctx()}.

-spec new(woody:url()) -> t().

new(RootUrl) ->
    new(RootUrl, construct_context()).

-spec new(woody:url(), woody_context:ctx()) -> t().

new(RootUrl, Context) ->
    {RootUrl, Context}.

construct_context() ->
    woody_context:new().

-spec call(Name :: atom(), woody:func(), [any()], t()) ->
    {{ok, _Response} | {exception, _} | {error, _}, t()}.

call(ServiceName, Function, Args, {RootUrl, Context}) ->
    {Path, Service} = hg_proto:get_service_spec(ServiceName),
    Url = iolist_to_binary([RootUrl, Path]),
    Request = {Service, Function, Args},
    Result = try
        woody_client:call(
            Request,
            #{url => Url, event_handler => {scoper_woody_event_handler, #{
                client_scope => 'rpc.client',
                server_scope => 'rpc.server'
            }}},
            Context
        )
    catch
        error:Error ->
            {error, {Error, erlang:get_stacktrace()}}
    end,
    {Result, {RootUrl, Context}}.
