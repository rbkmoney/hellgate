-module(hg_woody_wrapper).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export_type([handler_opts/0]).

-type handler_opts() :: #{handler => module()}.

%% Callbacks

-callback(handle_function(woody_t:func(), woody_server_thrift_handler:args(), handler_opts()) ->
    term() | no_return()).

%% API
-export([call/3]).
-export([call/4]).
-export([call_safe/3]).
-export([call_safe/4]).


-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), handler_opts()) ->
    {term(), woody_client:context()} | no_return().

handle_function(Func, Args, Context, #{handler := Handler} = Opts) ->
    hg_context:set(Context),
    try
        Result = Handler:handle_function(Func, Args, Opts),
        {Result, hg_context:get()}
    catch
        throw:Reason ->
            throw({Reason, hg_context:get()})
    after
        hg_context:cleanup()
    end.

-spec call(atom(), woody_t:func(), list()) ->
    term() | no_return().

call(ServiceName, Function, Args) ->
    map_result_error(call_safe(ServiceName, Function, Args)).

-spec call(atom(), woody_t:func(), list(), Opts :: map()) ->
    term() | no_return().

call(ServiceName, Function, Args, Opts) ->
    map_result_error(call_safe(ServiceName, Function, Args, Opts)).

-spec call_safe(atom(), woody_t:func(), list()) ->
    term().

call_safe(ServiceName, Function, Args) ->
    Url = get_service_url(ServiceName),
    call_safe(ServiceName, Function, Args, #{url => Url}).

-spec call_safe(atom(), woody_t:func(), list(), Opts :: map()) ->
    term().

call_safe(ServiceName, Function, Args, Opts) ->
    Service = get_service_modname(ServiceName),
    Context0 = hg_context:get(),
    {Result, Context} = woody_client:call_safe(Context0, {Service, Function, Args}, Opts),
    hg_context:update(Context),
    Result.

%% Internal functions

map_result_error({error, Reason}) ->
    error(Reason);
map_result_error({exception, _} = Exception) ->
    throw(Exception);
map_result_error(Result) ->
    Result.

get_service_url(ServiceName) ->
    maps:get(ServiceName, genlib_app:env(hellgate, service_urls)).

get_service_modname(ServiceName) ->
    case ServiceName of
        'Automaton' ->
            {dmsl_state_processing_thrift, 'Automaton'};
        'Accounter' ->
            {dmsl_accounter_thrift, 'Accounter'};
        'EventSink' ->
            {dmsl_state_processing_thrift, 'EventSink'};
        'ProviderProxy' ->
            {dmsl_proxy_provider_thrift, 'ProviderProxy'};
        'InspectorProxy' ->
            {dmsl_proxy_inspector_thrift, 'InspectorProxy'};
        _ ->
            error({unknown_service, ServiceName})
    end.


