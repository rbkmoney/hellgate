-module(hg_woody_wrapper).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export_type([handler_opts/0]).

-type handler_opts() :: #{handler => module()}.

%% Callbacks

-callback(handle_function(woody:func(), woody:args(), handler_opts()) ->
    term() | no_return()).

%% API
-export([call/3]).
-export([call/4]).
-export([raise/1]).


-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), handler_opts()) ->
    {ok, term()} | no_return().

handle_function(Func, Args, Context, #{handler := Handler} = Opts) ->
    hg_context:set(Context),
    try
        Result = Handler:handle_function(Func, Args, Opts),
        {ok, Result}
    catch
        throw:Reason ->
            raise(Reason)
    after
        hg_context:cleanup()
    end.


-spec call(atom(), woody:func(), list()) ->
    term().

call(ServiceName, Function, Args) ->
    Url = get_service_url(ServiceName),
    call(ServiceName, Function, Args, #{url => Url}).

-spec call(atom(), woody:func(), list(), Opts :: map()) ->
    term().

call(ServiceName, Function, Args, Opts) ->
    Service = get_service_modname(ServiceName),
    Context = hg_context:get(),
    woody_client:call({Service, Function, Args}, Opts#{event_handler => {hg_woody_event_handler, undefined}}, Context).

-spec raise(term()) ->
    no_return().

raise(Exception) ->
    woody_error:raise(business, Exception).

%% Internal functions

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
        'MerchantProxy' ->
            {dmsl_proxy_merchant_thrift, 'MerchantProxy'};
        _ ->
            error({unknown_service, ServiceName})
    end.


