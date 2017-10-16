-module(hg_woody_wrapper).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export_type([handler_opts/0]).
-export_type([client_opts/0]).

-type handler_opts() :: #{
    handler => module(),
    user_identity => undefined | woody_user_identity:user_identity()
}.

-type client_opts() :: #{
    url            := woody:url(),
    transport_opts => [{_, _}]
}.

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
        Result = Handler:handle_function(
            Func,
            Args,
            Opts
        ),
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

-spec call(atom(), woody:func(), list(), client_opts()) ->
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

-spec get_service_modname(atom()) ->
    {module(), atom()}.

get_service_modname(ServiceName) ->
    {get_service_module(ServiceName), ServiceName}.

get_service_module('Automaton') ->
    mg_proto_state_processing_thrift;
get_service_module('Accounter') ->
    dmsl_accounter_thrift;
get_service_module('EventSink') ->
    mg_proto_state_processing_thrift;
get_service_module('ProviderProxy') ->
    dmsl_proxy_provider_thrift;
get_service_module('InspectorProxy') ->
    dmsl_proxy_inspector_thrift;
get_service_module('MerchantProxy') ->
    dmsl_proxy_merchant_thrift;
get_service_module('PartyManagement') ->
    dmsl_payment_processing_thrift;
get_service_module(ServiceName) ->
    error({unknown_service, ServiceName}).
