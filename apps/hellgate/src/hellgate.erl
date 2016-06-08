%%% @doc Public API, supervisor and application startup.
%%% @end

-module(hellgate).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%%
%% API
%%
-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok.
stop() ->
    application:stop(?MODULE).

%%
%% Supervisor callbacks
%%
init([]) ->
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [get_api_child_spec()]
    }}.

get_api_child_spec() ->
    woody_server:child_spec(
        ?MODULE,
        #{
            ip => hg_utils:get_hostname_ip(genlib_app:env(?MODULE, host, "localhost")),
            port => genlib_app:env(?MODULE, port, 8800),
            net_opts => [],
            event_handler => hg_api_event_handler,
            handlers => [
                construct_service_handler(invoicing, hg_api_invoicing_handler, []),
                construct_service_handler(processor, hg_api_processor_handler, [])
            ]
        }
    ).

construct_service_handler(Name, Module, Opts) ->
    {Name, Path, Service} = hg_proto:get_service_spec(Name),
    {Path, {Service, Module, Opts}}.

%%
%% Application callbacks
%%
-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
