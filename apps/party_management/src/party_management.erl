%%% @doc Public API, supervisor and application startup.
%%% @end

-module(party_management).

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

% 30 seconds
-define(DEFAULT_HANDLING_TIMEOUT, 30000).

%%
%% API
%%
-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Options = application:get_env(?MODULE, cache_options, #{}),
    {ok,
        {
            #{strategy => one_for_all, intensity => 6, period => 30},
            [pm_party_cache:cache_child_spec(party_cache, Options)]
        }}.

%% Application callbacks

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
