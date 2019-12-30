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

-define(DEFAULT_HANDLING_TIMEOUT, 30000).  % 30 seconds

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

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
%%    PMMachineHandlers = [
%%        pm_party_machine
%%    ],
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [
%%            pm_machine:get_child_spec(PMMachineHandlers)
        ]
    }}.

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
