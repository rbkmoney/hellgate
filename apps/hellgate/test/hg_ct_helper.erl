-module(hg_ct_helper).

-export([start_app/1]).
-export([start_app/2]).

%%

-type app_name() :: atom().

-spec start_app(app_name()) -> [app_name()].

start_app(lager = AppName) ->
    start_app(AppName, [
        {async_threshold, 1},
        {async_threshold_window, 0},
        {error_logger_hwm, 600},
        {suppress_application_start_stop, true},
        {handlers, [
            {lager_common_test_backend, [debug, false]}
        ]}
    ]);

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) -> [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).
