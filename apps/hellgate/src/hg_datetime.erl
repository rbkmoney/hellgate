-module(hg_datetime).

%%

-export([format_dt/1]).
-export([format_ts/1]).
-export([format_now/0]).

-type datetime() :: calendar:datetime().
-type unix_timestamp() :: integer().

%%

-spec format_dt(datetime()) -> binary().

format_dt(Dt = {_, _}) ->
    hg_utils:unwrap_result(rfc3339:format(Dt)).

-spec format_ts(unix_timestamp()) -> binary().

format_ts(Ts) when is_integer(Ts) ->
    hg_utils:unwrap_result(rfc3339:format(Ts, seconds)).

-spec format_now() -> binary().

format_now() ->
    hg_utils:unwrap_result(rfc3339:format(erlang:system_time())).
