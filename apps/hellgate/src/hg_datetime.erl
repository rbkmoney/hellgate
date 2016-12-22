-module(hg_datetime).

%%

-export([format_dt/1]).
-export([format_ts/1]).
-export([format_now/0]).
-export([now/0]).
-export([to_integer/1]).

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

-spec to_integer(binary()) -> unix_timestamp().

to_integer(BinaryTimestamp) ->
    hg_utils:unwrap_result(rfc3339:to_time(BinaryTimestamp)).

-spec now() -> unix_timestamp().

now() ->
    erlang:system_time().
