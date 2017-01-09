-module(hg_datetime).

%%

-export([format_dt/1]).
-export([format_ts/1]).
-export([format_now/0]).
-export([now/0]).
-export([to_integer/1]).
-export([compare/2]).

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

-spec compare(Timestamp, Timestamp) -> later | earlier | simultaneously when
    Timestamp :: unix_timestamp() | binary().

compare(T1, T2) when is_binary(T1) ->
    compare(to_integer(T1), T2);
compare(T1, T2) when is_binary(T2) ->
    compare(T1, to_integer(T2));
compare(T1, T2) when is_integer(T1) andalso is_integer(T2) ->
    case T1 > T2 of
        true ->
            later;
        false when T1 < T2 ->
            earlier;
        false when T1 =:= T2 ->
            simultaneously
    end.

