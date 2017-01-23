-module(hg_datetime).

%%

-export([format_dt/1]).
-export([format_ts/1]).
-export([format_now/0]).
-export([compare/2]).
-export([between/2]).
-export([between/3]).
-export([add_interval/2]).

-include_lib("dmsl/include/dmsl_base_thrift.hrl").

-type datetime() :: calendar:datetime().
-type unix_timestamp() :: integer().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().
-type timestamp_interval() :: dmsl_base_thrift:'TimestampInterval'().

%%

-spec format_dt(datetime()) -> timestamp().

format_dt(Dt = {_, _}) ->
    hg_utils:unwrap_result(rfc3339:format(Dt)).

-spec format_ts(unix_timestamp()) -> timestamp().

format_ts(Ts) when is_integer(Ts) ->
    hg_utils:unwrap_result(rfc3339:format(Ts, seconds)).

-spec format_now() -> timestamp().

format_now() ->
    hg_utils:unwrap_result(rfc3339:format(erlang:system_time())).

-spec compare(timestamp(), timestamp()) -> later | earlier | simultaneously.

compare(T1, T2) when is_binary(T1) andalso is_binary(T2) ->
    compare_int(to_integer(T1), to_integer(T2)).

% Compare exclusivly! undefined == ∞
-spec between(timestamp(), timestamp() | undefined, timestamp() | undefined) -> true | false.

between(Timestamp, Start, End) ->
    case Start of
        undefined ->
            true;
        _ ->
            case compare(Timestamp, Start) of
                later ->
                    true;
                _ ->
                    false
            end
    end
    andalso
    case End of
        undefined ->
            true;
        _ ->
            case compare(Timestamp, End) of
                earlier ->
                    true;
                _ ->
                    false
            end
    end.

-spec between(timestamp(), timestamp_interval()) -> true | false.

between(Timestamp, #'TimestampInterval'{lower_bound = LB, upper_bound = UB}) ->
    case LB of
        undefined ->
            true;
        #'TimestampIntervalBound'{bound_type = LBType, bound_time = LBTimestamp} ->
            case compare(Timestamp, LBTimestamp) of
                later ->
                    true;
                simultaneously when LBType == inclusive ->
                    true;
                _ ->
                    false
            end
    end
    andalso
    case UB of
        undefined ->
            true;
        #'TimestampIntervalBound'{bound_type = UBType, bound_time = UBTimestamp} ->
            case compare(Timestamp, UBTimestamp) of
                earlier ->
                    true;
                simultaneously when UBType == inclusive ->
                    true;
                _ ->
                    false
            end
    end.

-spec add_interval(timestamp(), {Years, Months, Days}) -> timestamp() when
    Years :: integer() | undefined,
    Months :: integer() | undefined,
    Days :: integer() | undefined.

add_interval(Timestamp, {YY, MM, DD}) ->
    IntervalSeconds = (nvl(YY) * 365 + nvl(MM) * 30 + nvl(DD)) * 86400,
    TSInt = to_integer(Timestamp),
    TSSeconds = erlang:convert_time_unit(TSInt, native, seconds),
    format_ts(TSSeconds + IntervalSeconds).

%% Internal functions

-spec to_integer(timestamp()) -> integer().

to_integer(BinaryTimestamp) ->
    hg_utils:unwrap_result(rfc3339:to_time(BinaryTimestamp)).

compare_int(T1, T2) ->
    case T1 > T2 of
        true ->
            later;
        false when T1 < T2 ->
            earlier;
        false when T1 =:= T2 ->
            simultaneously
    end.

nvl(Val) ->
    nvl(Val, 0).

nvl(undefined, Default) ->
    Default;

nvl(Val, _) ->
    Val.

