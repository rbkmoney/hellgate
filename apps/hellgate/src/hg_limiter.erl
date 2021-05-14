-module(hg_limiter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type timestamp() :: binary().
-type cash() :: dmsl_domain_thrift:'Cash'().

-type turnover_selector() :: dmsl_domain_thrift:'TurnoverLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().

-export([get_turnover_limits/1]).
-export([check_limits/2]).
-export([hold/1]).
-export([hold/4]).
-export([commit/1]).
-export([partial_commit/1]).
-export([rollback/1]).
-export([rollback/4]).

-define(const(Bool), {constant, Bool}).

-spec get_turnover_limits(turnover_selector()) -> [turnover_limit()].
get_turnover_limits(TurnoverLimitSelector) ->
    reduce_limits(TurnoverLimitSelector).

-spec check_limits([turnover_limit()], timestamp()) ->
    {ok, [hg_limiter_client:limit()]}
    | {error, {limit_overflow, [binary()]}}.
check_limits(TurnoverLimits, Timestamp) ->
    try
        check_limits(TurnoverLimits, Timestamp, [])
    catch
        throw:limit_overflow ->
            IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
            {error, {limit_overflow, IDs}}
    end.

check_limits([], _, Limits) ->
    {ok, Limits};
check_limits([T | TurnoverLimits], Timestamp, Acc) ->
    #domain_TurnoverLimit{id = LimitID} = T,
    Limit = hg_limiter_client:get(LimitID, Timestamp),
    #proto_limiter_Limit{
        id = LimitID,
        cash = Cash
    } = Limit,
    LimiterAmount = Cash#domain_Cash.amount,
    UpperBoundary = T#domain_TurnoverLimit.upper_boundary,
    case LimiterAmount < UpperBoundary of
        true ->
            check_limits(TurnoverLimits, Timestamp, [Limit | Acc]);
        false ->
            logger:info("Limit with id ~p overflowed", [LimitID]),
            throw(limit_overflow)
    end.

-spec hold([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) -> ok.
hold(Limits, LimitChangeID, Cash, Timestamp) ->
    hold(gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp)).

-spec hold([hg_limiter_client:limit_change()]) -> ok.
hold(LimitChanges) ->
    lists:foreach(fun hg_limiter_client:hold/1, LimitChanges).

-spec commit([hg_limiter_client:limit_change()]) -> ok.
commit(LimitChanges) ->
    lists:foreach(fun hg_limiter_client:commit/1, LimitChanges).

-spec partial_commit([hg_limiter_client:limit_change()]) -> ok.
partial_commit(LimitChanges) ->
    lists:foreach(fun hg_limiter_client:partial_commit/1, LimitChanges).

-spec rollback([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) -> ok.
rollback(Limits, LimitChangeID, Cash, Timestamp) ->
    rollback(
        gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp)
    ).

-spec rollback([hg_limiter_client:limit_change()]) -> ok.
rollback(LimitChanges) ->
    lists:foreach(fun hg_limiter_client:rollback/1, LimitChanges).

-spec gen_limit_changes([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) ->
    [hg_limiter_client:limit_change()].
gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp) ->
    [
        #proto_limiter_LimitChange{
            id = Limit#proto_limiter_Limit.id,
            change_id = LimitChangeID,
            cash = Cash,
            operation_timestamp = Timestamp
        }
        || Limit <- Limits
    ].

reduce_limits(undefined) ->
    logger:info("Operation limits haven't been set on provider terms."),
    [];
reduce_limits({value, Limits}) ->
    Limits.
