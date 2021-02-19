-module(hg_limiter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type timestamp() :: binary().
-type varset() :: pm_selector:varset().
-type revision() :: hg_domain:revision().
-type cash() :: dmsl_domain_thrift:'Cash'().

-type turnover_selector() :: dmsl_domain_thrift:'TurnoverLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().

-export([get_turnover_limits/3]).
-export([check_limits/2]).
-export([hold/1]).
-export([hold/4]).
-export([commit/1]).
-export([partial_commit/1]).
-export([rollback/1]).
-export([rollback/4]).

-define(const(Bool), {constant, Bool}).

-spec get_turnover_limits(turnover_selector(), varset(), revision()) -> [turnover_limit()].
get_turnover_limits(TurnoverLimitSelector, VS, Revision) ->
    reduce_limits(TurnoverLimitSelector, VS, Revision).

-spec check_limits([turnover_limit()], timestamp()) -> [hg_limiter_client:limit()].
check_limits(TurnoverLimits, Timestamp) ->
    try
        check_limits(TurnoverLimits, Timestamp, [])
    catch
        throw:limit_overflow ->
            IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
            throw({limit_overflow, IDs})
    end.

check_limits([], _, Limits) ->
    Limits;
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
    LimitChanges = gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp),
    hold(LimitChanges).

-spec hold([hg_limiter_client:limit_change()]) -> ok.
hold(LimitChanges) ->
    [hg_limiter_client:hold(Change) || Change <- LimitChanges],
    ok.

-spec commit([hg_limiter_client:limit_change()]) -> ok.
commit(LimitChanges) ->
    [hg_limiter_client:commit(Change) || Change <- LimitChanges],
    ok.

-spec partial_commit([hg_limiter_client:limit_change()]) -> ok.
partial_commit(LimitChanges) ->
    [hg_limiter_client:partial_commit(Change) || Change <- LimitChanges],
    ok.

-spec rollback([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) -> ok.
rollback(Limits, LimitChangeID, Cash, Timestamp) ->
    LimitChanges = gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp),
    rollback(LimitChanges).

-spec rollback([hg_limiter_client:limit_change()]) -> ok.
rollback(LimitChanges) ->
    [hg_limiter_client:rollback(Change) || Change <- LimitChanges],
    ok.

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

reduce_limits(undefined, _, _) ->
    logger:info("Operation limits haven't been set on provider terms."),
    [];
reduce_limits({decisions, Decisions}, VS, Revision) ->
    reduce_limits_decisions(Decisions, VS, Revision);
reduce_limits({value, Limits}, _VS, _Revision) ->
    Limits.

reduce_limits_decisions([], _VS, _Rev) ->
    [];
reduce_limits_decisions([D | Decisions], VS, Rev) ->
    Predicate = D#domain_TurnoverLimitDecision.if_,
    TurnoverLimitSelector = D#domain_TurnoverLimitDecision.then_,
    case pm_selector:reduce_predicate(Predicate, VS, Rev) of
        ?const(false) ->
            reduce_limits_decisions(Decisions, VS, Rev);
        ?const(true) ->
            reduce_limits(TurnoverLimitSelector, VS, Rev)
    end.
