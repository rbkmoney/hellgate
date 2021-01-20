-module(hg_limiter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type timestamp() :: limiter_client:timestamp().
-type terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type varset() :: hg_routing:varset().
-type revision() :: hg_domain:revision().
-type cash() :: dmsl_domain_thrift:'Cash'().

-type turnover_limit() :: dmsl_proto_limiter_thrift:'TurnoverLimits'().

-export([get_turnover_limits/3]).
-export([check_limits/3]).
-export([hold/4]).
-export([commit/1]).
-export([rollback/1]).

-define(const(Bool), {constant, Bool}).

-spec get_turnover_limits(terms(), varset(), revision()) -> [turnover_limit()].

get_turnover_limits(ProviderTerms, VS, Revision) ->
    TurnoverLimitSelector = ProviderTerms#domain_PaymentsProvisionTerms.turnover_limits,
    reduce_limits(TurnoverLimitSelector, VS, Revision).

-spec check_limits([turnover_limit()], cash(), timestamp()) ->
    {ok, [hg_limiter_client:limit()]} |
    {error, {not_found, hg_limiter_client:limit_id()}} |
    {error, {invalid_request, Reason::binary()}} |
    {error, {limit_overflow, hg_limiter_client:limit()}}.

check_limits(TurnoverLimits, OperationAmount, Timestamp) ->
    check_limits(TurnoverLimits, OperationAmount, Timestamp, []).

check_limits([], _, _, Limits) ->
    {ok, Limits};
check_limits([T | TurnoverLimits], OperationAmount, Timestamp, Acc) ->
    #domain_TurnoverLimit{id = LimitID} = T,

    case hg_limiter_client:get(LimitID, Timestamp) of
        {ok, Limit} ->
            #proto_limiter_Limit{
                id = LimitID,
                cash = Cash
            %% TODO Limit has got creation_time$reload_time. Should we consider it,
            %% when check limit
            } = Limit,
            LimiterAmount = Cash#domain_Cash.amount,
            Amount = LimiterAmount + OperationAmount#domain_Cash.amount,
            UpperBoundary = T#domain_TurnoverLimit.upper_boundary,

            case Amount < UpperBoundary of
                true ->
                    check_limits(TurnoverLimits, OperationAmount, Timestamp, [Limit | Acc]);
                false ->
                    logger:warning("Limit with id ~p is overflow", [LimitID]),
                    {error, {limit_overflow, Limit}}
            end;
        {error, {not_found, LimitID}} = Error ->
            ErrorMsg = "Unable get limit by id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, error, not_found]),
            Error;
        {error, {invalid_request, Description}} = Error ->
            ErrorMsg = "Unable get limit by id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, invalid_request, Description]),
            Error
    end.

-spec hold([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) ->
    ok |
    {error, {not_found, hg_limiter_client:limit_id()}} |
    {error, {invalid_request, Reason::binary()}} |
    {error, {error, {currency_conflict, {LimitCurrency::binary(), ChangeCurrency::binary()}}}}.

hold(Limits, LimitChangeID, Cash, Timestamp) ->
    LimitChanges = gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp),
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:hold(LimitChange) of
                ok ->
                    ok;
                {error, {_, _} = Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        {error, {not_found, LimitID}} = Error ->
            ErrorMsg = "Unable hold limit with id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, error, not_found]),
            Error;
        {error, {invalid_request, Description}} = Error ->
            ErrorMsg = "Unable hold limit from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [invalid_request, Description]),
            Error;
        {error, {currency_conflict, Description}} = Error ->
            ErrorMsg = "Unable hold limit from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [currency_conflict, Description]),
            Error
    end.

-spec commit([hg_limiter_client:limit_change()]) -> ok.

commit(LimitChanges) ->
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:commit(LimitChange) of
                ok ->
                    ok;
                {error, {_, _} = Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        {error, {not_found, LimitID}} = Error ->
            ErrorMsg = "Unable commit limit with id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, error, not_found]),
            Error;
        {error, {invalid_request, Description}} = Error ->
            ErrorMsg = "Unable commit limit from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [invalid_request, Description]),
            Error
    end.

-spec rollback([hg_limiter_client:limit_change()]) -> ok.

rollback(LimitChanges) ->
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:rollback(LimitChange) of
                ok ->
                    ok;
                {error, {_, _} = Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        {error, {not_found, LimitID}} = Error ->
            ErrorMsg = "Unable rollback limit with id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, error, not_found]),
            Error;
        {error, {invalid_request, Description}} = Error ->
            ErrorMsg = "Unable rollback limit from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [invalid_request, Description]),
            Error
    end.

-spec gen_limit_changes([hg_limiter_client:limit()], hg_limiter_client:change_id(), cash(), timestamp()) ->
    [hg_limiter_client:limit_change()].

gen_limit_changes(Limits, LimitChangeID, Cash, Timestamp) ->
    [#proto_limiter_LimitChange{
        id = Limit#proto_limiter_Limit.id,
        change_id = LimitChangeID,
        cash = Cash,
        operation_timestamp = Timestamp
    } || Limit <- Limits].

reduce_limits(undefined, _, _) ->
    logger:warning("Operation limits haven't been set on provider terms."),
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
            reduce_limits(TurnoverLimitSelector, VS, Rev);
        _ ->
            logger:warning(
                "Operation limit misconfiguration, can't reduce decision. Predicate: ~p Varset: ~p",
                [Predicate, VS]
            ),
            []
    end.
