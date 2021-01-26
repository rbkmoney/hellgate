-module(hg_limiter).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type timestamp() :: binary().
% -type payment_terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type varset() :: pm_selector:varset().
-type revision() :: hg_domain:revision().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type level() :: production | development.

-type turnover_selector() :: dmsl_domain_thrift:'TurnoverLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().

-export([get_turnover_limits/3]).
-export([check_limits/3]).
-export([hold/4]).
-export([commit/1]).
-export([partial_commit/1]).
-export([rollback/1]).

-export([handle_result/2]).

-define(const(Bool), {constant, Bool}).

-spec get_turnover_limits(turnover_selector(), varset(), revision()) -> [turnover_limit()].

get_turnover_limits(TurnoverLimitSelector, VS, Revision) ->
    % TurnoverLimitSelector = PaymentsProvisionTerms#domain_PaymentsProvisionTerms.turnover_limits,
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
            } = Limit,
            LimiterAmount = Cash#domain_Cash.amount,
            Amount = LimiterAmount + OperationAmount#domain_Cash.amount,
            UpperBoundary = T#domain_TurnoverLimit.upper_boundary,
            case Amount < UpperBoundary#domain_Cash.amount of
                true ->
                    check_limits(TurnoverLimits, OperationAmount, Timestamp, [Limit | Acc]);
                false ->
                    logger:warning("Limit with id ~p is overflow", [LimitID]),
                    {error, {limit_overflow, Limit}}
            end;
        {error, {ErrorType, LimitID}} = Error when
        ErrorType == not_found orelse
        ErrorType == invalid_request ->
            ErrorMsg = "Unable get limit by id ~p from proto limiter, ~p:~p",
            logger:error(ErrorMsg, [LimitID, error, not_found]),
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
                {error, Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        throw:{ErrorType, _} = Error when
        ErrorType == not_found orelse
        ErrorType == invalid_request orelse
        ErrorType == currency_conflict ->
            {error, Error}
    end.

-spec commit([hg_limiter_client:limit_change()]) ->
    ok |
    {error, {not_found, hg_limiter_client:limit_id()}} |
    {error, {not_found, {limit_change, hg_limiter_client:change_id()}}} |
    {error, {invalid_request, Description :: binary()}}.

commit(LimitChanges) ->
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:commit(LimitChange) of
                ok ->
                    ok;
                {error, Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        throw:{ErrorType, _} = Error when
        ErrorType == not_found orelse
        ErrorType == invalid_request ->
            {error, Error}
    end.

-spec partial_commit([hg_limiter_client:limit_change()]) ->
    ok |
    {error, {not_found, hg_limiter_client:limit_id()}} |
    {error, {not_found, {limit_change, hg_limiter_client:change_id()}}} |
    {error, {forbidden_operation_amount, {cash(), cash_range()}}} |
    {error, {invalid_request, Description :: binary()}}.

partial_commit(LimitChanges) ->
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:partial_commit(LimitChange) of
                ok ->
                    ok;
                {error, Error} ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        throw:{ErrorType, _} = Error when
        ErrorType == not_found orelse
        ErrorType == invalid_request orelse
        ErrorType == forbidden_operation_amount ->
            {error, Error}
    end.

-spec rollback([hg_limiter_client:limit_change()]) ->
    ok |
    {error, {not_found, hg_limiter_client:limit_id()}} |
    {error, {not_found, {limit_change, hg_limiter_client:change_id()}}} |
    {error, {invalid_request, Description :: binary()}}.

rollback(LimitChanges) ->
    try
        lists:foreach(fun(LimitChange) ->
            case hg_limiter_client:rollback(LimitChange) of
                ok ->
                    ok;
                {error, Error}  ->
                    throw(Error)
            end
        end, LimitChanges)
    catch
        throw:{ErrorType, _} = Error when
        ErrorType == not_found orelse
        ErrorType == invalid_request ->
            {error, Error}
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

-spec handle_result(level(), function()) -> ok.

handle_result(production, ProcessLimitFun) ->
    case ProcessLimitFun() of
        ok ->
            ok;
        {error, Error} ->
            error(Error)
    end;
handle_result(development, ProcessLimitFun) ->
    try
        _ = ProcessLimitFun(),
        ok
    catch
        error:{woody_error, {_Source, Class, _Details}} when
            Class =:= resource_unavailable orelse
            Class =:= result_unknown orelse
            Class =:= result_unexpected
        ->
            ok
    end.


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
            reduce_limits(TurnoverLimitSelector, VS, Rev);
        _ ->
            logger:warning(
                "Operation limit misconfiguration, can't reduce decision. Predicate: ~p Varset: ~p",
                [Predicate, VS]
            ),
            []
    end.
