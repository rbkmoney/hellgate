-module(hg_limiter_client).

-include_lib("limiter_proto/include/lim_limiter_thrift.hrl").

-export([get/3]).
-export([hold/3]).
-export([commit/3]).

-type limit() :: lim_limiter_thrift:'Limit'().
-type limit_id() :: lim_limiter_thrift:'LimitID'().
-type limit_change() :: lim_limiter_thrift:'LimitChange'().
-type context() :: lim_limiter_thrift:'LimitContext'().
-type clock() :: lim_limiter_thrift:'Clock'().

-export_type([limit/0]).
-export_type([limit_id/0]).
-export_type([limit_change/0]).
-export_type([context/0]).
-export_type([clock/0]).

-spec get(limit_id(), clock(), context()) -> {ok, limit()} | {error, not_found}.
get(LimitID, Clock, Context) ->
    Args = {LimitID, Clock, Context},
    Opts = hg_woody_wrapper:get_service_options(limiter),
    case hg_woody_wrapper:call(limiter, 'Get', Args, Opts) of
        {ok, Limit} ->
            {ok, Limit};
        {exception, #limiter_LimitNotFound{}} ->
            {error, not_found};
        {exception, #'limiter_base_InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec hold(limit_change(), clock(), context()) -> clock() | no_return().
hold(LimitChange, Clock, Context) ->
    LimitID = LimitChange#limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    Args = {LimitChange, Clock, Context},

    case hg_woody_wrapper:call(limiter, 'Hold', Args, Opts) of
        {ok, ClockUpdated} ->
            ClockUpdated;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitID});
        {exception, #'limiter_base_InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec commit(limit_change(), clock(), context()) -> clock() | no_return().
commit(LimitChange, Clock, Context) ->
    Opts = hg_woody_wrapper:get_service_options(limiter),
    Args = {LimitChange, Clock, Context},
    case hg_woody_wrapper:call(limiter, 'Commit', Args, Opts) of
        {ok, ClockUpdated} ->
            ClockUpdated;
        {exception, #limiter_LimitNotFound{}} ->
            error({not_found, LimitChange#limiter_LimitChange.id});
        {exception, #limiter_LimitChangeNotFound{}} ->
            error({not_found, {limit_change, LimitChange#limiter_LimitChange.change_id}});
        {exception, #limiter_base_InvalidRequest{errors = Errors}} ->
            error({invalid_request, Errors});
        {exception, #limiter_ForbiddenOperationAmount{} = Ex} ->
            Amount = Ex#limiter_ForbiddenOperationAmount.amount,
            CashRange = Ex#limiter_ForbiddenOperationAmount.allowed_range,
            error({forbidden_op_amount, {Amount, CashRange}})
    end.
