-module(hg_limiter_client).

-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type limit_id() :: dmsl_proto_limiter_thrift:'LimitID'().
-type change_id() :: dmsl_proto_limiter_thrift:'LimitChangeID'().
-type limit() :: dmsl_proto_limiter_thrift:'Limit'().
-type limit_change() :: dmsl_proto_limiter_thrift:'LimitChange'().

-type timestamp() :: binary().

-export_type([limit/0]).
-export_type([limit_id/0]).
-export_type([change_id/0]).
-export_type([limit_change/0]).

-export([get/2]).
-export([hold/1]).
-export([commit/1]).
-export([partial_commit/1]).
-export([rollback/1]).

-spec get(limit_id(), timestamp()) -> limit().
get(LimitID, Timestamp) ->
    Args = {LimitID, Timestamp},
    Opts = hg_woody_wrapper:get_service_options(limiter),
    case hg_woody_wrapper:call(limiter, 'Get', Args, Opts) of
        {ok, Limit} ->
            Limit;
        {exception, #proto_limiter_LimitNotFound{}} ->
            error({not_found, LimitID});
        {exception, #'InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec hold(limit_change()) -> ok.
hold(_) ->
    ok.
% TODO: Uncomment&Deploy second deployment
% Uncomment&Deploy this code, delete skip LimitChangeNotFound
% hold(LimitChange) ->
%     LimitID = LimitChange#proto_limiter_LimitChange.id,
%     Opts = hg_woody_wrapper:get_service_options(limiter),
%     case hg_woody_wrapper:call(limiter, 'Hold', {LimitChange}, Opts) of
%         {ok, ok} ->
%             ok;
%         {exception, #proto_limiter_LimitNotFound{}} ->
%             error({not_found, LimitID});
%         {exception, #'InvalidRequest'{errors = Errors}} ->
%             error({invalid_request, Errors})
%     end.

-spec commit(limit_change()) -> ok.
commit(LimitChange) ->
    Opts = hg_woody_wrapper:get_service_options(limiter),
    case hg_woody_wrapper:call(limiter, 'Commit', {LimitChange}, Opts) of
        {ok, ok} ->
            ok;
        {exception, #proto_limiter_LimitNotFound{}} ->
            error({not_found, LimitChange#proto_limiter_LimitChange.id});
        {exception, #proto_limiter_LimitChangeNotFound{}} ->
            ok;
        % TODO: Uncomment&Deploy
        % error({not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}});
        {exception, #'InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec partial_commit(limit_change()) -> ok.
partial_commit(LimitChange) ->
    Opts = hg_woody_wrapper:get_service_options(limiter),
    case hg_woody_wrapper:call(limiter, 'PartialCommit', {LimitChange}, Opts) of
        {ok, ok} ->
            ok;
        {exception, #proto_limiter_LimitNotFound{}} ->
            error({not_found, LimitChange#proto_limiter_LimitChange.id});
        {exception, #proto_limiter_LimitChangeNotFound{}} ->
            ok;
        % TODO: Uncomment&Deploy
        % error({not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}});
        {exception, #proto_limiter_ForbiddenOperationAmount{amount = Amount, allowed_range = CashRange}} ->
            error({forbidden_operation_amount, {Amount, CashRange}});
        {exception, #'InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.

-spec rollback(limit_change()) -> ok.
rollback(LimitChange) ->
    LimitID = LimitChange#proto_limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    case hg_woody_wrapper:call(limiter, 'Rollback', {LimitChange}, Opts) of
        {ok, ok} ->
            ok;
        {exception, #proto_limiter_LimitNotFound{}} ->
            error({not_found, LimitID});
        {exception, #proto_limiter_LimitChangeNotFound{}} ->
            ok;
        % TODO: Uncomment&Deploy
        % error({not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}});
        {exception, #'InvalidRequest'{errors = Errors}} ->
            error({invalid_request, Errors})
    end.
