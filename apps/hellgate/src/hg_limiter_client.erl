-module(hg_limiter_client).

-type limit_id() :: dmsl_proto_limiter_thrift:'LimitID'().
-type change_id() :: dmsl_proto_limiter_thrift:'LimitChangeID'().
-type limit() :: dmsl_proto_limiter_thrift:'Limit'().
-type limit_change() :: dmsl_proto_limiter_thrift:'LimitChange'().

-type timestamp() :: binary().
-type symbolic_code() :: binary().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-export_type([limit/0]).

-export([get/2]).
-export([hold/1]).
-export([commit/1]).
-export([partial_commit/1]).
-export([rollback/1]).

-spec get(limit_id(), timestamp()) ->
    {ok, limit()} |
    {error, {not_found, limit_id()}} |
    {error, {invalid_request, Description :: binary()}}.

get(LimitID, Timestamp) ->
    Args = {LimitID, Timestamp},
    Opts = hg_woody_wrapper:get_service_options(limiter),
    try
        case hg_woody_wrapper:call(limiter, 'Get', Args, Opts) of
            {ok, Limit} ->
                {ok, Limit};
            {exception, #proto_limiter_LimitNotFound{}} ->
                {error, {not_found, LimitID}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                {error, {invalid_request, Errors}}
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            String = "Unable to get limit by id ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason);
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            String = "Unable to get limit by id ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason)
    end.

-spec hold(limit_change()) ->
    ok |
    {error, {not_found, limit_id()}} |
    {error, invalid_request} |
    {error, {currency_conflict, {LimitCurrency::symbolic_code(), ChangeCurrency::symbolic_code()}}}.

hold(LimitChange) ->
    LimitID = LimitChange#proto_limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    try
        case hg_woody_wrapper:call(limiter, 'Hold', {LimitChange}, Opts) of
            {ok, ok} ->
                ok;
            {exception, #proto_limiter_LimitNotFound{}} ->
                {error, {not_found, LimitID}};
            {exception, #proto_limiter_InconsistentLimitCurrency{
                limit_currency = LimitCurrency,
                change_currency = ChangeCurrency
            }} ->
                {error, {currency_conflict, {LimitCurrency, ChangeCurrency}}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                {error, {invalid_request, Errors}}
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            String = "Unable to hold limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason);
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            String = "Unable to hold limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason)
    end.

-spec commit(limit_change()) ->
    ok |
    {error, {not_found, limit_id()}} |
    {error, {not_found, {limit_change, change_id()}}} |
    {error, {invalid_request, Description :: binary()}}.

commit(LimitChange) ->
    LimitID = LimitChange#proto_limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    try
        case hg_woody_wrapper:call(limiter, 'Commit', {LimitChange}, Opts) of
            {ok, ok} ->
                ok;
            {exception, #proto_limiter_LimitNotFound{}} ->
                {error, {not_found, LimitChange#proto_limiter_LimitChange.id}};
            {exception, #proto_limiter_LimitChangeNotFound{}} ->
                {error, {not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                {error, {invalid_request, Errors}}
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            String = "Unable to commit limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason);
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            String = "Unable to commit limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason)
    end.

-spec partial_commit(limit_change()) ->
    ok |
    {error, {not_found, limit_id()}} |
    {error, {not_found, {limit_change, change_id()}}} |
    {error, {forbidden_operation_amount, {cash(), cash_range()}}} |
    {error, {invalid_request, Description :: binary()}}.

partial_commit(LimitChange) ->
    LimitID = LimitChange#proto_limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    try
        case hg_woody_wrapper:call(limiter, 'PartialCommit', {LimitChange}, Opts) of
            {ok, ok} ->
                ok;
            {exception, #proto_limiter_LimitNotFound{}} ->
                {error, {not_found, LimitChange#proto_limiter_LimitChange.id}};
            {exception, #proto_limiter_LimitChangeNotFound{}} ->
                {error, {not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}}};
            {exception, #proto_limiter_ForbiddenOperationAmount{amount = Amount, allowed_range = CashRange}} ->
                {error, {forbidden_operation_amount, {Amount, CashRange}}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                {error, {invalid_request, Errors}}
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            String = "Unable partial commit limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason);
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            String = "Unable partial commit limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason)
    end.

-spec rollback(limit_change()) ->
    ok |
    {error, {not_found, limit_id()}} |
    {error, {not_found, {limit_change, change_id()}}} |
    {error, {invalid_request, Description :: binary()}}.

rollback(LimitChange) ->
    LimitID = LimitChange#proto_limiter_LimitChange.id,
    Opts = hg_woody_wrapper:get_service_options(limiter),
    try
        case hg_woody_wrapper:call(limiter, 'Rollback', {LimitChange}, Opts) of
            {ok, ok} ->
                ok;
            {exception, #proto_limiter_LimitNotFound{}} ->
                {error, {not_found, LimitID}};
            {exception, #proto_limiter_LimitChangeNotFound{}} ->
                {error, {not_found, {limit_change, LimitChange#proto_limiter_LimitChange.change_id}}};
            {exception, #'InvalidRequest'{errors = Errors}} ->
                {error, {invalid_request, Errors}}
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            String = "Unable to rollback limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason);
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            String = "Unable to rollback limit change ~p from proto limiter, ~p:~p",
            _ = logger:error(String, [LimitID, error, Reason]),
            error(Reason)
    end.
