-module(limiter_client).

-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-type id() :: integer().
-type change_id() :: integer().
-type timestamp() :: binary().
-type invalid_request() :: [binary()].
-type currency() :: #{
    symbolic_code := binary()
}.
-type cash() :: #{
    amount := integer(),
    currency := currency()
}.

-type limit() :: #{
    id := id(),
    cash := cash(),
    creation_time => timestamp(),
    reload_time => timestamp(),
    description => binary()
}.

-export([get/2]).
-export([hold/4]).
-export([commit/4]).
-export([rollback/4]).

-spec get(id(), timestamp()) ->
    {ok, limit()} |
    {error, not_found | invalid_request()}.

get(LimitID, Timestamp) ->
    Args = [LimitID, Timestamp],
    WoodyCtx = construct_context(),
    case limiter_client_woody:call(limiter, 'Get', Args, WoodyCtx) of
        {ok, Limiter} ->
            {ok, marshal(limiter, Limiter)};
        {error, _Error} ->
            {error, not_impl}
    end.

-spec hold(id(), change_id(), cash(), timestamp()) -> ok | {error, not_impl}.

hold(LimitID, LimitChangeID, Cash, OperationTimestamp) ->
    WoodyCtx = construct_context(),
    Args = [#proto_limiter_LimitChange{
        id = LimitID,
        change_id = LimitChangeID,
        cash = unmarshal(cash, Cash),
        operation_timestamp = OperationTimestamp
    }],
    case limiter_client_woody:call(limiter, 'Hold', Args, WoodyCtx) of
        {ok, _} ->
            ok;
        {error, _Error} ->
            {error, not_impl}
    end.

-spec commit(id(), change_id(), cash(), timestamp()) -> ok | {error, not_impl}.

commit(LimitID, LimitChangeID, Cash, OperationTimestamp) ->
    WoodyCtx = construct_context(),
    Args = [#proto_limiter_LimitChange{
        id = LimitID,
        change_id = LimitChangeID,
        cash = unmarshal(cash, Cash),
        operation_timestamp = OperationTimestamp
    }],
    case limiter_client_woody:call(limiter, 'Commit', Args, WoodyCtx) of
        {ok, _} ->
            ok;
        {error, _Error} ->
            {error, not_impl}
    end.

-spec rollback(id(), change_id(), cash(), timestamp()) -> ok | {error, not_impl}.

rollback(LimitID, LimitChangeID, Cash, OperationTimestamp) ->
    WoodyCtx = construct_context(),
    Args = [#proto_limiter_LimitChange{
        id = LimitID,
        change_id = LimitChangeID,
        cash = unmarshal(cash, Cash),
        operation_timestamp = OperationTimestamp
    }],
    case limiter_client_woody:call(limiter, 'Commit', Args, WoodyCtx) of
        {ok, _} ->
            ok;
        {error, _Error} ->
            {error, not_impl}
    end.

construct_context() ->
    woody_context:new().

marshal(limiter, Limiter) ->
    #{
        id => Limiter#proto_limiter_Limit.id,
        cash => marshal(cash, Limiter#proto_limiter_Limit.cash),
        creation_time => Limiter#proto_limiter_Limit.creation_time,
        reload_time => Limiter#proto_limiter_Limit.reload_time,
        description => Limiter#proto_limiter_Limit.description
    };
marshal(cash, Cash) ->
    #domain_Cash{amount = Amount, currency = Currency} = Cash,
    #{
        amount => Amount,
        currency => marshal(currency, Currency)
    };
marshal(currency, Currency) ->
    #domain_CurrencyRef{symbolic_code = SymCode} = Currency,
    #{
        symbolic_code => SymCode
    }.

unmarshal(cash, Cash) ->
    #domain_Cash{
        amount = maps:get(amount, Cash),
        currency = unmarshal(currency, maps:get(currency, Cash))
    };
unmarshal(currency, Currency) ->
    #domain_CurrencyRef{
        symbolic_code = maps:get(symbolic_code, Currency)
    }.
