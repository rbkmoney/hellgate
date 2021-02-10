-module(hg_dummy_limiter).

-behaviour(hg_woody_wrapper).

-include_lib("hellgate/include/domain.hrl").

-type limit_id() :: binary().
-type amount() :: integer().

-export([handle_function/3]).
-export([get_port/0]).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).
-export([init/0]).
-export([init/2]).
-export([delete/0]).
-export([get_amount/1]).

-include_lib("damsel/include/dmsl_proto_limiter_thrift.hrl").

-define(COWBOY_PORT, 30001).

-define(limit(LimitID, Cash, Timestamp), #proto_limiter_Limit{
    id = LimitID,
    cash = Cash,
    creation_time = Timestamp
}).

-spec get_port() -> integer().
get_port() ->
    ?COWBOY_PORT.

-spec init() -> ok.
init() ->
    ets:new(?MODULE, [set, named_table, public]),
    ets:insert(?MODULE, {limiter, #{}}),
    ok.

-spec init(limit_id(), amount()) -> ok.
init(LimitID, Amount) ->
    ets:new(?MODULE, [set, named_table, public]),
    ets:insert(
        ?MODULE,
        {limiter, #{
            LimitID => #{hold => false, amount => Amount}
        }}
    ),
    ok.

-spec delete() -> ok.
delete() ->
    true = ets:delete(?MODULE).

-spec get_amount(limit_id()) -> integer().
get_amount(ID) ->
    [{limiter, L}] = ets:lookup(?MODULE, limiter),
    case maps:get(ID, L, undefined) of
        undefined ->
            0;
        Limit ->
            maps:get(amount, Limit)
    end.

set_hold(LimitID, Flag) when is_boolean(Flag) ->
    [{limiter, L}] = ets:lookup(?MODULE, limiter),
    Limit = maps:get(LimitID, L, #{amount => 0}),
    ets:insert(
        ?MODULE,
        {limiter, L#{
            LimitID => Limit#{hold => Flag}
        }}
    ),
    ok.

commit_hold(ID, Amount) ->
    [{limiter, L}] = ets:lookup(?MODULE, limiter),
    case maps:get(ID, L) of
        #{hold := false} ->
            error;
        #{hold := true, amount := A} ->
            ets:insert(
                ?MODULE,
                {limiter, L#{
                    ID => #{hold => false, amount => A + Amount}
                }}
            ),
            ok
    end.

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function('Get', {LimitID, Timestamp}, _Opts) ->
    Amount = get_amount(LimitID),
    ?limit(LimitID, ?cash(Amount, <<"RUB">>), Timestamp);
handle_function('Hold', {#proto_limiter_LimitChange{id = ID}}, _Opts) ->
    set_hold(ID, true);
handle_function('Rollback', {#proto_limiter_LimitChange{id = ID}}, _Opts) ->
    set_hold(ID, false);
handle_function('PartialCommit', {#proto_limiter_LimitChange{id = ID, cash = #domain_Cash{amount = Amount}}}, _Opts) ->
    case commit_hold(ID, Amount) of
        ok ->
            ok;
        error ->
            throw({error, {ID, <<"hold not set before partial commit">>}})
    end;
handle_function('Commit', {#proto_limiter_LimitChange{id = ID, cash = #domain_Cash{amount = Amount}}}, _Opts) ->
    case commit_hold(ID, Amount) of
        ok ->
            ok;
        error ->
            throw({error, {ID, <<"hold not set before commit">>}})
    end.

-spec get_service_spec() -> hg_proto:service_spec().
get_service_spec() ->
    {"/test/proxy/limiter/dummy", {dmsl_proto_limiter_thrift, 'Limiter'}}.

-spec get_http_cowboy_spec() -> #{}.
get_http_cowboy_spec() ->
    Dispatch = cowboy_router:compile([{'_', [{"/", ?MODULE, []}]}]),
    #{
        listener_ref => ?MODULE,
        acceptors_count => 10,
        transport_opts => [{port, ?COWBOY_PORT}],
        proto_opts => #{env => #{dispatch => Dispatch}}
    }.
