-module(hg_dummy_limiter).

-behaviour(hg_woody_wrapper).

-include_lib("hellgate/include/domain.hrl").

-export([handle_function/3]).
-export([get_port/0]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).

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

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function('Get', {<<"3">>, _Timestamp}, _Opts) ->
    throw(#proto_limiter_LimitNotFound{});
handle_function('Get', {<<"5">> = LimitID, Timestamp}, _Opts) ->
    ?limit(LimitID, ?cash(1000001, <<"RUB">>), Timestamp);
handle_function('Get', {LimitID, Timestamp}, _Opts) ->
    ?limit(LimitID, ?cash(0, <<"RUB">>), Timestamp);
handle_function('Hold', {_LimitChange}, _Opts) ->
    ok;
handle_function('PartialCommit', {#proto_limiter_LimitChange{id = <<"4">>}}, _Opts) ->
    throw(#proto_limiter_LimitChangeNotFound{});
handle_function('PartialCommit', {_LimitChange}, _Opts) ->
    ok;
handle_function('Commit', {_LimitChange}, _Opts) ->
    ok.

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
