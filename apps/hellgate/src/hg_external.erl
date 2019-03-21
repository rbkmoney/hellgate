-module(hg_external).

%% API

-export([do/5]).
-export([make_id/3]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/2]).
-export([process_call/2]).
-export([process_signal/2]).

-type id() :: hg_machine:id().
-type response(R) :: R.
-type state() :: hg_msgpack_marshalling:msgpack_value().
-type reply(R) :: {reply, response(R), state()}.

-export_type([id/0]).
-export_type([response/1]).
-export_type([state/0]).
-export_type([reply/1]).

-define(NS, <<"hg/external">>).

%%% API

-spec do(id(), atom(), atom(), atom(), any()) ->
    any().

do(ID, Module, CreateFun, GetFun, Args) ->
    ok = start(ID),
    call(ID, Module, CreateFun, GetFun, Args).

-spec make_id(binary(), binary(), binary()) ->
    binary().

make_id(PartyID, Type, ExternalID) ->
    <<PartyID/binary, "/", Type/binary, "/", ExternalID/binary>>.

%%% Machine callbacks

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init([], hg_machine:machine()) ->
    hg_machine:result().

init([], _Machine) ->
    #{
        auxst => undefined
    }.

-spec process_call({do, atom(), atom(), atom(), term()}, hg_machine:machine()) ->
    {hg_machine:response(), hg_machine:result()}.

process_call({do, Module, CreateFun, _GetFun, Args}, #{aux_state := undefined}) ->
    handle_call(Module, CreateFun, Args);

process_call({do, Module, _CreateFun, GetFun, _Args}, #{aux_state := State}) ->
    handle_call(Module, GetFun, State).

-spec process_signal(hg_machine:signal(), hg_machine:machine()) ->
    hg_machine:result().

process_signal(_Signal, _Machine) ->
    #{}.

%%% Internal functions

-spec start(id()) ->
    ok | no_return().

start(ID) ->
    map_start_error(hg_machine:start(?NS, ID, [])).

call(ID, Module, CreateFun, GetFun, Args) ->
    map_error(hg_machine:call(?NS, ID, {do, Module, CreateFun, GetFun, Args})).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, exists}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, Reason}) ->
    error(Reason).

handle_call(Module, Fun, Args) ->
    try Module:Fun(Args) of
        {reply, Response, State} ->
            {{ok, Response}, #{auxst => State}}
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.
