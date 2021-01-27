-module(hg_ct_limiter_handler).

-export([create_storage/0]).
-export([delete_storage/0]).
-export([get_error/0]).

-export([handle_error/1]).


-spec handle_error(ok | {error, any()}) -> ok.
handle_error(ok) ->
    ok;
handle_error({error, Error}) ->
    try
        ets:insert(?MODULE, {error, Error})
    catch
        error:badarg ->
            create_storage(),
            ct:print("can't insert value ~p", [Error]),
            ets:insert(?MODULE, {error, Error})
    end,
    error(Error).

-spec get_error() -> ok | {error, _}.
get_error() ->
    case ets:lookup(?MODULE, error) of
        [] -> ok;
        [{error, Error}] -> Error
    end.

-spec create_storage() -> capi_idemp_features:event_handler().
create_storage() ->
    %% TODO delete named_table. Make opportunity for concurrent tests.
    ets:new(?MODULE, [set, public, named_table]).

-spec delete_storage() -> _.
delete_storage() ->
    ets:delete(?MODULE).
