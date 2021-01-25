-module(hg_ct_limiter_handler).

-export([create_storage/0]).
-export([delete_storage/0]).
-export([get_error/0]).

-export([handle_result/2]).

-type level() :: production | development.

-spec handle_result(level(), function()) -> ok.

handle_result(_, ProcessLimitFun) ->
    case ProcessLimitFun() of
        ok ->
            ok;
        {error, _} = Error ->
            ets:insert(?MODULE, Error),
            % insert(Error),
            error(Error)
    end.

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
