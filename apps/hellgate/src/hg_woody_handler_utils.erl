-module(hg_woody_handler_utils).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type user_info() :: dmsl_payment_processing_thrift:'UserInfo'().
-type user_identity() :: woody_user_identity:user_identity().

-export([get_user_identity/0]).
-export([collect_user_identity/1]).
-export([set_user_identity/1]).
-export([assume_user_identity/1]).

-spec get_user_identity() -> woody_user_identity:user_identity() | undefined.
get_user_identity() ->
    try
        Context = hg_context:load(),
        woody_user_identity:get(hg_context:get_woody_context(Context))
    catch
        throw:{missing_required, _Key} ->
            undefined
    end.

-spec collect_user_identity(user_info()) -> user_identity().
collect_user_identity(UserInfo) ->
    case get_user_identity() of
        V when V /= undefined ->
            V;
        undefined ->
            map_user_info(UserInfo)
    end.

-spec set_user_identity(user_identity()) -> ok.
set_user_identity(UserIdentity) ->
    hg_context:save(hg_context:set_user_identity(UserIdentity, hg_context:load())).

-spec assume_user_identity(user_info()) -> ok.
assume_user_identity(UserInfo) ->
    case get_user_identity() of
        V when V /= undefined ->
            ok;
        undefined ->
            set_user_identity(map_user_info(UserInfo))
    end.

map_user_info(#payproc_UserInfo{id = PartyID, type = Type}) ->
    #{
        id => PartyID,
        realm => map_user_type(Type)
    }.

map_user_type({external_user, #payproc_ExternalUser{}}) ->
    <<"external">>;
map_user_type({internal_user, #payproc_InternalUser{}}) ->
    <<"internal">>;
map_user_type({service_user, #payproc_ServiceUser{}}) ->
    <<"service">>.
