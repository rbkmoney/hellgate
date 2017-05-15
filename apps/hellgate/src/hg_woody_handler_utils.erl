-module(hg_woody_handler_utils).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([get_user_identity/1]).

-spec get_user_identity(dmsl_payment_processing_thrift:'UserInfo'()) ->
    woody_user_identity:user_identity().

get_user_identity(UserInfo) ->
    try
        Context = hg_context:get(),
        woody_user_identity:get(Context)
    catch
        throw:{missing_required, _Key} ->
            map_user_info(UserInfo)
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
