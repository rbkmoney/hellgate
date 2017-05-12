-module(hg_access_control).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%%% HG access controll

-export([check_user/3]).
-export([check_user_info/2]).
-export([check_user_identity/2]).


-spec check_user(
    undefined | woody_user_identity:user_identity(),
    dmsl_payment_processing_thrift:'UserInfo'(),
    dmsl_domain_thrift:'PartyID'()
) -> ok | invalid_user.

check_user(undefined, UserInfo, PartyID) ->
    check_user_info(UserInfo, PartyID);

check_user(UserIdentity, _UserInfo, PartyID) ->
    check_user_identity(UserIdentity, PartyID).

%%

-spec check_user_identity(woody_user_identity:user_identity(), dmsl_domain_thrift:'PartyID'())->
    ok | invalid_user.

check_user_identity(#{id := PartyID, realm := <<"external">>}, PartyID) ->
    ok;
check_user_identity(#{id := _AnyID, realm := <<"internal">>}, _PartyID) ->
    ok;
check_user_identity(#{id := _AnyID, realm := <<"service">>}, _PartyID) ->
    ok;
check_user_identity(_, _) ->
    invalid_user.

-spec check_user_info(dmsl_payment_processing_thrift:'UserInfo'(), dmsl_domain_thrift:'PartyID'()) ->
    ok | invalid_user.

check_user_info(#payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}}, PartyID) ->
    ok;
check_user_info(#payproc_UserInfo{id = _AnyID, type = {internal_user, #payproc_InternalUser{}}}, _PartyID) ->
    ok;
check_user_info(#payproc_UserInfo{id = _AnyID, type = {service_user, #payproc_ServiceUser{}}}, _PartyID) ->
    ok;
check_user_info(_, _) ->
    invalid_user.
