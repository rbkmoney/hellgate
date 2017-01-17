-module(hg_access_control).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%%% HG access controll

-export([check_user_info/2]).

%%
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