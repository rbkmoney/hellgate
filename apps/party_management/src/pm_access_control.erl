-module(pm_access_control).

%%% HG access controll

-export([check_user/2]).

-spec check_user(woody_user_identity:user_identity(), dmsl_domain_thrift:'PartyID'())->
    ok | invalid_user.

check_user(#{id := PartyID, realm := <<"external">>}, PartyID) ->
    ok;
check_user(#{id := _AnyID, realm := <<"internal">>}, _PartyID) ->
    ok;
 %% @TODO must be deleted when we get rid of #payproc_ServiceUser
check_user(#{id := _AnyID, realm := <<"service">>}, _PartyID) ->
    ok;
check_user(_, _) ->
    invalid_user.
