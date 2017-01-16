-module(hg_security).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%%% HG security system. Much complex. Very eneterprise. So wow.
% ░░░░░░░░░▄░░░░░░░░░░░░░░▄░░░░
% ░░░░░░░░▌▒█░░░░░░░░░░░▄▀▒▌░░░
% ░░░░░░░░▌▒▒█░░░░░░░░▄▀▒▒▒▐░░░
% ░░░░░░░▐▄▀▒▒▀▀▀▀▄▄▄▀▒▒▒▒▒▐░░░
% ░░░░░▄▄▀▒░▒▒▒▒▒▒▒▒▒█▒▒▄█▒▐░░░
% ░░░▄▀▒▒▒░░░▒▒▒░░░▒▒▒▀██▀▒▌░░░
% ░░▐▒▒▒▄▄▒▒▒▒░░░▒▒▒▒▒▒▒▀▄▒▒▌░░
% ░░▌░░▌█▀▒▒▒▒▒▄▀█▄▒▒▒▒▒▒▒█▒▐░░
% ░▐░░░▒▒▒▒▒▒▒▒▌██▀▒▒░░░▒▒▒▀▄▌░
% ░▌░▒▄██▄▒▒▒▒▒▒▒▒▒░░░░░░▒▒▒▒▌░
% ▀▒▀▐▄█▄█▌▄░▀▒▒░░░░░░░░░░▒▒▒▐░
% ▐▒▒▐▀▐▀▒░▄▄▒▄▒▒▒▒▒▒░▒░▒░▒▒▒▒▌
% ▐▒▒▒▀▀▄▄▒▒▒▄▒▒▒▒▒▒▒▒░▒░▒░▒▒▐░
% ░▌▒▒▒▒▒▒▀▀▀▒▒▒▒▒▒░▒░▒░▒░▒▒▒▌░
% ░▐▒▒▒▒▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▒▄▒▒▐░░
% ░░▀▄▒▒▒▒▒▒▒▒▒▒▒░▒░▒░▒▄▒▒▒▒▌░░
% ░░░░▀▄▒▒▒▒▒▒▒▒▒▒▄▄▄▀▒▒▒▒▄▀░░░
% ░░░░░░▀▄▄▄▄▄▄▀▀▀▒▒▒▒▒▄▄▀░░░░░
% ░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▀▀░░░░░░░░

-export([check_user_info/2]).

%%
-spec check_user_info(dmsl_payment_processing_thrift:'UserInfo'(), dmsl_domain_thrift:'PartyID'()) ->
    ok | no_return().

check_user_info(#payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}}, PartyID) ->
    ok;
check_user_info(#payproc_UserInfo{id = _AnyID, type = {internal_user, #payproc_InternalUser{}}}, _PartyID) ->
    ok;
check_user_info(#payproc_UserInfo{id = _AnyID, type = {service_user, #payproc_ServiceUser{}}}, _PartyID) ->
    ok;
check_user_info(_, _) ->
    throw(#payproc_InvalidUser{}).