-module(hg_party_woody_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(partymgmt,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

%% Party

handle_function_('Create', [UserInfo, PartyID, PartyParams], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:start(PartyID, PartyParams);

handle_function_('Get', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:get_party(PartyID);

handle_function_('Block', [UserInfo, PartyID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {block, Reason});

handle_function_('Unblock', [UserInfo, PartyID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {unblock, Reason});

handle_function_('Suspend', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, suspend);

handle_function_('Activate', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, activate);

%% Contract

handle_function_('GetContract', [UserInfo, PartyID, ContractID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_contract(ContractID, Party);

%% Shop

handle_function_('GetShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_shop(ID, Party);

handle_function_('BlockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {block_shop, ID, Reason});

handle_function_('UnblockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {unblock_shop, ID, Reason});

handle_function_('SuspendShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {suspend_shop, ID});

handle_function_('ActivateShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {activate_shop, ID});

%% Claim

handle_function_('CreateClaim', [UserInfo, PartyID, Changeset], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {create_claim, Changeset});

handle_function_('GetClaim', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:get_claim(ID, PartyID);

handle_function_('GetClaims', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:get_claims(PartyID);

handle_function_('AcceptClaim', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {accept_claim, ID});

handle_function_('UpdateClaim', [UserInfo, PartyID, ID, Changeset], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {update_claim, ID, Changeset});

handle_function_('DenyClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {deny_claim, ID, Reason});

handle_function_('RevokeClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {revoke_claim, ID, Reason});

%% Event

handle_function_('GetEvents', [UserInfo, PartyID, Range], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    hg_party_machine:get_public_history(PartyID, AfterID, Limit);

%% ShopAccount

handle_function_('GetAccountState', [UserInfo, PartyID, AccountID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_account_state(AccountID, Party);

handle_function_('GetShopAccount', [UserInfo, PartyID, ShopID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_shop_account(ShopID, Party).

%%

-spec assert_party_accessible(dmsl_payment_processing_thrift:'UserInfo'(), dmsl_domain_thrift:'PartyID'()) ->
    ok | no_return().

assert_party_accessible(UserInfo, PartyID) ->
    case hg_access_control:check_user_info(UserInfo, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

set_party_mgmt_meta(PartyID, #payproc_UserInfo{id = ID, type = {Type, _}}) ->
    hg_log_scope:set_meta(#{
        party_id => PartyID,
        user_info => #{id => ID, type => Type}
    }).


