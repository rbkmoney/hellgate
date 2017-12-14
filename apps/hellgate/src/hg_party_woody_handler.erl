-module(hg_party_woody_handler).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(partymgmt,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

%% Party

handle_function_('Create', [UserInfo, PartyID, PartyParams], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:start(PartyID, PartyParams);

handle_function_('Checkout', [UserInfo, PartyID, RevisionParam], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    checkout_party(PartyID, RevisionParam);

handle_function_('Get', [UserInfo, PartyID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:get_party(PartyID);

handle_function_('Block', [UserInfo, PartyID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {block, Reason});

handle_function_('Unblock', [UserInfo, PartyID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {unblock, Reason});

handle_function_('Suspend', [UserInfo, PartyID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, suspend);

handle_function_('Activate', [UserInfo, PartyID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, activate);

%% Contract

handle_function_('GetContract', [UserInfo, PartyID, ContractID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    ensure_contract(hg_party:get_contract(ContractID, Party));

handle_function_('ComputeContractTerms', [UserInfo, PartyID, ContractID, Timestamp], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Contract = ensure_contract(hg_party:get_contract(ContractID, Party)),
    Revision = hg_domain:head(),
    hg_party:reduce_terms(
        hg_party:get_terms(Contract, Timestamp, Revision),
        #{party => Party},
        Revision
    );

%% Shop

handle_function_('GetShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    ensure_shop(hg_party:get_shop(ID, Party));

handle_function_('BlockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {block_shop, ID, Reason});

handle_function_('UnblockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {unblock_shop, ID, Reason});

handle_function_('SuspendShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {suspend_shop, ID});

handle_function_('ActivateShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {activate_shop, ID});

handle_function_('ComputeShopTerms', [UserInfo, PartyID, ShopID, Timestamp], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Shop = hg_party:get_shop(ShopID, Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Revision = hg_domain:head(),
    VS = #{
        party => Party,
        shop => Shop,
        category => Shop#domain_Shop.category,
        currency => (Shop#domain_Shop.account)#domain_ShopAccount.currency
    },
    hg_party:reduce_terms(hg_party:get_terms(Contract, Timestamp, Revision), VS, Revision);

%% Claim

handle_function_('CreateClaim', [UserInfo, PartyID, Changeset], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {create_claim, Changeset});

handle_function_('GetClaim', [UserInfo, PartyID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:get_claim(ID, PartyID);

handle_function_('GetClaims', [UserInfo, PartyID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:get_claims(PartyID);

handle_function_('AcceptClaim', [UserInfo, PartyID, ID, ClaimRevision], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {accept_claim, ID, ClaimRevision});

handle_function_('UpdateClaim', [UserInfo, PartyID, ID, ClaimRevision, Changeset], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {update_claim, ID, ClaimRevision, Changeset});

handle_function_('DenyClaim', [UserInfo, PartyID, ID, ClaimRevision, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {deny_claim, ID, ClaimRevision, Reason});

handle_function_('RevokeClaim', [UserInfo, PartyID, ID, ClaimRevision, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {revoke_claim, ID, ClaimRevision, Reason});

%% Event

handle_function_('GetEvents', [UserInfo, PartyID, Range], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    hg_party_machine:get_public_history(PartyID, AfterID, Limit);

%% ShopAccount

handle_function_('GetAccountState', [UserInfo, PartyID, AccountID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_account_state(AccountID, Party);

handle_function_('GetShopAccount', [UserInfo, PartyID, ShopID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_shop_account(ShopID, Party);

%% PartyMeta

handle_function_('GetMeta', [UserInfo, PartyID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:get_meta(PartyID);

handle_function_('GetMetaData', [UserInfo, PartyID, NS], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:get_metadata(NS, PartyID);

handle_function_('SetMetaData', [UserInfo, PartyID, NS, Data], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {set_metadata, NS, Data});

handle_function_('RemoveMetaData', [UserInfo, PartyID, NS], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    hg_party_machine:call(PartyID, {remove_metadata, NS});

%% Payment Institutions

handle_function_(
    'ComputePaymentInstitutionTerms',
    [UserInfo, PartyID, PaymentInstitutionRef],
    _Opts
) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    ok = assert_party_accessible(PartyID),
    Revision = hg_domain:head(),
    case hg_domain:find(Revision, {payment_institution, PaymentInstitutionRef}) of
        #domain_PaymentInstitution{} = P ->
            VS = #{party => hg_party_machine:get_party(PartyID)},
            ContractTemplate = get_default_contract_template(P, VS, Revision),
            Terms = hg_party:get_terms(ContractTemplate, hg_datetime:format_now(), Revision),
            hg_party:reduce_terms(Terms, VS, Revision);
        notfound ->
            throw(#payproc_PaymentInstitutionNotFound{})
    end.

%%

-spec assert_party_accessible(
    dmsl_domain_thrift:'PartyID'()
) ->
    ok | no_return().

assert_party_accessible(PartyID) ->
    UserIdentity = hg_woody_handler_utils:get_user_identity(),
    case hg_access_control:check_user(UserIdentity, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

set_party_mgmt_meta(PartyID) ->
    scoper:add_meta(#{party_id => PartyID}).

assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

checkout_party(PartyID, RevisionParam) ->
    try
        hg_party_machine:checkout(PartyID, RevisionParam)
    catch
        error:revision_not_found ->
            throw(#payproc_InvalidPartyRevision{})
    end.

ensure_contract(#domain_Contract{} = Contract) ->
    Contract;
ensure_contract(undefined) ->
    throw(#payproc_ContractNotFound{}).

ensure_shop(#domain_Shop{} = Shop) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

get_default_contract_template(#domain_PaymentInstitution{default_contract_template = ContractSelector}, VS, Revision) ->
    ContractTemplateRef = hg_selector:reduce_to_value(ContractSelector, VS, Revision),
    hg_domain:get(Revision, {contract_template, ContractTemplateRef}).
