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
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:start(PartyID, PartyParams);

handle_function_('Checkout', [UserInfo, PartyID, RevisionParam], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    checkout_party(PartyID, RevisionParam, #payproc_InvalidPartyRevision{});

handle_function_('Get', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_party(PartyID);

handle_function_('GetRevision', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_last_revision(PartyID);

handle_function_('Block', [UserInfo, PartyID, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {block, party, Reason});

handle_function_('Unblock', [UserInfo, PartyID, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {unblock, party, Reason});

handle_function_('Suspend', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {suspend, party});

handle_function_('Activate', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {activate, party});

%% Contract

handle_function_('GetContract', [UserInfo, PartyID, ContractID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    ensure_contract(hg_party:get_contract(ContractID, Party));

handle_function_('ComputeContractTerms', [UserInfo, PartyID, ContractID, Timestamp], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Contract = ensure_contract(hg_party:get_contract(ContractID, Party)),
    Revision = hg_domain:head(),
    hg_party:reduce_terms(
        hg_party:get_terms(Contract, Timestamp, Revision),
        #{
            party_id => PartyID,
            identification_level => get_identification_level(Contract, Party)
        },
        Revision
    );

%% Shop

handle_function_('GetShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    ensure_shop(hg_party:get_shop(ID, Party));

handle_function_('BlockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {block, {shop, ID}, Reason});

handle_function_('UnblockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {unblock, {shop, ID}, Reason});

handle_function_('SuspendShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {suspend, {shop, ID}});

handle_function_('ActivateShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {activate, {shop, ID}});

handle_function_('ComputeShopTerms', [UserInfo, PartyID, ShopID, Timestamp], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Shop = ensure_shop(hg_party:get_shop(ShopID, Party)),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Revision = hg_domain:head(),
    VS = #{
        party_id => PartyID,
        shop_id  => ShopID,
        category => Shop#domain_Shop.category,
        currency => (Shop#domain_Shop.account)#domain_ShopAccount.currency,
        identification_level => get_identification_level(Contract, Party)
    },
    hg_party:reduce_terms(hg_party:get_terms(Contract, Timestamp, Revision), VS, Revision);

%% Wallet

handle_function_('ComputeWalletTerms', [UserInfo, PartyID, ContractID, WalletID, Currency, Timestamp], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Contract = hg_party:get_contract(ContractID, Party),
    Revision = hg_domain:head(),
    VS = #{
        party_id => PartyID,
        wallet_id => WalletID,
        currency => Currency,
        identification_level => get_identification_level(Contract, Party)
    },
    hg_party:reduce_terms(hg_party:get_terms(Contract, Timestamp, Revision), VS, Revision);
handle_function_('ComputeWalletTermsNew', [UserInfo, PartyID, ContractID, Timestamp, Varset], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Contract = hg_party:get_contract(ContractID, Party),
    Revision = hg_domain:head(),
    VS0 = #{
        identification_level => get_identification_level(Contract, Party)
    },
    VS1 = prepare_varset(PartyID, Varset, VS0),
    hg_party:reduce_terms(hg_party:get_terms(Contract, Timestamp, Revision), VS1, Revision);

%% Claim

handle_function_('CreateClaim', [UserInfo, PartyID, Changeset], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {create_claim, Changeset});

handle_function_('GetClaim', [UserInfo, PartyID, ID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_claim(ID, PartyID);

handle_function_('GetClaims', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_claims(PartyID);

handle_function_('AcceptClaim', [UserInfo, PartyID, ID, ClaimRevision], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {accept_claim, ID, ClaimRevision});

handle_function_('UpdateClaim', [UserInfo, PartyID, ID, ClaimRevision, Changeset], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {update_claim, ID, ClaimRevision, Changeset});

handle_function_('DenyClaim', [UserInfo, PartyID, ID, ClaimRevision, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {deny_claim, ID, ClaimRevision, Reason});

handle_function_('RevokeClaim', [UserInfo, PartyID, ID, ClaimRevision, Reason], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {revoke_claim, ID, ClaimRevision, Reason});

%% Event

handle_function_('GetEvents', [UserInfo, PartyID, Range], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    hg_party_machine:get_public_history(PartyID, AfterID, Limit);

%% ShopAccount

handle_function_('GetAccountState', [UserInfo, PartyID, AccountID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_account_state(AccountID, Party);

handle_function_('GetShopAccount', [UserInfo, PartyID, ShopID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = hg_party_machine:get_party(PartyID),
    hg_party:get_shop_account(ShopID, Party);

%% PartyMeta

handle_function_('GetMeta', [UserInfo, PartyID], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_meta(PartyID);

handle_function_('GetMetaData', [UserInfo, PartyID, NS], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:get_metadata(NS, PartyID);

handle_function_('SetMetaData', [UserInfo, PartyID, NS, Data], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {set_metadata, NS, Data});

handle_function_('RemoveMetaData', [UserInfo, PartyID, NS], _Opts) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    hg_party_machine:call(PartyID, {remove_metadata, NS});

%% Payment Institutions

handle_function_(
    'ComputePaymentInstitutionTerms',
    [UserInfo, PartyID, PaymentInstitutionRef, Varset],
    _Opts
) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Revision = hg_domain:head(),
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    VS = prepare_varset(PartyID, Varset),
    ContractTemplate = get_default_contract_template(PaymentInstitution, VS, Revision),
    Terms = hg_party:get_terms(ContractTemplate, hg_datetime:format_now(), Revision),
    hg_party:reduce_terms(Terms, VS, Revision);

%% Payouts adhocs

handle_function_(
    'ComputePayoutCashFlow',
    [UserInfo, PartyID, #payproc_PayoutParams{id = ShopID, amount = Amount, timestamp = Timestamp} = PayoutParams],
    _Opts
) ->
    ok = set_meta_and_check_access(UserInfo, PartyID),
    Party = checkout_party(PartyID, {timestamp, Timestamp}),
    Shop = ensure_shop(hg_party:get_shop(ShopID, Party)),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Currency = Amount#domain_Cash.currency,
    ok = hg_invoice_utils:validate_currency(Currency, Shop),
    PayoutTool = get_payout_tool(Shop, Contract, PayoutParams),
    VS = #{
        party_id => PartyID,
        shop_id  => ShopID,
        category => Shop#domain_Shop.category,
        currency => Currency,
        cost     => Amount,
        payout_method => hg_payout_tool:get_method(PayoutTool)
    },
    Revision = hg_domain:head(),
    case hg_party:get_terms(Contract, Timestamp, Revision) of
        #domain_TermSet{payouts = PayoutsTerms} when PayoutsTerms /= undefined ->
            compute_payout_cash_flow(Amount, PayoutsTerms, Shop, Contract, VS, Revision);
        #domain_TermSet{payouts = undefined} ->
            throw(#payproc_OperationNotPermitted{})
    end.

%%

get_payout_tool(_Shop, _Contract, #payproc_PayoutParams{payout_tool_id = ToolID})
    when ToolID =/= undefined
->
    ToolID;
get_payout_tool(Shop, Contract, _PayoutParams) ->
    hg_contract:get_payout_tool(Shop#domain_Shop.payout_tool_id, Contract).

set_meta_and_check_access(UserInfo, PartyID) ->
    ok = assume_user_identity(UserInfo),
    _ = set_party_mgmt_meta(PartyID),
    assert_party_accessible(PartyID).

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
    checkout_party(PartyID, RevisionParam, #payproc_PartyNotExistsYet{}).

checkout_party(PartyID, RevisionParam, Exception) ->
    try
        hg_party_machine:checkout(PartyID, RevisionParam)
    catch
        error:revision_not_found ->
            throw(Exception)
    end.

ensure_contract(#domain_Contract{} = Contract) ->
    Contract;
ensure_contract(undefined) ->
    throw(#payproc_ContractNotFound{}).

ensure_shop(#domain_Shop{} = Shop) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

get_payment_institution(PaymentInstitutionRef, Revision) ->
    case hg_domain:find(Revision, {payment_institution, PaymentInstitutionRef}) of
        #domain_PaymentInstitution{} = P ->
            P;
        notfound ->
            throw(#payproc_PaymentInstitutionNotFound{})
    end.

get_default_contract_template(#domain_PaymentInstitution{default_contract_template = ContractSelector}, VS, Revision) ->
    ContractTemplateRef = hg_selector:reduce_to_value(ContractSelector, VS, Revision),
    hg_domain:get(Revision, {contract_template, ContractTemplateRef}).

compute_payout_cash_flow(
    Amount,
    #domain_PayoutsServiceTerms{fees = CashFlowSelector},
    Shop,
    Contract,
    VS,
    Revision
) ->
    Cashflow = hg_selector:reduce_to_value(CashFlowSelector, VS, Revision),
    CashFlowContext = #{operation_amount => Amount},
    Currency = Amount#domain_Cash.currency,
    AccountMap = collect_payout_account_map(Currency, Shop, Contract, VS, Revision),
    hg_cashflow:finalize(Cashflow, CashFlowContext, AccountMap).

collect_payout_account_map(
    Currency,
    #domain_Shop{account = ShopAccount},
    #domain_Contract{payment_institution = PaymentInstitutionRef},
    VS,
    Revision
) ->
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    SystemAccount = hg_payment_institution:get_system_account(Currency, VS, Revision, PaymentInstitution),
    #{
        {merchant , settlement} => ShopAccount#domain_ShopAccount.settlement,
        {merchant , guarantee } => ShopAccount#domain_ShopAccount.guarantee,
        {merchant , payout    } => ShopAccount#domain_ShopAccount.payout,
        {system   , settlement} => SystemAccount#domain_SystemAccount.settlement
    }.

prepare_varset(PartyID, #payproc_Varset{} = V) ->
    prepare_varset(PartyID, V, #{}).

prepare_varset(PartyID, #payproc_Varset{} = V, VS0) ->
    genlib_map:compact(VS0#{
        party_id => PartyID,
        category => V#payproc_Varset.category,
        currency => V#payproc_Varset.currency,
        cost => V#payproc_Varset.amount,
        payment_tool => prepare_payment_tool_var(V#payproc_Varset.payment_method),
        payout_method => V#payproc_Varset.payout_method,
        wallet_id => V#payproc_Varset.wallet_id
    }).

prepare_payment_tool_var(PaymentMethodRef) when PaymentMethodRef /= undefined ->
    hg_payment_tool:create_from_method(PaymentMethodRef);
prepare_payment_tool_var(undefined) ->
    undefined.

get_identification_level(#domain_Contract{contractor_id = undefined, contractor = Contractor}, _) ->
    %% TODO legacy, remove after migration
    case Contractor of
        {legal_entity, _} ->
            full;
        _ ->
            none
    end;
get_identification_level(#domain_Contract{contractor_id = ContractorID}, Party) ->
    Contractor = hg_party:get_contractor(ContractorID, Party),
    Contractor#domain_PartyContractor.status.
