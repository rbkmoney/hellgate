-module(hg_claim).

-include("party_events.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([create/5]).
-export([update/5]).
-export([accept/4]).
-export([deny/3]).
-export([revoke/3]).
-export([apply/3]).

-export([get_id/1]).
-export([get_revision/1]).
-export([get_status/1]).
-export([set_status/4]).
-export([is_pending/1]).
-export([is_accepted/1]).
-export([is_need_acceptance/3]).
-export([is_conflicting/5]).
-export([update_changeset/4]).

-export([assert_revision/2]).
-export([assert_pending/1]).
-export([assert_applicable/4]).
-export([assert_acceptable/4]).
-export([raise_invalid_changeset/1]).

%% Types

-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim_status()    :: dmsl_payment_processing_thrift:'ClaimStatus'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().

-type party()           :: hg_party:party().

-type timestamp()       :: hg_datetime:timestamp().
-type revision()        :: hg_domain:revision().

%% Interface

-spec get_id(claim()) ->
    claim_id().

get_id(#payproc_Claim{id = ID}) ->
    ID.

-spec get_revision(claim()) ->
    claim_revision().

get_revision(#payproc_Claim{revision = Revision}) ->
    Revision.

-spec create(claim_id(), changeset(), party(), timestamp(), revision()) ->
    claim() | no_return().

create(ID, Changeset, Party, Timestamp, Revision) ->
    ok = assert_changeset_applicable(Changeset, Timestamp, Revision, Party),
    #payproc_Claim{
        id        = ID,
        status    = ?pending(),
        changeset = Changeset,
        revision = 1,
        created_at = Timestamp
    }.

-spec update(changeset(), claim(), party(), timestamp(), revision()) ->
    claim() | no_return().

update(NewChangeset, #payproc_Claim{changeset = OldChangeset} = Claim, Party, Timestamp, Revision) ->
    TmpChangeset = merge_changesets(OldChangeset, NewChangeset),
    ok = assert_changeset_applicable(TmpChangeset, Timestamp, Revision, Party),
    update_changeset(NewChangeset, get_next_revision(Claim), Timestamp, Claim).

-spec update_changeset(changeset(), claim_revision(), timestamp(), claim()) ->
    claim().

update_changeset(NewChangeset, NewRevision, Timestamp, #payproc_Claim{changeset = OldChangeset} = Claim) ->
    Claim#payproc_Claim{
        revision = NewRevision,
        updated_at = Timestamp,
        changeset = merge_changesets(OldChangeset, NewChangeset)
    }.

-spec accept(timestamp(), revision(), party(), claim()) ->
    claim() | no_return().

accept(Timestamp, DomainRevision, Party, Claim) ->
    ok = assert_acceptable(Claim, Timestamp, DomainRevision, Party),
    Effects = make_effects(Timestamp, DomainRevision, Claim),
    set_status(?accepted(Effects), get_next_revision(Claim), Timestamp, Claim).

-spec deny(binary(), timestamp(), claim()) ->
    claim().

deny(Reason, Timestamp, Claim) ->
    set_status(?denied(Reason), get_next_revision(Claim), Timestamp, Claim).

-spec revoke(binary(), timestamp(), claim()) ->
    claim().

revoke(Reason, Timestamp, Claim) ->
    set_status(?revoked(Reason), get_next_revision(Claim), Timestamp, Claim).

-spec set_status(claim_status(), claim_revision(), timestamp(), claim()) ->
    claim().

set_status(Status, NewRevision, Timestamp, Claim) ->
    Claim#payproc_Claim{
        revision = NewRevision,
        updated_at = Timestamp,
        status = Status
    }.

-spec get_status(claim()) ->
    claim_status().

get_status(#payproc_Claim{status = Status}) ->
    Status.

-spec is_pending(claim()) ->
    boolean().

is_pending(#payproc_Claim{status = ?pending()}) ->
    true;
is_pending(_) ->
    false.

-spec is_accepted(claim()) ->
    boolean().

is_accepted(#payproc_Claim{status = ?accepted(_)}) ->
    true;
is_accepted(_) ->
    false.

-spec is_need_acceptance(claim(), party(), revision()) ->
    boolean().

is_need_acceptance(Claim, Party, Revision) ->
    is_changeset_need_acceptance(get_changeset(Claim), Party, Revision).

-spec is_conflicting(claim(), claim(), timestamp(), revision(), party()) ->
    boolean().

is_conflicting(Claim1, Claim2, Timestamp, Revision, Party) ->
    has_changeset_conflict(get_changeset(Claim1), get_changeset(Claim2), Timestamp, Revision, Party).

-spec apply(claim(), timestamp(), party()) ->
    party().

apply(#payproc_Claim{status = ?accepted(Effects)}, Timestamp, Party) ->
    apply_effects(Effects, Timestamp, Party).

%% Implementation

get_changeset(#payproc_Claim{changeset = Changeset}) ->
    Changeset.

get_next_revision(#payproc_Claim{revision = ClaimRevision}) ->
    ClaimRevision + 1.

is_changeset_need_acceptance(Changeset, Party, Revision) ->
    lists:any(fun(Change) -> is_change_need_acceptance(Change, Party, Revision) end, Changeset).

is_change_need_acceptance(?shop_modification(ID, Modification), Party, Revision) ->
    Shop = hg_party:get_shop(ID, Party),
    is_shop_modification_need_acceptance(Shop, Modification, Party, Revision);
is_change_need_acceptance(?contract_modification(ID, Modification), Party, Revision) ->
    Contract = hg_party:get_contract(ID, Party),
    is_contract_modification_need_acceptance(Contract, Modification, Revision);
is_change_need_acceptance(_, _, _) ->
    true.

is_shop_modification_need_acceptance(undefined, {creation, ShopParams}, Party, Revision) ->
    Contract = hg_party:get_contract(ShopParams#payproc_ShopParams.contract_id, Party),
    case Contract of
        undefined ->
            % contract not exists, so it should be created in same claim
            % we can check contract creation and forget about this shop change
            false;
        #domain_Contract{} ->
            hg_contract:is_live(Contract, Revision)
    end;
is_shop_modification_need_acceptance(undefined, _AnyModification, _, _) ->
    % shop does not exist, so it should be created in same claim
    % we can check shop creation and forget about this shop change
    false;
is_shop_modification_need_acceptance(Shop, _AnyModification, Party, Revision) ->
    % shop exist, so contract should be
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    hg_contract:is_live(Contract, Revision).

is_contract_modification_need_acceptance(undefined, {creation, ContractParams}, Revision) ->
    PaymentInstitution = hg_domain:get(
        Revision,
        {payment_institution, ContractParams#payproc_ContractParams.payment_institution}
    ),
    hg_payment_institution:is_live(PaymentInstitution);
is_contract_modification_need_acceptance(undefined, _AnyModification, _) ->
    % contract does not exist, so it should be created in same claim
    % we can check contract creation and forget about this contract change
    false;
is_contract_modification_need_acceptance(Contract, _AnyModification, Revision) ->
    % contract exist
    hg_contract:is_live(Contract, Revision).

has_changeset_conflict(Changeset, ChangesetPending, Timestamp, Revision, Party) ->
    % NOTE We can safely assume that conflict is essentially the fact that two changesets are
    %      overlapping. Provided that any change is free of side effects (like computing unique
    %      identifiers), we can test if there's any overlapping by just applying changesets to the
    %      current state in different order and comparing produced states. If they're the same then
    %      there is no overlapping in changesets.
    Party1 = apply_effects(
        make_changeset_safe_effects(
            merge_changesets(ChangesetPending, Changeset),
            Timestamp,
            Revision
        ),
        Timestamp,
        Party
    ),
    Party2 = apply_effects(
        make_changeset_safe_effects(
            merge_changesets(Changeset, ChangesetPending),
            Timestamp,
            Revision
        ),
        Timestamp,
        Party
    ),
    Party1 /= Party2.

merge_changesets(ChangesetBase, Changeset) ->
    % TODO Evaluating a possibility to drop server-side claim merges completely, since it's the
    %      source of unwelcomed complexity. In the meantime this naïve implementation would suffice.
    ChangesetBase ++ Changeset.

make_effects(Timestamp, Revision, Claim) ->
    make_changeset_effects(get_changeset(Claim), Timestamp, Revision).

make_changeset_effects(Changeset, Timestamp, Revision) ->
    squash_effects(lists:map(
        fun(Change) ->
            hg_claim_effect:make(Change, Timestamp, Revision)
        end,
        Changeset
    )).

make_changeset_safe_effects(Changeset, Timestamp, Revision) ->
    squash_effects(lists:map(
        fun(Change) ->
            hg_claim_effect:make_safe(Change, Timestamp, Revision)
        end,
        Changeset
    )).

squash_effects(Effects) ->
    squash_effects(Effects, []).

squash_effects([?contract_effect(_, _) = Effect | Others], Squashed) ->
    squash_effects(Others, squash_contract_effect(Effect, Squashed));
squash_effects([?shop_effect(_, _) = Effect | Others], Squashed) ->
    squash_effects(Others, squash_shop_effect(Effect, Squashed));
squash_effects([Effect | Others], Squashed) ->
    squash_effects(Others, Squashed ++ [Effect]);
squash_effects([], Squashed) ->
    Squashed.

squash_contract_effect(?contract_effect(_, {created, _}) = Effect, Squashed) ->
    Squashed ++ [Effect];
squash_contract_effect(?contract_effect(ContractID, Mod) = Effect, Squashed) ->
    % Try to find contract creation in squashed effects
    {ReversedEffects, AppliedFlag} = lists:foldl(
        fun
            (?contract_effect(ID, {created, Contract}), {Acc, false}) when ID =:= ContractID ->
                % Contract creation found, lets update it with this claim effect
                {[?contract_effect(ID, {created, update_contract(Mod, Contract)}) | Acc], true};
            (?contract_effect(ID, {created, _}), {_, true}) when ID =:= ContractID ->
                % One more created contract with same id - error.
                raise_invalid_changeset(?invalid_contract(ID, {already_exists, ID}));
            (E, {Acc, Flag}) ->
                {[E | Acc], Flag}
        end,
        {[], false},
        Squashed
    ),
    case AppliedFlag of
        true ->
            lists:reverse(ReversedEffects);
        false ->
            % Contract creation not found, so this contract created earlier and we shuold just
            % add this claim effect to the end of squashed effects
            lists:reverse([Effect | ReversedEffects])
    end.

squash_shop_effect(?shop_effect(_, {created, _}) = Effect, Squashed) ->
    Squashed ++ [Effect];
squash_shop_effect(?shop_effect(ShopID, Mod) = Effect, Squashed) ->
    % Try to find shop creation in squashed effects
    {ReversedEffects, AppliedFlag} = lists:foldl(
        fun
            (?shop_effect(ID, {created, Shop}), {Acc, false}) when ID =:= ShopID ->
                % Shop creation found, lets update it with this claim effect
                {[?shop_effect(ID, {created, update_shop(Mod, Shop)}) | Acc], true};
            (?shop_effect(ID, {created, _}), {_, true}) when ID =:= ShopID ->
                % One more shop with same id - error.
                raise_invalid_changeset(?invalid_shop(ID, {already_exists, ID}));
            (E, {Acc, Flag}) ->
                {[E | Acc], Flag}
        end,
        {[], false},
        Squashed
    ),
    case AppliedFlag of
        true ->
            lists:reverse(ReversedEffects);
        false ->
            % Shop creation not found, so this shop created earlier and we shuold just
            % add this claim effect to the end of squashed effects
            lists:reverse([Effect | ReversedEffects])
    end.

apply_effects(Effects, Timestamp, Party) ->
    lists:foldl(
        fun(Effect, AccParty) ->
            apply_claim_effect(Effect, Timestamp, AccParty)
        end,
        Party,
        Effects
    ).

apply_claim_effect(?contractor_effect(ID, Effect), _, Party) ->
    apply_contractor_effect(ID, Effect, Party);
apply_claim_effect(?contract_effect(ID, Effect), Timestamp, Party) ->
    apply_contract_effect(ID, Effect, Timestamp, Party);
apply_claim_effect(?shop_effect(ID, Effect), _, Party) ->
    apply_shop_effect(ID, Effect, Party);
apply_claim_effect(?wallet_effect(ID, Effect), _, Party) ->
    apply_wallet_effect(ID, Effect, Party).

apply_contractor_effect(_, {created, PartyContractor}, Party) ->
    hg_party:set_contractor(PartyContractor, Party);
apply_contractor_effect(ID, Effect, Party) ->
    PartyContractor = hg_party:get_contractor(ID, Party),
    hg_party:set_contractor(update_contractor(Effect, PartyContractor), Party).

update_contractor({identification_level_changed, Level}, PartyContractor) ->
    PartyContractor#domain_PartyContractor{status = Level};
update_contractor(
    {identity_documents_changed, #payproc_ContractorIdentityDocumentsChanged{
        identity_documents = Docs
    }},
    PartyContractor
) ->
    PartyContractor#domain_PartyContractor{identity_documents = Docs}.

apply_contract_effect(_, {created, Contract}, Timestamp, Party) ->
    hg_party:set_new_contract(Contract, Timestamp, Party);
apply_contract_effect(ID, Effect, _, Party) ->
    Contract = hg_party:get_contract(ID, Party),
    hg_party:set_contract(update_contract(Effect, Contract), Party).

update_contract({status_changed, Status}, Contract) ->
    Contract#domain_Contract{status = Status};
update_contract({adjustment_created, Adjustment}, Contract) ->
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    Contract#domain_Contract{adjustments = Adjustments};
update_contract({payout_tool_created, PayoutTool}, Contract) ->
    PayoutTools = Contract#domain_Contract.payout_tools ++ [PayoutTool],
    Contract#domain_Contract{payout_tools = PayoutTools};
update_contract(
    {payout_tool_info_changed, #payproc_PayoutToolInfoChanged{payout_tool_id = PayoutToolID, info = Info}},
    Contract
) ->
    PayoutTool = hg_contract:get_payout_tool(PayoutToolID, Contract),
    hg_contract:set_payout_tool(PayoutTool#domain_PayoutTool{payout_tool_info = Info}, Contract);
update_contract({legal_agreement_bound, LegalAgreement}, Contract) ->
    Contract#domain_Contract{legal_agreement = LegalAgreement};
update_contract({report_preferences_changed, ReportPreferences}, Contract) ->
    Contract#domain_Contract{report_preferences = ReportPreferences};
update_contract({contractor_changed, ContractorID}, Contract) ->
    Contract#domain_Contract{contractor_id = ContractorID}.

apply_shop_effect(_, {created, Shop}, Party) ->
    hg_party:set_shop(Shop, Party);
apply_shop_effect(ID, Effect, Party) ->
    Shop = hg_party:get_shop(ID, Party),
    hg_party:set_shop(update_shop(Effect, Shop), Party).

update_shop({category_changed, Category}, Shop) ->
    Shop#domain_Shop{category = Category};
update_shop({details_changed, Details}, Shop) ->
    Shop#domain_Shop{details = Details};
update_shop(
    {contract_changed, #payproc_ShopContractChanged{contract_id = ContractID, payout_tool_id = PayoutToolID}},
    Shop
) ->
    Shop#domain_Shop{contract_id = ContractID, payout_tool_id = PayoutToolID};
update_shop({payout_tool_changed, PayoutToolID}, Shop) ->
    Shop#domain_Shop{payout_tool_id = PayoutToolID};
update_shop({location_changed, Location}, Shop) ->
    Shop#domain_Shop{location = Location};
update_shop({proxy_changed, _}, Shop) ->
    % deprecated
    Shop;
update_shop(?payout_schedule_changed(BusinessScheduleRef), Shop) ->
    Shop#domain_Shop{payout_schedule = BusinessScheduleRef};
update_shop({account_created, Account}, Shop) ->
    Shop#domain_Shop{account = Account}.

apply_wallet_effect(_, {created, Wallet}, Party) ->
    hg_party:set_wallet(Wallet, Party);
apply_wallet_effect(ID, Effect, Party) ->
    Wallet = hg_party:get_wallet(ID, Party),
    hg_party:set_wallet(update_wallet(Effect, Wallet), Party).

update_wallet({account_created, Account}, Wallet) ->
    Wallet#domain_Wallet{account = Account}.

-spec raise_invalid_changeset(dmsl_payment_processing_thrift:'InvalidChangesetReason'()) ->
    no_return().

raise_invalid_changeset(Reason) ->
    throw(#payproc_InvalidChangeset{reason = Reason}).

%% Asserts

-spec assert_revision(claim(), claim_revision())    -> ok | no_return().

assert_revision(#payproc_Claim{revision = Revision}, Revision) ->
    ok;
assert_revision(_, _) ->
    throw(#payproc_InvalidClaimRevision{}).

-spec assert_pending(claim())                       -> ok | no_return().

assert_pending(#payproc_Claim{status = ?pending()}) ->
    ok;
assert_pending(#payproc_Claim{status = Status}) ->
    throw(#payproc_InvalidClaimStatus{status = Status}).

-spec assert_applicable(claim(), timestamp(), revision(), party()) ->
    ok | no_return().

assert_applicable(Claim, Timestamp, Revision, Party) ->
    assert_changeset_applicable(get_changeset(Claim), Timestamp, Revision, Party).

-spec assert_changeset_applicable(changeset(), timestamp(), revision(), party()) ->
    ok | no_return().

assert_changeset_applicable([Change | Others], Timestamp, Revision, Party) ->
    case Change of
        ?contract_modification(ID, Modification) ->
            Contract = hg_party:get_contract(ID, Party),
            ok = assert_contract_change_applicable(ID, Modification, Contract);
        ?shop_modification(ID, Modification) ->
            Shop = hg_party:get_shop(ID, Party),
            ok = assert_shop_change_applicable(ID, Modification, Shop, Party, Revision);
        ?contractor_modification(ID, Modification) ->
            Contractor = hg_party:get_contractor(ID, Party),
            ok = assert_contractor_change_applicable(ID, Modification, Contractor);
        ?wallet_modification(ID, Modification) ->
            Wallet = hg_party:get_wallet(ID, Party),
            ok = assert_wallet_change_applicable(ID, Modification, Wallet)
    end,
    Effect = hg_claim_effect:make_safe(Change, Timestamp, Revision),
    assert_changeset_applicable(Others, Timestamp, Revision, apply_claim_effect(Effect, Timestamp, Party));
assert_changeset_applicable([], _, _, _) ->
    ok.

assert_contract_change_applicable(_, {creation, _}, undefined) ->
    ok;
assert_contract_change_applicable(ID, {creation, _}, #domain_Contract{}) ->
    raise_invalid_changeset(?invalid_contract(ID, {already_exists, ID}));
assert_contract_change_applicable(ID, _AnyModification, undefined) ->
    raise_invalid_changeset(?invalid_contract(ID, {not_exists, ID}));
assert_contract_change_applicable(ID, ?contract_termination(_), Contract) ->
    case hg_contract:is_active(Contract) of
        true ->
            ok;
        false ->
            raise_invalid_changeset(?invalid_contract(ID, {invalid_status, Contract#domain_Contract.status}))
    end;
assert_contract_change_applicable(ID, ?adjustment_creation(AdjustmentID, _), Contract) ->
    case hg_contract:get_adjustment(AdjustmentID, Contract) of
        undefined ->
            ok;
        _ ->
            raise_invalid_changeset(?invalid_contract(ID, {contract_adjustment_already_exists, AdjustmentID}))
    end;
assert_contract_change_applicable(ID, ?payout_tool_creation(PayoutToolID, _), Contract) ->
    case hg_contract:get_payout_tool(PayoutToolID, Contract) of
        undefined ->
            ok;
        _ ->
            raise_invalid_changeset(?invalid_contract(ID, {payout_tool_already_exists, PayoutToolID}))
    end;
assert_contract_change_applicable(ID, ?payout_tool_info_modification(PayoutToolID, _), Contract) ->
    case hg_contract:get_payout_tool(PayoutToolID, Contract) of
        undefined ->
            raise_invalid_changeset(?invalid_contract(ID, {payout_tool_not_exists, PayoutToolID}));
        _ ->
            ok
    end;
assert_contract_change_applicable(_, _, _) ->
    ok.

assert_shop_change_applicable(_, {creation, _}, undefined, _, _) ->
    ok;
assert_shop_change_applicable(ID, _AnyModification, undefined, _, _) ->
    raise_invalid_changeset(?invalid_shop(ID, {not_exists, ID}));
assert_shop_change_applicable(ID, {creation, _}, #domain_Shop{}, _, _) ->
    raise_invalid_changeset(?invalid_shop(ID, {already_exists, ID}));
assert_shop_change_applicable(
    _ID,
    {shop_account_creation, _},
    #domain_Shop{account = Account},
    _Party,
    _Revision
) when Account /= undefined ->
    throw(#'InvalidRequest'{errors = [<<"Can't change shop's account">>]});
assert_shop_change_applicable(
    _ID,
    {contract_modification, #payproc_ShopContractModification{contract_id = NewContractID}},
    #domain_Shop{contract_id = OldContractID},
    Party,
    Revision
) ->
    OldContract = hg_party:get_contract(OldContractID, Party),
    case hg_party:get_contract(NewContractID, Party) of
        #domain_Contract{} = NewContract ->
            assert_payment_institution_realm_equals(OldContract, NewContract, Revision);
        undefined ->
            raise_invalid_changeset(?invalid_contract(NewContractID, {not_exists, NewContractID}))
    end;
assert_shop_change_applicable(_, _, _, _, _) ->
    ok.

assert_contractor_change_applicable(_, {creation, _}, undefined) ->
    ok;
assert_contractor_change_applicable(ID, _AnyModification, undefined) ->
    raise_invalid_changeset(?invalid_contractor(ID, {not_exists, ID}));
assert_contractor_change_applicable(ID, {creation, _}, #domain_PartyContractor{}) ->
    raise_invalid_changeset(?invalid_contractor(ID, {already_exists, ID}));
assert_contractor_change_applicable(_, _, _) ->
    ok.

assert_wallet_change_applicable(_, {creation, _}, undefined) ->
    ok;
assert_wallet_change_applicable(ID, _AnyModification, undefined) ->
    raise_invalid_changeset(?invalid_wallet(ID, {not_exists, ID}));
assert_wallet_change_applicable(ID, {creation, _}, #domain_Wallet{}) ->
    raise_invalid_changeset(?invalid_wallet(ID, {already_exists, ID}));
assert_wallet_change_applicable(
    _ID,
    {account_creation, _},
    #domain_Wallet{account = Account}
) when Account /= undefined ->
    throw(#'InvalidRequest'{errors = [<<"Can't change wallet's account">>]});
assert_wallet_change_applicable(_, _, _) ->
    ok.

assert_payment_institution_realm_equals(
    #domain_Contract{id = OldContractID, payment_institution = OldRef},
    #domain_Contract{id = NewContractID, payment_institution = NewRef},
    Revision
) ->
    OldRealm = get_payment_institution_realm(OldRef, Revision, OldContractID),
    case get_payment_institution_realm(NewRef, Revision, NewContractID) of
        OldRealm ->
            ok;
        _NewRealm ->
            raise_invalid_payment_institution(NewContractID, NewRef)
    end.

get_payment_institution_realm(Ref, Revision, ContractID) ->
    case hg_domain:find(Revision, {payment_institution, Ref}) of
        #domain_PaymentInstitution{} = P ->
            hg_payment_institution:get_realm(P);
        notfound ->
            raise_invalid_payment_institution(ContractID, Ref)
    end.

-spec assert_acceptable(claim(), timestamp(), revision(), party()) ->
    ok | no_return().

assert_acceptable(Claim, Timestamp, Revision, Party0) ->
    Changeset = get_changeset(Claim),
    Effects = make_changeset_safe_effects(Changeset, Timestamp, Revision),
    Party = apply_effects(Effects, Timestamp, Party0),
    hg_party:assert_party_objects_valid(Timestamp, Revision, Party).

-spec raise_invalid_payment_institution(
    dmsl_domain_thrift:'ContractID'(),
    dmsl_domain_thrift:'PaymentInstitutionRef'() | undefined
) ->
    no_return().

raise_invalid_payment_institution(ContractID, Ref) ->
    raise_invalid_changeset(?invalid_contract(
        ContractID,
        {invalid_object_reference, #payproc_InvalidObjectReference{
            ref = make_optional_domain_ref(payment_institution, Ref)
        }}
    )).

make_optional_domain_ref(_, undefined) ->
    undefined;
make_optional_domain_ref(Type, Ref) ->
    {Type, Ref}.
