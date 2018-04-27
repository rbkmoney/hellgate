-module(hg_claim).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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
-export([raise_invalid_changeset/1]).

%% Types

-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim_status()    :: dmsl_payment_processing_thrift:'ClaimStatus'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().

-type party()           :: dmsl_domain_thrift:'Party'().

-type timestamp()       :: dmsl_base_thrift:'Timestamp'().
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
    ok = assert_changeset_acceptable(get_changeset(Claim), Timestamp, DomainRevision, Party),
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
    % TODO abstraction leek
    PaymentInstitution#domain_PaymentInstitution.realm =:= live;
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
            make_change_effect(Change, Timestamp, Revision)
        end,
        Changeset
    )).

make_change_effect(?contract_modification(ID, Modification), Timestamp, Revision) ->
    try
        ?contract_effect(ID, make_contract_modification_effect(ID, Modification, Timestamp, Revision))
    catch
        throw:{payment_institution_invalid, Ref} ->
            raise_invalid_changeset(?invalid_contract(
                ID,
                {invalid_object_reference, #payproc_InvalidObjectReference{
                    ref = make_optional_domain_ref(payment_institution, Ref)
                }}
            ));
        throw:{template_invalid, Ref} ->
            raise_invalid_changeset(?invalid_contract(
                ID,
                {invalid_object_reference, #payproc_InvalidObjectReference{
                    ref = make_optional_domain_ref(contract_template, Ref)
                }}
            ))
    end;

make_change_effect(?shop_modification(ID, Modification), Timestamp, Revision) ->
    ?shop_effect(ID, make_shop_modification_effect(ID, Modification, Timestamp, Revision)).

make_contract_modification_effect(ID, {creation, ContractParams}, Timestamp, Revision) ->
    {created, hg_contract:create(ID, ContractParams, Timestamp, Revision)};
make_contract_modification_effect(_, ?contract_termination(_), Timestamp, _) ->
    {status_changed, {terminated, #domain_ContractTerminated{terminated_at = Timestamp}}};
make_contract_modification_effect(_, ?adjustment_creation(AdjustmentID, Params), Timestamp, Revision) ->
    {adjustment_created, hg_contract:create_adjustment(AdjustmentID, Params, Timestamp, Revision)};
make_contract_modification_effect(_, ?payout_tool_creation(PayoutToolID, Params), Timestamp, _) ->
    {payout_tool_created, hg_payout_tool:create(PayoutToolID, Params, Timestamp)};
make_contract_modification_effect(_, {legal_agreement_binding, LegalAgreement}, _, _) ->
    {legal_agreement_bound, LegalAgreement};
make_contract_modification_effect(ID, {report_preferences_modification, ReportPreferences}, _, Revision) ->
    _ = assert_report_schedule_valid(ID, ReportPreferences, Revision),
    {report_preferences_changed, ReportPreferences}.

make_shop_modification_effect(ID, {creation, ShopParams}, Timestamp, _) ->
    {created, hg_party:create_shop(ID, ShopParams, Timestamp)};
make_shop_modification_effect(_, {category_modification, Category}, _, _) ->
    {category_changed, Category};
make_shop_modification_effect(_, {details_modification, Details}, _, _) ->
    {details_changed, Details};
make_shop_modification_effect(_, ?shop_contract_modification(ContractID, PayoutToolID), _, _) ->
    {contract_changed, #payproc_ShopContractChanged{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }};
make_shop_modification_effect(_, {payout_tool_modification, PayoutToolID}, _, _) ->
    {payout_tool_changed, PayoutToolID};
make_shop_modification_effect(_, ?proxy_modification(Proxy), _, _) ->
    {proxy_changed, #payproc_ShopProxyChanged{proxy = Proxy}};
make_shop_modification_effect(_, {location_modification, Location}, _, _) ->
    {location_changed, Location};
make_shop_modification_effect(_, {shop_account_creation, Params}, _, _) ->
    {account_created, create_shop_account(Params)};
make_shop_modification_effect(ID, ?payout_schedule_modification(BusinessScheduleRef), _, Revision) ->
    _ = assert_payout_schedule_valid(ID, BusinessScheduleRef, Revision),
    ?payout_schedule_changed(BusinessScheduleRef).

create_shop_account(#payproc_ShopAccountParams{currency = Currency}) ->
    create_shop_account(Currency);
create_shop_account(#domain_CurrencyRef{symbolic_code = SymbolicCode} = CurrencyRef) ->
    GuaranteeID = hg_accounting:create_account(SymbolicCode),
    SettlementID = hg_accounting:create_account(SymbolicCode),
    PayoutID = hg_accounting:create_account(SymbolicCode),
    #domain_ShopAccount{
        currency = CurrencyRef,
        settlement = SettlementID,
        guarantee = GuaranteeID,
        payout = PayoutID
    }.

make_changeset_safe_effects(Changeset, Timestamp, Revision) ->
    squash_effects(lists:map(
        fun(Change) ->
            make_change_safe_effect(Change, Timestamp, Revision)
        end,
        Changeset
    )).

make_change_safe_effect(
    ?shop_modification(ID, {shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}),
    _Timestamp,
    _Revision
) ->
    ?shop_effect(ID,
        {account_created, #domain_ShopAccount{
            currency = Currency,
            settlement = 0,
            guarantee = 0,
            payout = 0
        }}
    );

make_change_safe_effect(Change, Timestamp, Revision) ->
    make_change_effect(Change, Timestamp, Revision).

squash_effects(Effects) ->
    squash_effects(Effects, []).

squash_effects([?contract_effect(_, _) = Effect | Others], Squashed) ->
    squash_effects(Others, squash_contract_effect(Effect, Squashed));
squash_effects([?shop_effect(_, _) = Effect | Others], Squashed) ->
    squash_effects(Others, squash_shop_effect(Effect, Squashed));
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

apply_claim_effect(?contract_effect(ID, Effect), Timestamp, Party) ->
    apply_contract_effect(ID, Effect, Timestamp, Party);
apply_claim_effect(?shop_effect(ID, Effect), _, Party) ->
    apply_shop_effect(ID, Effect, Party).

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
update_contract({legal_agreement_bound, LegalAgreement}, Contract) ->
    Contract#domain_Contract{legal_agreement = LegalAgreement};
update_contract({report_preferences_changed, ReportPreferences}, Contract) ->
    Contract#domain_Contract{report_preferences = ReportPreferences}.

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

assert_changeset_applicable([Change | Others], Timestamp, Revision, Party) ->
    case Change of
        ?contract_modification(ID, Modification) ->
            Contract = hg_party:get_contract(ID, Party),
            ok = assert_contract_change_applicable(ID, Modification, Contract);
        ?shop_modification(ID, Modification) ->
            Shop = hg_party:get_shop(ID, Party),
            ok = assert_shop_change_applicable(ID, Modification, Shop, Party)
    end,
    Effect = make_change_safe_effect(Change, Timestamp, Revision),
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
assert_contract_change_applicable(_, _, _) ->
    ok.

assert_shop_change_applicable(_, {creation, _}, undefined, _) ->
    ok;
assert_shop_change_applicable(ID, _AnyModification, undefined, _) ->
    raise_invalid_changeset(?invalid_shop(ID, {not_exists, ID}));
assert_shop_change_applicable(ID, {creation, _}, #domain_Shop{}, _) ->
    raise_invalid_changeset(?invalid_shop(ID, {already_exists, ID}));
assert_shop_change_applicable(
    _ID,
    {contract_modification, #payproc_ShopContractModification{contract_id = NewContractID}},
    #domain_Shop{contract_id = OldContractID},
    Party
) ->
    assert_payment_institutions_equals(OldContractID, NewContractID, Party);
assert_shop_change_applicable(_, _, _, _) ->
    ok.

assert_payment_institutions_equals(OldContractID, NewContractID, Party) ->
    #domain_Contract{payment_institution = OldRef} = hg_party:get_contract(OldContractID, Party),
    case hg_party:get_contract(NewContractID, Party) of
        #domain_Contract{payment_institution = OldRef} ->
            ok;
        #domain_Contract{payment_institution = NewRef} ->
            % Can't change shop's payment institution
            raise_invalid_changeset(?invalid_contract(
                NewContractID,
                {invalid_object_reference, #payproc_InvalidObjectReference{
                    ref = make_optional_domain_ref(payment_institution, NewRef)
                }}
            ));
        undefined ->
            raise_invalid_changeset(?invalid_contract(NewContractID, {not_exists, NewContractID}))
    end.

assert_changeset_acceptable(Changeset, Timestamp, Revision, Party0) ->
    Effects = make_changeset_safe_effects(Changeset, Timestamp, Revision),
    Party = apply_effects(Effects, Timestamp, Party0),
    hg_party:assert_party_objects_valid(Timestamp, Revision, Party).

assert_report_schedule_valid(_, #domain_ReportPreferences{service_acceptance_act_preferences = undefined}, _) ->
    ok;
assert_report_schedule_valid(
    ID,
    #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = BusinessScheduleRef
        }
    },
    Revision
) ->
    assert_valid_object_ref({contract, ID}, {business_schedule, BusinessScheduleRef}, Revision).

assert_payout_schedule_valid(ID, #domain_BusinessScheduleRef{} = BusinessScheduleRef, Revision) ->
    assert_valid_object_ref({shop, ID}, {business_schedule, BusinessScheduleRef}, Revision);
assert_payout_schedule_valid(_, undefined, _) ->
    ok.

assert_valid_object_ref(Prefix, Ref, Revision) ->
    case hg_domain:exists(Revision, Ref) of
        true ->
            ok;
        false ->
            raise_invalid_object_ref(Prefix, Ref)
    end.

raise_invalid_object_ref(Prefix, Ref) ->
    Ex = {invalid_object_reference, #payproc_InvalidObjectReference{ref = Ref}},
    raise_invalid_object_ref_(Prefix, Ex).

raise_invalid_object_ref_({shop, ID}, Ex) ->
    raise_invalid_changeset(?invalid_shop(ID, Ex));
raise_invalid_object_ref_({contract, ID}, Ex) ->
    raise_invalid_changeset(?invalid_contract(ID, Ex)).

make_optional_domain_ref(_, undefined) ->
    undefined;
make_optional_domain_ref(Type, Ref) ->
    {Type, Ref}.
