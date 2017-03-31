-module(hg_claim).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([create/5]).
-export([update/5]).
-export([accept/4]).
-export([deny/2]).
-export([revoke/2]).
-export([apply/3]).

-export([get_id/1]).
-export([get_status/1]).
-export([set_status/2]).
-export([is_pending/1]).
-export([is_accepted/1]).
-export([is_need_acceptance/1]).
-export([is_conflicting/5]).
-export([update_changeset/2]).
-export([craete_party_initial_claim/4]).

-export([assert_revision/2]).
-export([assert_pending/1]).

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

-spec craete_party_initial_claim(claim_id(), party(), timestamp(), revision()) ->
    claim().

craete_party_initial_claim(ID, Party, Timestamp, Revision) ->
    % FIXME
    Email = (Party#domain_Party.contact_info)#domain_PartyContactInfo.email,
    {ContractID, ContractParams} = hg_party:get_test_contract_params(Email, Revision),
    {PayoutToolID, PayoutToolParams} = hg_party:get_test_payout_tool_params(Revision),
    {ShopID, ShopParams} = hg_party:get_test_shop_params(ContractID, PayoutToolID, Revision),
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams}),
        ?contract_modification(ContractID, ?payout_tool_creation(PayoutToolID, PayoutToolParams)),
        ?shop_modification(ShopID, {creation, ShopParams})
    ],
    create(ID, Changeset, Party, Timestamp, Revision).

-spec create(claim_id(), changeset(), party(), timestamp(), revision()) ->
    claim() | no_return().

create(ID, Changeset, Party, Timestamp, Revision) ->
    ok = assert_changeset_applicable(Changeset, Timestamp, Revision, Party),
    #payproc_Claim{
        id        = ID,
        status    = ?pending(),
        changeset = merge_changesets(Changeset, ensure_shop_account_creation(Changeset, Party, Timestamp, Revision)),
        revision = 1
    }.

-spec update(changeset(), claim(), party(), timestamp(), revision()) ->
    claim() | no_return().

update(NewChangeset, #payproc_Claim{changeset = OldChangeset} = Claim, Party, Timestamp, Revision) ->
    TmpChangeset = merge_changesets(OldChangeset, NewChangeset),
    ok = assert_changeset_applicable(TmpChangeset, Timestamp, Revision, Party),
    Changeset = merge_changesets(NewChangeset, ensure_shop_account_creation(TmpChangeset, Party, Timestamp, Revision)),
    update_changeset(Changeset, Claim).

% FIXME dirty ad hoc for event appliance
-spec update_changeset(changeset(), claim()) ->
    claim().

update_changeset(NewChangeset, #payproc_Claim{changeset = OldChangeset} = Claim) ->
    Claim#payproc_Claim{
        revision = get_next_revision(Claim),
        changeset = merge_changesets(OldChangeset, NewChangeset)
    }.

-spec accept(timestamp(), revision(), party(), claim()) ->
    claim() | no_return().

accept(Timestamp, DomainRevision, Party, Claim) ->
    ok = assert_changeset_acceptable(get_changeset(Claim), Timestamp, DomainRevision, Party),
    Effects = make_effects(Timestamp, DomainRevision, Claim ),
    set_status(?accepted(Timestamp, Effects), Claim).

-spec deny(binary(), claim()) ->
    claim().

deny(Reason, Claim) ->
    set_status(?denied(Reason), Claim).

-spec revoke(binary(), claim()) ->
    claim().

revoke(Reason, Claim) ->
    set_status(?revoked(Reason), Claim).

% FIXME dirty ad hoc for event appliance
-spec set_status(claim_status(), claim()) ->
    claim().

set_status(Status, Claim) ->
    Claim#payproc_Claim{
        revision = get_next_revision(Claim),
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

is_accepted(#payproc_Claim{status = ?accepted(_,_)}) ->
    true;
is_accepted(_) ->
    false.

-spec is_need_acceptance(claim()) ->
    boolean().

is_need_acceptance(Claim) ->
    is_changeset_need_acceptance(get_changeset(Claim)).

-spec is_conflicting(claim(), claim(), timestamp(), revision(), party()) ->
    boolean().

is_conflicting(Claim1, Claim2, Timestamp, Revision, Party) ->
    has_changeset_conflict(get_changeset(Claim1), get_changeset(Claim2), Timestamp, Revision, Party).

-spec apply(claim(), timestamp(), party()) ->
    party().

apply(#payproc_Claim{status = ?accepted(_, Effects)}, Timestamp, Party) ->
    apply_effects(Effects, Timestamp, Party).

%% Implementation

get_changeset(#payproc_Claim{changeset = Changeset}) ->
    Changeset.

get_next_revision(#payproc_Claim{revision = ClaimRevision}) ->
    ClaimRevision + 1.

is_changeset_need_acceptance(Changeset) ->
    lists:any(fun is_change_need_acceptance/1, Changeset).

is_change_need_acceptance(?shop_modification(_, Modification)) ->
    is_shop_modification_need_acceptance(Modification);
is_change_need_acceptance(_) ->
    true.

is_shop_modification_need_acceptance({details_modification, _}) ->
    false;
is_shop_modification_need_acceptance({proxy_modification, _}) ->
    false;
is_shop_modification_need_acceptance({shop_account_creation, _}) ->
    false;
is_shop_modification_need_acceptance(_) ->
    true.

ensure_shop_account_creation(Changeset, Party, Timestamp, Revision) ->
    {CreatedShops, Party1} = lists:foldl(
        fun (?shop_modification(ID, {creation, ShopParams}), {Shops, P}) ->
                {[hg_party:create_shop(ID, ShopParams, Timestamp) | Shops], P};
            (?shop_modification(ID, {shop_account_creation, _}), {Shops, P}) ->
                {lists:keydelete(ID, #domain_Shop.id, Shops), P};
            (?contract_modification(ID, {creation, Params}), {Shops, P}) ->
                Contract = hg_party:create_contract(ID, Params, Timestamp, Revision),
                {Shops, hg_party:set_new_contract(Contract, Timestamp, P)};
            (_, Acc) ->
                Acc
        end,
        {[], Party},
        Changeset
    ),
    [create_shop_account_change(Shop, Party1, Timestamp, Revision) || Shop <- CreatedShops].

create_shop_account_change(Shop, Party, Timestamp, Revision) ->
    Currency = try
        hg_party:get_new_shop_currency(Shop, Party, Timestamp, Revision)
    catch
        throw:{contract_not_exists, ContractID} ->
            raise_invalid_changeset({contract_not_exists, ContractID})
    end,
    ?shop_modification(
        hg_party:get_shop_id(Shop),
        {shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}
    ).

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
    lists:map(
        fun(Change) ->
            make_change_effect(Change, Timestamp, Revision)
        end,
        Changeset
    ).

make_change_effect(?contract_modification(ID, Modification), Timestamp, Revision) ->
    ?contract_effect(ID, make_contract_modification_effect(ID, Modification, Timestamp, Revision));

make_change_effect(?shop_modification(ID, Modification), Timestamp, _Revision) ->
    ?shop_effect(ID, make_shop_modification_effect(ID, Modification, Timestamp)).

make_contract_modification_effect(ID, {creation, ContractParams}, Timestamp, Revision) ->
    {created, hg_party:create_contract(ID, ContractParams, Timestamp, Revision)};
make_contract_modification_effect(_, ?contract_termination(TerminatedAt), _, _) ->
    {status_changed, {terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}};
make_contract_modification_effect(_, ?adjustment_creation(AdjustmentID, Params), Timestamp, Revision) ->
    {adjustment_created, hg_party:create_contract_adjustment(AdjustmentID, Params, Timestamp, Revision)};
make_contract_modification_effect(_, ?payout_tool_creation(PayoutToolID, Params), Timestamp, _) ->
    {payout_tool_created, hg_party:create_payout_tool(PayoutToolID, Params, Timestamp)};
make_contract_modification_effect(_, {legal_agreement_binding, LegalAgreement}, _, _) ->
    {legal_agreement_bound, LegalAgreement}.

make_shop_modification_effect(ID, {creation, ShopParams}, Timestamp) ->
    {created, hg_party:create_shop(ID, ShopParams, Timestamp)};
make_shop_modification_effect(_, {category_modification, Category}, _) ->
    {category_changed, Category};
make_shop_modification_effect(_, {details_modification, Details}, _) ->
    {details_changed, Details};
make_shop_modification_effect(_, ?shop_contract_modification(ContractID, PayoutToolID), _) ->
    {contract_changed, #payproc_ShopContractChanged{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }};
make_shop_modification_effect(_, {payout_tool_modification, PayoutToolID}, _) ->
    {payout_tool_changed, PayoutToolID};
make_shop_modification_effect(_, ?proxy_modification(Proxy), _) ->
    {proxy_changed, #payproc_ShopProxyChanged{proxy = Proxy}};
make_shop_modification_effect(_, {location_modification, Location}, _) ->
    {location_changed, Location};
make_shop_modification_effect(_, {shop_account_creation, Params}, _) ->
    {account_created, create_shop_account(Params)}.

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
    lists:map(
        fun(Change) ->
            make_change_safe_effect(Change, Timestamp, Revision)
        end,
        Changeset
    ).

make_change_safe_effect(
    ?shop_modification(ID, {shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}),
    _Timestamp,
    _Revision
) ->
    ?shop_effect(ID,
        {account_created, #domain_ShopAccount{
            currency = Currency,
            settlement = 42,
            guarantee = 42,
            payout = 42
        }}
    );

make_change_safe_effect(Change, Timestamp, Revision) ->
    make_change_effect(Change, Timestamp, Revision).

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
apply_contract_effect(ID, {status_changed, Status}, _, Party) ->
    hg_party:set_contract_status(ID, Status, Party);
apply_contract_effect(ID, {adjustment_created, Adjustment}, _, Party) ->
    hg_party:set_contract_adjustment(ID, Adjustment, Party);
apply_contract_effect(ID, {payout_tool_created, PayoutTool}, _, Party) ->
    hg_party:set_contract_payout_tool(ID, PayoutTool, Party);
apply_contract_effect(ID, {legal_agreement_bound, LegalAgreement}, _, Party) ->
    hg_party:set_contract_legal_agreement(ID, LegalAgreement, Party).

apply_shop_effect(_, {created, Shop}, Party) ->
    hg_party:set_new_shop(Shop, Party);
apply_shop_effect(ID, {category_changed, Category}, Party) ->
    hg_party:set_shop_category(ID, Category, Party);
apply_shop_effect(ID, {details_changed, Details}, Party) ->
    hg_party:set_shop_details(ID, Details, Party);
apply_shop_effect(
    ID,
    {contract_changed, #payproc_ShopContractChanged{contract_id = ContractID, payout_tool_id = PayoutToolID}},
    Party
) ->
    hg_party:set_shop_contract(ID, ContractID, PayoutToolID, Party);
apply_shop_effect(ID, {payout_tool_changed, PayoutToolID}, Party) ->
    hg_party:set_shop_payout_tool(ID, PayoutToolID, Party);
apply_shop_effect(ID, {location_changed, Location}, Party) ->
    hg_party:set_shop_location(ID, Location, Party);
apply_shop_effect(ID, {proxy_changed, #payproc_ShopProxyChanged{proxy = Proxy}}, Party) ->
    hg_party:set_shop_proxy(ID, Proxy, Party);
apply_shop_effect(ID, {account_created, Account}, Party) ->
    hg_party:set_shop_account(ID, Account, Party).

-spec raise_invalid_changeset(dmsl_payment_processing_thrift:'InvalidChangesetReason'()) ->
    no_return().

raise_invalid_changeset(Reason) ->
    throw(#payproc_InvalidChangeset{reason = Reason}).

-spec raise_invalid_request(binary()) ->
    no_return().

raise_invalid_request(Error) ->
    throw(#'InvalidRequest'{errors = [Error]}).

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
            ok = assert_contract_change_applicable(ID, Modification, Timestamp, Revision, Contract);
        ?shop_modification(ID, Modification) ->
            Shop = hg_party:get_shop(ID, Party),
            ok = assert_shop_change_applicable(ID, Modification, Shop)
    end,
    Effect = make_change_safe_effect(Change, Timestamp, Revision),
    assert_changeset_applicable(Others, Timestamp, Revision, apply_claim_effect(Effect, Timestamp, Party));
assert_changeset_applicable([], _, _, _) ->
    ok.

assert_contract_change_applicable(_, {creation, _}, _, _, undefined) ->
    ok;
assert_contract_change_applicable(ID, {creation, _}, _, _, #domain_Contract{}) ->
    raise_invalid_changeset({contract_already_exists, ID});
assert_contract_change_applicable(ID, _AnyModification, _, _, undefined) ->
    raise_invalid_changeset({contract_not_exists, ID});
assert_contract_change_applicable(ID, ?contract_termination(_), _, _, Contract) ->
    case hg_party:is_contract_active(Contract) of
        true ->
            ok;
        false ->
            raise_invalid_changeset({contract_status_invalid, #payproc_ContractStatusInvalid{
                contract_id = ID,
                status = hg_party:get_contract_status(Contract)
            }})
    end;
assert_contract_change_applicable(_, ?adjustment_creation(AdjustmentID, _), _, _, Contract) ->
    case hg_party:get_contract_adjustment(AdjustmentID, Contract) of
        undefined ->
            ok;
        _ ->
            raise_invalid_changeset({contract_adjustment_already_exists, AdjustmentID})
    end;
assert_contract_change_applicable(_, ?payout_tool_creation(PayoutToolID, _), Timestamp, Revision, Contract) ->
    ok = assert_contract_live(Contract, Timestamp, Revision),
    case hg_party:get_contract_payout_tool(PayoutToolID, Contract) of
        undefined ->
            ok;
        _ ->
            raise_invalid_changeset({payout_tool_already_exists, PayoutToolID})
    end;
assert_contract_change_applicable(_, _, _, _, _) ->
    ok.

assert_shop_change_applicable(_, {creation, _}, undefined) ->
    ok;
assert_shop_change_applicable(ID, {creation, _}, #domain_Shop{}) ->
    raise_invalid_changeset({shop_already_exists, ID});
assert_shop_change_applicable(ID, _AnyModification, undefined) ->
    raise_invalid_changeset({shop_not_exists, ID});
assert_shop_change_applicable(_, _, _) ->
    ok.

assert_changeset_acceptable(Changeset, Timestamp, Revision, Party0) ->
    Effects = make_changeset_safe_effects(Changeset, Timestamp, Revision),
    Party = apply_effects(Effects, Timestamp, Party0),
    hg_party:assert_party_objects_valid(Timestamp, Revision, Party).

assert_contract_live(Contract, Timestamp, Revision) ->
    case hg_party:is_test_contract(Contract, Timestamp, Revision) of
        true ->
            raise_invalid_request(<<"creating payout tool for test contract unavailable">>);
        false ->
            ok
    end.
