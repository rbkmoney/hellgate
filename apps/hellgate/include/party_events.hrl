-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(party_ev(PartyChanges), {party_changes, PartyChanges}).

-define(party_created(PartyID, ContactInfo, Timestamp),
    {party_created, #payproc_PartyCreated{
        id = PartyID,
        contact_info = ContactInfo,
        created_at = Timestamp
    }}).

-define(party_blocking(Blocking), {party_blocking, Blocking}).
-define(party_suspension(Suspension), {party_suspension, Suspension}).

-define(party_meta_set(NS, Data),
    {party_meta_set, #payproc_PartyMetaSet{
        ns = NS,
        data = Data
    }}).

-define(party_meta_removed(NS), {party_meta_removed, NS}).

-define(shop_blocking(ID, Blocking),
    {shop_blocking, #payproc_ShopBlocking{shop_id = ID, blocking = Blocking}}).
-define(shop_suspension(ID, Suspension),
    {shop_suspension, #payproc_ShopSuspension{shop_id = ID, suspension = Suspension}}).

-define(blocked(Reason, Since), {blocked, #domain_Blocked{reason = Reason, since = Since}}).
-define(unblocked(Reason, Since), {unblocked, #domain_Unblocked{reason = Reason, since = Since}}).
-define(unblocked(Since), {unblocked, #domain_Unblocked{reason = <<"">>, since = Since}}).

-define(active(Since), {active, #domain_Active{since = Since}}).
-define(suspended(Since), {suspended, #domain_Suspended{since = Since}}).

-define(contract_modification(ID, Modification),
    {contract_modification, #payproc_ContractModificationUnit{id = ID, modification = Modification}}).

-define(contract_termination(Reason),
    {termination, #payproc_ContractTermination{reason = Reason}}).

-define(adjustment_creation(ID, Params),
    {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
        adjustment_id = ID,
        modification = {creation, Params}
    }}).

-define(payout_tool_creation(ID, Params),
    {payout_tool_modification, #payproc_PayoutToolModificationUnit{
        payout_tool_id = ID,
        modification = {creation, Params}
    }}).

-define(shop_modification(ID, Modification),
    {shop_modification, #payproc_ShopModificationUnit{id = ID, modification = Modification}}).

-define(shop_contract_modification(ContractID, PayoutToolID),
    {contract_modification, #payproc_ShopContractModification{contract_id = ContractID, payout_tool_id = PayoutToolID}}).

-define(proxy_modification(Proxy),
    {proxy_modification, #payproc_ProxyModification{proxy = Proxy}}).

-define(payout_schedule_modification(BusinessScheduleRef),
    {payout_schedule_modification, #payproc_ScheduleModification{schedule = BusinessScheduleRef}}).

-define(contract_effect(ID, Effect),
    {contract_effect, #payproc_ContractEffectUnit{contract_id = ID, effect = Effect}}).

-define(shop_effect(ID, Effect),
    {shop_effect, #payproc_ShopEffectUnit{shop_id = ID, effect = Effect}}).

-define(payout_schedule_changed(BusinessScheduleRef),
    {payout_schedule_changed, #payproc_ScheduleChanged{schedule = BusinessScheduleRef}}).

-define(claim_created(Claim),
    {claim_created, Claim}).

-define(claim_updated(ID, Changeset, ClaimRevision, Timestamp),
    {claim_updated, #payproc_ClaimUpdated{id = ID, changeset = Changeset, revision = ClaimRevision, updated_at = Timestamp}}).

-define(claim_status_changed(ID, Status, ClaimRevision, Timestamp),
    {claim_status_changed, #payproc_ClaimStatusChanged{id = ID, status = Status, revision = ClaimRevision, changed_at = Timestamp}}).

-define(pending(),
    {pending, #payproc_ClaimPending{}}).
-define(accepted(Effects),
    {accepted, #payproc_ClaimAccepted{effects = Effects}}).
-define(denied(Reason),
    {denied, #payproc_ClaimDenied{reason = Reason}}).
-define(revoked(Reason),
    {revoked, #payproc_ClaimRevoked{reason = Reason}}).

-define(account_created(ShopAccount),
    {account_created, #payproc_ShopAccountCreated{account = ShopAccount}}).

-define(revision_changed(Timestamp, Revision),
    {revision_changed, #payproc_PartyRevisionChanged{
        timestamp = Timestamp,
        revision = Revision
    }}).

-define(invalid_shop(ID, Reason),
    {invalid_shop, #payproc_InvalidShop{id = ID, reason = Reason}}).

-define(invalid_contract(ID, Reason),
    {invalid_contract, #payproc_InvalidContract{id = ID, reason = Reason}}).

-endif.
