-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(party_ev(Body), {party_event, Body}).

-define(party_created(Party), {party_created, Party}).
-define(party_blocking(Blocking), {party_blocking, Blocking}).
-define(party_suspension(Suspension), {party_suspension, Suspension}).

-define(shop_blocking(ID, Blocking),
    {shop_blocking, #payproc_ShopBlocking{shop_id = ID, blocking = Blocking}}).
-define(shop_suspension(ID, Suspension),
    {shop_suspension, #payproc_ShopSuspension{shop_id = ID, suspension = Suspension}}).

-define(blocked(Reason), #domain_Blocked{reason = Reason}).
-define(unblocked(Reason), #domain_Unblocked{reason = Reason}).

-define(active(), #domain_Active{}).
-define(suspended(), #domain_Suspended{}).

-define(contract_modification(ID, Modification),
    {contract_modification, #payproc_ContractModificationUnit{id = ID, modification = Modification}}).

-define(contract_termination(TerminatedAt),
    {termination, #payproc_ContractTermination{terminated_at = TerminatedAt}}).

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

-define(contract_effect(ID, Effect),
    {contract_effect, #payproc_ContractEffectUnit{contract_id = ID, effect = Effect}}).

-define(shop_effect(ID, Effect),
    {shop_effect, #payproc_ShopEffectUnit{shop_id = ID, effect = Effect}}).

-define(claim_created(Claim),
    {claim_created, Claim}).

-define(claim_updated(ID, Changeset),
    {claim_updated, #payproc_ClaimUpdated{id = ID, changeset = Changeset}}).

-define(claim_status_changed(ID, Status),
    {claim_status_changed, #payproc_ClaimStatusChanged{id = ID, status = Status}}).

-define(pending(),
    {pending, #payproc_ClaimPending{}}).
-define(accepted(AcceptedAt, Effects),
    {accepted, #payproc_ClaimAccepted{accepted_at = AcceptedAt, effects = Effects}}).
-define(denied(Reason),
    {denied, #payproc_ClaimDenied{reason = Reason}}).
-define(revoked(Reason),
    {revoked, #payproc_ClaimRevoked{reason = Reason}}).

-define(account_created(ShopAccount),
    {account_created, #payproc_ShopAccountCreated{account = ShopAccount}}).

-endif.
