-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(party_ev(Body), {party_event, Body}).

-define(party_created(Party), {party_created, Party}).
-define(claim_created(Claim),
    {claim_created, Claim}
).
-define(claim_status_changed(ID, Status),
    {claim_status_changed,
        #payproc_ClaimStatusChanged{id = ID, status = Status}}
).

-define(contract_creation(Contract),
    {contract_creation, Contract}).

-define(contract_termination(ID, TerminatedAt, Reason), 
    {contract_modification, #payproc_ContractModificationUnit{
        id = ID,
        modification = {termination, #payproc_ContractTermination{terminated_at = TerminatedAt, reason = Reason}}
    }}).

-define(contract_adjustment_creation(ID, Adjustment), 
    {contract_modification, #payproc_ContractModificationUnit{ id = ID, modification = {adjustment_creation, Adjustment}}}).

-define(payout_account_creation(PayoutAccount),
    {payout_account_creation, PayoutAccount}).

-define(shop_creation(Shop),
    {shop_creation, Shop}).
-define(shop_modification(ID, Modification),
    {shop_modification, #payproc_ShopModificationUnit{id = ID, modification = Modification}}).

-define(pending(),
    {pending, #payproc_ClaimPending{}}).
-define(accepted(AcceptedAt),
    {accepted, #payproc_ClaimAccepted{accepted_at = AcceptedAt}}).
-define(denied(Reason),
    {denied, #payproc_ClaimDenied{reason = Reason}}).
-define(revoked(Reason),
    {revoked, #payproc_ClaimRevoked{reason = Reason}}).

-define(suspended(),
    {suspended, #domain_Suspended{}}).
-define(active(),
    {active, #domain_Active{}}).
-define(blocked(Reason),
    {blocked, #domain_Blocked{reason = Reason}}).
-define(unblocked(Reason),
    {unblocked, #domain_Unblocked{reason = Reason}}).

-define(accounts_created(ShopAccountSet),
    {accounts_created, #payproc_ShopAccountSetCreated{accounts = ShopAccountSet}}).

-endif.
