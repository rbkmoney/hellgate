-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(party_ev(Body), {party_event, Body}).

-define(party_created(Party, Revision),
    {party_created,
        #payproc_PartyCreated{party = #payproc_PartyState{party = Party, revision = Revision}}}
).
-define(claim_created(Claim),
    {claim_created,
        #payproc_ClaimCreated{claim = Claim}}
).
-define(claim_status_changed(ID, Status),
    {claim_status_changed,
        #payproc_ClaimStatusChanged{id = ID, status = Status}}
).

-define(shop_modification(ID, Modification),
    {shop_modification, #payproc_ShopModificationUnit{id = ID, modification = Modification}}).

-define(pending(),
    {pending, #payproc_ClaimPending{}}).
-define(accepted(Revision),
    {accepted, #payproc_ClaimAccepted{revision = Revision}}).
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
    {accounts_created, #payproc_ShopAccountSetCreated{accounts = AccountShopSet}}).

-endif.
