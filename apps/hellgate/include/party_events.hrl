-ifndef(__hellgate_invoice_events__).
-define(__hellgate_invoice_events__, 42).

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

-define(pending(),
    {pending, #payproc_ClaimPending{}}).
-define(accepted(Revision),
    {approved, #payproc_ClaimAccepted{revision = Revision}}).
-define(denied(Reason),
    {denied, #payproc_ClaimDenied{reason = Reason}}).

-define(suspended(),
    {suspended, #domain_Suspended{}}).
-define(active(),
    {active, #domain_Active{}}).
-define(blocked(Reason),
    {blocked, #domain_Blocked{reason = Reason}}).
-define(unblocked(Reason),
    {unblocked, #domain_Unblocked{reason = Reason}}).

-endif.
