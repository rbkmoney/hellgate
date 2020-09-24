-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(party_ev(PartyChanges), {party_changes, PartyChanges}).

-define(party_created(PartyID, ContactInfo, Timestamp),
    {party_created, #payproc_PartyCreated{
        id = PartyID,
        contact_info = ContactInfo,
        created_at = Timestamp
    }}).

-define(shop_modification(ID, Modification),
    {shop_modification, #payproc_ShopModificationUnit{id = ID, modification = Modification}}).

-endif.
