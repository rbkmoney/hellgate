-ifndef(__hellgate_legacy_party_structures__).
-define(__hellgate_legacy_party_structures__, 42).

-define(legacy_party_created(Party),
    {party_created, {payproc_PartyCreated, Party}}).

-define(legacy_party(ID, ContactInfo, CreatedAt, Blocking, Suspension, Contracts, Shops),
    {domain_Party,
        ID,
        ContactInfo,
        CreatedAt,
        Blocking,
        Suspension,
        Contracts,
        Shops
    }).

-endif.
