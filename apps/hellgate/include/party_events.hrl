-ifndef(__hellgate_party_events__).
-define(__hellgate_party_events__, 42).

-define(shop_modification(ID, Modification),
    {shop_modification, #payproc_ShopModificationUnit{id = ID, modification = Modification}}).

-endif.
