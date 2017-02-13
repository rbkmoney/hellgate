-ifndef(__hellgate_domain__).
-define(__hellgate_domain__, 42).

-define(cash(Amount, Currency),
    #domain_Cash{amount = Amount, currency = Currency}).

-endif.
