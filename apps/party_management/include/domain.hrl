-ifndef(__pm_domain_hrl__).
-define(__pm_domain_hrl__, included).

-define(currency(SymCode),
    #domain_CurrencyRef{symbolic_code = SymCode}).

-define(cash(Amount, SymCode),
    #domain_Cash{amount = Amount, currency = ?currency(SymCode)}).

-endif.
