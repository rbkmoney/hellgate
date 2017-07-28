-ifndef(__hellgate_domain__).
-define(__hellgate_domain__, 42).

-define(cash(Amount, Currency),
    #domain_Cash{amount = Amount, currency = Currency}).
-define(currency(SymCode),
    #domain_CurrencyRef{symbolic_code = SymCode}).

-define(external_failure(Code),
    ?external_failure(Code, undefined)).
-define(external_failure(Code, Description),
    {external_failure, #domain_ExternalFailure{code = Code, description = Description}}).

-define(operation_timeout(),
    {operation_timeout, #domain_OperationTimeout{}}).

-endif.
