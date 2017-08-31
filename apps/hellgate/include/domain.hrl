-ifndef(__hellgate_domain__).
-define(__hellgate_domain__, 42).

-define(currency(SymCode),
    #domain_CurrencyRef{symbolic_code = SymCode}).
-define(cash(Amount, SymCode),
    #domain_Cash{amount = Amount, currency = ?currency(SymCode)}).

-define(route(ProviderRef, TerminalRef),
    #domain_InvoicePaymentRoute{
        provider = ProviderRef,
        terminal = TerminalRef
    }).

-define(external_failure(Code),
    ?external_failure(Code, undefined)).
-define(external_failure(Code, Description),
    {external_failure, #domain_ExternalFailure{code = Code, description = Description}}).

-define(operation_timeout(),
    {operation_timeout, #domain_OperationTimeout{}}).

-define(invoice_payment_flow_instant(),
    {instant, #domain_InvoicePaymentFlowInstant{}}).

-define(invoice_payment_flow_hold(OnHoldExpiration, HeldUntil),
    {hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}).

-define(hold_lifetime(HoldLifetime),
    #domain_HoldLifetime{seconds = HoldLifetime}).

-endif.
