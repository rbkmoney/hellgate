-ifndef(__hellgate_domain__).
-define(__hellgate_domain__, 42).

-define(currency(SymCode),
    #domain_CurrencyRef{symbolic_code = SymCode}).

-define(cash(Amount, SymCode),
    #domain_Cash{amount = Amount, currency = ?currency(SymCode)}).

-define(route(ProviderRef, TerminalRef),
    #domain_PaymentRoute{
        provider = ProviderRef,
        terminal = TerminalRef
    }).

-define(failure(Code),
    ?failure(Code, undefined)).
-define(failure(Code, Reason),
    {failure, #domain_Failure{code = Code, reason = Reason}}).

-define(operation_timeout(),
    {operation_timeout, #domain_OperationTimeout{}}).

-define(invoice_payment_flow_instant(),
    {instant, #domain_InvoicePaymentFlowInstant{}}).

-define(invoice_payment_flow_hold(OnHoldExpiration, HeldUntil),
    {hold, #domain_InvoicePaymentFlowHold{on_hold_expiration = OnHoldExpiration, held_until = HeldUntil}}).

-define(hold_lifetime(HoldLifetime),
    #domain_HoldLifetime{seconds = HoldLifetime}).

-define(payment_resource_payer(Resource, ContactInfo),
    {payment_resource, #domain_PaymentResourcePayer{resource = Resource, contact_info = ContactInfo}}).

-define(customer_payer(CustomerID, CustomerBindingID, RecurrentPaytoolID, PaymentTool, ContactInfo),
    {customer, #domain_CustomerPayer{
        customer_id = CustomerID,
        customer_binding_id = CustomerBindingID,
        rec_payment_tool_id = RecurrentPaytoolID,
        payment_tool = PaymentTool,
        contact_info = ContactInfo
    }}
).

-define(recurrent_intention(TokenSource),
    #domain_RecurrentIntention{token_source = TokenSource}).

-define(payment_token_source(InvoiceID, PaymentID),
    {payment, #domain_PaymentRecurrentTokenSource{invoice_id = InvoiceID, payment_id = PaymentID}}).

-endif.
