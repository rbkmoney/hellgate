-ifndef(__hellgate_invoice_events__).
-define(__hellgate_invoice_events__, 42).

%% FIXME old names remain for simplicity, should be changes
-define(invoice_ev(InvoiceChanges), {invoice_changes, InvoiceChanges}).

-define(invoice_created(Invoice),
    {invoice_created,
        #payproc_InvoiceCreated{invoice = Invoice}}
).
-define(invoice_status_changed(Status),
    {invoice_status_changed,
        #payproc_InvoiceStatusChanged{status = Status}}
).

-define(payment_ev(PaymentID, Payload),
    {invoice_payment_change, #payproc_InvoicePaymentChange{
        id = PaymentID,
        payload = Payload
    }}
).

-define(invoice_paid(),
    {paid, #domain_InvoicePaid{}}).
-define(invoice_unpaid(),
    {unpaid, #domain_InvoiceUnpaid{}}).
-define(invoice_cancelled(Reason),
    {cancelled, #domain_InvoiceCancelled{details = Reason}}).
-define(invoice_fulfilled(Reason),
    {fulfilled, #domain_InvoiceFulfilled{details = Reason}}).

-endif.
