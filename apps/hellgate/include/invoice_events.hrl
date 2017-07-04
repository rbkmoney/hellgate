-ifndef(__hellgate_invoice_events__).
-define(__hellgate_invoice_events__, 42).

-define(invoice_ev(Body), {invoice_event, Body}).

-define(invoice_created(Invoice),
    {invoice_created,
        #payproc_InvoiceCreated{invoice = Invoice}}
).
-define(invoice_status_changed(Status),
    {invoice_status_changed,
        #payproc_InvoiceStatusChanged{status = Status}}
).

-define(payment_ev(PaymentID, Payload),
    {invoice_payment_event, #payproc_InvoicePaymentEvent{
        id = PaymentID,
        payload = Payload
    }}
).

-define(paid(),
    {paid, #domain_InvoicePaid{}}).
-define(unpaid(),
    {unpaid, #domain_InvoiceUnpaid{}}).
-define(cancelled(Reason),
    {cancelled, #domain_InvoiceCancelled{details = Reason}}).
-define(fulfilled(Reason),
    {fulfilled, #domain_InvoiceFulfilled{details = Reason}}).

-endif.
