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

-define(payment_ev(Body), {invoice_payment_event, Body}).

-define(payment_started(Payment),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{payment = Payment}}
).
-define(payment_started(Payment, Route, CashFlow),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{payment = Payment, route = Route, cash_flow = CashFlow}}
).
-define(payment_bound(PaymentID, Trx),
    {invoice_payment_bound,
        #payproc_InvoicePaymentBound{payment_id = PaymentID, trx = Trx}}
).
-define(payment_status_changed(PaymentID, Status),
    {invoice_payment_status_changed,
        #payproc_InvoicePaymentStatusChanged{payment_id = PaymentID, status = Status}}
).
-define(payment_interaction_requested(PaymentID, UserInteraction),
    {invoice_payment_interaction_requested,
        #payproc_InvoicePaymentInteractionRequested{
            payment_id = PaymentID,
            interaction = UserInteraction
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

-define(pending(),
    {pending, #domain_InvoicePaymentPending{}}).
-define(processed(),
    {processed, #domain_InvoicePaymentProcessed{}}).
-define(captured(),
    {captured, #domain_InvoicePaymentCaptured{}}).
-define(failed(Error),
    {failed, #domain_InvoicePaymentFailed{err = Error}}).

-endif.
