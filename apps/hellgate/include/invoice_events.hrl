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

-define(pending(),
    {pending, #domain_InvoicePaymentPending{}}).
-define(processed(),
    {processed, #domain_InvoicePaymentProcessed{}}).
-define(captured(),
    {captured, #domain_InvoicePaymentCaptured{}}).
-define(failed(Failure),
    {failed, #domain_InvoicePaymentFailed{failure = Failure}}).

-define(INVOICE_TPL_VIOLATED, "Template violation: ").
-define(INVOICE_TPL_NO_COST, <<?INVOICE_TPL_VIOLATED "missing invoice cost">>).
-define(INVOICE_TPL_BAD_COST, <<?INVOICE_TPL_VIOLATED "cost mismatch">>).
-define(INVOICE_TPL_BAD_CURRENCY, <<?INVOICE_TPL_VIOLATED "invalid currency">>).
-define(INVOICE_TPL_BAD_AMOUNT, <<?INVOICE_TPL_VIOLATED "invalid amount">>).
-endif.
