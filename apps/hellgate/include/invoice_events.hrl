-ifndef(__hellgate_invoice_events__).
-define(__hellgate_invoice_events__, 42).

%% FIXME old names remain for simplicity, should be changes
%% FIXME also [InvoiceChanges] remain for simplicity, cuz we don't have multi-payload events for now
-define(invoice_ev(InvoiceChanges), {invoice_changes, [InvoiceChanges]}).

-define(invoice_created(Invoice),
    {invoice_created,
        #payproc_InvoiceCreated{invoice = Invoice}}
).
-define(invoice_status_changed(Status),
    {invoice_status_changed,
        #payproc_InvoiceStatusChanged{status = Status}}
).

-define(payment_ev(Body), {invoice_payment_change, Body}).
-define(adjustment_ev(Body), {invoice_payment_adjustment_change, Body}).

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
-define(payment_inspected(PaymentID, RiskScore),
    {invoice_payment_inspected,
        #payproc_InvoicePaymentInspected{
            payment_id = PaymentID,
            risk_score = RiskScore
        }}
).

-define(adjustment_created(PaymentID, Adjustment),
    {invoice_payment_adjustment_created,
        #payproc_InvoicePaymentAdjustmentCreated{
            payment_id = PaymentID,
            adjustment = Adjustment
        }
    }
).

-define(adjustment_status_changed(PaymentID, AdjustmentID, Status),
    {invoice_payment_adjustment_status_changed,
        #payproc_InvoicePaymentAdjustmentStatusChanged{
            payment_id = PaymentID,
            adjustment_id = AdjustmentID,
            status = Status
        }
    }
).

-define(adjustment_pending(),
    {pending, #domain_InvoicePaymentAdjustmentPending{}}).
-define(adjustment_captured(At),
    {captured, #domain_InvoicePaymentAdjustmentCaptured{at = At}}).
-define(adjustment_cancelled(At),
    {cancelled, #domain_InvoicePaymentAdjustmentCancelled{at = At}}).

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
-define(cancelled(),
    {cancelled, #domain_InvoicePaymentCancelled{}}).
-define(failed(Failure),
    {failed, #domain_InvoicePaymentFailed{failure = Failure}}).

-endif.
