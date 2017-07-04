-ifndef(__hellgate_payment_events__).
-define(__hellgate_payment_events__, 42).

%% Payments

-define(payment_started(Payment),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{payment = Payment}
    }
).
-define(payment_started(Payment, Route, CashFlow),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{payment = Payment, route = Route, cash_flow = CashFlow}
    }
).
-define(payment_status_changed(Status),
    {invoice_payment_status_changed,
        #payproc_InvoicePaymentStatusChanged{status = Status}
    }
).

-define(pending(),
    {pending, #domain_InvoicePaymentPending{}}).
-define(processed(),
    {processed, #domain_InvoicePaymentProcessed{}}).
-define(captured(),
    {captured, #domain_InvoicePaymentCaptured{}}).
-define(failed(Failure),
    {failed, #domain_InvoicePaymentFailed{failure = Failure}}).

%% Sessions

-define(session_ev(Target, Payload),
    {invoice_payment_session_event, #payproc_InvoicePaymentSessionEvent{
        target = Target,
        payload = Payload
    }}
).

-define(session_started(),
    {invoice_payment_session_started,
        #payproc_InvoicePaymentSessionStarted{}
    }
).
-define(session_finished(),
    {invoice_payment_session_finished,
        #payproc_InvoicePaymentSessionFinished{}
    }
).
-define(session_suspended(),
    {invoice_payment_session_suspended,
        #payproc_InvoicePaymentSessionSuspended{}
    }
).
-define(session_activated(),
    {invoice_payment_session_activated,
        #payproc_InvoicePaymentSessionActivated{}
    }
).
-define(session_bound(Trx),
    {invoice_payment_session_transaction_bound,
        #payproc_InvoicePaymentSessionTransactionBound{trx = Trx}
    }
).
-define(session_proxy_st_changed(ProxySt),
    {invoice_payment_session_proxy_state_changed,
        #payproc_InvoicePaymentSessionProxyStateChanged{proxy_state = ProxySt}
    }
).
-define(interaction_requested(UserInteraction),
    {invoice_payment_session_interaction_requested,
        #payproc_InvoicePaymentSessionInteractionRequested{interaction = UserInteraction}
    }
).

%% Adjustments

-define(adjustment_ev(AdjustmentID, Payload),
    {invoice_payment_adjustment_event, #payproc_InvoicePaymentAdjustmentEvent{
        id = AdjustmentID,
        payload = Payload
    }}
).

-define(adjustment_created(Adjustment),
    {invoice_payment_adjustment_created,
        #payproc_InvoicePaymentAdjustmentCreated{adjustment = Adjustment}
    }
).

-define(adjustment_status_changed(Status),
    {invoice_payment_adjustment_status_changed,
        #payproc_InvoicePaymentAdjustmentStatusChanged{status = Status}
    }
).

-define(adjustment_pending(),
    {pending, #domain_InvoicePaymentAdjustmentPending{}}).
-define(adjustment_captured(At),
    {captured, #domain_InvoicePaymentAdjustmentCaptured{at = At}}).
-define(adjustment_cancelled(At),
    {cancelled, #domain_InvoicePaymentAdjustmentCancelled{at = At}}).

-endif.
