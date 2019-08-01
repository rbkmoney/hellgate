-ifndef(__hellgate_payment_events__).
-define(__hellgate_payment_events__, 42).

%% Payments

-define(payment_started(Payment),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{
            payment = Payment,
            risk_score = undefined,
            route = undefined,
            cash_flow = undefined
        }
    }
).
-define(payment_started(Payment, RiskScore, Route, CashFlow),
    {invoice_payment_started,
        #payproc_InvoicePaymentStarted{
            payment = Payment,
            risk_score = RiskScore,
            route = Route,
            cash_flow = CashFlow
        }
    }
).
-define(risk_score_changed(RiskScore),
    {invoice_payment_risk_score_changed,
        #payproc_InvoicePaymentRiskScoreChanged{risk_score = RiskScore}
    }
).
-define(route_changed(Route),
    {invoice_payment_route_changed,
        #payproc_InvoicePaymentRouteChanged{route = Route}
    }
).
-define(cash_flow_changed(CashFlow),
    {invoice_payment_cash_flow_changed,
        #payproc_InvoicePaymentCashFlowChanged{cash_flow = CashFlow}
    }
).
-define(payment_status_changed(Status),
    {invoice_payment_status_changed,
        #payproc_InvoicePaymentStatusChanged{status = Status}
    }
).

-define(rec_token_acquired(Token),
    {invoice_payment_rec_token_acquired,
        #payproc_InvoicePaymentRecTokenAcquired{token = Token}
    }
).

-define(payment_capture_started(Params),
    {invoice_payment_capture_started,
        #payproc_InvoicePaymentCaptureStarted{params = Params}
    }
).

-define(pending(),
    {pending, #domain_InvoicePaymentPending{}}).
-define(processed(),
    {processed, #domain_InvoicePaymentProcessed{}}).
-define(cancelled(),
    {cancelled, #domain_InvoicePaymentCancelled{}}).
-define(captured(),
    {captured, #domain_InvoicePaymentCaptured{}}).
-define(refunded(),
    {refunded, #domain_InvoicePaymentRefunded{}}).
-define(failed(Failure),
    {failed, #domain_InvoicePaymentFailed{failure = Failure}}).
-define(captured_with_reason(Reason),
    {captured, #domain_InvoicePaymentCaptured{reason = Reason}}).
-define(captured(Reason, Cost),
    {captured, #domain_InvoicePaymentCaptured{reason = Reason, cost = Cost}}).
-define(captured(Reason, Cost, Cart),
    {captured, #domain_InvoicePaymentCaptured{reason = Reason, cost = Cost, cart = Cart}}).
-define(cancelled_with_reason(Reason),
    {cancelled, #domain_InvoicePaymentCancelled{reason = Reason}}).

%% Sessions

-define(session_ev(Target, Payload),
    {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
        target = Target,
        payload = Payload
    }}
).

-define(session_started(),
    {session_started,
        #payproc_SessionStarted{}
    }
).
-define(session_finished(Result),
    {session_finished,
        #payproc_SessionFinished{result = Result}
    }
).
-define(session_suspended(Tag),
    {session_suspended,
        #payproc_SessionSuspended{tag = Tag}
    }
).
-define(session_activated(),
    {session_activated,
        #payproc_SessionActivated{}
    }
).
-define(trx_bound(Trx),
    {session_transaction_bound,
        #payproc_SessionTransactionBound{trx = Trx}
    }
).
-define(proxy_st_changed(ProxySt),
    {session_proxy_state_changed,
        #payproc_SessionProxyStateChanged{proxy_state = ProxySt}
    }
).
-define(interaction_requested(UserInteraction),
    {session_interaction_requested,
        #payproc_SessionInteractionRequested{interaction = UserInteraction}
    }
).

-define(session_succeeded(),
    {succeeded, #payproc_SessionSucceeded{}}
).
-define(session_failed(Failure),
    {failed, #payproc_SessionFailed{failure = Failure}}
).

%% Adjustments

-define(adjustment_ev(AdjustmentID, Payload),
    {invoice_payment_adjustment_change, #payproc_InvoicePaymentAdjustmentChange{
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
-define(adjustment_processed(),
    {processed, #domain_InvoicePaymentAdjustmentProcessed{}}).
-define(adjustment_captured(At),
    {captured, #domain_InvoicePaymentAdjustmentCaptured{at = At}}).
-define(adjustment_cancelled(At),
    {cancelled, #domain_InvoicePaymentAdjustmentCancelled{at = At}}).

%% Refunds

-define(refund_ev(RefundID, Payload),
    {invoice_payment_refund_change, #payproc_InvoicePaymentRefundChange{
        id = RefundID,
        payload = Payload
    }}
).

-define(refund_created(Refund, CashFlow),
    ?refund_created(Refund, CashFlow, undefined)
).

-define(refund_created(Refund, CashFlow, TrxInfo),
    {invoice_payment_refund_created,
        #payproc_InvoicePaymentRefundCreated{
            refund = Refund,
            cash_flow = CashFlow,
            transaction_info = TrxInfo
        }
    }
).

-define(refund_status_changed(Status),
    {invoice_payment_refund_status_changed,
        #payproc_InvoicePaymentRefundStatusChanged{status = Status}
    }
).

-define(refund_pending(),
    {pending, #domain_InvoicePaymentRefundPending{}}).
-define(refund_succeeded(),
    {succeeded, #domain_InvoicePaymentRefundSucceeded{}}).
-define(refund_failed(Failure),
    {failed, #domain_InvoicePaymentRefundFailed{failure = Failure}}).

-endif.
