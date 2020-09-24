-ifndef(__hellgate_legacy_structures__).
-define(__hellgate_legacy_structures__, 42).

%% Invoice

-define(legacy_invoice_created(Invoice),
    {invoice_created, {payproc_InvoiceCreated, Invoice}}).
-define(legacy_invoice_status_changed(Status),
    {invoice_status_changed, {payproc_InvoiceStatusChanged, Status}}).
-define(legacy_payment_ev(PaymentID, Payload),
    {invoice_payment_change, {payproc_InvoicePaymentChange, PaymentID, Payload}}).

-define(legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cost, Context),
    {domain_Invoice, ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cost, Context}).
-define(legacy_invoice(ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cost, Context, TemplateID),
    {domain_Invoice, ID, PartyID, ShopID, CreatedAt, Status, Details, Due, Cost, Context, TemplateID}).

-define(legacy_invoice_paid(),
    {paid, {domain_InvoicePaid}}).
-define(legacy_invoice_unpaid(),
    {unpaid, {domain_InvoiceUnpaid}}).
-define(legacy_invoice_cancelled(Reason),
    {cancelled, {domain_InvoiceCancelled, Reason}}).
-define(legacy_invoice_fulfilled(Reason),
    {fulfilled, {domain_InvoiceFulfilled, Reason}}).

-define(legacy_invoice_details(Product, Description),
    {domain_InvoiceDetails, Product, Description}).

%% Invoice template

-define(legacy_invoice_tpl_created(InvoiceTpl),
    {invoice_template_created, {payproc_InvoiceTemplateCreated, InvoiceTpl}}
).

-define(legacy_invoice_tpl_updated(Diff),
    {invoice_template_updated, {payproc_InvoiceTemplateUpdated, Diff}}
).

-define(legacy_invoice_tpl_deleted(),
    {invoice_template_deleted, {payproc_InvoiceTemplateDeleted}}
).

%% Payment

-define(legacy_payment_started(Payment, RiskScore, Route, Cashflow),
    {invoice_payment_started,
        {payproc_InvoicePaymentStarted, Payment, RiskScore, Route, Cashflow}}).
-define(legacy_payment_status_changed(Status),
    {invoice_payment_status_changed,
        {payproc_InvoicePaymentStatusChanged, Status}}).
-define(legacy_session_ev(Target, Payload),
    {invoice_payment_session_change,
        {payproc_InvoicePaymentSessionChange, Target, Payload}}).
-define(legacy_adjustment_ev(AdjustmentID, Payload),
    {invoice_payment_adjustment_change,
        {payproc_InvoicePaymentAdjustmentChange, AdjustmentID, Payload}}).

-define(legacy_payment(ID, CreatedAt, DomainRevision, Status, Payer, Cash, Context),
    {domain_InvoicePayment, ID, CreatedAt, DomainRevision, Status, Payer, Cash, Context}).

-define(legacy_pending(),
    {pending, {domain_InvoicePaymentPending}}).
-define(legacy_processed(),
    {processed, {domain_InvoicePaymentProcessed}}).
-define(legacy_failed(Failure),
    {failed, {domain_InvoicePaymentFailed, Failure}}).
-define(legacy_captured(),
    {captured, {domain_InvoicePaymentCaptured}}).
-define(legacy_cancelled(),
    {cancelled, {domain_InvoicePaymentCancelled}}).
-define(legacy_captured(Reason),
    {captured, {domain_InvoicePaymentCaptured, Reason}}).
-define(legacy_cancelled(Reason),
    {cancelled, {domain_InvoicePaymentCancelled, Reason}}).

-define(legacy_payer(PaymentTool, SessionId, ClientInfo, ContractInfo),
    {domain_Payer, PaymentTool, SessionId, ClientInfo, ContractInfo}).

-define(legacy_client_info(IpAddress, Fingerprint),
    {domain_ClientInfo, IpAddress, Fingerprint}).

-define(legacy_contract_info(PhoneNumber, Email),
    {domain_ContactInfo, PhoneNumber, Email}).

%% Sessions

-define(legacy_session_started(),
    {invoice_payment_session_started,
        {payproc_InvoicePaymentSessionStarted}}).
-define(legacy_session_finished(Result),
    {invoice_payment_session_finished,
        {payproc_InvoicePaymentSessionFinished, Result}}).
-define(legacy_session_suspended(),
    {invoice_payment_session_suspended,
        {payproc_InvoicePaymentSessionSuspended}}).
-define(legacy_session_activated(),
    {invoice_payment_session_activated,
        {payproc_InvoicePaymentSessionActivated}}).
-define(legacy_trx_bound(Trx),
    {invoice_payment_session_transaction_bound,
        {payproc_InvoicePaymentSessionTransactionBound, Trx}}).
-define(legacy_proxy_st_changed(ProxySt),
    {invoice_payment_session_proxy_state_changed,
        {payproc_InvoicePaymentSessionProxyStateChanged, ProxySt}}).
-define(legacy_interaction_requested(UserInteraction),
    {invoice_payment_session_interaction_requested,
        {payproc_InvoicePaymentSessionInteractionRequested, UserInteraction}}).

-define(legacy_session_succeeded(),
    {succeeded, {payproc_SessionSucceeded}}).
-define(legacy_session_failed(Failure),
    {failed, {payproc_SessionFailed, Failure}}).

-define(legacy_trx(ID, Timestamp, Extra),
    {domain_TransactionInfo, ID, Timestamp, Extra}).

-define(legacy_get_request(URI),
    {redirect, {get_request, {'BrowserGetRequest', URI}}}).

-define(legacy_post_request(URI, Form),
    {redirect, {post_request, {'BrowserPostRequest', URI, Form}}}).

-define(legacy_payment_terminal_reciept(SPID, DueDate),
    {payment_terminal_reciept, {'PaymentTerminalReceipt', SPID, DueDate}}).

%% Adjustments

-define(legacy_adjustment_created(Adjustment),
    {invoice_payment_adjustment_created,
        {payproc_InvoicePaymentAdjustmentCreated, Adjustment}}).
-define(legacy_adjustment_status_changed(Status),
    {invoice_payment_adjustment_status_changed,
        {payproc_InvoicePaymentAdjustmentStatusChanged, Status}}).

-define(legacy_adjustment_pending(),
    {pending, {domain_InvoicePaymentAdjustmentPending}}).
-define(legacy_adjustment_captured(At),
    {captured, {domain_InvoicePaymentAdjustmentCaptured, At}}).
-define(legacy_adjustment_cancelled(At),
    {cancelled, {domain_InvoicePaymentAdjustmentCancelled, At}}).

-define(legacy_adjustment(ID, Status, CreatedAt, Revision, Reason, NewCashFlow, OldCashFlowInverse),
    {domain_InvoicePaymentAdjustment, ID, Status, CreatedAt, Revision, Reason, NewCashFlow, OldCashFlowInverse}).

%% Other

-define(legacy_operation_timeout(),
    {operation_timeout, {domain_OperationTimeout}}).

-define(legacy_external_failure(Code, Description),
    {external_failure, {domain_ExternalFailure, Code, Description}}).

-define(legacy_final_cash_flow_posting(Code, Source, Destination, Volume, Details),
    {domain_FinalCashFlowPosting, Source, Destination, Volume, Details}).

-define(legacy_final_cash_flow_account(AccountType, AccountId),
    {domain_FinalCashFlowAccount, AccountType, AccountId}).

-define(legacy_bank_card(Token, PaymentSystem, Bin, MaskedPan),
    {bank_card, {domain_BankCard, Token, PaymentSystem, Bin, MaskedPan}}).

-define(legacy_route(Provider, Terminal),
    {domain_InvoicePaymentRoute, Provider, Terminal}).

-define(legacy_provider(ObjectID),
    {domain_ProviderRef, ObjectID}).

-define(legacy_terminal(ObjectID),
    {domain_TerminalRef, ObjectID}).

-endif.
