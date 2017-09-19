-ifndef(__hellgate_customer_events__).
-define(__hellgate_customer_events__, 42).

%%
%% Customers
%%

% Events

-define(customer_event(CustomerChanges), {customer_changes, CustomerChanges}).

-define(customer_created(Customer),
    {customer_created,
        #payproc_CustomerCreated{customer = Customer}}).

-define(customer_deleted(),
    {customer_deleted,
        #payproc_CustomerDeleted{}}).

-define(customer_status_changed(Status),
    {customer_status_changed,
        #payproc_CustomerStatusChanged{status = Status}}).

-define(customer_binding_changed(CustomerBindingID, Payload),
    {customer_binding_changed,
        #payproc_CustomerBindingChanged{id = CustomerBindingID, payload = Payload}}).

-define(customer_binding_started(CustomerBinding),
    {customer_binding_started,
        #payproc_CustomerBindingStarted{binding = CustomerBinding}}).

-define(customer_binding_status_changed(CustomerBindingStatus),
    {customer_binding_status_changed,
        #payproc_CustomerBindingStatusChanged{status = CustomerBindingStatus}}).

% Statuses

-define(customer_unready(),
    {unready,
        #payproc_CustomerUnready{}}).

-define(customer_ready(),
    {ready,
        #payproc_CustomerReady{}}).

-define(customer_binding_pending(),
    {pending,
        #payproc_CustomerBindingPending{}}).

-define(customer_binding_succeeded(),
    {succeeded,
        #payproc_CustomerBindingSucceeded{}}).

-define(customer_binding_failed(Failure),
    {failed,
        #payproc_CustomerBindingFailed{failure = Failure}}).

-endif.
