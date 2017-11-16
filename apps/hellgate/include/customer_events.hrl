-ifndef(__hellgate_customer_events__).
-define(__hellgate_customer_events__, 42).

%%
%% Customers
%%

% Events

-define(customer_event(CustomerChanges), {customer_changes, CustomerChanges}).

-define(customer_created(CustomerID, OwnerID, ShopID, Metadata, ContactInfo, CreatedAt),
    {customer_created,
        #payproc_CustomerCreated{
            customer_id  = CustomerID,
            owner_id     = OwnerID,
            shop_id      = ShopID,
            metadata     = Metadata,
            contact_info = ContactInfo,
            created_at   = CreatedAt
        }}).

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
    {started,
        #payproc_CustomerBindingStarted{binding = CustomerBinding}}).

-define(customer_binding_status_changed(CustomerBindingStatus),
    {status_changed,
        #payproc_CustomerBindingStatusChanged{status = CustomerBindingStatus}}).

-define(customer_binding_interaction_requested(UserInteraction),
    {interaction_requested,
        #payproc_CustomerBindingInteractionRequested{interaction = UserInteraction}}).

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

% Exceptions

-define(invalid_party_status(Status),
    #payproc_InvalidPartyStatus{status = Status}).

-define(invalid_shop_status(Status),
    #payproc_InvalidShopStatus{status = Status}).

-define(invalid_customer_status(Status),
    #payproc_InvalidCustomerStatus{status = Status}).

-endif.
