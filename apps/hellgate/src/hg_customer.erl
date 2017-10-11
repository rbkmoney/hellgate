%%%
%%% Customer machine
%%%

-module(hg_customer).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include("customer_events.hrl").

-define(NS, <<"customer">>).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).
-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).
-export([namespace     /0]).
-export([init          /2]).
-export([process_signal/2]).
-export([process_call  /2]).

%% Event provider callbacks

-behaviour(hg_event_provider).
-export([publish_event/2]).

%% Types

-define(SYNC_INTERVAL, 1).

-record(st, {
    customer       :: undefined | customer(),
    active_binding :: undefined | binding_id()
}).

-type customer()         :: dmsl_payment_processing_thrift:'Customer'().
-type customer_id()      :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer_params()  :: dmsl_payment_processing_thrift:'CustomerParams'().
-type customer_change()  :: dmsl_payment_processing_thrift:'CustomerChange'().

-type binding_id()     :: dmsl_payment_processing_thrift:'CustomerBindingID'().
-type binding_params() :: dmsl_payment_processing_thrift:'CustomerBindingParams'().

%%
%% Woody handler
%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().
handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(customer_management,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('Create', [CustomerParams], _Opts) ->
    CustomerID = hg_utils:unique_id(),
    ok = set_meta(CustomerID),
    PartyID = CustomerParams#payproc_CustomerParams.party_id,
    ShopID = CustomerParams#payproc_CustomerParams.shop_id,
    ok = assert_party_accessible(PartyID),
    Party = get_party(PartyID),
    Shop = ensure_shop_exists(hg_party:get_shop(ShopID, Party)),
    ok = assert_party_shop_operable(Shop, Party),
    ok = start(CustomerID, CustomerParams),
    get_customer(get_state(CustomerID));

handle_function_('Get', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_accessible(St),
    get_customer(St);

handle_function_('Delete', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    call(CustomerID, delete);

handle_function_('StartBinding', [CustomerID, CustomerBindingParams], _Opts) ->
    ok = set_meta(CustomerID),
    call(CustomerID, {start_binding, CustomerBindingParams});

handle_function_('GetActiveBinding', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_accessible(St),
    case try_get_active_binding(St) of
        Binding = #payproc_CustomerBinding{} ->
            Binding;
        undefined ->
            throw(?invalid_customer_status(get_customer_status(get_customer(St))))
    end;

handle_function_('GetEvents', [CustomerID, Range], _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_accessible(get_initial_state(CustomerID)),
    get_public_history(CustomerID, Range).

%%

get_party(PartyID) ->
    hg_party_machine:get_party(PartyID).

set_meta(ID) ->
    hg_log_scope:set_meta(#{customer_id => ID}).

get_history(Ref) ->
    History = hg_machine:get_history(?NS, Ref),
    unmarshal(map_history_error(History)).

get_history(Ref, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, Ref, AfterID, Limit),
    unmarshal(map_history_error(History)).

get_state(Ref) ->
    collapse_history(get_history(Ref)).

get_initial_state(Ref) ->
    collapse_history(get_history(Ref, undefined, 1)).

get_public_history(CustomerID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_customer_event(CustomerID, Ev) || Ev <- get_history(CustomerID, AfterID, Limit)].

publish_customer_event(CustomerID, {ID, Dt, Payload}) ->
    #payproc_Event{
        id = ID,
        created_at = Dt,
        source = {customer_id, CustomerID},
        payload = ?customer_event(Payload)
    }.

-spec start(customer_id(), customer_params()) ->
    ok | no_return().
start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

-spec call(customer_id(), _Args) ->
    _Result | no_return().
call(ID, Args) ->
    map_error(hg_machine:call(?NS, ID, Args)).

-spec map_error({ok, _Result} | {error, _Error}) ->
    _Result | no_return().
map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_CustomerNotFound{});
map_error({error, Reason}) ->
    error(Reason).

-spec map_history_error({ok, _Result} | {error, _Error}) ->
    _Result | no_return().
map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_CustomerNotFound{}).

-spec map_start_error({ok, term()} | {error, _Error}) ->
    ok | no_return().
map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

%%
%% Event provider callbacks
%%

-spec publish_event(customer_id(), [customer_change()]) ->
    hg_event_provider:public_event().
publish_event(CustomerID, Changes) when is_list(Changes) ->
    {{customer_id, CustomerID}, ?customer_event(unmarshal({list, change}, Changes))}.

%%
%% hg_machine callbacks
%%

-spec namespace() ->
    hg_machine:ns().
namespace() ->
    ?NS.

-spec init(customer_id(), customer_params()) ->
    hg_machine:result().
init(CustomerID, CustomerParams) ->
    Customer = create_customer(CustomerID, CustomerParams),
    handle_result(#{
        changes => [?customer_created(Customer)]
    }).

-spec process_signal(hg_machine:signal(), hg_machine:history()) ->
    hg_machine:result().
process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal(History)))).

handle_signal(timeout, St) ->
    Changes = sync_pending_bindings(St),
    Action = case get_pending_binding_set(merge_changes(Changes, St)) of
        [_BindingID | _] ->
            set_event_poll_timer();
        [] ->
            hg_machine_action:new()
    end,
    #{
        changes => Changes,
        action  => Action
    }.

-type call() ::
    {start_binding, binding_params()} |
    delete.

-spec process_call(call(), hg_machine:history()) ->
    {hg_machine:response(), hg_machine:result()}.
process_call(Call, History) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

handle_call(delete, St) ->
    ok = assert_customer_operable(St),
    #{
        response => ok,
        changes  => [?customer_deleted()]
    };
handle_call({start_binding, BindingParams}, St) ->
    ok = assert_customer_operable(St),
    start_binding(BindingParams, St).

handle_result(Params) ->
    Action = maps:get(action, Params, hg_machine_action:new()),
    case maps:find(response, Params) of
        {ok, Response} ->
            {{ok, Response}, {handle_result_changes(Params), Action}};
        error ->
            {handle_result_changes(Params), Action}
    end.

handle_result_changes(#{changes := Changes = [_ | _]}) ->
    [marshal(Changes)];
handle_result_changes(#{}) ->
    [].

%%

-include_lib("hellgate/include/recurrent_payment_tools.hrl").

start_binding(BindingParams, St) ->
    BindingID = create_binding_id(St),
    PaymentResource = BindingParams#payproc_CustomerBindingParams.payment_resource,
    RecurrentPaytoolID = create_recurrent_paytool(PaymentResource, St),
    Binding = construct_binding(BindingID, RecurrentPaytoolID, PaymentResource),
    Changes = [?customer_binding_changed(BindingID, ?customer_binding_started(Binding))],
    #{
        response => Binding,
        changes  => Changes,
        action   => set_event_poll_timer()
    }.

construct_binding(BindingID, RecPaymentToolID, PaymentResource) ->
    #payproc_CustomerBinding{
        id                  = BindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource    = PaymentResource,
        status              = ?customer_binding_pending()
    }.

create_binding_id(St) ->
    integer_to_binary(length(get_bindings(get_customer(St))) + 1).

sync_pending_bindings(St) ->
    sync_pending_bindings(get_pending_binding_set(St), St).

sync_pending_bindings([BindingID | Rest], St) ->
    Binding = try_get_binding(BindingID, get_customer(St)),
    Changes = sync_binding_state(Binding),
    Changes ++ sync_pending_bindings(Rest, St);
sync_pending_bindings([], _St) ->
    [].

sync_binding_state(Binding) ->
    RecurrentPaytoolID = get_binding_recurrent_paytool_id(Binding),
    RecurrentPaytoolChanges = get_recurrent_paytool_changes(RecurrentPaytoolID),
    BindingChanges = produce_binding_changes(RecurrentPaytoolChanges, Binding),
    wrap_binding_changes(get_binding_id(Binding), BindingChanges).

produce_binding_changes([RecurrentPaytoolChange | Rest], Binding) ->
    Changes = produce_binding_changes_(RecurrentPaytoolChange, Binding),
    Changes ++ produce_binding_changes(Rest, merge_binding_changes(Changes, Binding));
produce_binding_changes([], _Binding) ->
    [].

produce_binding_changes_(?recurrent_payment_tool_has_created(_, _, _), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [];
produce_binding_changes_(?recurrent_payment_tool_has_acquired(_), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [?customer_binding_status_changed(?customer_binding_succeeded())];
produce_binding_changes_(?recurrent_payment_tool_has_failed(Failure), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [?customer_binding_status_changed(?customer_binding_failed(Failure))];
produce_binding_changes_(?recurrent_payment_tool_has_abandoned() = Change, _Binding) ->
    error({unexpected, {'Unexpected recurrent payment tool change received', Change}});
produce_binding_changes_(?session_ev(_), _Binding) ->
    [].

create_recurrent_paytool(PaymentResource, St) ->
    create_recurrent_paytool(#payproc_RecurrentPaymentToolParams{
        party_id         = get_party_id(St),
        shop_id          = get_shop_id(St),
        payment_resource = PaymentResource
    }).

create_recurrent_paytool(Params) ->
    case issue_recurrent_paytools_call('Create', [Params]) of
        {ok, RecurrentPaytool} ->
            RecurrentPaytool#payproc_RecurrentPaymentTool.id;
        {exception, Exception = #payproc_InvalidUser{}} ->
            throw(Exception);
        {exception, Exception = #payproc_InvalidPartyStatus{}} ->
            throw(Exception);
        {exception, Exception = #payproc_InvalidShopStatus{}} ->
            throw(Exception);
        {exception, Exception = #payproc_InvalidContractStatus{}} ->
            throw(Exception);
        {exception, Exception = #payproc_OperationNotPermitted{}} ->
            throw(Exception)
    end.

get_recurrent_paytool_changes(RecurrentPaytoolID) ->
    EventRange = #payproc_EventRange{limit = 1337}, % FIXME
    {ok, Events} = issue_recurrent_paytools_call('GetEvents', [RecurrentPaytoolID, EventRange]),
    gather_recurrent_paytool_changes(Events).

gather_recurrent_paytool_changes(Events) ->
    lists:flatmap(
        fun (#payproc_RecurrentPaymentToolEvent{payload = Changes}) ->
            Changes
        end,
        Events
    ).

issue_recurrent_paytools_call(Function, Args) ->
    hg_woody_wrapper:call(recurrent_paytool, Function, Args).

set_event_poll_timer() ->
    % TODO rather dumb
    hg_machine_action:set_timeout(?SYNC_INTERVAL).

%%

create_customer(CustomerID, Params = #payproc_CustomerParams{}) ->
    #payproc_Customer{
        id             = CustomerID,
        owner_id       = Params#payproc_CustomerParams.party_id,
        shop_id        = Params#payproc_CustomerParams.shop_id,
        status         = ?customer_unready(),
        created_at     = hg_datetime:format_now(),
        bindings       = [],
        contact_info   = Params#payproc_CustomerParams.contact_info,
        metadata       = Params#payproc_CustomerParams.metadata
    }.

%%

collapse_history(History) ->
    lists:foldl(fun merge_event/2, #st{}, History).

merge_event({_ID, _, Changes}, St) ->
    merge_changes(Changes, St).

merge_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

merge_change(?customer_created(Customer), St) ->
    set_customer(Customer, St);
merge_change(?customer_deleted(), St) ->
    set_customer(undefined, St);
merge_change(?customer_status_changed(Status), St) ->
    Customer = get_customer(St),
    set_customer(Customer#payproc_Customer{status = Status}, St);
merge_change(?customer_binding_changed(BindingID, Payload), St) ->
    Customer = get_customer(St),
    Binding = try_get_binding(BindingID, Customer),
    Binding1 = merge_binding_change(Payload, Binding),
    BindingStatus = get_binding_status(Binding1),
    St1 = set_customer(set_binding(Binding1, Customer), St),
    St2 = update_active_binding(BindingID, BindingStatus, St1),
    St2.

update_active_binding(BindingID, ?customer_binding_succeeded(), St) ->
    set_active_binding_id(BindingID, St);
update_active_binding(_BindingID, _BindingStatus, St) ->
    St.

wrap_binding_changes(BindingID, Changes) ->
    [?customer_binding_changed(BindingID, C) || C <- Changes].

merge_binding_changes(Changes, Binding) ->
    lists:foldl(fun merge_binding_change/2, Binding, Changes).

merge_binding_change(?customer_binding_started(Binding), undefined) ->
    Binding;
merge_binding_change(?customer_binding_status_changed(BindingStatus), Binding) ->
    Binding#payproc_CustomerBinding{status = BindingStatus}.

get_party_id(#st{customer = #payproc_Customer{owner_id = PartyID}}) ->
    PartyID.

get_shop_id(#st{customer = #payproc_Customer{shop_id = ShopID}}) ->
    ShopID.

get_customer(#st{customer = Customer}) ->
    Customer.

set_customer(Customer, St = #st{}) ->
    St#st{customer = Customer}.

get_customer_status(#payproc_Customer{status = Status}) ->
    Status.

get_bindings(#payproc_Customer{bindings = Bindings}) ->
    Bindings.

try_get_binding(BindingID, Customer) ->
    case lists:keyfind(BindingID, #payproc_CustomerBinding.id, get_bindings(Customer)) of
        Binding = #payproc_CustomerBinding{} ->
            Binding;
        false ->
            undefined
    end.

set_binding(Binding, Customer = #payproc_Customer{bindings = Bindings}) ->
    BindingID = Binding#payproc_CustomerBinding.id,
    Customer#payproc_Customer{
        bindings = lists:keystore(BindingID, #payproc_CustomerBinding.id, Bindings, Binding)
    }.

get_pending_binding_set(St) ->
    Bindings = get_bindings(get_customer(St)),
    [get_binding_id(Binding) ||
        Binding <- Bindings, get_binding_status(Binding) == ?customer_binding_pending()
    ].

get_binding_id(#payproc_CustomerBinding{id = BindingID}) ->
    BindingID.

get_binding_status(#payproc_CustomerBinding{status = Status}) ->
    Status.

assert_binding_status(StatusName, #payproc_CustomerBinding{status = {StatusName, _}}) ->
    ok;
assert_binding_status(_StatusName, #payproc_CustomerBinding{status = Status}) ->
    error({unexpected, {'Unexpected customer binding status', Status}}).

get_binding_recurrent_paytool_id(#payproc_CustomerBinding{rec_payment_tool_id = ID}) ->
    ID.

try_get_active_binding(St) ->
    case get_active_binding_id(St) of
        BindingID when BindingID /= undefined ->
            try_get_binding(BindingID, get_customer(St));
        undefined ->
            undefined
    end.

get_active_binding_id(#st{active_binding = BindingID}) ->
    BindingID.

set_active_binding_id(BindingID, St = #st{}) ->
    St#st{active_binding = BindingID}.

%%
%% Validators and stuff
%%

assert_customer_present(#st{customer = undefined}) ->
    throw(#payproc_CustomerNotFound{});
assert_customer_present(_) ->
    ok.

assert_customer_accessible(St = #st{}) ->
    ok = assert_customer_present(St),
    ok = assert_party_accessible(get_party_id(St)),
    ok.

assert_party_accessible(PartyID) ->
    hg_invoice_utils:assert_party_accessible(PartyID).

assert_customer_operable(St = #st{}) ->
    ok    = assert_customer_accessible(St),
    Party = get_party(get_party_id(St)),
    Shop  = hg_party:get_shop(get_shop_id(St), Party),
    ok    = assert_party_shop_operable(Shop, Party),
    ok.

assert_party_shop_operable(Shop, Party) ->
    ok = assert_party_operable(Party),
    ok = assert_shop_operable(Shop),
    ok.

ensure_shop_exists(Shop) ->
    Shop = hg_invoice_utils:assert_shop_exists(Shop),
    Shop.

assert_party_operable(Party) ->
    Party = hg_invoice_utils:assert_party_operable(Party),
    ok.

assert_shop_operable(Shop) ->
    Shop = hg_invoice_utils:assert_shop_operable(Shop),
    ok.

%%
%% Marshalling
%%

marshal(Changes) ->
    Version = 1,
    marshal({list, {change, Version}}, Changes).

marshal({list, T}, Vs) when is_list(Vs) ->
    [marshal(T, V) || V <- Vs];

%% Changes

marshal({change, Version}, Change) ->
    [Version, marshal(change_payload, Change)];

marshal(change_payload, ?customer_created(Customer)) ->
    #{
        <<"change">>    => <<"created">>,
        <<"customer">>  => marshal(customer, Customer)
    };
marshal(change_payload, ?customer_deleted()) ->
    #{
        <<"change">> => <<"deleted">>
    };
marshal(change_payload, ?customer_status_changed(CustomerStatus)) ->
    #{
        <<"change">> => <<"status">>,
        <<"status">> => marshal(customer_status, CustomerStatus)
    };
marshal(change_payload, ?customer_binding_changed(CustomerBindingID, Payload)) ->
    #{
        <<"change">>     => <<"binding">>,
        <<"binding_id">> => marshal(str, CustomerBindingID),
        <<"payload">>    => marshal(binding_change_payload, Payload)
    };

%% Change components

marshal(
    customer,
    #payproc_Customer{
        id             = ID,
        owner_id       = OwnerID,
        shop_id        = ShopID,
        created_at     = CreatedAt,
        contact_info   = ContactInfo,
        metadata       = Metadata
    }
) ->
    #{
        <<"id">>         => marshal(str             , ID),
        <<"owner_id">>   => marshal(str             , OwnerID),
        <<"shop_id">>    => marshal(str             , ShopID),
        <<"created_at">> => marshal(str             , CreatedAt),
        <<"contact">>    => marshal(contact_info    , ContactInfo),
        <<"metadata">>   => marshal(metadata        , Metadata)
    };

marshal(customer_status, ?customer_unready()) ->
    <<"unready">>;
marshal(customer_status, ?customer_ready()) ->
    <<"ready">>;

marshal(
    binding,
    #payproc_CustomerBinding{
        id                  = ID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource    = PaymentResource
    }
) ->
    #{
        <<"id">>            => marshal(str              , ID),
        <<"recpaytool_id">> => marshal(str              , RecPaymentToolID),
        <<"payresource">>   => marshal(payment_resource , PaymentResource)
    };

marshal(
    contact_info,
    #domain_ContactInfo{
        phone_number = PhoneNumber,
        email        = Email
    }
) ->
    genlib_map:compact(#{
        <<"phone">> => marshal(str, PhoneNumber),
        <<"email">> => marshal(str, Email)
    });

marshal(
    payment_resource,
    #domain_DisposablePaymentResource{
        payment_tool       = PaymentTool,
        payment_session_id = PaymentSessionID,
        client_info        = ClientInfo
    }
) ->
    #{
        <<"paytool">>     => hg_payment_tool:marshal(PaymentTool),
        <<"session">>     => marshal(str           , PaymentSessionID),
        <<"client_info">> => marshal(client_info   , ClientInfo)
    };

marshal(
    client_info,
    #domain_ClientInfo{
        ip_address  = IPAddress,
        fingerprint = Fingerprint
    }
) ->
    genlib_map:compact(#{
        <<"ip">>          => marshal(str, IPAddress),
        <<"fingerprint">> => marshal(str, Fingerprint)
    });

marshal(binding_status, ?customer_binding_pending()) ->
    <<"pending">>;
marshal(binding_status, ?customer_binding_succeeded()) ->
    <<"succeeded">>;
marshal(binding_status, ?customer_binding_failed(Failure)) ->
    [
        <<"failed">>,
        marshal(failure, Failure)
    ];

marshal(binding_change_payload, ?customer_binding_started(CustomerBinding)) ->
    [
        <<"started">>,
        marshal(binding, CustomerBinding)
    ];
marshal(binding_change_payload, ?customer_binding_status_changed(CustomerBindingStatus)) ->
    [
        <<"status">>,
        marshal(binding_status, CustomerBindingStatus)
    ];

marshal(failure, {operation_timeout, _}) ->
    <<"operation_timeout">>;
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })];

marshal(metadata, Metadata) ->
    hg_msgpack_marshalling:marshal(json, Metadata);

marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, change}, Payload)}.

unmarshal({list, T}, Vs) when is_list(Vs) ->
    [unmarshal(T, V) || V <- Vs];

%% Changes

unmarshal(change, [Version, V]) ->
    unmarshal({change, Version}, V);

unmarshal({change, 1}, #{
    <<"change">>    := <<"created">>,
    <<"customer">>  := Customer
}) ->
    ?customer_created(unmarshal(customer, Customer));
unmarshal({change, 1}, #{
    <<"change">> := <<"deleted">>
}) ->
    ?customer_deleted();
unmarshal({change, 1}, #{
    <<"change">> := <<"status">>,
    <<"status">> := CustomerStatus
}) ->
    ?customer_status_changed(unmarshal(customer_status, CustomerStatus));
unmarshal({change, 1}, #{
    <<"change">>     := <<"binding">>,
    <<"binding_id">> := CustomerBindingID,
    <<"payload">>    := Payload
}) ->
    ?customer_binding_changed(
        unmarshal(str, CustomerBindingID),
        unmarshal(binding_change_payload, Payload)
    );

unmarshal(
    customer,
    #{
        <<"id">>         := ID,
        <<"owner_id">>   := OwnerID,
        <<"shop_id">>    := ShopID,
        <<"created_at">> := CreatedAt,
        <<"contact">>    := ContactInfo,
        <<"metadata">>   := Metadata
    }
) ->
    #payproc_Customer{
        id             = unmarshal(str             , ID),
        owner_id       = unmarshal(str             , OwnerID),
        shop_id        = unmarshal(str             , ShopID),
        status         = ?customer_unready(),
        created_at     = unmarshal(str             , CreatedAt),
        bindings       = [],
        contact_info   = unmarshal(contact_info    , ContactInfo),
        metadata       = unmarshal(metadata        , Metadata)
    };

unmarshal(customer_status, <<"unready">>) ->
    ?customer_unready();
unmarshal(customer_status, <<"ready">>) ->
    ?customer_ready();

unmarshal(
    binding,
    #{
        <<"id">>             := ID,
        <<"recpaytool_id">>  := RecPaymentToolID,
        <<"payresource">>    := PaymentResource
    }
) ->
    #payproc_CustomerBinding{
        id                  = unmarshal(str              , ID),
        rec_payment_tool_id = unmarshal(str              , RecPaymentToolID),
        payment_resource    = unmarshal(payment_resource , PaymentResource),
        status              = ?customer_binding_pending()
    };

unmarshal(
    payment_resource,
    #{
        <<"paytool">>     := PaymentTool,
        <<"session">>     := PaymentSessionID,
        <<"client_info">> := ClientInfo
    }
) ->
    #domain_DisposablePaymentResource{
        payment_tool       = hg_payment_tool:unmarshal(PaymentTool),
        payment_session_id = unmarshal(str           , PaymentSessionID),
        client_info        = unmarshal(client_info   , ClientInfo)
    };

unmarshal(client_info, ClientInfo) ->
    #domain_ClientInfo{
        ip_address      = unmarshal(str, genlib_map:get(<<"ip">>, ClientInfo)),
        fingerprint     = unmarshal(str, genlib_map:get(<<"fingerprint">>, ClientInfo))
    };

unmarshal(binding_status, <<"pending">>) ->
    ?customer_binding_pending();
unmarshal(binding_status, <<"succeeded">>) ->
    ?customer_binding_succeeded();
unmarshal(binding_status, [<<"failed">>, Failure]) ->
    ?customer_binding_failed(unmarshal(failure, Failure));

unmarshal(binding_change_payload, [<<"started">>, Binding]) ->
    ?customer_binding_started(unmarshal(binding, Binding));
unmarshal(binding_change_payload, [<<"status">>, BindingStatus]) ->
    ?customer_binding_status_changed(unmarshal(binding_status, BindingStatus));

unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, genlib_map:get(<<"description">>, ExternalFailure))
    }};

unmarshal(contact_info, ContactInfo) ->
    #domain_ContactInfo{
        phone_number    = unmarshal(str, genlib_map:get(<<"phone">>, ContactInfo)),
        email           = unmarshal(str, genlib_map:get(<<"email">>, ContactInfo))
    };

unmarshal(metadata, Metadata) ->
    hg_msgpack_marshalling:unmarshal(json, Metadata);

unmarshal(_, Other) ->
    Other.
