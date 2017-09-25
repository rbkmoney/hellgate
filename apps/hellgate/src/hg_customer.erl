%%%
%%% Customer machine
%%%

-module(hg_customer).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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

-record(st, {
    customer :: undefined | customer()
}).
-type st() :: #st{}.

-type customer()         :: dmsl_payment_processing_thrift:'Customer'().
-type customer_id()      :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer_params()  :: dmsl_payment_processing_thrift:'CustomerParams'().
-type customer_change()  :: dmsl_payment_processing_thrift:'CustomerChange'().

-type binding_id()     :: dmsl_payment_processing_thrift:'CustomerBindingID'().
-type binding_params() :: dmsl_payment_processing_thrift:'CustomerBindingParams'().

%%
%% Woody handler
%%

-spec handle_function(woody:func(), woody:args(), hg_woody_handler:handler_opts()) ->
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
handle_function_('GetEvents', [CustomerID, Range], _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_accessible(get_initial_state(CustomerID)),
    get_public_history(CustomerID, Range).

%%

-include("customer_events.hrl").

get_party(PartyID) ->
    hg_party_machine:get_party(PartyID).

set_meta(ID) ->
    hg_log_scope:set_meta(#{customer_id => ID}).

get_history(CustomerID) ->
    History = hg_machine:get_history(?NS, CustomerID),
    unmarshal(map_history_error(History)).

get_history(CustomerID, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, CustomerID, AfterID, Limit),
    unmarshal(map_history_error(History)).

get_state(CustomerID) ->
    collapse_history(get_history(CustomerID)).

get_initial_state(CustomerID) ->
    collapse_history(get_history(CustomerID, undefined, 1)).

get_public_history(CustomerID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_customer_event(CustomerID, Ev) || Ev <- get_history(CustomerID, AfterID, Limit)].

publish_customer_event(CustomerID, Event) ->
    {ID, Dt, Payload} = unmarshal(Event),
    #payproc_CustomerEvent{
        id = ID,
        created_at = Dt,
        source = CustomerID,
        payload = Payload
    }.

-spec start(customer_id(), customer_params()) ->
    ok | no_return().
start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

-spec call(customer_id(), _Args) ->
    _Result | no_return().
call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

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
    throw(#payproc_CustomerNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

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
    {CustomerID, ?customer_event(unmarshal({list, changes}, Changes))}.

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

handle_signal(timeout, _St = #st{}) ->
    #{}.

-type call() :: {start_binding, binding_params()}
              | delete.

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
    Changes = maps:get(changes, Params, []),
    Action = maps:get(action, Params, hg_machine_action:new()),
    case maps:find(response, Params) of
        {ok, Response} ->
            {{ok, Response}, {[marshal(Changes)], Action}};
        error ->
            {[marshal(Changes)], Action}
    end.

%%

start_binding(BindingParams, St) ->
    BindingID = create_binding_id(St),
    {Binding, {Changes, Action}} = init_binding(BindingID, BindingParams),
    #{
        response => Binding,
        changes  => Changes,
        action   => Action
    }.

init_binding(BindingID, BindingParams) ->
    % RecPaymentTool = hg_payment_processing:start_rec_payment_tool(),
    % RecPaymentToolID = get_rec_payment_tool_id(RecPaymentTool),
    RecPaymentToolID = <<"tsL9R7G7Iu">>,
    Binding = create_binding(BindingID, BindingParams, RecPaymentToolID),
    Changes = [?customer_binding_changed(BindingID, ?customer_binding_started(Binding))],
    Action = hg_machine_action:new(),
    {Binding, {Changes, Action}}.

create_binding(BindingID, BindingParams, RecPaymentToolID) ->
    #payproc_CustomerBinding{
        id                  = BindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource    = BindingParams#payproc_CustomerBindingParams.payment_resource,
        status              = ?customer_binding_pending()
    }.

-spec create_binding_id(st()) ->
    binding_id().
create_binding_id(#st{customer = #payproc_Customer{bindings = Bindings}}) ->
    integer_to_binary(length(Bindings) + 1).

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
        metadata       = Params#payproc_CustomerParams.metadata,
        active_binding = undefined
    }.

%%

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Changes}, St0) ->
            lists:foldl(fun merge_change/2, St0, Changes)
        end,
        #st{},
        History
    ).

merge_change(?customer_created(Customer), St) ->
    set_customer(Customer, St);
merge_change(?customer_deleted(), St) ->
    set_customer(undefined, St);
merge_change(?customer_status_changed(Status), St) ->
    Customer = get_customer(St),
    set_customer(Customer#payproc_Customer{status = Status}, St);
merge_change(?customer_binding_changed(BindingID, Payload), St) ->
    Customer = get_customer(St),
    Binding = try_get_customer_binding(BindingID, Customer),
    set_customer(set_customer_binding(merge_binding_change(Payload, Binding), Customer), St).

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

try_get_customer_binding(BindingID, #payproc_Customer{bindings = Bindings}) ->
    case lists:keyfind(BindingID, #payproc_CustomerBinding.id, Bindings) of
        Binding = #payproc_CustomerBinding{} ->
            Binding;
        false ->
            undefined
    end.

set_customer_binding(Binding, Customer = #payproc_Customer{bindings = Bindings}) ->
    BindingID = Binding#payproc_CustomerBinding.id,
    Customer#payproc_Customer{
        bindings = lists:keystore(BindingID, #payproc_CustomerBinding.id, Bindings, Binding)
    }.

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
    marshal({list, change}, Changes).

marshal({list, T}, Vs) when is_list(Vs) ->
    [marshal(T, V) || V <- Vs];

%% Changes

marshal(change, ?customer_created(Customer)) ->
    [1, #{
        <<"change">>    => <<"created">>,
        <<"customer">>  => marshal(customer, Customer)
    }];
marshal(change, ?customer_deleted()) ->
    [1, #{
        <<"change">> => <<"deleted">>
    }];
marshal(change, ?customer_status_changed(CustomerStatus)) ->
    [1, #{
        <<"change">> => <<"status_changed">>,
        <<"status">> => marshal(customer_status, CustomerStatus)
    }];
marshal(change, ?customer_binding_changed(CustomerBindingID, Payload)) ->
    [1, #{
        <<"change">>     => <<"binding_changed">>,
        <<"binding_id">> => marshal(str, CustomerBindingID),
        <<"payload">>    => marshal(binding_change_payload, Payload)
    }];

%% Change components

marshal(customer, #payproc_Customer{} = Customer) ->
    #{
        <<"id">> => marshal(str, Customer#payproc_Customer.id),
        <<"owner_id">> => marshal(str, Customer#payproc_Customer.owner_id),
        <<"shop_id">> => marshal(str, Customer#payproc_Customer.shop_id),
        <<"status">> => marshal(customer_status, Customer#payproc_Customer.status),
        <<"created_at">> => marshal(str, Customer#payproc_Customer.created_at),
        <<"bindings">> => marshal(bindings, Customer#payproc_Customer.bindings),
        <<"contact_info">> => marshal(contact_info, Customer#payproc_Customer.contact_info),
        <<"metadata">> => marshal(metadata, Customer#payproc_Customer.metadata),
        <<"active_binding">> => marshal(str, Customer#payproc_Customer.active_binding)
    };

marshal(customer_status, ?customer_unready()) ->
    <<"unready">>;
marshal(customer_status, ?customer_ready()) ->
    <<"ready">>;

marshal(bindings, Bindings) ->
    [marshal(binding, Binding) || Binding <- Bindings];

marshal(binding, #payproc_CustomerBinding{} = CustomerBinding) ->
    #{
        <<"id">> => marshal(str, CustomerBinding#payproc_CustomerBinding.id),
        <<"rec_payment_tool_id">> => marshal(str, CustomerBinding#payproc_CustomerBinding.rec_payment_tool_id),
        <<"payment_resource">> => marshal(disposable_payment_resource, CustomerBinding#payproc_CustomerBinding.payment_resource),
        <<"status">> => marshal(binding_status, CustomerBinding#payproc_CustomerBinding.status)
    };

marshal(contact_info, #domain_ContactInfo{} = ContactInfo) ->
    genlib_map:compact(#{
        <<"phone_number">>  => marshal(str, ContactInfo#domain_ContactInfo.phone_number),
        <<"email">>         => marshal(str, ContactInfo#domain_ContactInfo.email)
    });

marshal(disposable_payment_resource, #domain_DisposablePaymentResource{} = PaymentResource) ->
    #{
        <<"payment_tool">>       => hg_payment_tool:marshal(PaymentResource#domain_DisposablePaymentResource.payment_tool),
        <<"payment_session_id">> => marshal(str, PaymentResource#domain_DisposablePaymentResource.payment_session_id),
        <<"client_info">>        => marshal(client_info, PaymentResource#domain_DisposablePaymentResource.client_info)
    };

marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    genlib_map:compact(#{
        <<"ip_address">>    => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        <<"fingerprint">>   => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

marshal(binding_status, ?customer_binding_pending()) ->
    <<"pending">>;
marshal(binding_status, ?customer_binding_succeeded()) ->
    <<"succeeded">>;
marshal(binding_status, ?customer_binding_failed(Failure)) ->
    [
        <<"succeeded">>,
        marshal(failure, Failure)
    ];

marshal(binding_change_payload, ?customer_binding_started(CustomerBinding)) ->
    [
        <<"started">>,
        marshal(binding, CustomerBinding)
    ];
marshal(binding_change_payload, ?customer_binding_status_changed(CustomerBindingStatus)) ->
    [
        <<"status_changed">>,
        marshal(binding_status, CustomerBindingStatus)
    ];

marshal(failure, {operation_timeout, _}) ->
    <<"operation_timeout">>;
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })];

marshal(metadata, {nl, #json_Null{}}) ->
    <<"null">>;

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

unmarshal(change, [1, #{
    <<"change">>    := <<"created">>,
    <<"customer">>  := Customer
}]) ->
    ?customer_created(unmarshal(customer, Customer));
unmarshal(change, [1, #{
    <<"change">> := <<"deleted">>
}]) ->
    ?customer_deleted();
unmarshal(change, [1, #{
    <<"change">> := <<"status_changed">>,
    <<"status">> := CustomerStatus
}]) ->
    ?customer_status_changed(unmarshal(customer_status, CustomerStatus));
unmarshal(change, [1, #{
    <<"change">>     := <<"binding_changed">>,
    <<"binding_id">> := CustomerBindingID,
    <<"payload">>    := Payload
}]) ->
    ?customer_binding_changed(
        unmarshal(str, CustomerBindingID),
        unmarshal(binding_change_payload, Payload)
    );

unmarshal(customer, #{
    <<"id">> := Id,
    <<"owner_id">> := OwnerId,
    <<"shop_id">> := ShopId,
    <<"status">> := Status,
    <<"created_at">> := CreatedAt,
    <<"bindings">> := Bindings,
    <<"contact_info">> := ContactInfo,
    <<"metadata">> := Metadata,
    <<"active_binding">> := ActiveBinding
}) ->
    #payproc_Customer{
        id = unmarshal(str, Id),
        owner_id = unmarshal(str, OwnerId),
        shop_id = unmarshal(str, ShopId),
        status = unmarshal(customer_status, Status),
        created_at = unmarshal(str, CreatedAt),
        bindings = unmarshal(bindings, Bindings),
        contact_info = unmarshal(contact_info, ContactInfo),
        metadata = unmarshal(metadata, Metadata),
        active_binding = unmarshal(str, ActiveBinding)
    };

unmarshal(customer_status, <<"unready">>) ->
    ?customer_unready();
unmarshal(customer_status, <<"ready">>) ->
    ?customer_ready();

unmarshal(bindings, Bindings) ->
    [unmarshal(binding, Binding) || Binding <- Bindings];

unmarshal(binding, #{
    <<"id">> := Id,
    <<"rec_payment_tool_id">> := RecPaymentToolId,
    <<"payment_resource">> := PaymentResource,
    <<"status">> := Status
}) ->
    #payproc_CustomerBinding{
        id = unmarshal(str, Id),
        rec_payment_tool_id = unmarshal(str, RecPaymentToolId),
        payment_resource = unmarshal(disposable_payment_resource, PaymentResource),
        status = unmarshal(binding_status, Status)
    };

unmarshal(disposable_payment_resource, #{
    <<"payment_tool">> := PaymentTool,
    <<"payment_session_id">> := PaymentSessionId,
    <<"client_info">> := ClientInfo
}) ->
    #domain_DisposablePaymentResource{
        payment_tool = hg_payment_tool:unmarshal(PaymentTool),
        payment_session_id = unmarshal(str, PaymentSessionId),
        client_info = unmarshal(client_info, ClientInfo)
    };

unmarshal(client_info, ClientInfo) ->
    IpAddress = maps:get(<<"ip_address">>, ClientInfo, undefined),
    Fingerprint = maps:get(<<"fingerprint">>, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

unmarshal(binding_status, <<"pending">>) ->
    ?customer_binding_pending();
unmarshal(binding_status, <<"succeeded">>) ->
    ?customer_binding_succeeded();
unmarshal(binding_status, [
    <<"succeeded">>,
    Failure
]) ->
    ?customer_binding_failed(unmarshal(failure, Failure));

unmarshal(binding_change_payload, [
    <<"started">>,
    CustomerBinding
]) ->
    ?customer_binding_started(unmarshal(binding, CustomerBinding));
unmarshal(binding_change_payload, [
    <<"status_changed">>,
    CustomerBindingStatus
]) ->
    ?customer_binding_status_changed(unmarshal(binding_status, CustomerBindingStatus));

unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(contact_info, ContactInfo) ->
    PhoneNumber = maps:get(<<"phone_number">>, ContactInfo, undefined),
    Email = maps:get(<<"email">>, ContactInfo, undefined),
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(metadata, <<"null">>) ->
    {nl, #json_Null{}};

unmarshal(_, Other) ->
    Other.
