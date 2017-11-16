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
-export([process_signal/3]).
-export([process_call  /3]).

%% Event provider callbacks

-behaviour(hg_event_provider).
-export([publish_event/2]).

%% Types

-define(SYNC_INTERVAL, 1).
-define(REC_PAYTOOL_EVENTS_LIMIT, 10).

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
    scoper:scope(customer_management,
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
    _ = hg_recurrent_paytool:assert_operation_permitted(Shop, Party),
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
    scoper:add_meta(#{customer_id => ID}).

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
    handle_result(#{
        changes => [
            get_customer_created_event(CustomerID, CustomerParams)
        ],
        auxst => #{}
    }).

-spec process_signal(hg_machine:signal(), hg_machine:history(), hg_machine:auxst()) ->
    hg_machine:result().
process_signal(Signal, History, AuxSt) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal(History)), unmarshal(auxst, AuxSt))).

handle_signal(timeout, St0, AuxSt0) ->
    {Changes, AuxSt1} = sync_pending_bindings(St0, AuxSt0),
    St1 = merge_changes(Changes, St0),
    Action = case get_pending_binding_set(St1) of
        [_BindingID | _] ->
            set_event_poll_timer();
        [] ->
            hg_machine_action:new()
    end,
    #{
        changes => Changes ++ get_ready_changes(St1),
        action  => Action,
        auxst   => AuxSt1
    }.

get_ready_changes(#st{customer = #payproc_Customer{status = ?customer_unready()}} = St) ->
    case find_active_bindings(get_bindings(get_customer(St))) of
        [_ | _] ->
            [?customer_status_changed(?customer_ready())];
        [] ->
            []
    end;
get_ready_changes(_) ->
    [].

find_active_bindings(Bindings) ->
    lists:filtermap(fun(Binding) -> is_binding_succeeded(Binding) end, Bindings).

is_binding_succeeded(#payproc_CustomerBinding{status = ?customer_binding_succeeded()}) ->
    true;
is_binding_succeeded(_) ->
    false.

-type call() ::
    {start_binding, binding_params()} |
    delete.

-spec process_call(call(), hg_machine:history(), hg_machine:auxst()) ->
    {hg_machine:response(), hg_machine:result()}.
process_call(Call, History, _AuxSt) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, #{}}
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
    Result = handle_aux_state(Params, handle_result_changes(Params, handle_result_action(Params, #{}))),
    case maps:find(response, Params) of
        {ok, Response} ->
            {{ok, Response}, Result};
        error ->
            Result
    end.

handle_aux_state(#{auxst := AuxSt}, Acc) ->
    Acc#{auxst => marshal(auxst, AuxSt)};
handle_aux_state(#{}, Acc) ->
    Acc.

handle_result_changes(#{changes := Changes = [_ | _]}, Acc) ->
    Acc#{events => [marshal(Changes)]};
handle_result_changes(#{}, Acc) ->
    Acc.

handle_result_action(#{action := Action}, Acc) ->
    Acc#{action => Action};
handle_result_action(#{}, Acc) ->
    Acc.

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

sync_pending_bindings(St, AuxSt) ->
    sync_pending_bindings(get_pending_binding_set(St), St, AuxSt).

sync_pending_bindings([BindingID | Rest], St, AuxSt0) ->
    Binding = try_get_binding(BindingID, get_customer(St)),
    {Changes1, AuxSt1} = sync_binding_state(Binding, AuxSt0),
    {Changes2, AuxSt2} = sync_pending_bindings(Rest, St, AuxSt1),
    {Changes1 ++ Changes2, AuxSt2};
sync_pending_bindings([], _St, AuxSt) ->
    {[], AuxSt}.

sync_binding_state(Binding, AuxSt) ->
    RecurrentPaytoolID = get_binding_recurrent_paytool_id(Binding),
    LastEventID0 = get_binding_last_event_id(Binding, AuxSt),
    {RecurrentPaytoolChanges, LastEventID1} = get_recurrent_paytool_changes(RecurrentPaytoolID, LastEventID0),
    BindingChanges = produce_binding_changes(RecurrentPaytoolChanges, Binding),
    {wrap_binding_changes(get_binding_id(Binding), BindingChanges), update_aux_state(LastEventID1, Binding, AuxSt)}.

update_aux_state(undefined, _Binding, AuxSt) ->
    AuxSt;
update_aux_state(LastEventID, #payproc_CustomerBinding{id = BindingID}, AuxSt) ->
    maps:put(BindingID, LastEventID, AuxSt).

get_binding_last_event_id(#payproc_CustomerBinding{id = BindingID}, AuxSt) ->
    maps:get(BindingID, AuxSt, undefined).

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
produce_binding_changes_(?session_ev(?interaction_requested(UserInteraction)), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [?customer_binding_interaction_requested(UserInteraction)];
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

get_recurrent_paytool_changes(RecurrentPaytoolID, LastEventID) ->
    EventRange = construct_event_range(LastEventID),
    {ok, Events} = issue_recurrent_paytools_call('GetEvents', [RecurrentPaytoolID, EventRange]),
    {gather_recurrent_paytool_changes(Events), get_last_event_id(Events)}.

construct_event_range(undefined) ->
    #payproc_EventRange{limit = ?REC_PAYTOOL_EVENTS_LIMIT};
construct_event_range(LastEventID) ->
    #payproc_EventRange{'after' = LastEventID, limit = ?REC_PAYTOOL_EVENTS_LIMIT}.

get_last_event_id([_ | _] = Events) ->
    #payproc_RecurrentPaymentToolEvent{id = LastEventID} = lists:last(Events),
    LastEventID;
get_last_event_id([]) ->
    undefined.

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

get_customer_created_event(CustomerID, Params = #payproc_CustomerParams{}) ->
    OwnerID = Params#payproc_CustomerParams.party_id,
    ShopID = Params#payproc_CustomerParams.shop_id,
    ContactInfo = Params#payproc_CustomerParams.contact_info,
    Metadata = Params#payproc_CustomerParams.metadata,
    CreatedAt = hg_datetime:format_now(),
    ?customer_created(CustomerID, OwnerID, ShopID, Metadata, ContactInfo, CreatedAt).

%%

collapse_history(History) ->
    lists:foldl(fun merge_event/2, #st{}, History).

merge_event({_ID, _, Changes}, St) ->
    merge_changes(Changes, St).

merge_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

merge_change(?customer_created(_, _, _, _, _, _) = CustomerCreatedChange, St) ->
    Customer = create_customer(CustomerCreatedChange),
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
    Binding#payproc_CustomerBinding{status = BindingStatus};
merge_binding_change(?customer_binding_interaction_requested(_), Binding) ->
    Binding.

get_party_id(#st{customer = #payproc_Customer{owner_id = PartyID}}) ->
    PartyID.

get_shop_id(#st{customer = #payproc_Customer{shop_id = ShopID}}) ->
    ShopID.

get_customer(#st{customer = Customer}) ->
    Customer.

create_customer(?customer_created(CustomerID, OwnerID, ShopID, Metadata, ContactInfo, CreatedAt)) ->
    #payproc_Customer{
        id = CustomerID,
        owner_id = OwnerID,
        shop_id = ShopID,
        status = ?customer_unready(),
        created_at = CreatedAt,
        bindings = [],
        contact_info = ContactInfo,
        metadata = Metadata
    }.

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
    marshal({list, change}, Changes).

marshal({list, T}, Vs) when is_list(Vs) ->
    [marshal(T, V) || V <- Vs];

%% AuxState

marshal(auxst, AuxState) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(binding_id, K), marshal(event_id, V), Acc)
        end,
        #{},
        AuxState
    );

marshal(binding_id, BindingID) ->
    marshal(str, BindingID);

marshal(event_id, EventID) ->
    marshal(int, EventID);

%% Changes

marshal(change, Change) ->
    marshal(change_payload, Change);

marshal(change_payload, ?customer_created(CustomerID, OwnerID, ShopID, Metadata, ContactInfo, CreatedAt)) ->
    [2, #{
        <<"change">>       => <<"created">>,
        <<"customer_id">>  => marshal(str         , CustomerID),
        <<"owner_id">>     => marshal(str         , OwnerID),
        <<"shop_id">>      => marshal(str         , ShopID),
        <<"metadata">>     => marshal(metadata    , Metadata),
        <<"contact_info">> => marshal(contact_info, ContactInfo),
        <<"created_at">>   => marshal(str         , CreatedAt)
    }];
marshal(change_payload, ?customer_deleted()) ->
    [1, #{
        <<"change">> => <<"deleted">>
    }];
marshal(change_payload, ?customer_status_changed(CustomerStatus)) ->
    [1, #{
        <<"change">> => <<"status">>,
        <<"status">> => marshal(customer_status, CustomerStatus)
    }];
marshal(change_payload, ?customer_binding_changed(CustomerBindingID, Payload)) ->
    [1, #{
        <<"change">>     => <<"binding">>,
        <<"binding_id">> => marshal(str, CustomerBindingID),
        <<"payload">>    => marshal(binding_change_payload, Payload)
    }];

%% Change components

marshal(
    customer,
    #payproc_Customer{
        id           = ID,
        owner_id     = OwnerID,
        shop_id      = ShopID,
        created_at   = CreatedAt,
        contact_info = ContactInfo,
        metadata     = Metadata
    }
) ->
    #{
        <<"id">>           => marshal(str         , ID),
        <<"owner_id">>     => marshal(str         , OwnerID),
        <<"shop_id">>      => marshal(str         , ShopID),
        <<"created_at">>   => marshal(str         , CreatedAt),
        <<"contact_info">> => marshal(contact_info, ContactInfo),
        <<"metadata">>     => marshal(metadata    , Metadata)
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
marshal(binding_change_payload, ?customer_binding_interaction_requested(UserInteraction)) ->
    [
        <<"interaction">>,
        marshal(interaction, UserInteraction)
    ];

marshal(interaction, {redirect, Redirect}) ->
    [
        <<"redirect">>,
        marshal(redirect, Redirect)
    ];

marshal(redirect, {get_request, #'BrowserGetRequest'{uri = URI}}) ->
    [
        <<"get">>,
        marshal(str, URI)
    ];
marshal(redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}) ->
    [
        <<"post">>,
        #{
            <<"uri">>  => marshal(str, URI),
            <<"form">> => marshal(map_str, Form)
        }
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

%% AuxState

unmarshal(auxst, AuxState) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(unmarshal(binding_id, K), unmarshal(event_id, V), Acc)
        end,
        #{},
        AuxState
    );

unmarshal(binding_id, BindingID) ->
    unmarshal(str, BindingID);

unmarshal(event_id, EventID) ->
    unmarshal(int, EventID);

%% Changes

unmarshal(change, [Version, V]) ->
    unmarshal({change, Version}, V);

unmarshal({change, 2}, #{
    <<"change">>       := <<"created">>,
    <<"customer_id">>  := CustomerID,
    <<"owner_id">>     := OwnerID,
    <<"shop_id">>      := ShopID,
    <<"created_at">>   := CreatedAt,
    <<"contact_info">> := ContactInfo,
    <<"metadata">>     := Metadata
}) ->
    ?customer_created(
        unmarshal(str             , CustomerID),
        unmarshal(str             , OwnerID),
        unmarshal(str             , ShopID),
        unmarshal(metadata        , Metadata),
        unmarshal(contact_info    , ContactInfo),
        unmarshal(str             , CreatedAt)
    );
unmarshal({change, 1}, #{
    <<"change">>    := <<"created">>,
    <<"customer">>  := Customer
}) ->
    #payproc_Customer{
        id = CustomerID,
        owner_id = OwnerID,
        shop_id = ShopID,
        created_at = CreatedAt,
        contact_info = ContactInfo,
        metadata = Metadata
    } = unmarshal(customer, Customer),
    ?customer_created(CustomerID, OwnerID, ShopID, Metadata, ContactInfo, CreatedAt);
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
unmarshal(binding_change_payload, [<<"interaction">>, UserInteraction]) ->
    ?customer_binding_interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(interaction, [<<"redirect">>, Redirect]) ->
    {
        redirect,
        unmarshal(redirect, Redirect)
    };
unmarshal(redirect, [<<"get">>, URI]) ->
    {
        get_request,
        #'BrowserGetRequest'{uri = unmarshal(str, URI)}
    };
unmarshal(redirect, [<<"post">>, #{<<"uri">> := URI, <<"form">> := Form}]) ->
    {
        post_request,
        #'BrowserPostRequest'{uri = unmarshal(str, URI), form = unmarshal(map_str, Form)}
    };

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
