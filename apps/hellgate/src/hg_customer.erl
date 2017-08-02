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

-type event_id() :: dmsl_base_thrift:'EventID'().

-type customer()         :: dmsl_payment_processing_thrift:'Customer'().
-type customer_id()      :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer_params()  :: dmsl_payment_processing_thrift:'CustomerParams'().
-type customer_event()   :: dmsl_payment_processing_thrift:'CustomerChange'().

-type binding()        :: dmsl_payment_processing_thrift:'CustomerBinding'().
-type binding_id()     :: dmsl_payment_processing_thrift:'CustomerBindingID'().
-type binding_params() :: dmsl_payment_processing_thrift:'CustomerBindingParams'().
-type binding_event()  :: dmsl_payment_processing_thrift:'CustomerBindingChanged'().

-type rec_payment_tool_id() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolID'().

-type party()    :: dmsl_domain_thrift:'Party'().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type shop()     :: dmsl_domain_thrift:'Shop'().
-type shop_id()  :: dmsl_domain_thrift:'ShopID'().

% -type metadata()   :: dmsl_payment_processing_thrift:'Metadata'().
-type created_at() :: dmsl_base_thrift:'Timestamp'().
% -type user_info() :: dmsl_payment_processing_thrift:'UserInfo'().

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_handler:handler_opts()) ->
    term() | no_return().
handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(customer_management,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().
handle_function_('Create', [CustomerParams], _Opts) ->
    CustomerID = hg_utils:unique_id(),
    ok = set_meta(CustomerID),
    PartyID = CustomerParams#payproc_CustomerParams.party_id,
    ShopID = CustomerParams#payproc_CustomerParams.shop_id,
    Party = get_party(PartyID),
    Shop = ensure_shop_exists(hg_party:get_shop(ShopID, Party)),
    ok = assert_party_shop_operable(Shop, Party),
    ok = validate_customer_params(CustomerParams),
    ok = start(CustomerID, CustomerParams),
    get_customer_state(get_state(CustomerID));
handle_function_('Get', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_operable(St),
    get_customer_state(St);
handle_function_('Delete', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_operable(get_state(CustomerID)),
    call(CustomerID, delete);
handle_function_('StartBinding', [CustomerID, BindingParams], _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_operable(get_state(CustomerID)),
    call(CustomerID, {start_binding, BindingParams});
% handle_function_('GetActiveBinding', [UserInfo, CustomerID], _Opts) ->
%     ok = assume_user_identity(UserInfo),
%     ok = set_meta(CustomerID),
%     St = ensure_customer_accessible(get_state(CustomerID)),
%     ok = assert_customer_operable(St),
%     ok = assert_customer_status(ready, St),
%     get_binding_state(get_active_binding(St));
handle_function_('GetEvents', [CustomerID, Range], _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_operable(get_state(CustomerID)),
    get_public_history(CustomerID, Range).

-spec get_customer_state(st()) ->
    customer().
get_customer_state(St) ->
    History = get_history(get_customer_id(St)),
    ok = assert_customer_not_deleted(lists:last(History)),
    get_customer(St).

%%

-spec get_party(party_id()) ->
    party() | no_return().
get_party(PartyID) ->
    hg_party_machine:get_party(PartyID).

-spec set_meta(customer_id()) ->
    ok.
set_meta(ID) ->
    hg_log_scope:set_meta(#{customer_id => ID}).

-spec get_history(customer_id()) ->
    {ok, hg_machine:history()} | {error, notfound | failed} | no_return().
get_history(CustomerID) ->
    History = hg_machine:get_history(?NS, CustomerID),
    map_history_error(unmarshal_history_result(History)).

-spec get_history(customer_id(), undefined | event_id(), undefined | non_neg_integer()) ->
    {ok, hg_machine:history()} | {error, notfound | failed} | no_return().
get_history(CustomerID, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, CustomerID, AfterID, Limit),
    map_history_error(unmarshal_history_result(History)).

-spec get_state(customer_id()) ->
    st().
get_state(CustomerID) ->
    collapse_history(get_history(CustomerID)).

-spec get_public_history(customer_id(), term()) ->
    list().
get_public_history(CustomerID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_customer_event(CustomerID, Ev) || Ev <- get_history(CustomerID, AfterID, Limit)].

-spec publish_customer_event(customer_id(), {event_id(), created_at(), customer_event()}) ->
    term().
publish_customer_event(CustomerID, {ID, Dt, Event}) ->
    {Source, Ev} = publish_event(CustomerID, Event),
    #payproc_CustomerEvent{
        id = ID,
        created_at = Dt,
        source = Source,
        payload = Ev
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

unmarshal_history_result({ok, Result}) ->
    {ok, unmarshal(Result)};
unmarshal_history_result(Error) ->
    Error.

% %% Event provider callbacks

-include("customer_events.hrl").

-spec publish_event(customer_id(), customer_event()) ->
    hg_event_provider:public_event().
publish_event(CustomerID, Changes) when is_list(Changes) ->
    {{customer_id, CustomerID}, ?customer_event(unmarshal({list, changes}, Changes))}.

%% hg_machine callbacks

-spec namespace() ->
    hg_machine:ns().
namespace() ->
    ?NS.

-spec init(customer_id(), customer_params()) ->
    hg_machine:result(customer_event()).
init(CustomerID, CustomerParams) ->
    Customer = create_customer(CustomerID, CustomerParams),
    handle_result(#{
        changes => [?customer_created(Customer)],
        state   => #st{}
    }).

-spec process_signal(hg_machine:signal(), hg_machine:history(customer_event())) ->
    hg_machine:result(customer_event()).
process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(History))).

-spec handle_signal(atom(), st()) ->
    _Result.
handle_signal(timeout, _St = #st{}) ->
    ok.

-type call() :: {start_binding, binding_params()}
              | delete.

-spec process_call(call(), hg_machine:history(customer_event())) ->
    {hg_machine:response(), hg_machine:result(customer_event())}.
process_call(Call, History) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

-spec handle_call(call(), st()) ->
    _Result.
handle_call(delete, St) ->
    #{
        response => ok,
        changes  => [?customer_deleted()],
        state    => St
    };
handle_call({start_binding, BindingParams}, St) ->
    start_binding(BindingParams, St);
handle_call(_Call, _St) ->
    not_implemented.

-spec handle_result(_Result) ->
    {hg_machine:response(), hg_machine:result(customer_event())}.
handle_result(#{state := St} = Params) ->
    Changes = maps:get(changes, Params, []),
    Action = maps:get(action, Params, hg_machine_action:new()),
    ok = log_changes(Changes, St),
    case maps:get(response, Params, undefined) of
        undefined ->
            {[marshal(Changes)], Action};
        Response ->
            {{ok, Response}, {[marshal(Changes)], Action}}
    end.

% %%

-spec start_binding(binding_params(), st()) ->
    _Result.
start_binding(BindingParams, St) ->
    BindingID = create_binding_id(St),
    % Opts = get_binding_opts(St),

    {Binding, {Changes, Action}} = init_binding(BindingID, BindingParams),
    #{
        response => Binding,
        changes  => wrap_binding_changes(BindingID, Changes),
        action   => Action,
        state    => St
    }.

-spec init_binding(binding_id(), binding_params()) ->
    {binding(), {_Changes, _Action}}.
init_binding(BindingID, BindingParams) ->
    % RecPaymentTool = hg_payment_processing:start_rec_payment_tool(),
    % RecPaymentToolID = get_rec_payment_tool_id(RecPaymentTool),
    RecPaymentToolID = 1,
    Binding = create_binding(BindingID, BindingParams, RecPaymentToolID),
    Changes = [?customer_binding_pending()],
    Action = hg_machine_action:new(),
    {Binding, {Changes, Action}}.

-spec create_binding(binding_id(), binding_params(), rec_payment_tool_id()) ->
    binding().
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
    lager:info("Bindings ~p ~p", [Bindings, length(Bindings)]),
    integer_to_binary(length(Bindings) + 1).

% -spec get_binding_opts(st()) ->
%     map().
% get_binding_opts(St = #st{customer = Customer}) ->
%     #{
%         party    => get_party(get_party_id(St)),
%         customer => Customer
%     }.

-spec wrap_binding_changes(binding_id(), [binding_event()]) ->
    customer_event().
wrap_binding_changes(BindingID, Changes) ->
    [?customer_binding_changed(BindingID, C) || C <- Changes].

%%

-spec create_customer(customer_id(), customer_params()) ->
    customer().
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

-spec collapse_history([hg_machine:event(customer_event())]) ->
    st().
collapse_history(History) ->
    lager:info("kek ~p", [History]),
    lists:foldl(
        fun ({_ID, _, Changes}, St0) ->
            lists:foldl(fun merge_change/2, St0, Changes)
        end,
        #st{},
        History
    ).

-spec merge_change(term(), st()) ->
    st().
merge_change(?customer_created(Customer), St) ->
    St#st{customer = Customer};
merge_change(?customer_deleted(), St = #st{customer = C}) ->
    St#st{customer = C#payproc_Customer{status = ?customer_unready()}}.

-spec get_party_id(st()) ->
    party_id().
get_party_id(#st{customer = #payproc_Customer{owner_id = PartyID}}) ->
    PartyID.

-spec get_shop_id(st()) ->
    shop_id().
get_shop_id(#st{customer = #payproc_Customer{shop_id = ShopID}}) ->
    ShopID.

-spec get_customer_id(st()) ->
    customer_id().
get_customer_id(#st{customer = #payproc_Customer{id = ID}}) ->
    ID.

-spec get_customer(st()) ->
    customer().
get_customer(#st{customer = Customer}) ->
    Customer.


% %% Validators and stuff

-spec validate_customer_params(term()) ->
    ok.
validate_customer_params(#payproc_CustomerParams{}) ->
    ok.

-spec assert_customer_operable(st()) ->
    ok.
assert_customer_operable(St = #st{}) ->
    Party = get_party(get_party_id(St)),
    Shop  = hg_party:get_shop(get_shop_id(St), Party),
    ok    = assert_party_shop_operable(Shop, Party),
    ok.

-spec assert_party_shop_operable(shop(), party()) ->
    ok.
assert_party_shop_operable(Shop, Party) ->
    ok = assert_party_operable(Party),
    ok = assert_shop_operable(Shop),
    ok.

-spec ensure_shop_exists(shop()) ->
    shop().
ensure_shop_exists(Shop) ->
    Shop = hg_invoice_utils:assert_shop_exists(Shop),
    Shop.

-spec assert_party_operable(party()) ->
    ok.
assert_party_operable(Party) ->
    Party = hg_invoice_utils:assert_party_operable(Party),
    ok.

-spec assert_shop_operable(shop()) ->
    ok.
assert_shop_operable(Shop) ->
    Shop = hg_invoice_utils:assert_shop_operable(Shop),
    ok.

assert_customer_not_deleted({_, _, [?customer_deleted()]}) ->
    throw(#payproc_CustomerNotFound{});
assert_customer_not_deleted(_) ->
    ok.

% %%

-spec log_changes([customer_event()], st()) ->
    ok.
log_changes(Changes, St) ->
    lists:foreach(fun (C) -> log_change(C, St) end, Changes).

-spec log_change(customer_event(), st()) ->
    ok.
log_change(Change, St) ->
    case get_log_params(Change, St) of
        {ok, #{type := Type, params := Params, message := Message}} ->
            _ = lager:log(info, [{Type, Params}], Message),
            ok;
        undefined ->
            ok
    end.

-spec get_log_params(customer_event(), st()) ->
    tuple().
get_log_params(?customer_created(Customer), _St) ->
    get_customer_event_log(customer_created, unready, Customer);
get_log_params(?customer_deleted(), #st{customer = Customer}) ->
    get_customer_event_log(customer_deleted, unready, Customer).

-spec get_customer_event_log(atom(), atom(), customer()) ->
    tuple().
get_customer_event_log(EventType, StatusName, Customer) ->
    {ok, #{
        type    => customer_event,
        params  => [{type, EventType}, {status, StatusName} | get_customer_params(Customer)],
        message => get_message(EventType)
    }}.

-spec get_customer_params(customer()) ->
    list().
get_customer_params(Customer) ->
    #payproc_Customer{
        id             = ID,
        owner_id       = PartyID,
        shop_id        = ShopID,
        status         = Status,
        created_at     = CreatedAt,
        bindings       = Bindings,
        metadata       = Metadata,
        active_binding = ActiveBinding
    } = Customer,
    [{id, ID}, {party_id, PartyID}, {shop_id, ShopID}, {status, Status}, {created_at, CreatedAt},
     {bindings, Bindings}, {metadata, Metadata}, {active_binding, ActiveBinding}].

-spec get_message(atom()) ->
    string().
get_message(customer_created) ->
    "Customer is created";
get_message(_EventType) ->
    "Unknown event type".

%%
%% Marshalling
%%

marshal(Changes) when is_list(Changes) ->
    [marshal(change, Change) || Change <- Changes].

%% Changes

marshal(change, ?customer_created(Customer)) ->
    #{
        <<"change">>    => <<"created">>,
        <<"customer">>   => marshal(customer, Customer)
    };
marshal(change, ?customer_deleted()) ->
    #{
        <<"change">>    => <<"customer_deleted">>
    };

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
        <<"active_binding">> => marshal(binding, Customer#payproc_Customer.active_binding)
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
    <<"customer_binding_pending">>;
marshal(binding_status, ?customer_binding_succeeded()) ->
    <<"customer_binding_succeeded">>;
marshal(binding_status, ?customer_binding_failed(Failure)) ->
    [
        <<"customer_binding_succeeded">>,
        marshal(failure, Failure)
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

%% Unmarshalling

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, changes}, Payload)}.

unmarshal({list, changes}, Changes) when is_list(Changes) ->
    [unmarshal(change, Change) || Change <- Changes];

%% Changes

unmarshal(change, #{
    <<"change">>    := <<"created">>,
    <<"customer">>  := Customer
}) ->
    ?customer_created(unmarshal(customer, Customer));

unmarshal(change, #{
    <<"change">> := <<"customer_deleted">>
}) ->
    ?customer_deleted();

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
        active_binding = unmarshal(binding, ActiveBinding)
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

unmarshal(binding_status, <<"customer_binding_pending">>) ->
    ?customer_binding_pending();
unmarshal(binding_status, <<"customer_binding_succeeded">>) ->
    ?customer_binding_succeeded();
unmarshal(binding_status, [
    <<"customer_binding_succeeded">>,
    Failure
]) ->
    ?customer_binding_failed(unmarshal(failure, Failure));

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
