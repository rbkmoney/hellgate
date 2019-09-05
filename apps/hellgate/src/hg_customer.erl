%%%
%%% Customer machine
%%%

-module(hg_customer).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
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

-define(SYNC_INTERVAL, 5).
-define(SYNC_OUTDATED_INTERVAL, 86400).  % 1 day
-define(REC_PAYTOOL_EVENTS_LIMIT, 10).
-define(MAX_BINDING_DURATION, #'TimeSpan'{hours = 3}).

-record(st, {
    customer       :: undefined | customer(),
    active_binding :: undefined | binding_id(),
    binding_starts :: map(),
    created_at     :: undefined | hg_datetime:timestamp()
}).

-type customer()         :: dmsl_payment_processing_thrift:'Customer'().
-type customer_id()      :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer_params()  :: dmsl_payment_processing_thrift:'CustomerParams'().
-type customer_change()  :: dmsl_payment_processing_thrift:'CustomerChange'().
-type binding_id()       :: dmsl_payment_processing_thrift:'CustomerBindingID'().

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
    DomainRevison = hg_domain:head(),
    CustomerID = hg_utils:unique_id(),
    ok = set_meta(CustomerID),
    PartyID = CustomerParams#payproc_CustomerParams.party_id,
    ShopID = CustomerParams#payproc_CustomerParams.shop_id,
    ok = assert_party_accessible(PartyID),
    Party = hg_party:get_party(PartyID),
    Shop = ensure_shop_exists(hg_party:get_shop(ShopID, Party)),
    ok = assert_party_shop_operable(Shop, Party),
    _ = hg_recurrent_paytool:assert_operation_permitted(Shop, Party, DomainRevison),
    ok = start(CustomerID, CustomerParams),
    get_customer(get_state(CustomerID));

handle_function_('Get', [CustomerID], _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_accessible(St),
    get_customer(St);

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
    get_public_history(CustomerID, Range);

handle_function_(Fun, [CustomerID | _Tail] = Args, _Opts) when
    Fun =:= 'Delete' orelse
    Fun =:= 'StartBinding'
->
    ok = set_meta(CustomerID),
    call(CustomerID, Fun, Args).

%%

set_meta(ID) ->
    scoper:add_meta(#{customer_id => ID}).

get_history(Ref) ->
    History = hg_machine:get_history(?NS, Ref),
    unmarshal_history(map_history_error(History)).

get_history(Ref, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, Ref, AfterID, Limit),
    unmarshal_history(map_history_error(History)).

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
start(ID, Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'CustomerParams'}},
    EncodedParams = hg_proto_utils:serialize(Type, Params),
    map_start_error(hg_machine:start(?NS, ID, EncodedParams)).

call(ID, Function, Args) ->
    case hg_machine:thrift_call(?NS, ID, customer_management, {'CustomerManagement', Function}, Args) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {exception, Exception} ->
            erlang:throw(Exception);
        {error, Error} ->
            map_error(Error)
    end.

-spec map_error(notfound | any()) ->
    no_return().
map_error(notfound) ->
    throw(#payproc_InvoiceTemplateNotFound{});
map_error(Reason) ->
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

-spec publish_event(customer_id(), hg_machine:event_payload()) ->
    hg_event_provider:public_event().
publish_event(CustomerID, Payload) ->
    {{customer_id, CustomerID}, ?customer_event(unmarshal_event_payload(Payload))}.

%%
%% hg_machine callbacks
%%

-spec namespace() ->
    hg_machine:ns().
namespace() ->
    ?NS.

-spec init(binary(), hg_machine:machine()) ->
    hg_machine:result().
init(EncodedParams, #{id := CustomerID}) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'CustomerParams'}},
    CustomerParams = hg_proto_utils:deserialize(Type, EncodedParams),
    handle_result(#{
        changes => [
            get_customer_created_event(CustomerID, CustomerParams)
        ],
        auxst => #{}
    }).

-spec process_signal(hg_machine:signal(), hg_machine:machine()) ->
    hg_machine:result().
process_signal(Signal, #{history := History,  aux_state := AuxSt}) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal_history(History)), unmarshal(auxst, AuxSt))).

handle_signal(timeout, St0, AuxSt0) ->
    {Changes, AuxSt1} = sync_pending_bindings(St0, AuxSt0),
    St1 = merge_changes(Changes, St0),
    Action = case detect_binding_status(St1) of
        all_ready ->
            hg_machine_action:new();
        waiting ->
            set_event_poll_timer(actual);
        waiting_outdated ->
            set_event_poll_timer(outdated)
    end,
    #{
        changes => Changes ++ get_ready_changes(St1),
        action  => Action,
        auxst   => AuxSt1
    }.

detect_binding_status(St) ->
    case get_pending_binding_set(St) of
        [] ->
            all_ready;
        Pending ->
            case get_outdated_binding_set(St) of
                Outdated when Outdated =:= Pending ->
                    waiting_outdated;
                _Outdated ->
                    waiting
            end
    end.

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

-type call() :: hg_machine:thrift_call().

-spec process_call(call(), hg_machine:machine()) ->
    {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try
        handle_result(handle_call(Call, St))
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

handle_call({{'CustomerManagement', 'Delete'}, [_CustomerID]}, St) ->
    ok = assert_customer_operable(St),
    #{
        response => ok,
        changes  => [?customer_deleted()]
    };
handle_call({{'CustomerManagement', 'StartBinding'}, [_CustomerID, BindingParams]}, St) ->
    ok = assert_customer_operable(St),
    start_binding(BindingParams, St).

handle_result(Params) ->
    Result = handle_aux_state(Params, handle_result_changes(Params, handle_result_action(Params, #{}))),
    case maps:find(response, Params) of
        {ok, ok} ->
            {ok, Result};
        {ok, {ok, _Reply} = Response} ->
            {Response, Result};
        error ->
            Result
    end.

handle_aux_state(#{auxst := AuxSt}, Acc) ->
    Acc#{auxst => marshal(auxst, AuxSt)};
handle_aux_state(#{}, Acc) ->
    Acc.

handle_result_changes(#{changes := Changes = [_ | _]}, Acc) ->
    Acc#{events => [marshal_event_payload(Changes)]};
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
    PaytoolID = hg_utils:unique_id(),
    DomainRevision = hg_domain:head(),
    PartyID = get_party_id(St),
    PartyRevision = hg_party:get_party_revision(PartyID),
    Binding = construct_binding(BindingID, PaytoolID, PaymentResource, PartyRevision, DomainRevision),
    PaytoolParams = create_paytool_params(Binding, St),
    _ = validate_paytool_params(PaytoolParams),
    Changes = [?customer_binding_changed(BindingID, ?customer_binding_started(Binding, hg_datetime:format_now()))],
    {ok, _} = create_recurrent_paytool(PaytoolParams), %% @TODO: Remove after migration
    #{
        response => {ok, Binding},
        changes  => Changes,
        action   => set_event_poll_timer(actual) %% @TODO: Remove after migration
    }.

validate_paytool_params(PaytoolParams) ->
    try
        ok = hg_recurrent_paytool:validate_paytool_params(PaytoolParams)
    catch
        throw:(Exception = #payproc_InvalidUser{}) ->
            throw(Exception);
        throw:(Exception = #payproc_InvalidPartyStatus{}) ->
            throw(Exception);
        throw:(Exception = #payproc_InvalidShopStatus{}) ->
            throw(Exception);
        throw:(Exception = #payproc_InvalidContractStatus{}) ->
            throw(Exception);
        throw:(Exception = #payproc_OperationNotPermitted{}) ->
            throw(Exception);
        throw:(#payproc_InvalidPaymentMethod{}) ->
            throw(#payproc_OperationNotPermitted{})
    end.

construct_binding(BindingID, RecPaymentToolID, PaymentResource, PartyRevision, DomainRevision) ->
    #payproc_CustomerBinding{
        id                  = BindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource    = PaymentResource,
        status              = ?customer_binding_pending(),
        party_revision      = PartyRevision,
        domain_revision     = DomainRevision
    }.

create_binding_id(St) ->
    integer_to_binary(length(get_bindings(get_customer(St))) + 1).

sync_pending_bindings(St, AuxSt) ->
    sync_pending_bindings(get_pending_binding_set(St), St, AuxSt).

sync_pending_bindings([BindingID | Rest], St, AuxSt0) ->
    Binding = try_get_binding(BindingID, get_customer(St)),
    {Changes1, AuxSt1} = sync_binding_state(Binding, St, AuxSt0),
    {Changes2, AuxSt2} = sync_pending_bindings(Rest, St, AuxSt1),
    {Changes1 ++ Changes2, AuxSt2};
sync_pending_bindings([], _St, AuxSt) ->
    {[], AuxSt}.

sync_binding_state(Binding, St, AuxSt) ->
    RecurrentPaytoolID = get_binding_recurrent_paytool_id(Binding),
    LastEventID0 = get_binding_last_event_id(Binding, AuxSt),
    case get_recurrent_paytool_changes(RecurrentPaytoolID, LastEventID0) of
        {ok, {RecurrentPaytoolChanges, LastEventID1}} ->
            BindingChanges = produce_binding_changes(RecurrentPaytoolChanges, Binding),
            {wrap_binding_changes(get_binding_id(Binding), BindingChanges), update_aux_state(LastEventID1, Binding, AuxSt)};
        {error, paytool_not_found} -> % lazily create paytool
            PaytoolParams = create_paytool_params(Binding, St),
            {ok, _} = create_recurrent_paytool(PaytoolParams),
            {[], AuxSt}
    end.

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

create_paytool_params(
    #payproc_CustomerBinding{
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource    = PaymentResource,
        party_revision      = PartyRevision,
        domain_revision     = DomainRevision
    },
    St
) ->
    #payproc_RecurrentPaymentToolParams{
        id               = RecPaymentToolID,
        party_id         = get_party_id(St),
        party_revision   = PartyRevision,
        domain_revision  = DomainRevision,
        shop_id          = get_shop_id(St),
        payment_resource = PaymentResource
    }.

create_recurrent_paytool(Params) ->
    issue_recurrent_paytools_call('Create', [Params]).

get_recurrent_paytool_events(RecurrentPaytoolID, EventRange) ->
    issue_recurrent_paytools_call('GetEvents', [RecurrentPaytoolID, EventRange]).

get_recurrent_paytool_changes(RecurrentPaytoolID, LastEventID) ->
    EventRange = construct_event_range(LastEventID),
    case get_recurrent_paytool_events(RecurrentPaytoolID, EventRange) of
        {ok, Events} ->
            {ok, {gather_recurrent_paytool_changes(Events), get_last_event_id(Events)}};
        {exception, #payproc_RecurrentPaymentToolNotFound{}} ->
            {error, paytool_not_found}
    end.

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

set_event_poll_timer(Type) ->
    hg_machine_action:set_timeout(get_event_pol_timeout(Type)).

get_event_pol_timeout(actual) ->
    ?SYNC_INTERVAL;
get_event_pol_timeout(outdated) ->
    ?SYNC_OUTDATED_INTERVAL - rand:uniform(?SYNC_OUTDATED_INTERVAL div 10).

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
    lists:foldl(fun merge_event/2, #st{binding_starts = #{}}, History).

merge_event({_ID, _, Changes}, St) ->
    merge_changes(Changes, St).

merge_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

merge_change(?customer_created(_, _, _, _, _, CreatedAt) = CustomerCreatedChange, St) ->
    Customer = create_customer(CustomerCreatedChange),
    St2 = set_customer(Customer, St),
    set_create_customer_timestamp(CreatedAt, St2);
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
    update_bindigs_start(BindingID, Payload, St2).

update_active_binding(BindingID, ?customer_binding_succeeded(), St) ->
    set_active_binding_id(BindingID, St);
update_active_binding(_BindingID, _BindingStatus, St) ->
    St.

update_bindigs_start(BindingID, ?customer_binding_started(_Binding, Timestamp), St) ->
    #st{binding_starts = Starts} = St,
    St#st{binding_starts = Starts#{BindingID => Timestamp}};
update_bindigs_start(_BindingID, _OtherChange, St) ->
    St.

wrap_binding_changes(BindingID, Changes) ->
    [?customer_binding_changed(BindingID, C) || C <- Changes].

merge_binding_changes(Changes, Binding) ->
    lists:foldl(fun merge_binding_change/2, Binding, Changes).

merge_binding_change(?customer_binding_started(Binding, _Timestamp), undefined) ->
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

set_create_customer_timestamp(CreatedAt, St = #st{}) ->
    St#st{created_at = CreatedAt}.

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

get_outdated_binding_set(St) ->
    Bindings = get_bindings(get_customer(St)),
    [get_binding_id(Binding) ||
        Binding <- Bindings, is_binding_outdated(Binding, St) =:= true
    ].

is_binding_outdated(#payproc_CustomerBinding{id = BindingId, status = ?customer_binding_pending()}, St) ->
    BindingStart = get_binding_start_timestamp(BindingId, St),
    Now = hg_datetime:format_now(),
    Deadline = hg_datetime:add_time_span(?MAX_BINDING_DURATION, BindingStart),
    case hg_datetime:compare(Now, Deadline) of
        later ->
            true;
        _Other ->
            false
    end;
is_binding_outdated(_Bindinf, _St) ->
    false.

get_binding_id(#payproc_CustomerBinding{id = BindingID}) ->
    BindingID.

get_binding_status(#payproc_CustomerBinding{status = Status}) ->
    Status.

get_binding_start_timestamp(BindingId, #st{binding_starts = Starts} = St) ->
    BindingStart = maps:get(BindingId, Starts),
    % Old bindings has `undefined` start timestamp.
    % Using customer create timestamp instead.
    case BindingStart of
        undefined ->
            St#st.created_at;
        _ ->
            BindingStart
    end.

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

get_active_binding_id(#st{customer = #payproc_Customer{active_binding_id = BindingID}}) ->
    BindingID.

set_active_binding_id(BindingID, St = #st{customer = Customer}) ->
    St#st{customer = Customer#payproc_Customer{active_binding_id = BindingID}}.

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
    Party = hg_party:get_party(get_party_id(St)),
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

-define(BINARY_BINDING_STATUS_PENDING, <<"pending">>).
-define(BINARY_BINDING_STATUS_SUCCEEDED, <<"succeeded">>).
-define(BINARY_BINDING_STATUS_FAILED(Failure), [<<"failed">>, Failure]).

-spec marshal_event_payload([customer_change()]) ->
    hg_machine:event_payload().
marshal_event_payload(Changes) ->
    #{format_version => undefined, data => marshal({list, change}, Changes)}.

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
        payment_resource    = PaymentResource,
        status              = Status,
        party_revision      = PartyRevision,
        domain_revision     = DomainRevision
    }
) ->
    #{
        <<"id">>              => marshal(str              , ID),
        <<"recpaytool_id">>   => marshal(str              , RecPaymentToolID),
        <<"payresource">>     => marshal(payment_resource , PaymentResource),
        <<"status">>          => marshal(binding_status   , Status),
        <<"party_revision">>  => marshal(int              , PartyRevision),
        <<"domain_revision">> => marshal(int              , DomainRevision)
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
    ?BINARY_BINDING_STATUS_PENDING;
marshal(binding_status, ?customer_binding_succeeded()) ->
    ?BINARY_BINDING_STATUS_SUCCEEDED;
marshal(binding_status, ?customer_binding_failed(Failure)) ->
    ?BINARY_BINDING_STATUS_FAILED(
        marshal(failure, Failure)
    );

marshal(binding_change_payload, ?customer_binding_started(CustomerBinding, Timestamp)) ->
    [
        <<"started">>,
        marshal(binding, CustomerBinding),
        marshal(str, Timestamp)
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

marshal(sub_failure, undefined) ->
    undefined;
marshal(sub_failure, #domain_SubFailure{} = SubFailure) ->
    genlib_map:compact(#{
        <<"code">> => marshal(str        , SubFailure#domain_SubFailure.code),
        <<"sub" >> => marshal(sub_failure, SubFailure#domain_SubFailure.sub )
    });

marshal(failure, {operation_timeout, _}) ->
    [1, <<"operation_timeout">>];
marshal(failure, {failure, #domain_Failure{} = Failure}) ->
    [1, [<<"failure">>, genlib_map:compact(#{
        <<"code"  >> => marshal(str        , Failure#domain_Failure.code  ),
        <<"reason">> => marshal(str        , Failure#domain_Failure.reason),
        <<"sub"   >> => marshal(sub_failure, Failure#domain_Failure.sub   )
    })]];

marshal(metadata, Metadata) ->
    hg_msgpack_marshalling:marshal(json, Metadata);

marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

-spec unmarshal_history([hg_machine:event()]) ->
    [hg_machine:event([customer_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) ->
    hg_machine:event([customer_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) ->
    [customer_change()].
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, change}, Changes).

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
        <<"id">>              := ID,
        <<"recpaytool_id">>   := RecPaymentToolID,
        <<"payresource">>     := PaymentResource
    } = Binding
) ->
    Status = maps:get(<<"status">>, Binding, ?BINARY_BINDING_STATUS_PENDING),
    PartyRevision = maps:get(<<"party_revision">>, Binding, undefined),
    DomainRevision = maps:get(<<"domain_revision">>, Binding, undefined),
    #payproc_CustomerBinding{
        id                  = unmarshal(str             , ID),
        rec_payment_tool_id = unmarshal(str             , RecPaymentToolID),
        payment_resource    = unmarshal(payment_resource, PaymentResource),
        status              = unmarshal(binding_status  , Status),
        party_revision      = unmarshal(int             , PartyRevision),
        domain_revision     = unmarshal(int             , DomainRevision)
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

unmarshal(binding_status, ?BINARY_BINDING_STATUS_PENDING) ->
    ?customer_binding_pending();
unmarshal(binding_status, ?BINARY_BINDING_STATUS_SUCCEEDED) ->
    ?customer_binding_succeeded();
unmarshal(binding_status, ?BINARY_BINDING_STATUS_FAILED(Failure)) ->
    ?customer_binding_failed(unmarshal(failure, Failure));

unmarshal(binding_change_payload, [<<"started">>, Binding]) ->
    ?customer_binding_started(unmarshal(binding, Binding), undefined);
unmarshal(binding_change_payload, [<<"started">>, Binding, Timestamp]) ->
    ?customer_binding_started(unmarshal(binding, Binding), Timestamp);
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

unmarshal(sub_failure, undefined) ->
    undefined;
unmarshal(sub_failure, #{<<"code">> := Code} = SubFailure) ->
    #domain_SubFailure{
        code   = unmarshal(str        , Code),
        sub    = unmarshal(sub_failure, maps:get(<<"sub">>, SubFailure, undefined))
    };

unmarshal(failure, [1, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, [<<"failure">>, #{<<"code">> := Code} = Failure]]) ->
    {failure, #domain_Failure{
        code   = unmarshal(str        , Code),
        reason = unmarshal(str        , maps:get(<<"reason">>, Failure, undefined)),
        sub    = unmarshal(sub_failure, maps:get(<<"sub"   >>, Failure, undefined))
    }};

unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {failure, #domain_Failure{
        code   = unmarshal(str, Code),
        reason = unmarshal(str, Description)
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

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec event_pol_timer_test() -> _.
event_pol_timer_test() ->
    ?assertEqual(get_event_pol_timeout(actual), ?SYNC_INTERVAL),
    ?assert(get_event_pol_timeout(outdated) =< ?SYNC_OUTDATED_INTERVAL),
    ?assert(get_event_pol_timeout(outdated) >= ?SYNC_OUTDATED_INTERVAL * 0.9).

-endif.
