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

-export([namespace/0]).
-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).
-export([process_repair/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%% Types

-define(SYNC_INTERVAL, 5).
% 1 day
-define(SYNC_OUTDATED_INTERVAL, 60 * 60 * 24).
-define(REC_PAYTOOL_EVENTS_LIMIT, 10).
-define(MAX_BINDING_DURATION, 60 * 60 * 3).

-record(st, {
    customer :: undefined | customer(),
    active_binding :: undefined | binding_id(),
    binding_activity :: #{binding_id() => integer()},
    created_at :: undefined | hg_datetime:timestamp()
}).

-type customer() :: dmsl_payment_processing_thrift:'Customer'().
-type customer_id() :: dmsl_payment_processing_thrift:'CustomerID'().
-type customer_params() :: dmsl_payment_processing_thrift:'CustomerParams'().
-type customer_change() :: dmsl_payment_processing_thrift:'CustomerChange'().
-type binding_id() :: dmsl_payment_processing_thrift:'CustomerBindingID'().

%%
%% Woody handler
%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        customer_management,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('Create', {CustomerParams}, _Opts) ->
    DomainRevision = hg_domain:head(),
    CustomerID = hg_utils:unique_id(),
    ok = set_meta(CustomerID),
    PartyID = CustomerParams#payproc_CustomerParams.party_id,
    ShopID = CustomerParams#payproc_CustomerParams.shop_id,
    ok = assert_party_accessible(PartyID),
    Party = hg_party:get_party(PartyID),
    Shop = ensure_shop_exists(hg_party:get_shop(ShopID, Party)),
    ok = assert_party_shop_operable(Shop, Party),
    _ = hg_recurrent_paytool:assert_operation_permitted(Shop, Party, DomainRevision),
    ok = start(CustomerID, CustomerParams),
    get_customer(get_state(CustomerID));
%% TODO Удалить после перехода на новый протокол
handle_function_('Get', {CustomerID, undefined}, _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_accessible(St),
    get_customer(St);
handle_function_('Get', {CustomerID, #payproc_EventRange{'after' = AfterID, limit = Limit}}, _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID, AfterID, Limit),
    ok = assert_customer_accessible(St),
    get_customer(St);
handle_function_('GetActiveBinding', {CustomerID}, _Opts) ->
    ok = set_meta(CustomerID),
    St = get_state(CustomerID),
    ok = assert_customer_accessible(St),
    case try_get_active_binding(St) of
        Binding = #payproc_CustomerBinding{} ->
            Binding;
        undefined ->
            throw(?invalid_customer_status(get_customer_status(get_customer(St))))
    end;
handle_function_('GetEvents', {CustomerID, Range}, _Opts) ->
    ok = set_meta(CustomerID),
    ok = assert_customer_accessible(get_initial_state(CustomerID)),
    get_public_history(CustomerID, Range);
handle_function_(Fun, Args, _Opts) when
    Fun =:= 'Delete' orelse
        Fun =:= 'StartBinding'
->
    CustomerID = element(1, Args),
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

get_state(Ref, AfterID, Limit) ->
    collapse_history(get_history(Ref, AfterID, Limit)).

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

-spec start(customer_id(), customer_params()) -> ok | no_return().
start(ID, Params) ->
    EncodedParams = marshal_customer_params(Params),
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

-spec map_error(notfound | any()) -> no_return().
map_error(notfound) ->
    throw(#payproc_CustomerNotFound{});
map_error(Reason) ->
    error(Reason).

-spec map_history_error({ok, _Result} | {error, _Error}) -> _Result | no_return().
map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_CustomerNotFound{}).

-spec map_start_error({ok, term()} | {error, _Error}) -> ok | no_return().
map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

%%
%% Event provider callbacks
%%

-spec publish_event(customer_id(), hg_machine:event_payload()) -> hg_event_provider:public_event().
publish_event(CustomerID, Payload) ->
    {{customer_id, CustomerID}, ?customer_event(unmarshal_event_payload(Payload))}.

%%
%% hg_machine callbacks
%%

-spec namespace() -> hg_machine:ns().
namespace() ->
    ?NS.

-spec init(binary(), hg_machine:machine()) -> hg_machine:result().
init(EncodedParams, #{id := CustomerID}) ->
    CustomerParams = unmarshal_customer_params(EncodedParams),
    handle_result(#{
        changes => [
            get_customer_created_event(CustomerID, CustomerParams)
        ],
        auxst => #{}
    }).

-spec process_repair(hg_machine:args(), hg_machine:machine()) -> no_return().
process_repair(_, _) ->
    erlang:error({not_implemented, repair}).

-spec process_signal(hg_machine:signal(), hg_machine:machine()) -> hg_machine:result().
process_signal(Signal, #{history := History, aux_state := AuxSt}) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal_history(History)), unmarshal(auxst, AuxSt))).

handle_signal(timeout, St0, AuxSt0) ->
    {Changes, St1, AuxSt1} = sync_pending_bindings(St0, AuxSt0),
    St2 = merge_changes(Changes, St1),
    Action =
        case detect_pending_waiting(St2) of
            all_ready ->
                hg_machine_action:new();
            {waiting, WaitingTime} ->
                hg_machine_action:set_timeout(get_event_pol_timeout(WaitingTime))
        end,
    #{
        changes => Changes ++ get_ready_changes(St2),
        action => Action,
        auxst => AuxSt1
    }.

detect_pending_waiting(State) ->
    Pending = get_pending_binding_set(State),
    PendingActivityTime = lists:foldl(
        fun
            (BindingID, undefined) ->
                get_binding_activity_time(BindingID, State);
            (BindingID, LastEventTime) ->
                EventTime = get_binding_activity_time(BindingID, State),
                erlang:max(LastEventTime, EventTime)
        end,
        undefined,
        Pending
    ),
    case PendingActivityTime of
        undefined ->
            all_ready;
        LastEventTime ->
            Now = os:system_time(second),
            {waiting, Now - LastEventTime}
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

-spec process_call(call(), hg_machine:machine()) -> {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try
        handle_result(handle_call(Call, St))
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

handle_call({{'CustomerManagement', 'Delete'}, {_CustomerID}}, St) ->
    ok = assert_customer_operable(St),
    #{
        response => ok,
        changes => [?customer_deleted()]
    };
handle_call({{'CustomerManagement', 'StartBinding'}, {_CustomerID, BindingParams}}, St) ->
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
    #payproc_CustomerBindingParams{
        customer_binding_id = MaybeBindingID,
        payment_resource = PaymentResource,
        rec_payment_tool_id = MaybePaytoolID
    } = BindingParams,
    BindingID = hg_utils:uid(MaybeBindingID),
    PaytoolID = hg_utils:uid(MaybePaytoolID),
    DomainRevision = hg_domain:head(),
    PartyID = get_party_id(St),
    PartyRevision = hg_party:get_party_revision(PartyID),
    Binding = construct_binding(BindingID, PaytoolID, PaymentResource, PartyRevision, DomainRevision),
    PaytoolParams = create_paytool_params(Binding, St),
    _ = validate_paytool_params(PaytoolParams),
    Changes = [?customer_binding_changed(BindingID, ?customer_binding_started(Binding, hg_datetime:format_now()))],
    #{
        response => {ok, Binding},
        changes => Changes,
        action => hg_machine_action:instant()
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
        id = BindingID,
        rec_payment_tool_id = RecPaymentToolID,
        payment_resource = PaymentResource,
        status = ?customer_binding_pending(),
        party_revision = PartyRevision,
        domain_revision = DomainRevision
    }.

sync_pending_bindings(St, AuxSt) ->
    sync_pending_bindings(get_pending_binding_set(St), St, AuxSt).

sync_pending_bindings([BindingID | Rest], St0, AuxSt0) ->
    Binding = try_get_binding(BindingID, get_customer(St0)),
    {Changes1, St1, AuxSt1} = sync_binding_state(Binding, St0, AuxSt0),
    {Changes2, St2, AuxSt2} = sync_pending_bindings(Rest, St1, AuxSt1),
    {Changes1 ++ Changes2, St2, AuxSt2};
sync_pending_bindings([], St, AuxSt) ->
    {[], St, AuxSt}.

sync_binding_state(Binding, St, AuxSt) ->
    BindingID = get_binding_id(Binding),
    RecurrentPaytoolID = get_binding_recurrent_paytool_id(Binding),
    LastEventID0 = get_binding_last_event_id(BindingID, AuxSt),
    case get_recurrent_paytool_changes(RecurrentPaytoolID, LastEventID0) of
        {ok, {RecurrentPaytoolChanges, LastEventID1, LastEventTime}} ->
            BindingChanges = produce_binding_changes(RecurrentPaytoolChanges, Binding),
            WrappedChanges = wrap_binding_changes(BindingID, BindingChanges),
            UpdatedAuxState = update_aux_state(LastEventID1, BindingID, AuxSt),
            UpdatedState = update_binding_activity(BindingID, LastEventTime, St),
            {WrappedChanges, UpdatedState, UpdatedAuxState};
        % lazily create paytool
        {error, paytool_not_found} ->
            PaytoolParams = create_paytool_params(Binding, St),
            {ok, _} = create_recurrent_paytool(PaytoolParams),
            {[], St, AuxSt}
    end.

update_aux_state(undefined, _BindingID, AuxSt) ->
    AuxSt;
update_aux_state(LastEventID, BindingID, AuxSt) ->
    maps:put(BindingID, LastEventID, AuxSt).

get_binding_last_event_id(BindingID, AuxSt) ->
    maps:get(BindingID, AuxSt, undefined).

produce_binding_changes([RecurrentPaytoolChange | Rest], Binding) ->
    Changes = produce_binding_changes_(RecurrentPaytoolChange, Binding),
    Changes ++ produce_binding_changes(Rest, merge_binding_changes(Changes, Binding));
produce_binding_changes([], _Binding) ->
    [].

produce_binding_changes_(?recurrent_payment_tool_has_created(_), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [];
produce_binding_changes_(?recurrent_payment_tool_risk_score_changed(_), Binding) ->
    ok = assert_binding_status(pending, Binding),
    [];
produce_binding_changes_(?recurrent_payment_tool_route_changed(_), Binding) ->
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
        payment_resource = PaymentResource,
        party_revision = PartyRevision,
        domain_revision = DomainRevision
    },
    St
) ->
    #payproc_RecurrentPaymentToolParams{
        id = RecPaymentToolID,
        party_id = get_party_id(St),
        party_revision = PartyRevision,
        domain_revision = DomainRevision,
        shop_id = get_shop_id(St),
        payment_resource = PaymentResource
    }.

create_recurrent_paytool(Params) ->
    issue_recurrent_paytools_call('Create', {Params}).

get_recurrent_paytool_events(RecurrentPaytoolID, EventRange) ->
    issue_recurrent_paytools_call('GetEvents', {RecurrentPaytoolID, EventRange}).

get_recurrent_paytool_changes(RecurrentPaytoolID, AfterEventID) ->
    EventRange = construct_event_range(AfterEventID),
    case get_recurrent_paytool_events(RecurrentPaytoolID, EventRange) of
        {ok, []} ->
            {ok, {[], undefined, undefined}};
        {ok, Events} ->
            #payproc_RecurrentPaymentToolEvent{id = LastEventID, created_at = LastEventTime} = lists:last(Events),
            {ok, {gather_recurrent_paytool_changes(Events), LastEventID, LastEventTime}};
        {exception, #payproc_RecurrentPaymentToolNotFound{}} ->
            {error, paytool_not_found}
    end.

construct_event_range(undefined) ->
    #payproc_EventRange{limit = ?REC_PAYTOOL_EVENTS_LIMIT};
construct_event_range(LastEventID) ->
    #payproc_EventRange{'after' = LastEventID, limit = ?REC_PAYTOOL_EVENTS_LIMIT}.

gather_recurrent_paytool_changes(Events) ->
    lists:flatmap(
        fun(#payproc_RecurrentPaymentToolEvent{payload = Changes}) ->
            Changes
        end,
        Events
    ).

issue_recurrent_paytools_call(Function, Args) ->
    hg_woody_wrapper:call(recurrent_paytool, Function, Args).

get_event_pol_timeout(WaitingTime) ->
    case WaitingTime < app_binding_outdate_timeout() of
        true ->
            Retry = app_binding_max_sync_interval(),
            erlang:min(Retry, erlang:max(1, WaitingTime));
        _ ->
            Retry = app_binding_outdated_sync_interval(),
            Retry - rand:uniform(Retry div 10)
    end.

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
    lists:foldl(fun merge_event/2, #st{binding_activity = #{}}, History).

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
    update_binding_activity(BindingID, Payload, St2).

update_active_binding(BindingID, ?customer_binding_succeeded(), St) ->
    set_active_binding_id(BindingID, St);
update_active_binding(_BindingID, _BindingStatus, St) ->
    St.

update_binding_activity(BindingID, LastEventTime, St) when is_binary(LastEventTime) ->
    EventTime = hg_datetime:parse(LastEventTime, second),
    #st{binding_activity = Bindings} = St,
    St#st{binding_activity = Bindings#{BindingID => EventTime}};
update_binding_activity(BindingID, ?customer_binding_started(_Binding, Timestamp), St) ->
    update_binding_activity(BindingID, Timestamp, St);
update_binding_activity(_BindingID, _OtherChange, St) ->
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
    [
        get_binding_id(Binding)
        || Binding <- Bindings, get_binding_status(Binding) == ?customer_binding_pending()
    ].

get_binding_id(#payproc_CustomerBinding{id = BindingID}) ->
    BindingID.

get_binding_status(#payproc_CustomerBinding{status = Status}) ->
    Status.

get_binding_activity_time(BindingID, #st{binding_activity = Bindings} = St) ->
    case maps:get(BindingID, Bindings, undefined) of
        undefined ->
            % Old bindings has `undefined` start timestamp.
            % Using customer create timestamp instead.
            hg_datetime:parse(St#st.created_at, second);
        EventTime ->
            EventTime
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
    ok = assert_customer_accessible(St),
    Party = hg_party:get_party(get_party_id(St)),
    Shop = hg_party:get_shop(get_shop_id(St), Party),
    ok = assert_party_shop_operable(Shop, Party),
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

%% Config

app_binding_max_sync_interval() ->
    Config = genlib_app:env(hellgate, binding, #{}),
    app_parse_timespan(genlib_map:get(max_sync_interval, Config, ?SYNC_INTERVAL)).

app_binding_outdated_sync_interval() ->
    Config = genlib_app:env(hellgate, binding, #{}),
    app_parse_timespan(genlib_map:get(outdated_sync_interval, Config, ?SYNC_OUTDATED_INTERVAL)).

app_binding_outdate_timeout() ->
    Config = genlib_app:env(hellgate, binding, #{}),
    app_parse_timespan(genlib_map:get(outdate_timeout, Config, ?MAX_BINDING_DURATION)).

app_parse_timespan(Value) when is_integer(Value) ->
    Value;
app_parse_timespan(Value) ->
    genlib_format:parse_timespan(Value) div 1000.

%%
%% Marshalling
%%

-define(BINARY_BINDING_STATUS_PENDING, <<"pending">>).
-define(BINARY_BINDING_STATUS_SUCCEEDED, <<"succeeded">>).
-define(BINARY_BINDING_STATUS_FAILED(Failure), [<<"failed">>, Failure]).

-spec marshal_event_payload([customer_change()]) -> hg_machine:event_payload().
marshal_event_payload(Changes) ->
    wrap_event_payload({customer_changes, Changes}).

wrap_event_payload(Payload) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    Bin = hg_proto_utils:serialize(Type, Payload),
    #{
        format_version => 1,
        data => {bin, Bin}
    }.

-spec marshal_customer_params(customer_params()) -> binary().
marshal_customer_params(Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'CustomerParams'}},
    hg_proto_utils:serialize(Type, Params).

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
marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

-spec unmarshal_history([hg_machine:event()]) -> [hg_machine:event([customer_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) -> hg_machine:event([customer_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) -> [customer_change()].
unmarshal_event_payload(#{format_version := 1, data := {bin, Changes}}) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    {customer_changes, Buf} = hg_proto_utils:deserialize(Type, Changes),
    Buf;
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, change}, Changes).

-spec unmarshal_customer_params(binary()) -> customer_params().
unmarshal_customer_params(Bin) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'CustomerParams'}},
    hg_proto_utils:deserialize(Type, Bin).

unmarshal({list, T}, Vs) when is_list(Vs) ->
    [unmarshal(T, V) || V <- Vs];
%% Aux State

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
    <<"change">> := <<"created">>,
    <<"customer_id">> := CustomerID,
    <<"owner_id">> := OwnerID,
    <<"shop_id">> := ShopID,
    <<"created_at">> := CreatedAt,
    <<"contact_info">> := ContactInfo,
    <<"metadata">> := Metadata
}) ->
    ?customer_created(
        unmarshal(str, CustomerID),
        unmarshal(str, OwnerID),
        unmarshal(str, ShopID),
        unmarshal(metadata, Metadata),
        unmarshal(contact_info, ContactInfo),
        unmarshal(str, CreatedAt)
    );
unmarshal({change, 1}, #{
    <<"change">> := <<"created">>,
    <<"customer">> := Customer
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
    <<"change">> := <<"binding">>,
    <<"binding_id">> := CustomerBindingID,
    <<"payload">> := Payload
}) ->
    ?customer_binding_changed(
        unmarshal(str, CustomerBindingID),
        unmarshal(binding_change_payload, Payload)
    );
unmarshal(
    customer,
    #{
        <<"id">> := ID,
        <<"owner_id">> := OwnerID,
        <<"shop_id">> := ShopID,
        <<"created_at">> := CreatedAt,
        <<"contact">> := ContactInfo,
        <<"metadata">> := Metadata
    }
) ->
    #payproc_Customer{
        id = unmarshal(str, ID),
        owner_id = unmarshal(str, OwnerID),
        shop_id = unmarshal(str, ShopID),
        status = ?customer_unready(),
        created_at = unmarshal(str, CreatedAt),
        bindings = [],
        contact_info = unmarshal(contact_info, ContactInfo),
        metadata = unmarshal(metadata, Metadata)
    };
unmarshal(customer_status, <<"unready">>) ->
    ?customer_unready();
unmarshal(customer_status, <<"ready">>) ->
    ?customer_ready();
unmarshal(
    binding,
    #{
        <<"id">> := ID,
        <<"recpaytool_id">> := RecPaymentToolID,
        <<"payresource">> := PaymentResource
    } = Binding
) ->
    Status = maps:get(<<"status">>, Binding, ?BINARY_BINDING_STATUS_PENDING),
    PartyRevision = maps:get(<<"party_revision">>, Binding, undefined),
    DomainRevision = maps:get(<<"domain_revision">>, Binding, undefined),
    #payproc_CustomerBinding{
        id = unmarshal(str, ID),
        rec_payment_tool_id = unmarshal(str, RecPaymentToolID),
        payment_resource = unmarshal(payment_resource, PaymentResource),
        status = unmarshal(binding_status, Status),
        party_revision = unmarshal(int, PartyRevision),
        domain_revision = unmarshal(int, DomainRevision)
    };
unmarshal(
    payment_resource,
    #{
        <<"paytool">> := PaymentTool,
        <<"session">> := PaymentSessionID,
        <<"client_info">> := ClientInfo
    }
) ->
    #domain_DisposablePaymentResource{
        payment_tool = hg_payment_tool:unmarshal(PaymentTool),
        payment_session_id = unmarshal(str, PaymentSessionID),
        client_info = unmarshal(client_info, ClientInfo)
    };
unmarshal(client_info, ClientInfo) ->
    #domain_ClientInfo{
        ip_address = unmarshal(str, genlib_map:get(<<"ip">>, ClientInfo)),
        fingerprint = unmarshal(str, genlib_map:get(<<"fingerprint">>, ClientInfo))
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
        code = unmarshal(str, Code),
        sub = unmarshal(sub_failure, maps:get(<<"sub">>, SubFailure, undefined))
    };
unmarshal(failure, [1, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, [<<"failure">>, #{<<"code">> := Code} = Failure]]) ->
    {failure, #domain_Failure{
        code = unmarshal(str, Code),
        reason = unmarshal(str, maps:get(<<"reason">>, Failure, undefined)),
        sub = unmarshal(sub_failure, maps:get(<<"sub">>, Failure, undefined))
    }};
unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {failure, #domain_Failure{
        code = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};
unmarshal(contact_info, ContactInfo) ->
    #domain_ContactInfo{
        phone_number = unmarshal(str, genlib_map:get(<<"phone">>, ContactInfo)),
        email = unmarshal(str, genlib_map:get(<<"email">>, ContactInfo))
    };
unmarshal(metadata, Metadata) ->
    hg_msgpack_marshalling:unmarshal(json, Metadata);
unmarshal(_, Other) ->
    Other.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec event_pol_timer_test_() -> _.

event_pol_timer_test_() ->
    {setup,
        fun() ->
            application:unset_env(hellgate, binding)
        end,
        [
            ?_assertEqual(get_event_pol_timeout(1), 1),
            ?_assertEqual(get_event_pol_timeout(?SYNC_INTERVAL + 1), ?SYNC_INTERVAL),
            ?_assert(get_event_pol_timeout(?MAX_BINDING_DURATION) =< ?SYNC_OUTDATED_INTERVAL),
            ?_assert(get_event_pol_timeout(?MAX_BINDING_DURATION) >= ?SYNC_OUTDATED_INTERVAL * 0.9)
        ]}.

-spec app_config_test_() -> _.

app_config_test_() ->
    [
        {setup,
            fun() ->
                application:set_env(hellgate, binding, #{
                    max_sync_interval => <<"16s">>,
                    outdated_sync_interval => <<"32m">>,
                    outdate_timeout => <<"64s">>
                })
            end,
            [
                ?_assertEqual(16, app_binding_max_sync_interval()),
                ?_assertEqual(32 * 60, app_binding_outdated_sync_interval()),
                ?_assertEqual(64, app_binding_outdate_timeout())
            ]},
        {setup,
            fun() ->
                application:set_env(hellgate, binding, #{
                    max_sync_interval => 32,
                    outdated_sync_interval => 64,
                    outdate_timeout => 128
                })
            end,
            [
                ?_assertEqual(32, app_binding_max_sync_interval()),
                ?_assertEqual(64, app_binding_outdated_sync_interval()),
                ?_assertEqual(128, app_binding_outdate_timeout())
            ]},
        {setup,
            fun() ->
                application:unset_env(hellgate, binding)
            end,
            [
                ?_assertEqual(?SYNC_INTERVAL, app_binding_max_sync_interval()),
                ?_assertEqual(?SYNC_OUTDATED_INTERVAL, app_binding_outdated_sync_interval()),
                ?_assertEqual(?MAX_BINDING_DURATION, app_binding_outdate_timeout())
            ]}
    ].

-endif.
