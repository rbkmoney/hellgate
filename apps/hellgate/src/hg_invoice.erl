%%% Invoice machine
%%%
%%% TODO
%%%  - REFACTOR WITH FIRE
%%%     - proper concepts
%%%        - simple lightweight lower-level machines (middlewares (?)) for:
%%%           - handling callbacks idempotently
%%%           - state collapsing (?)
%%%           - simpler flow control (?)
%%%           - event publishing (?)
%%%  - unify somehow with operability assertions from hg_party
%%%  - should party blocking / suspension be version-locked? probably _not_
%%%  - if someone has access to a party then it has access to an invoice
%%%    belonging to this party

-module(hg_invoice).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-define(NS, <<"invoice">>).

-export([process_callback/2]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%%

-record(st, {
    invoice :: undefined | invoice(),
    payments = [] :: [{payment_id(), payment_st()}],
    pending = invoice ::
        invoice |
        {payment, payment_id()}
}).

-type st() :: #st{}.

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(invoicing,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_('Create', [UserInfo, InvoiceParams], _Opts) ->
    InvoiceID = hg_utils:unique_id(),
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    PartyID = InvoiceParams#payproc_InvoiceParams.party_id,
    ShopID = InvoiceParams#payproc_InvoiceParams.shop_id,
    _ = assert_party_accessible(PartyID),
    Party = get_party(PartyID),
    Shop = assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = assert_party_shop_operable(Shop, Party),
    ok = validate_invoice_params(InvoiceParams, Shop),
    ok = start(InvoiceID, InvoiceParams),
    get_invoice_state(get_state(InvoiceID));

handle_function_('Get', [UserInfo, InvoiceID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_invoice_state(St);

handle_function_('GetEvents', [UserInfo, InvoiceID, Range], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    _ = assert_invoice_accessible(get_initial_state(InvoiceID)),
    get_public_history(InvoiceID, Range);

handle_function_('StartPayment', [UserInfo, InvoiceID, PaymentParams], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    get_payment_state(call(InvoiceID, {start_payment, PaymentParams}));

handle_function_('GetPayment', [UserInfo, InvoiceID, PaymentID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    get_payment_state(get_payment_session(PaymentID, St));

handle_function_('CapturePayment', [UserInfo, InvoiceID, PaymentID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {capture_payment, PaymentID, Reason});

handle_function_('CancelPayment', [UserInfo, InvoiceID, PaymentID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {cancel_payment, PaymentID, Reason});

handle_function_('CreatePaymentAdjustment', [UserInfo, InvoiceID, PaymentID, Params], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {create_payment_adjustment, PaymentID, Params});

handle_function_('GetPaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    St = assert_invoice_accessible(get_state(InvoiceID)),
    hg_invoice_payment:get_adjustment(ID, get_payment_session(PaymentID, St));

handle_function_('CapturePaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {capture_payment_adjustment, PaymentID, ID});

handle_function_('CancelPaymentAdjustment', [UserInfo, InvoiceID, PaymentID, ID], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID, PaymentID),
    call(InvoiceID, {cancel_payment_adjustment, PaymentID, ID});

handle_function_('Fulfill', [UserInfo, InvoiceID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, {fulfill, Reason});

handle_function_('Rescind', [UserInfo, InvoiceID, Reason], _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_invoicing_meta(InvoiceID),
    call(InvoiceID, {rescind, Reason}).

assert_invoice_operable(St) ->
    Party = get_party(get_party_id(St)),
    Shop  = hg_party:get_shop(get_shop_id(St), Party),
    assert_party_shop_operable(Shop, Party).

assert_party_shop_operable(Shop, Party) ->
    _ = assert_party_operable(Party),
    assert_shop_operable(Shop).

get_party(PartyID) ->
    hg_party_machine:get_party(PartyID).

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_Invoice{
        invoice = Invoice,
        payments = [
            get_payment_state(PaymentSession) ||
                {_PaymentID, PaymentSession} <- Payments
        ]
    }.

get_payment_state(PaymentSession) ->
    #payproc_InvoicePayment{
        payment = hg_invoice_payment:get_payment(PaymentSession),
        adjustments = hg_invoice_payment:get_adjustments(PaymentSession)
    }.

set_invoicing_meta(InvoiceID) ->
    hg_log_scope:set_meta(#{invoice_id => InvoiceID}).

set_invoicing_meta(InvoiceID, PaymentID) ->
    hg_log_scope:set_meta(#{invoice_id => InvoiceID, payment_id => PaymentID}).

%%

-type tag()               :: dmsl_base_thrift:'Tag'().
-type callback()          :: _. %% FIXME
-type callback_response() :: _. %% FIXME

-spec process_callback(tag(), callback()) ->
    {ok, callback_response()} | {error, invalid_callback | notfound | failed} | no_return().

process_callback(Tag, Callback) ->
    case hg_machine:call(?NS, {tag, Tag}, {callback, Callback}) of
        {ok, {ok, _} = Ok} ->
            Ok;
        {ok, {exception, invalid_callback}} ->
            {error, invalid_callback};
        {error, _} = Error ->
            Error
    end.

%%

get_history(InvoiceID) ->
    map_history_error(hg_machine:get_history(?NS, InvoiceID)).

get_history(InvoiceID, AfterID, Limit) ->
    map_history_error(hg_machine:get_history(?NS, InvoiceID, AfterID, Limit)).

get_state(InvoiceID) ->
    collapse_history(get_history(InvoiceID)).

get_initial_state(InvoiceID) ->
    collapse_history(get_history(InvoiceID, undefined, 1)).

get_public_history(InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_invoice_event(InvoiceID, Ev) || Ev <- get_history(InvoiceID, AfterID, Limit)].

publish_invoice_event(InvoiceID, {ID, Dt, Event}) ->
    {Source, Ev} = publish_event(InvoiceID, Event),
    #payproc_Event{
        id = ID,
        source = Source,
        created_at = Dt,
        payload = Ev
    }.

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{});
map_error({error, Reason}) ->
    error(Reason).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_InvoiceNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

%%

-include("invoice_events.hrl").

-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().
-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type adjustment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type adjustment_id() :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type payment_st() :: hg_invoice_payment:st().

-type ev() ::
    [dmsl_payment_processing_thrift:'InvoiceChange'()].

-define(invalid_invoice_status(Status),
    #payproc_InvalidInvoiceStatus{status = Status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec publish_event(invoice_id(), ev()) ->
    hg_event_provider:public_event().

publish_event(InvoiceID, Changes) when is_list(Changes) ->
    {{invoice_id, InvoiceID}, ?invoice_ev(Changes)}.

%%

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(invoice_id(), invoice_params()) ->
    hg_machine:result(ev()).

init(ID, InvoiceParams = #payproc_InvoiceParams{party_id = PartyID}) ->
    Invoice = create_invoice(ID, InvoiceParams, PartyID),
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    handle_result(#{
        changes => [?invoice_created(Invoice)],
        action  => set_invoice_timer(#st{invoice = Invoice}),
        state   => #st{}
    }).

%%

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(History))).

handle_signal(timeout, St = #st{pending = {payment, PaymentID}}) ->
    % there's a payment pending
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_signal(timeout, PaymentID, PaymentSession, St);
handle_signal(timeout, St = #st{pending = invoice}) ->
    % invoice is expired
    handle_expiration(St);

handle_signal({repair, _}, St) ->
    #{
        state => St
    }.

handle_expiration(St) ->
    #{
        changes => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(overdue)))],
        state   => St
    }.

%%

-type call() ::
    {start_payment, payment_params()} |
    {capture_payment, payment_id(), binary()} |
    {cancel_payment, payment_id(), binary()} |
    {create_payment_adjustment , payment_id(), adjustment_params()} |
    {capture_payment_adjustment, payment_id(), adjustment_id()} |
    {cancel_payment_adjustment , payment_id(), adjustment_id()} |
    {fulfill, binary()} |
    {rescind, binary()} |
    {callback, callback()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    St = collapse_history(History),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

handle_call({start_payment, PaymentParams}, St) ->
    % TODO consolidate these assertions somehow
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    start_payment(PaymentParams, St);

handle_call({capture_payment, PaymentID, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    {ok, {Changes, Action}} = hg_invoice_payment:capture_payment(PaymentSession, Reason),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes),
        action => Action,
        state => St
    };

handle_call({cancel_payment, PaymentID, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    PaymentSession = get_payment_session(PaymentID, St),
    {ok, {Changes, Action}} = hg_invoice_payment:cancel_payment(PaymentSession, Reason),
    #{
        response => ok,
        changes => wrap_payment_changes(PaymentID, Changes),
        action => Action,
        state => St
    };

handle_call({fulfill, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(paid, St),
    #{
        response => ok,
        changes  => [?invoice_status_changed(?invoice_fulfilled(hg_utils:format_reason(Reason)))],
        state    => St
    };

handle_call({rescind, Reason}, St) ->
    _ = assert_invoice_accessible(St),
    _ = assert_invoice_operable(St),
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    #{
        response => ok,
        changes  => [?invoice_status_changed(?invoice_cancelled(hg_utils:format_reason(Reason)))],
        action   => hg_machine_action:unset_timer(),
        state    => St
    };

handle_call({create_payment_adjustment, PaymentID, Params}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = get_payment_opts(St),
    {Adjustment, {Changes, Action}} = hg_invoice_payment:create_adjustment(Params, PaymentSession, Opts),
    #{
        response => Adjustment,
        changes  => wrap_payment_changes(PaymentID, Changes),
        action   => Action,
        state    => St
    };

handle_call({capture_payment_adjustment, PaymentID, ID}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = get_payment_opts(St),
    {ok, {Changes, Action}} = hg_invoice_payment:capture_adjustment(ID, PaymentSession, Opts),
    #{
        response => ok,
        changes  => wrap_payment_changes(PaymentID, Changes),
        action   => Action,
        state    => St
    };

handle_call({cancel_payment_adjustment, PaymentID, ID}, St) ->
    _ = assert_invoice_accessible(St),
    PaymentSession = get_payment_session(PaymentID, St),
    Opts = get_payment_opts(St),
    {ok, {Changes, Action}} = hg_invoice_payment:cancel_adjustment(ID, PaymentSession, Opts),
    #{
        response => ok,
        changes  => wrap_payment_changes(PaymentID, Changes),
        action   => Action,
        state    => St
    };

handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

dispatch_callback({provider, Payload}, St = #st{pending = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_call({callback, Payload}, PaymentID, PaymentSession, St);
dispatch_callback(_Callback, _St) ->
    throw(invalid_callback).

assert_invoice_status(Status, #st{invoice = Invoice}) ->
    assert_invoice_status(Status, Invoice);
assert_invoice_status(Status, #domain_Invoice{status = {Status, _}}) ->
    ok;
assert_invoice_status(_Status, #domain_Invoice{status = Invalid}) ->
    throw(?invalid_invoice_status(Invalid)).

assert_no_pending_payment(#st{pending = {payment, PaymentID}}) ->
    throw(?payment_pending(PaymentID));
assert_no_pending_payment(_) ->
    ok.

set_invoice_timer(#st{invoice = #domain_Invoice{due = Due}}) ->
    hg_machine_action:set_deadline(Due).

%%

-include("payment_events.hrl").

start_payment(PaymentParams, St) ->
    PaymentID = create_payment_id(St),
    Opts = get_payment_opts(St),
    % TODO make timer reset explicit here
    {PaymentSession, {Changes1, _}} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts),
    {ok, {Changes2, Action}} = hg_invoice_payment:start_session(?processed()),
    #{
        response => PaymentSession,
        changes  => wrap_payment_changes(PaymentID, Changes1 ++ Changes2),
        action   => Action,
        state    => St
    }.

process_payment_signal(Signal, PaymentID, PaymentSession, St) ->
    Opts = get_payment_opts(St),
    PaymentResult = hg_invoice_payment:process_signal(Signal, PaymentSession, Opts),
    handle_payment_result(PaymentResult, PaymentID, PaymentSession, St).

process_payment_call(Call, PaymentID, PaymentSession, St) ->
    Opts = get_payment_opts(St),
    {Response, PaymentResult} = hg_invoice_payment:process_call(Call, PaymentSession, Opts),
    maps:merge(#{response => Response}, handle_payment_result(PaymentResult, PaymentID, PaymentSession, St)).

handle_payment_result(Result, PaymentID, PaymentSession, St) ->
    case Result of
        {next, {Changes, Action}} ->
            #{
                changes => wrap_payment_changes(PaymentID, Changes),
                action  => Action,
                state   => St
            };
        {done, {Changes1, _}} ->
            PaymentSession1 = lists:foldl(fun hg_invoice_payment:merge_change/2, PaymentSession, Changes1),
            Payment = hg_invoice_payment:get_payment(PaymentSession1),
            case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
                ?processed() ->
                    {ok, {Changes2, Action}} = hg_invoice_payment:start_session_by_flow(PaymentSession1),
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1 ++ Changes2),
                        action  => Action,
                        state   => St
                    };
                ?captured() ->
                    Changes2 = [?invoice_status_changed(?invoice_paid())],
                    Action = get_payment_action(Payment),
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1) ++ Changes2,
                        action  => Action,
                        state   => St
                    };
                ?failed(_) ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        action  => set_invoice_timer(St),
                        state   => St
                    };
                ?cancelled() ->
                    #{
                        changes => wrap_payment_changes(PaymentID, Changes1),
                        action  => set_invoice_timer(St),
                        state   => St
                    }
            end
    end.

wrap_payment_changes(PaymentID, Changes) ->
    [?payment_ev(PaymentID, C) || C <- Changes].

get_payment_opts(St = #st{invoice = Invoice}) ->
    #{
        party => checkout_party(St),
        invoice => Invoice
    }.

checkout_party(St = #st{invoice = #domain_Invoice{created_at = CreationTimestamp}}) ->
    PartyID = get_party_id(St),
    hg_party_machine:checkout(PartyID, CreationTimestamp).

handle_result(#{state := St} = Params) ->
    Changes = maps:get(changes, Params, []),
    Action = maps:get(action, Params, hg_machine_action:new()),
    _ = log_changes(Changes, St),
    case maps:get(response, Params, undefined) of
        undefined ->
            {[Changes], Action};
        Response ->
            {{ok, Response}, {[Changes], Action}}
    end.

%%

create_invoice(ID, V = #payproc_InvoiceParams{}, PartyID) ->
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner_id        = PartyID,
        created_at      = hg_datetime:format_now(),
        status          = ?invoice_unpaid(),
        cost            = V#payproc_InvoiceParams.cost,
        due             = V#payproc_InvoiceParams.due,
        details         = V#payproc_InvoiceParams.details,
        context         = V#payproc_InvoiceParams.context
    }.

create_payment_id(#st{payments = Payments}) ->
    integer_to_binary(length(Payments) + 1).

get_payment_status(#domain_InvoicePayment{status = Status}) ->
    Status.

-include("domain.hrl").

get_payment_action(#domain_InvoicePayment{flow = Flow}) ->
    case Flow of
        ?invoice_payment_flow_instant() ->
            hg_machine_action:new();
        ?invoice_payment_flow_hold(_, _) ->
            hg_machine_action:unset_timer()
    end.

%%

-spec collapse_history([hg_machine:event(ev())]) -> st().

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Changes}, St0) ->
            lists:foldl(fun merge_change/2, St0, Changes)
        end,
        #st{},
        History
    ).

merge_change(?invoice_created(Invoice), St) ->
    St#st{invoice = Invoice};
merge_change(?invoice_status_changed(Status), St = #st{invoice = I}) ->
    St#st{invoice = I#domain_Invoice{status = Status}};
merge_change(?payment_ev(PaymentID, Event), St) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    PaymentSession1 = hg_invoice_payment:merge_change(Event, PaymentSession),
    St1 = set_payment_session(PaymentID, PaymentSession1, St),
    case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
        ?pending() ->
            St1#st{pending = {payment, PaymentID}};
        ?processed() ->
            St1#st{pending = {payment, PaymentID}};
        _ ->
            St1#st{pending = invoice}
    end.

get_party_id(#st{invoice = #domain_Invoice{owner_id = PartyID}}) ->
    PartyID.

get_shop_id(#st{invoice = #domain_Invoice{shop_id = ShopID}}) ->
    ShopID.

get_payment_session(PaymentID, St) ->
    case try_get_payment_session(PaymentID, St) of
        PaymentSession when PaymentSession /= undefined ->
            PaymentSession;
        undefined ->
            throw(#payproc_InvoicePaymentNotFound{})
    end.

try_get_payment_session(PaymentID, #st{payments = Payments}) ->
    case lists:keyfind(PaymentID, 1, Payments) of
        {PaymentID, PaymentSession} ->
            PaymentSession;
        false ->
            undefined
    end.

set_payment_session(PaymentID, PaymentSession, St = #st{payments = Payments}) ->
    St#st{payments = lists:keystore(PaymentID, 1, Payments, {PaymentID, PaymentSession})}.

%%

get_shop_currency(#domain_Shop{account = #domain_ShopAccount{currency = Currency}}) ->
    Currency.

assert_party_operable(#domain_Party{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_party_unblocked(Blocking),
    _ = assert_party_active(Suspension),
    V.

assert_party_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidPartyStatus{status = {blocking, V}}).

assert_party_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidPartyStatus{status = {suspension, V}}).

assert_shop_exists(#domain_Shop{} = V) ->
    V;
assert_shop_exists(undefined) ->
    throw(#payproc_ShopNotFound{}).

assert_shop_operable(#domain_Shop{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_shop_unblocked(Blocking),
    _ = assert_shop_active(Suspension),
    V.

assert_shop_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidShopStatus{status = {blocking, V}}).

assert_shop_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidShopStatus{status = {suspension, V}}).

%%

validate_invoice_params(
    #payproc_InvoiceParams{
        cost = #domain_Cash{
            currency = Currency,
            amount = Amount
        }
    },
    Shop
) ->
    _ = validate_amount(Amount),
    _ = validate_currency(Currency, get_shop_currency(Shop)),
    ok.

validate_amount(Amount) when Amount > 0 ->
    %% TODO FIX THIS ASAP! Amount should be specified in contract terms.
    ok;
validate_amount(_) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid amount">>]}).

validate_currency(Currency, Currency) ->
    ok;
validate_currency(_, _) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid currency">>]}).

assert_invoice_accessible(St = #st{}) ->
    assert_party_accessible(get_party_id(St)),
    St.

assert_party_accessible(PartyID) ->
    UserIdentity = get_user_identity(),
    case hg_access_control:check_user(UserIdentity, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

get_user_identity() ->
    hg_woody_handler_utils:get_user_identity().

%%

-include("domain.hrl").

log_changes(Changes, St) ->
    lists:foreach(fun (C) -> log_change(C, St) end, Changes).

log_change(Change, St) ->
    case get_log_params(Change, St) of
        {ok, #{type := Type, params := Params, message := Message}} ->
            _ = lager:log(info, [{Type, Params}], Message),
            ok;
        undefined ->
            ok
    end.

get_log_params(?invoice_created(Invoice), _St) ->
    get_invoice_event_log(invoice_created, unpaid, Invoice);
get_log_params(?invoice_status_changed({StatusName, _}), #st{invoice = Invoice}) ->
    get_invoice_event_log(invoice_status_changed, StatusName, Invoice);
get_log_params(?payment_ev(PaymentID, Change), St = #st{invoice = Invoice}) ->
    PaymentSession = try_get_payment_session(PaymentID, St),
    case hg_invoice_payment:get_log_params(Change, PaymentSession) of
        {ok, Params} ->
            {ok, maps:update_with(
                params,
                fun (V) ->
                    [{invoice, get_invoice_params(Invoice)} | V]
                end,
                Params
            )};
        undefined ->
            undefined
    end.

get_invoice_event_log(EventType, StatusName, Invoice) ->
    {ok, #{
        type => invoice_event,
        params => [{type, EventType}, {status, StatusName} | get_invoice_params(Invoice)],
        message => get_message(EventType)
    }}.

get_invoice_params(Invoice) ->
    #domain_Invoice{
        id = Id,
        owner_id = PartyID,
        cost = ?cash(Amount, #domain_CurrencyRef{symbolic_code = Currency}),
        shop_id = ShopID
    } = Invoice,
    [{id, Id}, {owner_id, PartyID}, {cost, [{amount, Amount}, {currency, Currency}]}, {shop_id, ShopID}].

get_message(invoice_created) ->
    "Invoice is created";
get_message(invoice_status_changed) ->
    "Invoice status is changed".
