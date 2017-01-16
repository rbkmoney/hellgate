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
%%%           - timer preservation on calls (?)
%%%  - do not make payment ids so complex, a sequence would suffice
%%%     - alter `Invoicing.GetPayment` signature
%%%  - if a party or shop is blocked / suspended, is it an `InvalidStatus` or 503?
%%%    let'em enjoy unexpected exception in the meantime :)
%%%  - unify somehow with operability assertions from hg_party
%%%  - it is inadequately painful to handle rolling context correctly
%%%     - some kind of an api client process akin to the `hg_client_api`?
%%%  - should party blocking / suspension be version-locked? probably _not_
%%%  - we should probably check access rights and operability without going into
%%%    invoice machine
%%%  - looks like payment state is a better place to put version lock
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
    invoice :: invoice(),
    payments = [] :: [{payment_id(), payment_st()}],
    sequence = 0 :: 0 | sequence(),
    pending = invoice ::
        invoice |
        {payment, payment_id()} |
        {session, session()}
}).

-type st() :: #st{}.

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function('Create', [UserInfo, InvoiceParams], _Opts) ->
    #payproc_InvoiceParams{party_id = PartyID, shop_id = ShopID} = InvoiceParams,
    ok = hg_security:check_user_info(UserInfo, PartyID),
    ID = hg_utils:unique_id(),
    Party = get_party(UserInfo, PartyID),
    Shop = validate_party_shop(ShopID, Party),
    ok = validate_invoice_params(InvoiceParams, Shop),
    ok = start(ID, {InvoiceParams, PartyID}),
    ID;

handle_function('Get', [UserInfo, InvoiceID], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    St = get_state(InvoiceID),
    _Party = get_party(UserInfo, get_party_id(St)),
    get_invoice_state(St);

handle_function('GetEvents', [UserInfo, InvoiceID, Range], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    get_public_history(InvoiceID, Range);

handle_function('StartPayment', [UserInfo, InvoiceID, PaymentParams], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    St0 = get_initial_state(InvoiceID),
    Party = get_party(UserInfo, get_party_id(St0)),
    _Shop = validate_party_shop(get_shop_id(St0), Party),
    call(InvoiceID, {start_payment, PaymentParams});

handle_function('GetPayment', [UserInfo, InvoiceID, PaymentID], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    St = get_state(InvoiceID),
    case get_payment_session(PaymentID, St) of
        PaymentSession when PaymentSession /= undefined ->
            hg_invoice_payment:get_payment(PaymentSession);
        undefined ->
            throw(#payproc_InvoicePaymentNotFound{})
    end;

handle_function('Fulfill', [UserInfo, InvoiceID, Reason], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    call(InvoiceID, {fulfill, Reason});

handle_function('Rescind', [UserInfo, InvoiceID, Reason], _Opts) ->
    validate_user_info(UserInfo, InvoiceID),
    call(InvoiceID, {rescind, Reason}).

get_party(UserInfo, PartyID) ->
    hg_party:get(UserInfo, PartyID).

validate_party_shop(ShopID, Party) ->
    _ = assert_party_operable(Party),
    _ = assert_shop_operable(Shop = get_party_shop(ShopID, Party)),
    Shop.

get_invoice_state(#st{invoice = Invoice, payments = Payments}) ->
    #payproc_InvoiceState{
        invoice = Invoice,
        payments = [
            hg_invoice_payment:get_payment(PaymentSession) ||
                {_PaymentID, PaymentSession} <- Payments
        ]
    }.

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
    {History, _LastID} = get_history(InvoiceID),
    collapse_history(History).

get_initial_state(InvoiceID) ->
    {History, _LastID} = get_history(InvoiceID, undefined, 1),
    collapse_history(History).

get_public_history(InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    hg_history:get_public_history(
        fun (ID, Lim) -> get_history(InvoiceID, ID, Lim) end,
        fun (Event) -> publish_invoice_event(InvoiceID, Event) end,
        AfterID, Limit
    ).

publish_invoice_event(InvoiceID, {ID, Dt, Event}) ->
    case publish_event(InvoiceID, Event) of
        {true, {Source, Seq, Ev}} ->
            {true, #payproc_Event{id = ID, source = Source, created_at = Dt, sequence = Seq, payload = Ev}};
        false ->
            false
    end.

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
    throw(#payproc_UserInvoiceNotFound{});
map_error({error, Reason}) ->
    error(Reason).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

%%

-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_status() :: dmsl_domain_thrift:'InvoiceStatus'().
-type user_info() :: dmsl_payment_processing_thrift:'UserInfo'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().
-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: hg_invoice_payment:st().
-type sequence() :: pos_integer().
-type proxy_state() :: dmsl_proxy_thrift:'ProxyState'().

-type ev() ::
    {public, sequence(), dmsl_payment_processing_thrift:'EventPayload'()} |
    {private, sequence(), private_event()}.

-type private_event() ::
    session_event(). %% TODO hg_invoice_payment:private_event() ?

-type session_event() ::
    {session_event,
        {started, invoice_status()} |
        finished |
        {proxy_state_changed, proxy_state()}
    }.

-type session() :: #{
    status => invoice_status(),
    proxy_state => undefined | proxy_state()
}.

-define(session_ev(E), {session_event, E}).

-include("invoice_events.hrl").

-define(invalid_invoice_status(Status),
    #payproc_InvalidInvoiceStatus{status = Status}).
-define(payment_pending(PaymentID),
    #payproc_InvoicePaymentPending{id = PaymentID}).

-spec publish_event(invoice_id(), hg_machine:event(ev())) ->
    {true, hg_event_provider:public_event()} | false.

publish_event(InvoiceID, {public, Seq, Ev = ?invoice_ev(_)}) ->
    {true, {{invoice, InvoiceID}, Seq, Ev}};
publish_event(InvoiceID, {public, Seq, {{payment, _}, Ev = ?payment_ev(_)}}) ->
    {true, {{invoice, InvoiceID}, Seq, ?invoice_ev(Ev)}};
publish_event(_InvoiceID, _Event) ->
    false.

%%

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(invoice_id(), {invoice_params(), dmsl_domain_thrift:'PartyID'()}) ->
    hg_machine:result(ev()).

init(ID, {InvoiceParams, PartyID}) ->
    Invoice = create_invoice(ID, InvoiceParams, PartyID),
    Event = {public, ?invoice_ev(?invoice_created(Invoice))},
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    ok(Event, #st{}, set_invoice_timer(#st{invoice = Invoice})).

%%

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

process_signal(Signal, History) ->
    handle_signal(Signal, collapse_history(History)).

handle_signal(timeout, St = #st{pending = {payment, PaymentID}}) ->
    % there's a payment pending
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_signal(timeout, PaymentID, PaymentSession, St);
handle_signal(timeout, St = #st{pending = {session, Session}}) ->
    % there's a session pending
    process_session_signal(timeout, Session, St);
handle_signal(timeout, St = #st{pending = invoice}) ->
    % invoice is expired
    handle_expiration(St);

handle_signal({repair, _}, St) ->
    ok([], St, restore_timer(St)).

handle_expiration(St) ->
    Event = {public, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(overdue))))},
    ok(Event, St).

%%

-type call() ::
    {start_payment, payment_params(), user_info()} |
    {fulfill, binary(), user_info()} |
    {rescind, binary(), user_info()} |
    {callback, callback()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    St = collapse_history(History),
    try handle_call(Call, St) catch
        {exception, Exception} ->
            {{exception, Exception}, {[], restore_timer(St)}}
    end.

-spec raise(term()) -> no_return().

raise(What) ->
    throw({exception, What}).

handle_call({start_payment, PaymentParams}, St) ->
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    start_payment(PaymentParams, St);

handle_call({fulfill, Reason}, St) ->
    _ = assert_invoice_status(paid, St),
    Event = {public, ?invoice_ev(?invoice_status_changed(?fulfilled(format_reason(Reason))))},
    respond(ok, Event, St);

handle_call({rescind, Reason}, St) ->
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    Event = {public, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(Reason))))},
    respond(ok, Event, St);

handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

dispatch_callback({provider, Payload}, St = #st{pending = {payment, PaymentID}}) ->
    PaymentSession = get_payment_session(PaymentID, St),
    process_payment_call({callback, Payload}, PaymentID, PaymentSession, St);
dispatch_callback(_Callback, _St) ->
    raise(invalid_callback).

assert_invoice_status(Status, #st{invoice = Invoice}) ->
    assert_invoice_status(Status, Invoice);
assert_invoice_status(Status, #domain_Invoice{status = {Status, _}}) ->
    ok;
assert_invoice_status(_Status, #domain_Invoice{status = Invalid}) ->
    raise(?invalid_invoice_status(Invalid)).

assert_no_pending_payment(#st{pending = {payment, PaymentID}}) ->
    raise(?payment_pending(PaymentID));
assert_no_pending_payment(_) ->
    ok.

restore_timer(St = #st{invoice = #domain_Invoice{status = Status}, pending = Pending}) ->
    case Pending of
        invoice when Status == ?unpaid() ->
            set_invoice_timer(St);
        invoice ->
            hg_machine_action:new();
        _ ->
            % TODO how to restore timer properly then, magic number for now
            hg_machine_action:set_timeout(10)
    end.

set_invoice_timer(#st{invoice = #domain_Invoice{due = Due}}) ->
    hg_machine_action:set_deadline(Due).

%%

start_payment(PaymentParams, St) ->
    PaymentID = create_payment_id(St),
    Party = checkout_party(St),
    Opts = get_payment_opts(Party, St),
    {Events1, _} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts),
    {Events2, Action} = hg_invoice_payment:start_session(?processed()),
    Events = wrap_payment_events(PaymentID, Events1 ++ Events2),
    respond(PaymentID, Events, St, Action).

process_payment_signal(Signal, PaymentID, PaymentSession, St) ->
    Party = checkout_party(St),
    Opts = get_payment_opts(Party, St),
    PaymentResult = hg_invoice_payment:process_signal(Signal, PaymentSession, Opts),
    handle_payment_result(PaymentResult, PaymentID, PaymentSession, Party, St).

process_payment_call(Call, PaymentID, PaymentSession, St) ->
    Party = checkout_party(St),
    Opts = get_payment_opts(Party, St),
    {Response, PaymentResult} = hg_invoice_payment:process_call(Call, PaymentSession, Opts),
    {{ok, Response}, handle_payment_result(PaymentResult, PaymentID, PaymentSession, Party, St)}.

handle_payment_result(Result, PaymentID, PaymentSession, Party, St) ->
    case Result of
        {next, {Events, Action}} ->
            ok(wrap_payment_events(PaymentID, Events), St, Action);
        {done, {Events1, _}} ->
            PaymentSession1 = lists:foldl(fun hg_invoice_payment:merge_event/2, PaymentSession, Events1),
            case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
                ?processed() ->
                    {Events2, Action} = hg_invoice_payment:start_session(?captured()),
                    ok(wrap_payment_events(PaymentID, Events1 ++ Events2), St, Action);
                ?captured() ->
                    {Events2, Action} = start_session(?paid(), Party, St),
                    ok(wrap_payment_events(PaymentID, Events1) ++ Events2, St, Action);
                ?failed(_) ->
                    ok(wrap_payment_events(PaymentID, Events1), St, set_invoice_timer(St))
            end
    end.

wrap_payment_events(PaymentID, Events) ->
    lists:map(fun
        (E = ?payment_ev(_)) ->
            {public, {{payment, PaymentID}, E}};
        (E) ->
            {private, {{payment, PaymentID}, E}}
    end, Events).

get_payment_opts(Party, #st{invoice = Invoice}) ->
    #{
        party => Party,
        invoice => Invoice
    }.

%%

start_session(Status, Party, St) ->
    Shop = get_party_shop(get_shop_id(St), Party),
    case Shop#domain_Shop.proxy of
        Proxy when Proxy /= undefined ->
            Event = {private, ?session_ev({started, Status})},
            {[Event], hg_machine_action:instant()};
        undefined ->
            finalize_session(Status, St)
    end.

process_session_signal(timeout, Session, St) ->
    Party = checkout_party(St),
    Revision = hg_domain:head(),
    ProxyContext = construct_proxy_context(Session, Party, St, Revision),
    {ok, ProxyResult} = issue_handle_event_call(ProxyContext, Party, St, Revision),
    {Events, Action} = handle_proxy_result(ProxyResult, Session, St),
    ok(Events, St, Action).

finalize_session(Status, St) ->
    Event = {public, ?invoice_ev(?invoice_status_changed(Status))},
    {[Event], restore_timer(St)}.

%%

-include_lib("dmsl/include/dmsl_proxy_merchant_thrift.hrl").

handle_proxy_result(#prxmerch_ProxyResult{intent = {_, Intent}, next_state = ProxyState}, Session, St) ->
    handle_proxy_intent(Intent, ProxyState, Session, St).

handle_proxy_intent(#prxmerch_FinishIntent{}, _ProxyState, #{status := Status}, St) ->
    Event = {private, ?session_ev(finished)},
    {Events, Action} = finalize_session(Status, St),
    {[Event | Events], Action};

handle_proxy_intent(#prxmerch_SleepIntent{timer = Timer}, ProxyState, _Session, _St) ->
    Event = {private, ?session_ev({proxy_state_changed, ProxyState})},
    {[Event], hg_machine_action:set_timer(Timer)}.

construct_proxy_context(Session, Party, St, Revision) ->
    Shop  = get_party_shop(get_shop_id(St), Party),
    Proxy = Shop#domain_Shop.proxy,
    #prxmerch_Context{
        session = construct_proxy_session(Session),
        invoice = collect_invoice_info(Party, Shop, St#st.invoice, Revision),
        options = collect_proxy_options(Proxy, Revision)
    }.

construct_proxy_session(#{status := Status, proxy_state := ProxyState}) ->
    #prxmerch_Session{
        event = {status_changed, #prxmerch_InvoiceStatusChanged{status = construct_proxy_status(Status)}},
        state = ProxyState
    }.

construct_proxy_status(?paid()) ->
    {paid, #prxmerch_InvoicePaid{}}.

collect_invoice_info(Party, Shop, Invoice, Revision) ->
    Cost = Invoice#domain_Invoice.cost,
    #prxmerch_InvoiceInfo{
        party = #prxmerch_Party{
            id = Party#domain_Party.id
        },
        shop = #prxmerch_Shop{
            id      = Shop#domain_Shop.id,
            details = Shop#domain_Shop.details
        },
        invoice = #prxmerch_Invoice{
            id         = Invoice#domain_Invoice.id,
            created_at = Invoice#domain_Invoice.created_at,
            due        = Invoice#domain_Invoice.due,
            details    = Invoice#domain_Invoice.details,
            context    = Invoice#domain_Invoice.context,
            cost       = #prxmerch_Cash{
                amount     = Cost#domain_Cash.amount,
                currency   = hg_domain:get(Revision, {currency, Cost#domain_Cash.currency})
            }
        }
    }.

collect_proxy_options(Proxy, Revision) ->
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    maps:merge(
        Proxy#domain_Proxy.additional,
        ProxyDef#domain_ProxyDefinition.options
    ).

issue_handle_event_call(ProxyContext, Party, St, Revision) ->
    CallOpts = get_call_options(Party, St, Revision),
    issue_call('HandleInvoiceEvent', [ProxyContext], CallOpts).

issue_call(Func, Args, CallOpts) ->
    try
        hg_woody_wrapper:call('MerchantProxy', Func, Args, CallOpts)
    catch
        {exception, Exception} ->
            error({proxy_failure, Exception})
    end.

get_call_options(Party, St, Revision) ->
    Shop     = get_party_shop(get_shop_id(St), Party),
    Proxy    = Shop#domain_Shop.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    #{url => ProxyDef#domain_ProxyDefinition.url}.

%%

checkout_party(St = #st{invoice = #domain_Invoice{created_at = CreationTimestamp}}) ->
    PartyID = get_party_id(St),
    hg_party:checkout(PartyID, CreationTimestamp).

%%

ok(Event, St) ->
    ok(Event, St, hg_machine_action:new()).
ok(Event, St, Action) ->
    {sequence_events(wrap_event_list(Event), St), Action}.

respond(Response, Event, St) ->
    respond(Response, Event, St, hg_machine_action:new()).
respond(Response, Event, St, Action) ->
    {{ok, Response}, {sequence_events(wrap_event_list(Event), St), Action}}.

wrap_event_list(Event) when is_tuple(Event) ->
    [Event];
wrap_event_list(Events) when is_list(Events) ->
    Events.

sequence_events(Evs, St) ->
    {SequencedEvs, _} = lists:mapfoldl(fun sequence_event_/2, St#st.sequence, Evs),
    SequencedEvs.

sequence_event_({public, Ev}, Seq) ->
    {{public, Seq + 1, Ev}, Seq + 1};
sequence_event_({private, Ev}, Seq) ->
    {{private, Seq, Ev}, Seq}.

%%

create_invoice(ID, V = #payproc_InvoiceParams{}, PartyID) ->
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner_id        = PartyID,
        created_at      = hg_datetime:format_now(),
        status          = ?unpaid(),
        cost            = V#payproc_InvoiceParams.cost,
        due             = V#payproc_InvoiceParams.due,
        details         = V#payproc_InvoiceParams.details,
        context         = V#payproc_InvoiceParams.context
    }.

create_payment_id(#st{payments = Payments}) ->
    integer_to_binary(length(Payments) + 1).

get_payment_status(#domain_InvoicePayment{status = Status}) ->
    Status.

%%

-spec collapse_history([ev()]) -> st().

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, {_, Seq, Ev}}, St) -> merge_event(Ev, St#st{sequence = Seq}) end,
        #st{},
        History
    ).

merge_event(?invoice_ev(Event), St) ->
    merge_invoice_event(Event, St);
merge_event(?session_ev(Event), St) ->
    merge_session_event(Event, St);
merge_event({{payment, PaymentID}, Event}, St) ->
    PaymentSession = get_payment_session(PaymentID, St),
    PaymentSession1 = hg_invoice_payment:merge_event(Event, PaymentSession),
    St1 = set_payment_session(PaymentID, PaymentSession1, St),
    case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
        ?pending() ->
            St1#st{pending = {payment, PaymentID}};
        ?processed() ->
            St1#st{pending = {payment, PaymentID}};
        _ ->
            St1#st{pending = invoice}
    end.

merge_invoice_event(?invoice_created(Invoice), St) ->
    St#st{invoice = Invoice};
merge_invoice_event(?invoice_status_changed(Status), St = #st{invoice = I}) ->
    St#st{invoice = I#domain_Invoice{status = Status}}.

get_party_id(#st{invoice = #domain_Invoice{owner_id = PartyID}}) ->
    PartyID.

merge_session_event({started, Status}, St = #st{}) ->
    St#st{pending = {session, #{status => Status, proxy_state => undefined}}};
merge_session_event(finished, St = #st{pending = {session, _}}) ->
    St#st{pending = invoice};
merge_session_event({proxy_state_changed, ProxyState}, St = #st{pending = {session, Session}}) ->
    St#st{pending = {session, Session#{proxy_state := ProxyState}}}.

get_shop_id(#st{invoice = #domain_Invoice{shop_id = ShopID}}) ->
    ShopID.

get_payment_session(PaymentID, #st{payments = Payments}) ->
    case lists:keyfind(PaymentID, 1, Payments) of
        {PaymentID, PaymentSession} ->
            PaymentSession;
        false ->
            undefined
    end.

set_payment_session(PaymentID, PaymentSession, St = #st{payments = Payments}) ->
    St#st{payments = lists:keystore(PaymentID, 1, Payments, {PaymentID, PaymentSession})}.

%%

%% TODO: fix this dirty hack
format_reason({Pre, V}) ->
    genlib:format("~s: ~s", [Pre, genlib:to_binary(V)]);
format_reason(V) ->
    genlib:to_binary(V).

%%

get_party_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

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

assert_shop_operable(undefined) ->
    throw(#payproc_ShopNotFound{});
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

validate_user_info(UserInfo, InvoiceID) ->
    St = get_state(InvoiceID),
    PartyID = get_party_id(St),
    ok = hg_security:check_user_info(UserInfo, PartyID).

