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

-export([process_callback/3]).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/3]).
-export([process_signal/3]).
-export([process_call/3]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%%

-record(st, {
    invoice :: invoice(),
    payments = [] :: [{payment_id(), payment_st()}],
    sequence = 0 :: 0 | sequence()
}).

-type st() :: #st{}.

%%

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {term(), woody_client:context()} | no_return().

handle_function('Create', {UserInfo, InvoiceParams}, Context0, _Opts) ->
    ID = hg_utils:unique_id(),
    #payproc_InvoiceParams{party_id = PartyID, shop_id = ShopID} = InvoiceParams,
    {PartyState, Context1} = get_party(UserInfo, PartyID, Context0),
    Shop = validate_party_shop(ShopID, PartyState, Context1),
    ok = validate_invoice_params(InvoiceParams, Shop, Context1),
    {ok, Context2} = start(ID, {InvoiceParams, construct_party_ref(PartyState)}, Context1),
    {ID, Context2};

handle_function('Get', {UserInfo, InvoiceID}, Context0, _Opts) ->
    {St, Context1} = get_state(UserInfo, InvoiceID, Context0),
    {_PartyState, Context2} = get_party(UserInfo, get_party_ref(St), Context1),
    {get_invoice_state(St), Context2};

handle_function('GetEvents', {UserInfo, InvoiceID, Range}, Context0, _Opts) ->
    %% TODO access control
    get_public_history(UserInfo, InvoiceID, Range, Context0);

handle_function('StartPayment', {UserInfo, InvoiceID, PaymentParams}, Context0, _Opts) ->
    {St0, Context1} = get_initial_state(UserInfo, InvoiceID, Context0),
    {PartyState, Context2} = get_party(UserInfo, get_party_ref(St0), Context1),
    _Shop = validate_party_shop(get_shop_id(St0), PartyState, Context2),
    call(InvoiceID, {start_payment, PaymentParams}, Context2);

handle_function('GetPayment', {UserInfo, InvoiceID, PaymentID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, InvoiceID, Context0),
    case get_payment_session(PaymentID, St) of
        PaymentSession when PaymentSession /= undefined ->
            {hg_invoice_payment:get_payment(PaymentSession), Context};
        undefined ->
            throw({#payproc_InvoicePaymentNotFound{}, Context})
    end;

handle_function('Fulfill', {_UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    %% TODO access control
    call(InvoiceID, {fulfill, Reason}, Context0);

handle_function('Rescind', {_UserInfo, InvoiceID, Reason}, Context0, _Opts) ->
    %% TODO access control
    call(InvoiceID, {rescind, Reason}, Context0).

get_party(UserInfo, #domain_PartyRef{id = PartyID}, Context) ->
    get_party(UserInfo, PartyID, Context);
get_party(UserInfo, PartyID, Context) ->
    hg_party:get(UserInfo, PartyID, Context).

validate_party_shop(ShopID, #payproc_PartyState{party = Party}, Context) ->
    _ = assert_party_operable(Party, Context),
    _ = assert_shop_operable(Shop = get_party_shop(ShopID, Party), Context),
    Shop.

construct_party_ref(#payproc_PartyState{party = #domain_Party{id = ID}, revision = Revision}) ->
    #domain_PartyRef{id = ID, revision = Revision}.

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

-spec process_callback(tag(), callback(), woody_client:context()) ->
    {{ok, callback_response()} | {error, notfound | failed}, woody_client:context()} | no_return().

process_callback(Tag, Callback, Context) ->
    hg_machine:call(?NS, {tag, Tag}, {callback, Callback}, opts(Context)).

%%

get_history(_UserInfo, InvoiceID, Context) ->
    map_history_error(hg_machine:get_history(?NS, InvoiceID, opts(Context))).

get_history(_UserInfo, InvoiceID, AfterID, Limit, Context) ->
    map_history_error(hg_machine:get_history(?NS, InvoiceID, AfterID, Limit, opts(Context))).

get_state(UserInfo, InvoiceID, Context0) ->
    {{History, _LastID}, Context} = get_history(UserInfo, InvoiceID, Context0),
    {collapse_history(History), Context}.

get_initial_state(UserInfo, InvoiceID, Context0) ->
    {{History, _LastID}, Context} = get_history(UserInfo, InvoiceID, undefined, 1, Context0),
    {collapse_history(History), Context}.

get_public_history(UserInfo, InvoiceID, #payproc_EventRange{'after' = AfterID, limit = Limit}, Context) ->
    hg_history:get_public_history(
        fun (ID, Lim, Ctx) -> get_history(UserInfo, InvoiceID, ID, Lim, Ctx) end,
        fun (Event) -> publish_invoice_event(InvoiceID, Event) end,
        AfterID, Limit,
        Context
    ).

publish_invoice_event(InvoiceID, {ID, Dt, Event}) ->
    case publish_event(InvoiceID, Event) of
        {true, {Source, Seq, Ev}} ->
            {true, #payproc_Event{id = ID, source = Source, created_at = Dt, sequence = Seq, payload = Ev}};
        false ->
            false
    end.

start(ID, Args, Context) ->
    map_start_error(hg_machine:start(?NS, ID, Args, opts(Context))).

call(ID, Args, Context) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args, opts(Context))).

map_error({{ok, CallResult}, Context}) ->
    case CallResult of
        {exception, Reason} ->
            throw({Reason, Context});
        {ok, Result} ->
            {Result, Context}
    end;
map_error({{error, notfound}, Context}) ->
    throw({#payproc_UserInvoiceNotFound{}, Context});
map_error({{error, Reason}, _Context}) ->
    error(Reason).

map_history_error({{ok, Result}, Context}) ->
    {Result, Context};
map_history_error({{error, notfound}, Context}) ->
    throw({#payproc_PartyNotFound{}, Context});
map_history_error({{error, Reason}, _Context}) ->
    error(Reason).

map_start_error({{ok, _}, Context}) ->
    {ok, Context};
map_start_error({{error, Reason}, _Context}) ->
    error(Reason).

opts(Context) ->
    #{client_context => Context}.

%%

-type party_ref() :: dmsl_domain_thrift:'PartyRef'().
-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type invoice_id() :: dmsl_domain_thrift:'InvoiceID'().
-type user_info() :: dmsl_payment_processing_thrift:'UserInfo'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().
-type payment_id() :: dmsl_domain_thrift:'InvoicePaymentID'().
-type payment_st() :: hg_invoice_payment:st().
-type sequence() :: pos_integer().

-type ev() ::
    {public, sequence(), dmsl_payment_processing_thrift:'EventPayload'()} |
    {private, sequence(), private_event()}.

-type private_event() ::
    term(). %% TODO hg_invoice_payment:private_event() ?

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

-spec init(invoice_id(), {invoice_params(), party_ref()}, hg_machine:context()) ->
    {hg_machine:result(ev()), hg_machine:context()}.

init(ID, {InvoiceParams, PartyRef}, Context) ->
    Invoice = create_invoice(ID, InvoiceParams, PartyRef),
    Event = {public, ?invoice_ev(?invoice_created(Invoice))},
    % TODO ugly, better to roll state and events simultaneously, hg_party-like
    {ok(Event, #st{}, set_invoice_timer(#st{invoice = Invoice})), Context}.

%%

-spec process_signal(hg_machine:signal(), hg_machine:history(ev()), hg_machine:context()) ->
    {hg_machine:result(ev()), hg_machine:context()}.

process_signal(Signal, History, Context) ->
    handle_signal(Signal, collapse_history(History), Context).

handle_signal(timeout, St, Context) ->
    case get_pending_payment(St) of
        {PaymentID, PaymentSession} ->
            % there's a payment pending
            process_payment_signal(timeout, PaymentID, PaymentSession, St, Context);
        undefined ->
            % invoice is expired
            handle_expiration(St, Context)
    end;

handle_signal({repair, _}, St, Context) ->
    {ok([], St, restore_timer(St)), Context}.

handle_expiration(St, Context) ->
    Event = {public, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(overdue))))},
    {ok(Event, St), Context}.

%%

-type call() ::
    {start_payment, payment_params(), user_info()} |
    {fulfill, binary(), user_info()} |
    {rescind, binary(), user_info()} |
    {callback, callback()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev()), woody_client:context()) ->
    {{response(), hg_machine:result(ev())}, woody_client:context()}.

process_call(Call, History, Context) ->
    St = collapse_history(History),
    try handle_call(Call, St, Context) catch
        {exception, Exception} ->
            {{{exception, Exception}, {[], restore_timer(St)}}, Context}
    end.

-spec raise(term()) -> no_return().

raise(What) ->
    throw({exception, What}).

handle_call({start_payment, PaymentParams}, St, Context) ->
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    start_payment(PaymentParams, St, Context);

handle_call({fulfill, Reason}, St, Context) ->
    _ = assert_invoice_status(paid, St),
    Event = {public, ?invoice_ev(?invoice_status_changed(?fulfilled(format_reason(Reason))))},
    {respond(ok, Event, St), Context};

handle_call({rescind, Reason}, St, Context) ->
    _ = assert_invoice_status(unpaid, St),
    _ = assert_no_pending_payment(St),
    Event = {public, ?invoice_ev(?invoice_status_changed(?cancelled(format_reason(Reason))))},
    {respond(ok, Event, St), Context};

handle_call({callback, Callback}, St, Context) ->
    dispatch_callback(Callback, St, Context).

dispatch_callback({provider, Payload}, St, Context) ->
    case get_pending_payment(St) of
        {PaymentID, PaymentSession} ->
            process_payment_call({callback, Payload}, PaymentID, PaymentSession, St, Context);
        undefined ->
            raise(no_pending_payment) % FIXME
    end.

assert_invoice_status(Status, #st{invoice = Invoice}) ->
    assert_invoice_status(Status, Invoice);
assert_invoice_status(Status, #domain_Invoice{status = {Status, _}}) ->
    ok;
assert_invoice_status(_Status, #domain_Invoice{status = Invalid}) ->
    raise(?invalid_invoice_status(Invalid)).

assert_no_pending_payment(St) ->
    case get_pending_payment(St) of
        undefined ->
            ok;
        {PaymentID, _} ->
            raise(?payment_pending(PaymentID))
    end.

restore_timer(St) ->
    set_invoice_timer(St).

set_invoice_timer(St = #st{invoice = #domain_Invoice{status = Status, due = Due}}) ->
    case get_pending_payment(St) of
        undefined when Status == ?unpaid() ->
            hg_machine_action:set_deadline(Due);
        undefined ->
            hg_machine_action:new();
        {_, _} ->
            % TODO how to restore timer properly then, magic number for now
            hg_machine_action:set_timeout(10)
    end.

%%

start_payment(PaymentParams, St, Context0) ->
    PaymentID = create_payment_id(St),
    {Party, Context} = checkout_party(St, Context0),
    Opts = get_payment_opts(Party, St),
    {{Events1, _}, Context1} = hg_invoice_payment:init(PaymentID, PaymentParams, Opts, Context),
    {{Events2, Action}, Context2} = hg_invoice_payment:start_session(?processed(), Context1),
    Events = wrap_payment_events(PaymentID, Events1 ++ Events2),
    {respond(PaymentID, Events, St, Action), Context2}.

process_payment_signal(Signal, PaymentID, PaymentSession, St, Context0) ->
    {Party, Context} = checkout_party(St, Context0),
    Opts = get_payment_opts(Party, St),
    case hg_invoice_payment:process_signal(Signal, PaymentSession, Opts, Context) of
        {{next, {Events, Action}}, Context1} ->
            {ok(wrap_payment_events(PaymentID, Events), St, Action), Context1};
        {{done, {Events1, _}}, Context1} ->
            PaymentSession1 = lists:foldl(fun hg_invoice_payment:merge_event/2, PaymentSession, Events1),
            case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
                ?processed() ->
                    {{Events2, Action}, Context2} = hg_invoice_payment:start_session(?captured(), Context1),
                    {ok(wrap_payment_events(PaymentID, Events1 ++ Events2), St, Action), Context2};
                ?captured() ->
                    Events2 = [{public, ?invoice_ev(?invoice_status_changed(?paid()))}],
                    {ok(wrap_payment_events(PaymentID, Events1) ++ Events2, St), Context1};
                ?failed(_) ->
                    %% TODO: fix this dirty hack
                    TmpPayments = lists:keydelete(PaymentID, 1, St#st.payments),
                    {
                        ok(wrap_payment_events(PaymentID, Events1), St, restore_timer(St#st{payments = TmpPayments})),
                        Context1
                    }
            end
    end.

process_payment_call(Call, PaymentID, PaymentSession, St, Context0) ->
    {Party, Context} = checkout_party(St, Context0),
    Opts = get_payment_opts(Party, St),
    case hg_invoice_payment:process_call(Call, PaymentSession, Opts, Context) of
        {{Response, {next, {Events, Action}}}, Context1} ->
            {respond(Response, wrap_payment_events(PaymentID, Events), St, Action), Context1};
        {{Response, {done, {Events1, _}}}, Context1} ->
            PaymentSession1 = lists:foldl(fun hg_invoice_payment:merge_event/2, PaymentSession, Events1),
            case get_payment_status(hg_invoice_payment:get_payment(PaymentSession1)) of
                ?processed() ->
                    {{Events2, Action}, Context2} = hg_invoice_payment:start_session(?captured(), Context1),
                    Events = wrap_payment_events(PaymentID, Events1 ++ Events2),
                    {respond(Response, Events, St, Action), Context2};
                ?captured() ->
                    Events2 = [{public, ?invoice_ev(?invoice_status_changed(?paid()))}],
                    {respond(Response, wrap_payment_events(PaymentID, Events1) ++ Events2, St), Context1};
                ?failed(_) ->
                    %% TODO: fix this dirty hack
                    TmpPayments = lists:keydelete(PaymentID, 1, St#st.payments),
                    {
                        respond(
                            Response,
                            wrap_payment_events(PaymentID, Events1),
                            St,
                            restore_timer(St#st{payments = TmpPayments})
                        ),
                        Context1
                    }
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

checkout_party(St, Context = #{client_context := ClientContext}) ->
    #domain_PartyRef{id = PartyID, revision = Revision} = get_party_ref(St),
    {Result, ClientContext1} = hg_party:checkout(PartyID, Revision, ClientContext),
    {Result, Context#{client_context := ClientContext1}}.

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

create_invoice(ID, V = #payproc_InvoiceParams{}, PartyRef) ->
    Revision = hg_domain:head(),
    Currency = hg_domain:get(Revision, {currency, V#payproc_InvoiceParams.currency}),
    #domain_Invoice{
        id              = ID,
        shop_id         = V#payproc_InvoiceParams.shop_id,
        owner           = PartyRef,
        created_at      = hg_datetime:format_now(),
        status          = ?unpaid(),
        domain_revision = Revision,
        due             = V#payproc_InvoiceParams.due,
        product         = V#payproc_InvoiceParams.product,
        description     = V#payproc_InvoiceParams.description,
        context         = V#payproc_InvoiceParams.context,
        cost            = #domain_Cash{
            amount          = V#payproc_InvoiceParams.amount,
            currency        = Currency
        }
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
merge_event({{payment, PaymentID}, Event}, St) ->
    PaymentSession = get_payment_session(PaymentID, St),
    set_payment_session(PaymentID, hg_invoice_payment:merge_event(Event, PaymentSession), St).

merge_invoice_event(?invoice_created(Invoice), St) ->
    St#st{invoice = Invoice};
merge_invoice_event(?invoice_status_changed(Status), St = #st{invoice = I}) ->
    St#st{invoice = I#domain_Invoice{status = Status}}.

get_party_ref(#st{invoice = #domain_Invoice{owner = PartyRef}}) ->
    PartyRef.

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

get_pending_payment(#st{payments = Payments}) ->
    find_pending_payment(Payments).

find_pending_payment([V = {_PaymentID, PaymentSession} | Rest]) ->
    case get_payment_status(hg_invoice_payment:get_payment(PaymentSession)) of
        ?pending() ->
            V;
        ?processed() ->
            V;
        _ ->
            find_pending_payment(Rest)
    end;
find_pending_payment([]) ->
    undefined.

%%

%% TODO: fix this dirty hack
format_reason({Pre, V}) ->
    genlib:format("~s: ~s", [Pre, genlib:to_binary(V)]);
format_reason(V) ->
    genlib:to_binary(V).

%%

get_party_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

get_shop_currency(#domain_Shop{accounts = #domain_ShopAccountSet{currency = Currency}}) ->
    Currency.

assert_party_operable(#domain_Party{blocking = Blocking, suspension = Suspension} = V, Context) ->
    _ = assert_party_unblocked(Blocking, Context),
    _ = assert_party_active(Suspension, Context),
    V.

assert_party_unblocked(V = {Status, _}, Context) ->
    Status == unblocked orelse throw({#payproc_InvalidPartyStatus{status = {blocking, V}}, Context}).

assert_party_active(V = {Status, _}, Context) ->
    Status == active orelse throw({#payproc_InvalidPartyStatus{status = {suspension, V}}, Context}).

assert_shop_operable(undefined, Context) ->
    throw({#payproc_ShopNotFound{}, Context});
assert_shop_operable(#domain_Shop{blocking = Blocking, suspension = Suspension} = V, Context) ->
    _ = assert_shop_unblocked(Blocking, Context),
    _ = assert_shop_active(Suspension, Context),
    V.

assert_shop_unblocked(V = {Status, _}, Context) ->
    Status == unblocked orelse throw({#payproc_InvalidShopStatus{status = {blocking, V}}, Context}).

assert_shop_active(V = {Status, _}, Context) ->
    Status == active orelse throw({#payproc_InvalidShopStatus{status = {suspension, V}}, Context}).

%%

validate_invoice_params(
    #payproc_InvoiceParams{
        currency = Currency,
        amount = Amount
    },
    Shop,
    Context
) ->
    _ = validate_amount(Amount, Context),
    _ = validate_currency(Currency, get_shop_currency(Shop), Context),
    ok.

validate_amount(Amount, _) when Amount > 0 ->
    ok;
validate_amount(_, Context) ->
    throw({#'InvalidRequest'{errors = [<<"Invalid amount">>]}, Context}).

validate_currency(Currency, Currency, _) ->
    ok;
validate_currency(_, _, Context) ->
    throw({#'InvalidRequest'{errors = [<<"Invalid currency">>]}, Context}).
