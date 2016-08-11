-module(hg_client).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-export([start/2]).
-export([start_link/2]).
-export([stop/1]).

%% Invoicing

-export([create_invoice/2]).
-export([get_invoice/2]).
-export([fulfill_invoice/3]).
-export([rescind_invoice/3]).
-export([start_payment/3]).
-export([pull_invoice_event/2]).
-export([pull_invoice_event/3]).

%% Partying

-export([create_party/2]).
-export([get_party/2]).
-export([block_party/3]).
-export([unblock_party/3]).
-export([suspend_party/2]).
-export([activate_party/2]).
-export([get_claim/3]).
-export([pull_party_event/2]).
-export([pull_party_event/3]).

%% Eventsink

-export([get_last_event_id/1]).
-export([pull_events/3]).

%%

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%%

-behaviour(woody_event_handler).
-export([handle_event/3]).

%%

-define(POLL_INTERVAL, 1000).
-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-opaque t() :: pid().

-export_type([t/0]).

-type machine_id() :: hg_base_thrift:'ID'().
-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type party_id() :: hg_domain_thrift:'PartyID'().
-type claim_id() :: hg_payment_processing_thrift:'ClaimID'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type event_id() :: hg_base_thrift:'EventID'().
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: hg_payment_processing_thrift:'InvoicePaymentParams'().

-spec start(woody_t:url(), user_info()) -> t().

start(RootUrl, UserInfo) ->
    start(start, RootUrl, UserInfo, construct_context()).

-spec start_link(woody_t:url(), user_info()) -> t().

start_link(RootUrl, UserInfo) ->
    start(start_link, RootUrl, UserInfo, construct_context()).

start(Mode, RootUrl, UserInfo, Context) ->
    {ok, Pid} = gen_server:Mode(?MODULE, {RootUrl, UserInfo, Context}, []),
    Pid.

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, ?MODULE).

-spec stop(t()) -> ok.

stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create_invoice(invoice_params(), t()) ->
    {ok, invoice_id()} | woody_client:result_error().

create_invoice(InvoiceParams, Client) ->
    call_invoicing(Client, 'Create', [InvoiceParams]).

-spec get_invoice(invoice_id(), t()) ->
    {ok, hg_payment_processing_thrift:'InvoiceState'()} | woody_client:result_error().

get_invoice(InvoiceID, Client) ->
    call_invoicing(Client, 'Get', [InvoiceID]).

-spec fulfill_invoice(invoice_id(), binary(), t()) ->
    ok | woody_client:result_error().

fulfill_invoice(InvoiceID, Reason, Client) ->
    call_invoicing(Client, 'Fulfill', [InvoiceID, Reason]).

-spec rescind_invoice(invoice_id(), binary(), t()) ->
    ok | woody_client:result_error().

rescind_invoice(InvoiceID, Reason, Client) ->
    call_invoicing(Client, 'Rescind', [InvoiceID, Reason]).

-spec start_payment(invoice_id(), payment_params(), t()) ->
    {ok, payment_id()} | woody_client:result_error().

start_payment(InvoiceID, PaymentParams, Client) ->
    call_invoicing(Client, 'StartPayment', [InvoiceID, PaymentParams]).

-spec pull_invoice_event(invoice_id(), t()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_invoice_event(InvoiceID, Client) ->
    pull_invoice_event(InvoiceID, ?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_invoice_event(invoice_id(), timeout(), t()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_invoice_event(InvoiceID, Timeout, Client) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {pull_machine_event, {invoice, InvoiceID}, Timeout}, infinity).

%%

-spec create_party(party_id(), t()) ->
    ok | woody_client:result_error().

create_party(PartyID, Client) ->
    call_party_mgmt(Client, 'Create', [PartyID]).

-spec get_party(party_id(), t()) ->
    {ok, hg_payment_processing_thrift:'PartyState'()} | woody_client:result_error().

get_party(PartyID, Client) ->
    call_party_mgmt(Client, 'Get', [PartyID]).

-spec block_party(party_id(), binary(), t()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

block_party(PartyID, Reason, Client) ->
    call_party_mgmt(Client, 'Block', [PartyID, Reason]).

-spec unblock_party(party_id(), binary(), t()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

unblock_party(PartyID, Reason, Client) ->
    call_party_mgmt(Client, 'Unblock', [PartyID, Reason]).

-spec suspend_party(party_id(), t()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

suspend_party(PartyID, Client) ->
    call_party_mgmt(Client, 'Suspend', [PartyID]).

-spec activate_party(party_id(), t()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

activate_party(PartyID, Client) ->
    call_party_mgmt(Client, 'Activate', [PartyID]).

-spec get_claim(party_id(), claim_id(), t()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

get_claim(PartyID, ID, Client) ->
    call_party_mgmt(Client, 'GetClaim', [PartyID, ID]).

-spec pull_party_event(party_id(), t()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_party_event(PartyID, Client) ->
    pull_party_event(PartyID, ?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_party_event(party_id(), timeout(), t()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_party_event(PartyID, Timeout, Client) ->
    gen_server:call(Client, {pull_machine_event, {party, PartyID}, Timeout}, infinity).

%%

-spec get_last_event_id(t()) ->
    {ok, event_id()} | none | woody_client:result_error().

get_last_event_id(Client) ->
    case call_eventsink(Client, 'GetLastEventID', []) of
        {ok, EventID} ->
            {ok, EventID};
        {exception, #payproc_NoLastEvent{}} ->
            none;
        Error ->
            Error
    end.

-spec pull_events(pos_integer(), timeout(), t()) ->
    {ok, [tuple()]} | woody_client:result_error().

pull_events(N, Timeout, Client) when N > 0 ->
    gen_server:call(Client, {pull_events, N, Timeout}, infinity).

%%

call_invoicing(Client, Function, Args) ->
    gen_server:call(Client, {call_w_user_info, invoicing, Function, Args}).

call_party_mgmt(Client, Function, Args) ->
    gen_server:call(Client, {call_w_user_info, party_management, Function, Args}).

call_eventsink(Client, Function, Args) ->
    gen_server:call(Client, {call, eventsink, Function, Args}).

%%

-record(cl, {
    root_url                  :: woody_t:url(),
    user_info                 :: user_info(),
    context                   :: woody_client:context(),
    last_sink_event           :: event_id(),
    last_machine_events = #{} :: #{{atom(), machine_id()} => event_id()}
}).

-type cl() :: #cl{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init({woody_t:url(), user_info(), woody_client:context()}) ->
    {ok, cl()}.

init({RootUrl, UserInfo, Context}) ->
    {ok, #cl{context = Context, user_info = UserInfo, root_url = RootUrl}}.

-spec handle_call(term(), callref(), cl()) ->
    {reply, term(), cl()} | {noreply, cl()}.

handle_call({call_w_user_info, Service, Function, Args}, _From, Client) ->
    UserInfo = get_user_info(Client),
    {Result, ClientNext} = issue_service_call(Service, Function, [UserInfo | Args], Client),
    {reply, Result, ClientNext};

handle_call({call, Service, Function, Args}, _From, Client) ->
    {Result, ClientNext} = issue_service_call(Service, Function, Args, Client),
    {reply, Result, ClientNext};

handle_call({pull_machine_event, MachineID, Timeout}, _From, Client) ->
    {Result, ClientNext} = poll_next_machine_event(MachineID, Timeout, Client),
    {reply, Result, ClientNext};

handle_call({pull_events, N, Timeout}, _From, Client) ->
    {Result, ClientNext} = poll_events(N, Timeout, Client),
    {reply, Result, ClientNext};

handle_call(Call, _From, State) ->
    _ = lager:warning("unexpected call received: ~tp", [Call]),
    {noreply, State}.

-spec handle_cast(_, cl()) ->
    {noreply, cl()}.

handle_cast(Cast, State) ->
    _ = lager:warning("unexpected cast received: ~tp", [Cast]),
    {noreply, State}.

-spec handle_info(_, cl()) ->
    {noreply, cl()}.

handle_info(Info, State) ->
    _ = lager:warning("unexpected info received: ~tp", [Info]),
    {noreply, State}.

-spec terminate(Reason, cl()) ->
    ok when
        Reason :: normal | shutdown | {shutdown, term()} | term().

terminate(_Reason, _State) ->
    ok.

-spec code_change(Vsn | {down, Vsn}, cl(), term()) ->
    {error, noimpl} when
        Vsn :: term().

code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.

%%

poll_next_machine_event(MachineID, Timeout, Client) ->
    Call = get_events_call(MachineID),
    case poll_events(get_last_machine_event(MachineID, Client), 1, Call, Timeout, [], Client) of
        {[], ClientNext} ->
            {timeout, ClientNext};
        {[Event], ClientNext} ->
            {{ok, strip_event(Event)}, update_last_machine_event(MachineID, Event, ClientNext)};
        Error ->
            Error
    end.

get_events_call({invoice, InvoiceID}) ->
    fun (Range, Cl) ->
        issue_service_call(invoicing, 'GetEvents', [get_user_info(Cl), InvoiceID, Range], Cl)
    end;
get_events_call({party, PartyID}) ->
    fun (Range, Cl) ->
        issue_service_call(party_management, 'GetEvents', [get_user_info(Cl), PartyID, Range], Cl)
    end.

get_last_machine_event(MachineID, #cl{last_machine_events = LastEvents}) ->
    genlib_map:get(MachineID, LastEvents).

update_last_machine_event(MachineID, Event, Client = #cl{last_machine_events = LastEvents}) ->
    #payproc_Event{id = EventID} = Event,
    Client#cl{last_machine_events = LastEvents#{MachineID => EventID}}.

poll_events(N, Timeout, Client) ->
    Call = fun (Range, Cl) -> issue_service_call(eventsink, 'GetEvents', [Range], Cl) end,
    case poll_events(get_last_sink_event(Client), N, Call, Timeout, [], Client) of
        {Events, ClientNext} when is_list(Events) ->
            {{ok, Events}, update_last_sink_event(Events, ClientNext)};
        Result ->
            Result
    end.

get_last_sink_event(#cl{last_sink_event = EventID}) ->
    EventID.

update_last_sink_event([], Client) ->
    Client;
update_last_sink_event(Events, Client) ->
    #payproc_Event{id = EventID} = lists:last(Events),
    Client#cl{last_sink_event = EventID}.

poll_events(_, _, _, Timeout, Acc, Client) when Timeout =< 0 ->
    {Acc, Client};
poll_events(After, N, Call, Timeout, Acc, Client) ->
    StartTs = genlib_time:ticks(),
    Range = construct_range(After, N, Acc),
    {Result, ClientNext} = Call(Range, Client),
    case Result of
        {ok, Events} when length(Events) == N ->
            {Acc ++ Events, ClientNext};
        {ok, Events} ->
            _ = timer:sleep(?POLL_INTERVAL),
            TimeoutLeft = compute_timeout_left(StartTs, Timeout),
            poll_events(After, N - length(Events), Call, TimeoutLeft, Acc ++ Events, ClientNext);
        _Error ->
            {Result, ClientNext}
    end.

construct_range(After, N, []) ->
    #payproc_EventRange{'after' = After, limit = N};
construct_range(_After, N, Events) ->
    #payproc_Event{id = LastEvent} = lists:last(Events),
    #payproc_EventRange{'after' = LastEvent, limit = N}.

strip_event(#payproc_Event{payload = Payload}) ->
    Payload.

issue_service_call(ServiceName, Function, Args, Client = #cl{context = Context, root_url = RootUrl}) ->
    {Path, Service} = hg_proto:get_service_spec(ServiceName),
    Url = iolist_to_binary([RootUrl, Path]),
    Request = {Service, Function, Args},
    {Result, ContextNext} = woody_client:call_safe(Context, Request, #{url => Url}),
    {Result, Client#cl{context = ContextNext}}.

compute_timeout_left(StartTs, TimeoutWas) ->
    TimeoutWas - (genlib_time:ticks() - StartTs) div 1000.

get_user_info(#cl{user_info = UserInfo}) ->
    UserInfo.

%%

-spec handle_event(EventType, RpcID, EventMeta)
    -> _ when
        EventType :: woody_event_handler:event_type(),
        RpcID ::  woody_t:rpc_id(),
        EventMeta :: woody_event_handler:event_meta_type().

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}) ->
    lager:error(
        maps:to_list(RpcID),
        "[client] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta) ->
    lager:debug(maps:to_list(RpcID), "[client] ~s: ~p", [EventType, EventMeta]).
