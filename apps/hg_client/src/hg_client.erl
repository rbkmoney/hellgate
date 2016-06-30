-module(hg_client).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-export([new/2]).
-export([new/3]).

-export([create_invoice/2]).
-export([get_invoice/2]).
-export([fulfill_invoice/3]).
-export([rescind_invoice/3]).
-export([start_payment/3]).

-export([get_next_event/2]).
-export([get_next_event/3]).

-export_type([t/0]).

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

-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type event_id() :: hg_payment_processing_thrift:'EventID'().
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: hg_payment_processing_thrift:'InvoicePaymentParams'().

-spec new(woody_t:url(), user_info()) -> t().

new(RootUrl, UserInfo) ->
    new(RootUrl, UserInfo, construct_context()).

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, ?MODULE).

-spec new(woody_t:url(), user_info(), woody_client:context()) -> t().

new(RootUrl, UserInfo, Context) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {RootUrl, UserInfo, Context}, []),
    Pid.

%%

-spec create_invoice(invoice_params(), t()) ->
    {{ok, invoice_id()} | woody_client:result_error(), t()}.

create_invoice(InvoiceParams, Client) ->
    do_service_call(Client, 'Create', [InvoiceParams]).

-spec get_invoice(invoice_id(), t()) ->
    {{ok, hg_payment_processing_thrift:'InvoiceState'()} | woody_client:result_error(), t()}.

get_invoice(InvoiceID, Client) ->
    do_service_call(Client, 'Get', [InvoiceID]).

-spec fulfill_invoice(invoice_id(), binary(), t()) ->
    {ok | woody_client:result_error(), t()}.

fulfill_invoice(InvoiceID, Reason, Client) ->
    do_service_call(Client, 'Fulfill', [InvoiceID, Reason]).

-spec rescind_invoice(invoice_id(), binary(), t()) ->
    {ok | woody_client:result_error(), t()}.

rescind_invoice(InvoiceID, Reason, Client) ->
    do_service_call(Client, 'Rescind', [InvoiceID, Reason]).

-spec start_payment(invoice_id(), payment_params(), t()) ->
    {{ok, payment_id()} | woody_client:result_error(), t()}.

start_payment(InvoiceID, PaymentParams, Client) ->
    do_service_call(Client, 'StartPayment', [InvoiceID, PaymentParams]).

-spec get_next_event(invoice_id(), t()) ->
    {{ok, tuple()} | timeout | woody_client:result_error(), t()}.

get_next_event(InvoiceID, Client) ->
    get_next_event(InvoiceID, ?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec get_next_event(invoice_id(), timeout(), t()) ->
    {{ok, tuple()} | timeout | woody_client:result_error(), t()}.

get_next_event(InvoiceID, Timeout, Client) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {get_next_event, InvoiceID, Timeout}, infinity).

do_service_call(Client, Function, Args) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {issue_service_call, Function, Args}, infinity).

%%

-record(cl, {
    root_url          :: woody_t:url(),
    user_info         :: user_info(),
    context           :: woody_client:context(),
    last_events = #{} :: #{invoice_id() => event_id()}
}).

-type cl() :: #cl{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init({woody_t:url(), user_info(), woody_client:context()}) ->
    {ok, cl()}.

init({RootUrl, UserInfo, Context}) ->
    {ok, #cl{context = Context, user_info = UserInfo, root_url = RootUrl}}.

-spec handle_call(term(), callref(), cl()) ->
    {reply, term(), cl()} | {noreply, cl()}.

handle_call({issue_service_call, Function, Args}, _From, Client) ->
    {Result, ClientNext} = issue_service_call(Function, [get_user_info(Client) | Args], Client),
    {reply, Result, ClientNext};

handle_call({get_next_event, InvoiceID, Timeout}, _From, Client) ->
    {Result, ClientNext} = poll_next_event(InvoiceID, Timeout, Client),
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

poll_next_event(_InvoiceID, Timeout, Client) when Timeout =< 0 ->
    {timeout, Client};
poll_next_event(InvoiceID, Timeout, Client) ->
    StartTs = genlib_time:ticks(),
    UserInfo = get_user_info(Client),
    Range = construct_range(InvoiceID, Client),
    {Result, ClientNext} = issue_service_call('GetEvents', [UserInfo, InvoiceID, Range], Client),
    case Result of
        {ok, []} ->
            _ = timer:sleep(?POLL_INTERVAL),
            poll_next_event(InvoiceID, compute_timeout_left(StartTs, Timeout), ClientNext);
        {ok, [#payproc_Event{id = EventID, payload = Event} | _Rest]} ->
            {{ok, Event}, update_last_events(InvoiceID, EventID, ClientNext)};
        {What, _} when What =:= exception; What =:= error ->
            {Result, ClientNext}
    end.

construct_range(InvoiceID, #cl{last_events = LastEvents}) ->
    #payproc_EventRange{'after' = genlib_map:get(InvoiceID, LastEvents), limit = 1}.

update_last_events(InvoiceID, EventID, Client = #cl{last_events = LastEvents}) ->
    Client#cl{last_events = LastEvents#{InvoiceID => EventID}}.

issue_service_call(Function, Args, Client = #cl{context = Context, root_url = RootUrl}) ->
    {_Name, Path, Service} = hg_proto:get_service_spec(invoicing),
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
