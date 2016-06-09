-module(hg_client).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-export([new/1]).
-export([new/2]).

-export([create_invoice/3]).
-export([get_invoice/3]).
-export([fulfill_invoice/4]).
-export([void_invoice/4]).
-export([start_payment/4]).

-export([get_next_event/4]).

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

-opaque t() :: pid().

-spec new(woody_t:url()) -> t().

new(RootUrl) ->
    new(RootUrl, construct_context()).

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, ?MODULE).

-spec new(woody_t:url(), woody_client:context()) -> t().

new(RootUrl, Context) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Context, RootUrl}, []),
    Pid.

%%

-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().
-type payment_id() :: hg_domain_thrift:'InvoicePaymentID'().
-type invoice_params() :: hg_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: hg_payment_processing_thrift:'InvoicePaymentParams'().

-spec create_invoice(user_info(), invoice_params(), t()) ->
    {{ok, invoice_id()} | woody_client:result_error(), t()}.

create_invoice(UserInfo, InvoiceParams, Client) ->
    do_service_call(Client, 'Create', [UserInfo, InvoiceParams]).

-spec get_invoice(user_info(), invoice_id(), t()) ->
    {{ok, hg_payment_processing_thrift:'InvoiceState'()} | woody_client:result_error(), t()}.

get_invoice(UserInfo, InvoiceID, Client) ->
    do_service_call(Client, 'Get', [UserInfo, InvoiceID]).

-spec fulfill_invoice(user_info(), invoice_id(), binary(), t()) ->
    {ok | woody_client:result_error(), t()}.

fulfill_invoice(UserInfo, InvoiceID, Reason, Client) ->
    do_service_call(Client, 'Fulfill', [UserInfo, InvoiceID, Reason]).

-spec void_invoice(user_info(), invoice_id(), binary(), t()) ->
    {ok | woody_client:result_error(), t()}.

void_invoice(UserInfo, InvoiceID, Reason, Client) ->
    do_service_call(Client, 'Void', [UserInfo, InvoiceID, Reason]).

-spec start_payment(user_info(), invoice_id(), payment_params(), t()) ->
    {{ok, payment_id()} | woody_client:result_error(), t()}.

start_payment(UserInfo, InvoiceID, PaymentParams, Client) ->
    do_service_call(Client, 'StartPayment', [UserInfo, InvoiceID, PaymentParams]).

-spec get_next_event(user_info(), invoice_id(), timeout(), t()) ->
    {{ok, tuple()} | timeout | woody_client:result_error(), t()}.

get_next_event(UserInfo, InvoiceID, Timeout, Client) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {get_next_event, InvoiceID, UserInfo, Timeout}, infinity).

do_service_call(Client, Function, Args) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {issue_service_call, Function, Args}, infinity).

%%

-record(cl, {
    root_url          :: woody_t:url(),
    context           :: woody_client:context(),
    last_events = #{} :: #{invoice_id() => hg_payment_processing_thrift:'EventID'()}
}).

-type cl() :: #cl{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init({woody_client:context(), woody_t:url()}) ->
    {ok, cl()}.

init({Context, RootUrl}) ->
    {ok, #cl{context = Context, root_url = RootUrl}}.

-spec handle_call(term(), callref(), cl()) ->
    {reply, term(), cl()} | {noreply, cl()}.

handle_call({issue_service_call, Function, Args}, _From, Client) ->
    {Result, ClientNext} = issue_service_call(Function, Args, Client),
    {reply, Result, ClientNext};

handle_call({get_next_event, InvoiceID, UserInfo, Timeout}, _From, Client) ->
    {Result, ClientNext} = poll_next_event(InvoiceID, UserInfo, Timeout, Client),
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

poll_next_event(_InvoiceID, _UserInfo, Timeout, Client) when Timeout =< 0 ->
    {timeout, Client};
poll_next_event(InvoiceID, UserInfo, Timeout, Client) ->
    Range = construct_range(InvoiceID, Client),
    StartTs = genlib_time:ticks(),
    {Result, ClientNext} = issue_service_call('GetEvents', [UserInfo, InvoiceID, Range], Client),
    case Result of
        {ok, []} ->
            _ = timer:sleep(?POLL_INTERVAL),
            poll_next_event(InvoiceID, UserInfo, compute_timeout_left(StartTs, Timeout), ClientNext);
        {ok, [#'Event'{id = EventID, ev = {_, Event}} | _Rest]} ->
            {{ok, Event}, update_last_events(InvoiceID, EventID, ClientNext)};
        {What, _} when What =:= exception; What =:= error ->
            {Result, ClientNext}
    end.

construct_range(InvoiceID, #cl{last_events = LastEvents}) ->
    #'EventRange'{'after' = genlib_map:get(InvoiceID, LastEvents), limit = 1}.

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
