-module(hg_client).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-export([new/1]).
-export([new/2]).

-export([create_invoice/3]).
-export([get_invoice/3]).
-export([fulfill_invoice/4]).
-export([void_invoice/4]).

-export([get_next_event/4]).

-export_type([t/0]).

%%

-behaviour(woody_event_handler).
-export([handle_event/3]).

%%

-define(POLL_INTERVAL, 1000).

-record(cl, {
    root_url      :: woody_t:url(),
    context       :: woody_client:context(),
    last_event_id :: hg_payment_processing_thrift:'EventID'()
}).

-opaque t() :: #cl{}.

-spec new(woody_t:url()) -> t().

new(RootUrl) ->
    new(RootUrl, construct_context()).

-spec new(woody_t:url(), woody_client:context()) -> t().

new(RootUrl, Context) ->
    #cl{context = Context, root_url = RootUrl}.

%%

-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type invoice_id() :: hg_domain_thrift:'InvoiceID'().

-spec create_invoice(user_info(), hg_payment_processing_thrift:'InvoiceParams'(), t()) ->
    {{ok, invoice_id()} | {exception, term()} | {error, term()}, t()}.

create_invoice(UserInfo, InvoiceParams, Client) ->
    call_service('Create', [UserInfo, InvoiceParams], Client).

-spec get_invoice(user_info(), invoice_id(), t()) ->
    {{ok, hg_payment_processing_thrift:'InvoiceState'()} | {exception, term()} | {error, term()}, t()}.

get_invoice(UserInfo, InvoiceID, Client) ->
    call_service('Get', [UserInfo, InvoiceID], Client).

-spec fulfill_invoice(user_info(), invoice_id(), binary(), t()) ->
    {ok | {exception, term()} | {error, term()}, t()}.

fulfill_invoice(UserInfo, InvoiceID, Reason, Client) ->
    call_service('Fulfill', [UserInfo, InvoiceID, Reason], Client).

-spec void_invoice(user_info(), invoice_id(), binary(), t()) ->
    {ok | {exception, term()} | {error, term()}, t()}.

void_invoice(UserInfo, InvoiceID, Reason, Client) ->
    call_service('Void', [UserInfo, InvoiceID, Reason], Client).

-spec get_next_event(user_info(), invoice_id(), timeout(), t()) ->
    {{ok, tuple()} | timeout | {exception, term()} | {error, term()}, t()}.

get_next_event(_UserInfo, _InvoiceID, Timeout, Client) when Timeout =< 0 ->
    {timeout, Client};
get_next_event(UserInfo, InvoiceID, Timeout, Client = #cl{last_event_id = After}) ->
    StartTs = genlib_time:ticks(),
    Range = #'EventRange'{'after' = After, limit = 1},
    {Result, ClientNext} = call_service('GetEvents', [UserInfo, InvoiceID, Range], Client),
    case Result of
        {ok, []} ->
            _ = timer:sleep(?POLL_INTERVAL),
            get_next_event(UserInfo, InvoiceID, compute_timeout_left(StartTs, Timeout), ClientNext);
        {ok, [#'Event'{id = EventID, ev = {_, Event}} | _Rest]} ->
            {{ok, Event}, ClientNext#cl{last_event_id = EventID}};
        {What, _} when What =:= exception; What =:= error ->
            Result
    end.

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, ?MODULE).

call_service(Function, Args, Client = #cl{context = Context, root_url = RootUrl}) ->
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
