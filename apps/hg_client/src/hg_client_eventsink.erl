-module(hg_client_eventsink).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([start/1]).
-export([start_link/1]).
-export([stop/1]).

-export([get_last_event_id/1]).
-export([pull_events/3]).

%% GenServer

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%

-type event_id() :: dmsl_base_thrift:'EventID'().

-spec start(hg_client_api:t()) -> pid().

start(ApiClient) ->
    start(start, ApiClient).

-spec start_link(hg_client_api:t()) -> pid().

start_link(ApiClient) ->
    start(start_link, ApiClient).

start(Mode, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, ApiClient, []),
    Pid.

-spec stop(pid()) -> ok.

stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec get_last_event_id(pid()) ->
    event_id() | none | woody_error:business_error().

get_last_event_id(Client) ->
    case gen_server:call(Client, {call, 'GetLastEventID', []}) of
        {ok, EventID} when is_integer(EventID) ->
            EventID;
        {exception, #payproc_NoLastEvent{}} ->
            none;
        Error ->
            Error
    end.

-spec pull_events(pos_integer(), timeout(), pid()) ->
    [tuple()] | woody_error:business_error().

pull_events(N, Timeout, Client) when N > 0 ->
    gen_server:call(Client, {pull_events, N, Timeout}, infinity).

%%

-record(st, {
    poller    :: hg_client_event_poller:t(),
    client    :: hg_client_api:t()
}).

-type st() :: #st{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init(hg_client_api:t()) ->
    {ok, st()}.

init(ApiClient) ->
    {ok, #st{
        client = ApiClient,
        poller = hg_client_event_poller:new(eventsink, 'GetEvents', [])
    }}.

-spec handle_call(term(), callref(), st()) ->
    {reply, term(), st()} | {noreply, st()}.

handle_call({call, Function, Args}, _From, St = #st{client = Client}) ->
    {Result, ClientNext} = hg_client_api:call(eventsink, Function, Args, Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_events, N, Timeout}, _From, St = #st{client = Client, poller = Poller}) ->
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(N, Timeout, Client, Poller),
    {reply, Result, St#st{client = ClientNext, poller = PollerNext}};

handle_call(Call, _From, State) ->
    _ = lager:warning("unexpected call received: ~tp", [Call]),
    {noreply, State}.

-spec handle_cast(_, st()) ->
    {noreply, st()}.

handle_cast(Cast, State) ->
    _ = lager:warning("unexpected cast received: ~tp", [Cast]),
    {noreply, State}.

-spec handle_info(_, st()) ->
    {noreply, st()}.

handle_info(Info, State) ->
    _ = lager:warning("unexpected info received: ~tp", [Info]),
    {noreply, State}.

-spec terminate(Reason, st()) ->
    ok when
        Reason :: normal | shutdown | {shutdown, term()} | term().

terminate(_Reason, _State) ->
    ok.

-spec code_change(Vsn | {down, Vsn}, st(), term()) ->
    {error, noimpl} when
        Vsn :: term().

code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.
