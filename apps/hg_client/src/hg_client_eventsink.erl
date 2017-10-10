-module(hg_client_eventsink).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([start/1]).
-export([start_link/1]).
-export([stop/1]).

-export([get_last_event_id/1]).
-export([pull_events/2]).
-export([pull_events/3]).
-export([pull_history/1]).

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

-define(DEFAULT_POLL_TIMEOUT, 1000).

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

-spec pull_events(pos_integer(), pid()) ->
    [tuple()] | woody_error:business_error().

pull_events(N, Client) when N > 0 ->
    pull_events(N, ?DEFAULT_POLL_TIMEOUT, Client).

-spec pull_events(pos_integer(), timeout(), pid()) ->
    [tuple()] | woody_error:business_error().

pull_events(N, Timeout, Client) when N > 0 ->
    gen_server:call(Client, {pull_events, N, Timeout}, infinity).

-spec pull_history(pid()) ->
    [tuple()] | woody_error:business_error().

pull_history(Client) ->
    gen_server:call(Client, {pull_history, 1000}, infinity).

%%

-type event() :: dmsl_payment_processing_thrift:'Event'().

-record(st, {
    poller    :: hg_client_event_poller:st(event()),
    client    :: hg_client_api:t()
}).

-type st() :: #st{}.
-type callref() :: {pid(), Tag :: reference()}.

-define(SERVICE, payment_processing_eventsink).

-spec init(hg_client_api:t()) ->
    {ok, st()}.

init(ApiClient) ->
    {ok, #st{
        client = ApiClient,
        poller = hg_client_event_poller:new(
            {?SERVICE, 'GetEvents', []},
            fun (Event) -> Event#payproc_Event.id end
        )
    }}.

-spec handle_call(term(), callref(), st()) ->
    {reply, term(), st()} | {noreply, st()}.

handle_call({call, Function, Args}, _From, St = #st{client = Client}) ->
    {Result, ClientNext} = hg_client_api:call(?SERVICE, Function, Args, Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_events, N, Timeout}, _From, St) ->
    {Result, StNext} = poll_events(N, Timeout, St),
    {reply, Result, StNext};

handle_call({pull_history, BatchSize}, _From, St) ->
    {Result, StNext} = poll_history(BatchSize, St),
    {reply, Result, StNext};

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

%%

poll_events(N, Timeout, St = #st{client = Client, poller = Poller}) ->
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(N, Timeout, Client, Poller),
    {Result, St#st{client = ClientNext, poller = PollerNext}}.

poll_history(BatchSize, St) ->
    poll_history(BatchSize, [], St).

poll_history(BatchSize, Acc, St) ->
    case poll_events(BatchSize, 0, St) of
        {Events, StNext} when length(Events) == BatchSize ->
            poll_history(BatchSize, Acc ++ Events, StNext);
        {Events, StNext} ->
            {Acc ++ Events, StNext}
    end.
