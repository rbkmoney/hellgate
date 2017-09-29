-module(hg_client_recurrent_paytool).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% API

-export([start/1]).
-export([start_link/1]).
-export([stop/1]).

-export([create/2]).
-export([get/2]).
-export([abandon/2]).

-export([pull_event/2]).
-export([pull_event/3]).

%% GenServer

-behaviour(gen_server).
-export([init       /1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate  /2]).
-export([code_change/3]).

%% Types

-type recurrent_paytool_id()     :: dmsl_payment_processing_thrift:'RecurrentPaymentToolID'().
-type recurrent_paytool()        :: dmsl_payment_processing_thrift:'RecurrentPaymentTool'().
-type recurrent_paytool_params() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolParams'().

%% API

-spec start(hg_client_api:t()) ->
    pid().
start(ApiClient) ->
    start(start, ApiClient).

-spec start_link(hg_client_api:t()) ->
    pid().
start_link(ApiClient) ->
    start(start_link, ApiClient).

start(Mode, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, ApiClient, []),
    Pid.

-spec stop(pid()) ->
    ok.
stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(recurrent_paytool_params(), pid()) ->
    recurrent_paytool() | woody_error:business_error().
create(Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Create', [Params]})).

-spec get(recurrent_paytool_id(), pid()) ->
    recurrent_paytool() | woody_error:business_error().
get(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Get', [ID]})).

-spec abandon(recurrent_paytool_id(), pid()) ->
    ok | woody_error:business_error().
abandon(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Abandon', [ID]})).

-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-spec pull_event(recurrent_paytool_id(), pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(ID, Client) ->
    pull_event(ID, ?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_event(recurrent_paytool_id(), timeout(), pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(ID, Timeout, Client) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {pull_event, ID, Timeout}, infinity).

map_result_error({ok, Result}) ->
    Result;
map_result_error({exception, _} = Exception) ->
    Exception;
map_result_error({error, Error}) ->
    error(Error).

%%

-type event() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolEvent'().

-record(st, {
    pollers   :: #{recurrent_paytool_id() => hg_client_event_poller:st(event())},
    client    :: hg_client_api:t()
}).
-type st() :: #st{}.

-type callref() :: {pid(), Tag :: reference()}.

-spec init(hg_client_api:t()) ->
    {ok, st()}.

init(ApiClient) ->
    {ok, #st{pollers = #{}, client = ApiClient}}.

-spec handle_call(term(), callref(), st()) ->
    {reply, term(), st()} | {noreply, st()}.

handle_call({call, Function, Args}, _From, St = #st{client = Client}) ->
    {Result, ClientNext} = hg_client_api:call(recurrent_paytools, Function, Args, Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_event, RecurrentPaytoolID, Timeout}, _From, St = #st{client = Client}) ->
    Poller = get_poller(RecurrentPaytoolID, St),
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(1, Timeout, Client, Poller),
    StNext = set_poller(RecurrentPaytoolID, PollerNext, St#st{client = ClientNext}),
    case Result of
        [] ->
            {reply, timeout, StNext};
        [Event] ->
            {reply, {ok, get_event_payload(Event)}, StNext};
        Error ->
            {reply, Error, StNext}
    end;

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

get_poller(ID, #st{pollers = Pollers}) ->
    maps:get(ID, Pollers, construct_poller(ID)).

set_poller(ID, Poller, St = #st{pollers = Pollers}) ->
    St#st{pollers = maps:put(ID, Poller, Pollers)}.

construct_poller(ID) ->
    hg_client_event_poller:new({recurrent_paytools, 'GetEvents', [ID]}, fun get_event_id/1).

get_event_id(#payproc_RecurrentPaymentToolEvent{id = ID}) ->
    ID.

get_event_payload(#payproc_RecurrentPaymentToolEvent{payload = Payload}) ->
    Payload.
