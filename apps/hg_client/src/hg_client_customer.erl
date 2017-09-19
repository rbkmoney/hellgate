-module(hg_client_customer).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% API

-export([start     /2]).
-export([start_link/2]).
-export([stop      /1]).

-export([create       /2]).
-export([get          /2]).
-export([delete       /2]).
-export([start_binding/3]).

%% GenServer

-behaviour(gen_server).
-export([init       /1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate  /2]).
-export([code_change/3]).

%% Types

-type user_info()       :: dmsl_payment_processing_thrift:'UserInfo'().
-type id()              :: dmsl_payment_processing_thrift:'CustomerID'().

-type customer()        :: dmsl_payment_processing_thrift:'Customer'().
-type customer_params() :: dmsl_payment_processing_thrift:'CustomerParams'().

-type customer_binding()        :: dmsl_payment_processing_thrift:'CustomerBinding'().
-type customer_binding_params() :: dmsl_payment_processing_thrift:'CustomerBindingParams'().

%% API

-spec start(user_info(), hg_client_api:t()) ->
    pid().
start(UserInfo, ApiClient) ->
    start(start, UserInfo, ApiClient).

-spec start_link(user_info(), hg_client_api:t()) ->
    pid().
start_link(UserInfo, ApiClient) ->
    start(start_link, UserInfo, ApiClient).

start(Mode, UserInfo, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, {UserInfo, ApiClient}, []),
    Pid.

-spec stop(pid()) ->
    ok.
stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(customer_params(), pid()) ->
    customer() | woody_error:business_error().
create(Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Create', [Params]})).

-spec get(id(), pid()) ->
    customer() | woody_error:business_error().
get(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Get', [ID]})).

-spec delete(id(), pid()) ->
    ok | woody_error:business_error().
delete(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Delete', [ID]})).

-spec start_binding(pid(), customer_binding_params(), pid()) ->
    customer_binding().
start_binding(ID, CustomerBindingParams, Client) ->
    map_result_error(gen_server:call(Client, {call, 'StartBinding', [ID, CustomerBindingParams]})).

map_result_error({ok, Result}) ->
    Result;
map_result_error({exception, _} = Exception) ->
    Exception;
map_result_error({error, Error}) ->
    error(Error).

%%

-record(st, {
    user_info :: user_info(),
    pollers   :: #{id() => hg_client_event_poller:t()},
    client    :: hg_client_api:t()
}).
-type st() :: #st{}.

-type callref() :: {pid(), Tag :: reference()}.

-spec init({user_info(), hg_client_api:t()}) ->
    {ok, st()}.

init({UserInfo, ApiClient}) ->
    {ok, #st{user_info = UserInfo, pollers = #{}, client = ApiClient}}.

-spec handle_call(term(), callref(), st()) ->
    {reply, term(), st()} | {noreply, st()}.

handle_call({call, Function, Args}, _From, St = #st{user_info = _UserInfo, client = Client}) ->
    {Result, ClientNext} = hg_client_api:call(customer_management, Function, Args, Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_event, CustomerID, Timeout}, _From, St = #st{client = Client}) ->
    Poller = get_poller(CustomerID, St),
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(1, Timeout, Client, Poller),
    StNext = set_poller(CustomerID, PollerNext, St#st{client = ClientNext}),
    case Result of
        [] ->
            {reply, timeout, StNext};
        [#payproc_Event{payload = Payload}] ->
            {reply, {ok, Payload}, StNext};
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

get_poller(ID, #st{user_info = UserInfo, pollers = Pollers}) ->
    maps:get(ID, Pollers, hg_client_event_poller:new(customer_management, 'GetEvents', [UserInfo, ID])).

set_poller(ID, Poller, St = #st{pollers = Pollers}) ->
    St#st{pollers = maps:put(ID, Poller, Pollers)}.
