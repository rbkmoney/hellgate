-module(hg_client_invoicing).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([start/2]).
-export([start_link/2]).
-export([stop/1]).

-export([create/2]).
-export([get/2]).
-export([fulfill/3]).
-export([rescind/3]).
-export([start_payment/3]).
-export([pull_event/2]).
-export([pull_event/3]).

%% GenServer

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%

-type user_info()      :: dmsl_payment_processing_thrift:'UserInfo'().
-type invoice_id()     :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_state()  :: dmsl_payment_processing_thrift:'InvoiceState'().
-type payment_id()     :: dmsl_domain_thrift:'InvoicePaymentID'().
-type invoice_params() :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type payment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().

-spec start(user_info(), hg_client_api:t()) -> pid().

start(UserInfo, ApiClient) ->
    start(start, UserInfo, ApiClient).

-spec start_link(user_info(), hg_client_api:t()) -> pid().

start_link(UserInfo, ApiClient) ->
    start(start_link, UserInfo, ApiClient).

start(Mode, UserInfo, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, {UserInfo, ApiClient}, []),
    Pid.

-spec stop(pid()) -> ok.

stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(invoice_params(), pid()) ->
    invoice_state() | woody_error:business_error().

create(InvoiceParams, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Create', [InvoiceParams]})).

-spec get(invoice_id(), pid()) ->
    invoice_state() | woody_error:business_error().

get(InvoiceID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Get', [InvoiceID]})).

-spec fulfill(invoice_id(), binary(), pid()) ->
    ok | woody_error:business_error().

fulfill(InvoiceID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Fulfill', [InvoiceID, Reason]})).

-spec rescind(invoice_id(), binary(), pid()) ->
    ok | woody_error:business_error().

rescind(InvoiceID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Rescind', [InvoiceID, Reason]})).

-spec start_payment(invoice_id(), payment_params(), pid()) ->
    payment_id() | woody_error:business_error().

start_payment(InvoiceID, PaymentParams, Client) ->
    map_result_error(gen_server:call(Client, {call, 'StartPayment', [InvoiceID, PaymentParams]})).

-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-spec pull_event(invoice_id(), pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(InvoiceID, Client) ->
    pull_event(InvoiceID, ?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_event(invoice_id(), timeout(), pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(InvoiceID, Timeout, Client) ->
    % FIXME: infinity sounds dangerous
    gen_server:call(Client, {pull_event, InvoiceID, Timeout}, infinity).

map_result_error({ok, Result}) ->
    Result;
map_result_error({exception, _} = Exception) ->
    Exception;
map_result_error({error, Error}) ->
    error(Error).

%%

-record(st, {
    user_info :: user_info(),
    pollers   :: #{invoice_id() => hg_client_event_poller:t()},
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

handle_call({call, Function, Args}, _From, St = #st{user_info = UserInfo, client = Client}) ->
    {Result, ClientNext} = hg_client_api:call(invoicing, Function, [UserInfo | Args], Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_event, InvoiceID, Timeout}, _From, St = #st{client = Client}) ->
    Poller = get_poller(InvoiceID, St),
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(1, Timeout, Client, Poller),
    StNext = set_poller(InvoiceID, PollerNext, St#st{client = ClientNext}),
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

get_poller(InvoiceID, #st{user_info = UserInfo, pollers = Pollers}) ->
    maps:get(InvoiceID, Pollers, hg_client_event_poller:new(invoicing, 'GetEvents', [UserInfo, InvoiceID])).

set_poller(InvoiceID, Poller, St = #st{pollers = Pollers}) ->
    St#st{pollers = maps:put(InvoiceID, Poller, Pollers)}.
