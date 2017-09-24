-module(hg_client_invoicing).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([start/2]).
-export([start_link/2]).
-export([stop/1]).

-export([create/2]).
-export([create_with_tpl/2]).
-export([get/2]).
-export([fulfill/3]).
-export([rescind/3]).

-export([start_payment/3]).
-export([get_payment/3]).
-export([cancel_payment/4]).
-export([capture_payment/4]).

-export([refund_payment/4]).
-export([get_payment_refund/4]).

-export([create_adjustment/4]).
-export([get_adjustment/4]).
-export([capture_adjustment/4]).
-export([cancel_adjustment/4]).

-export([compute_terms/2]).

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

-type user_info()          :: dmsl_payment_processing_thrift:'UserInfo'().
-type invoice_id()         :: dmsl_domain_thrift:'InvoiceID'().
-type invoice_state()      :: dmsl_payment_processing_thrift:'Invoice'().
-type payment()            :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()         :: dmsl_domain_thrift:'InvoicePaymentID'().
-type invoice_params()     :: dmsl_payment_processing_thrift:'InvoiceParams'().
-type invoice_params_tpl() :: dmsl_payment_processing_thrift:'InvoiceWithTemplateParams'().
-type payment_params()     :: dmsl_payment_processing_thrift:'InvoicePaymentParams'().
-type adjustment()         :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()      :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params()  :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type refund()             :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type refund_id()          :: dmsl_domain_thrift:'InvoicePaymentRefundID'().
-type refund_params()      :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type term_set()           :: dmsl_domain_thrift:'TermSet'().

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

-spec create_with_tpl(invoice_params_tpl(), pid()) ->
    invoice_state() | woody_error:business_error().

create_with_tpl(Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CreateWithTemplate', [Params]})).

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
    payment() | woody_error:business_error().

start_payment(InvoiceID, PaymentParams, Client) ->
    map_result_error(gen_server:call(Client, {call, 'StartPayment', [InvoiceID, PaymentParams]})).

-spec get_payment(invoice_id(), payment_id(), pid()) ->
    payment() | woody_error:business_error().

get_payment(InvoiceID, PaymentID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetPayment', [InvoiceID, PaymentID]})).

-spec cancel_payment(invoice_id(), payment_id(), binary(), pid()) ->
    ok | woody_error:business_error().

cancel_payment(InvoiceID, PaymentID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CancelPayment', [InvoiceID, PaymentID, Reason]})).

-spec capture_payment(invoice_id(), payment_id(), binary(), pid()) ->
    ok | woody_error:business_error().

capture_payment(InvoiceID, PaymentID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CapturePayment', [InvoiceID, PaymentID, Reason]})).

-spec refund_payment(invoice_id(), payment_id(), refund_params(), pid()) ->
    refund() | woody_error:business_error().

refund_payment(InvoiceID, PaymentID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RefundPayment', [InvoiceID, PaymentID, Params]})).

-spec get_payment_refund(invoice_id(), payment_id(), refund_id(), pid()) ->
    refund() | woody_error:business_error().

get_payment_refund(InvoiceID, PaymentID, RefundID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetPaymentRefund', [InvoiceID, PaymentID, RefundID]})).

-spec create_adjustment(invoice_id(), payment_id(), adjustment_params(), pid()) ->
    adjustment() | woody_error:business_error().

create_adjustment(InvoiceID, PaymentID, Params, Client) ->
    Args = [InvoiceID, PaymentID, Params],
    map_result_error(gen_server:call(Client, {call, 'CreatePaymentAdjustment', Args})).

-spec get_adjustment(invoice_id(), payment_id(), adjustment_params(), pid()) ->
    adjustment() | woody_error:business_error().

get_adjustment(InvoiceID, PaymentID, Params, Client) ->
    Args = [InvoiceID, PaymentID, Params],
    map_result_error(gen_server:call(Client, {call, 'GetPaymentAdjustment', Args})).

-spec capture_adjustment(invoice_id(), payment_id(), adjustment_id(), pid()) ->
    adjustment() | woody_error:business_error().

capture_adjustment(InvoiceID, PaymentID, ID, Client) ->
    Args = [InvoiceID, PaymentID, ID],
    map_result_error(gen_server:call(Client, {call, 'CapturePaymentAdjustment', Args})).

-spec cancel_adjustment(invoice_id(), payment_id(), adjustment_id(), pid()) ->
    adjustment() | woody_error:business_error().

cancel_adjustment(InvoiceID, PaymentID, ID, Client) ->
    Args = [InvoiceID, PaymentID, ID],
    map_result_error(gen_server:call(Client, {call, 'CancelPaymentAdjustment', Args})).

-spec compute_terms(invoice_id(), pid()) -> term_set().

compute_terms(InvoiceID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ComputeTerms', [InvoiceID]})).


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
