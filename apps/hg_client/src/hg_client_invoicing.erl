-module(hg_client_invoicing).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([start/1]).
-export([start/2]).
-export([start_link/1]).
-export([stop/1]).

-export([create/2]).
-export([create_with_tpl/2]).
-export([get/2]).
-export([get/3]).
-export([fulfill/3]).
-export([rescind/3]).
-export([repair/5]).
-export([repair_scenario/3]).

-export([start_payment/3]).
-export([get_payment/3]).
-export([cancel_payment/4]).
-export([capture_payment/4]).
-export([capture_payment/5]).
-export([capture_payment/6]).

-export([refund_payment/4]).
-export([refund_payment_manual/4]).
-export([get_payment_refund/4]).

-export([create_chargeback/4]).
-export([cancel_chargeback/5]).
-export([reject_chargeback/5]).
-export([accept_chargeback/5]).
-export([reopen_chargeback/5]).
-export([get_payment_chargeback/4]).

-export([create_adjustment/4]).
-export([get_adjustment/4]).
-export([capture_adjustment/4]).
-export([cancel_adjustment/4]).

-export([compute_terms/3]).

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

-type chargeback()               :: dmsl_domain_thrift:'InvoicePaymentChargeback'().
-type chargeback_id()            :: dmsl_domain_thrift:'InvoicePaymentChargebackID'().
-type chargeback_params()        :: dmsl_payment_processing_thrift:'InvoicePaymentChargebackParams'().
-type chargeback_cancel_params() :: dmsl_payment_processing_thrift:'InvoicePaymentChargebackCancelParams'().
-type chargeback_accept_params() :: dmsl_payment_processing_thrift:'InvoicePaymentChargebackAcceptParams'().
-type chargeback_reject_params() :: dmsl_payment_processing_thrift:'InvoicePaymentChargebackRejectParams'().
-type chargeback_reopen_params() :: dmsl_payment_processing_thrift:'InvoicePaymentChargebackReopenParams'().

-type term_set()           :: dmsl_domain_thrift:'TermSet'().
-type cash()               :: undefined | dmsl_domain_thrift:'Cash'().
-type cart()               :: undefined | dmsl_domain_thrift:'InvoiceCart'().
-type event_range()        :: dmsl_payment_processing_thrift:'EventRange'().
-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().

-spec start(hg_client_api:t()) -> pid().

start(ApiClient) ->
    start(start, undefined, ApiClient).

-spec start(user_info(), hg_client_api:t()) -> pid().

start(UserInfo, ApiClient) ->
    start(start, UserInfo, ApiClient).

-spec start_link(hg_client_api:t()) -> pid().

start_link(ApiClient) ->
    start(start_link, undefined, ApiClient).

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
    get(InvoiceID, Client, #payproc_EventRange{}).

-spec get(invoice_id(), pid(), event_range()) ->
    invoice_state() | woody_error:business_error().

get(InvoiceID, Client, EventRange) ->
    map_result_error(gen_server:call(Client, {call, 'Get', [InvoiceID, EventRange]})).


-spec fulfill(invoice_id(), binary(), pid()) ->
    ok | woody_error:business_error().

fulfill(InvoiceID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Fulfill', [InvoiceID, Reason]})).

-spec rescind(invoice_id(), binary(), pid()) ->
    ok | woody_error:business_error().

rescind(InvoiceID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Rescind', [InvoiceID, Reason]})).

-spec repair(invoice_id(), [tuple()], tuple() | undefined, tuple() | undefined, pid()) ->
    ok | woody_error:business_error().

repair(InvoiceID, Changes, Action, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Repair', [InvoiceID, Changes, Action, Params]})).

-spec repair_scenario(invoice_id(), hg_invoice_repair:scenario(), pid()) ->
    ok | woody_error:business_error().

repair_scenario(InvoiceID, Scenario, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RepairWithScenario', [InvoiceID, Scenario]})).

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
    capture_payment(InvoiceID, PaymentID, Reason, undefined, Client).

-spec capture_payment(invoice_id(), payment_id(), binary(), cash(), pid()) ->
    ok | woody_error:business_error().

capture_payment(InvoiceID, PaymentID, Reason, Cash, Client) ->
    capture_payment(InvoiceID, PaymentID, Reason, Cash, undefined, Client).

-spec capture_payment(invoice_id(), payment_id(), binary(), cash(), cart(), pid()) ->
    ok | woody_error:business_error().

capture_payment(InvoiceID, PaymentID, Reason, Cash, Cart, Client) ->
    Call = {
        call,
        'CapturePayment',
        [
            InvoiceID,
            PaymentID,
            #payproc_InvoicePaymentCaptureParams{
                reason = Reason,
                cash = Cash,
                cart = Cart
            }
        ]
    },
    map_result_error(gen_server:call(Client, Call)).

-spec create_chargeback(invoice_id(), payment_id(), chargeback_params(), pid()) ->
    chargeback() | woody_error:business_error().

create_chargeback(InvoiceID, PaymentID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CreateChargeback', [InvoiceID, PaymentID, Params]})).

-spec cancel_chargeback(invoice_id(), payment_id(), chargeback_id(), chargeback_cancel_params(), pid()) ->
    chargeback() | woody_error:business_error().

cancel_chargeback(InvoiceID, PaymentID, ChargebackID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CancelChargeback', [InvoiceID, PaymentID, ChargebackID, Params]})).

-spec reject_chargeback(invoice_id(), payment_id(), chargeback_id(), chargeback_reject_params(), pid()) ->
    chargeback() | woody_error:business_error().

reject_chargeback(InvoiceID, PaymentID, ChargebackID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RejectChargeback', [InvoiceID, PaymentID, ChargebackID, Params]})).

-spec accept_chargeback(invoice_id(), payment_id(), chargeback_id(), chargeback_accept_params(), pid()) ->
    chargeback() | woody_error:business_error().

accept_chargeback(InvoiceID, PaymentID, ChargebackID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'AcceptChargeback', [InvoiceID, PaymentID, ChargebackID, Params]})).

-spec reopen_chargeback(invoice_id(), payment_id(), chargeback_id(), chargeback_reopen_params(), pid()) ->
    chargeback() | woody_error:business_error().

reopen_chargeback(InvoiceID, PaymentID, ChargebackID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ReopenChargeback', [InvoiceID, PaymentID, ChargebackID, Params]})).

-spec get_payment_chargeback(invoice_id(), payment_id(), chargeback_id(), pid()) ->
    refund() | woody_error:business_error().

get_payment_chargeback(InvoiceID, PaymentID, ChargebackID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetPaymentChargeback', [InvoiceID, PaymentID, ChargebackID]})).

-spec refund_payment(invoice_id(), payment_id(), refund_params(), pid()) ->
    refund() | woody_error:business_error().

refund_payment(InvoiceID, PaymentID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RefundPayment', [InvoiceID, PaymentID, Params]})).

-spec refund_payment_manual(invoice_id(), payment_id(), refund_params(), pid()) ->
    refund() | woody_error:business_error().

refund_payment_manual(InvoiceID, PaymentID, Params, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CreateManualRefund', [InvoiceID, PaymentID, Params]})).

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

-spec compute_terms(invoice_id(), party_revision_param(), pid()) -> term_set().

compute_terms(InvoiceID, PartyRevision, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ComputeTerms', [InvoiceID, PartyRevision]})).


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

-type event() :: dmsl_payment_processing_thrift:'Event'().

-record(st, {
    user_info :: user_info(),
    pollers   :: #{invoice_id() => hg_client_event_poller:st(event())},
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
    _ = logger:warning("unexpected call received: ~tp", [Call]),
    {noreply, State}.

-spec handle_cast(_, st()) ->
    {noreply, st()}.

handle_cast(Cast, State) ->
    _ = logger:warning("unexpected cast received: ~tp", [Cast]),
    {noreply, State}.

-spec handle_info(_, st()) ->
    {noreply, st()}.

handle_info(Info, State) ->
    _ = logger:warning("unexpected info received: ~tp", [Info]),
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
    maps:get(InvoiceID, Pollers, construct_poller(UserInfo, InvoiceID)).

set_poller(InvoiceID, Poller, St = #st{pollers = Pollers}) ->
    St#st{pollers = maps:put(InvoiceID, Poller, Pollers)}.

construct_poller(UserInfo, InvoiceID) ->
    hg_client_event_poller:new(
        {invoicing, 'GetEvents', [UserInfo, InvoiceID]},
        fun (Event) -> Event#payproc_Event.id end
    ).
