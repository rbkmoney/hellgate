%%% Invoice payment submachine
%%%
%%% TODO
%%%  - make proper submachine interface
%%%  - distinguish between different error classes:
%%%     - regular operation error
%%%     - callback timeout
%%%     - internal error ?
%%%  - handle idempotent callbacks uniformly
%%%     - get rid of matches against session status
%%%  - tag machine with the provider trx
%%%     - distinguish between trx tags and callback tags
%%%     - tag namespaces
%%%  - clean the mess with error handling
%%%     - abuse transient error passthrough
%%%     - remove ability to throw `TryLater` from `HandlePaymentCallback`
%%%     - drop `TryLater` completely (?)
%%%  - think about safe clamping of timers returned by some proxy

-module(hg_invoice_payment).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").
-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

%% API

%% Machine like

-export([init/4]).
-export([start_session/2]).

-export([process_signal/4]).
-export([process_call/4]).

-export([merge_event/2]).

%%

-type st() :: {payment(), session()}.
-export_type([st/0]).

-type invoice()     :: hg_domain_thrift:'Invoice'().
-type payment()     :: hg_domain_thrift:'InvoicePayment'().
-type payment_id()  :: hg_domain_thrift:'InvoicePaymentID'().
-type target()      :: hg_proxy_provider_thrift:'Target'().
-type proxy_state() :: hg_proxy_thrift:'ProxyState'().

-type session() :: #{
    target      => target(),
    status      => active | suspended,
    proxy_state => proxy_state() | undefined,
    retry       => genlib_retry:strategy()
}.

%%

-include("invoice_events.hrl").

-type ev() ::
    {invoice_payment_event, hg_payment_processing_thrift:'InvoicePaymentEvent'()} |
    {session_event, session_ev()}.

-type session_ev() ::
    {started, session()} |
    {proxy_state_changed, proxy_state()} |
    {proxy_retry_changed, genlib_retry:strategy()} |
    suspended |
    activated.

-define(session_ev(E), {session_event, E}).

%%

-type opts() :: #{
    invoice => invoice(),
    proxy => _
    %% TODO
}.

-spec init(payment_id(), _, opts(), hg_machine:context()) ->
    {hg_machine:result(), hg_machine:context()}.

init(PaymentID, PaymentParams, #{invoice := Invoice}, Context) ->
    Payment = #domain_InvoicePayment{
        id           = PaymentID,
        created_at   = hg_datetime:format_now(),
        status       = ?pending(),
        cost         = Invoice#domain_Invoice.cost,
        payer        = PaymentParams#payproc_InvoicePaymentParams.payer
    },
    Events = [?payment_ev(?payment_started(Payment))],
    Action = hg_machine_action:new(),
    {{Events, Action}, Context}.

-spec start_session(target(), hg_machine:context()) ->
    {hg_machine:result(), hg_machine:context()}.

start_session(Target, Context) ->
    Events = [?session_ev({started, Target})],
    Action = hg_machine_action:instant(),
    {{Events, Action}, Context}.

%%

-spec process_signal(timeout, st(), opts(), hg_machine:context()) ->
    {{next | done, hg_machine:result()}, hg_machine:context()}.

process_signal(timeout, St, Options, Context) ->
    case get_status(St) of
        active ->
            process(St, Options, Context);
        suspended ->
            {fail(construct_error(<<"provider_timeout">>), St), Context}
    end.

-spec process_call({callback, _}, st(), opts(), hg_machine:context()) ->
    {{_, {next | done, hg_machine:result()}}, hg_machine:context()}. % FIXME

process_call({callback, Payload}, St, Options, Context) ->
    case get_status(St) of
        suspended ->
            handle_callback(Payload, St, Options, Context);
        active ->
            % there's ultimately no way how we could end up here
            error(invalid_session_status)
    end.

process(St, Options, Context) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_process_result(issue_process_call(ProxyContext, Options, Context), St).

handle_process_result({Result, Context}, St) ->
    case Result of
        {ok, ProxyResult} ->
            {handle_proxy_result(ProxyResult, St), Context};
        {exception, Exception} ->
            {handle_exception(Exception, St), Context};
        {error, Error} ->
            {handle_error(Error, St), Context}
    end.

handle_callback(Payload, St, Options, Context) ->
    ProxyContext = construct_proxy_context(St, Options),
    handle_callback_result(issue_callback_call(Payload, ProxyContext, Options, Context), St).

handle_callback_result({Result, Context}, St) ->
    case Result of
        {ok, #'CallbackResult'{result = ProxyResult, response = Response}} ->
            {What, {Events, Action}} = handle_proxy_result(ProxyResult, St),
            {{Response, {What, {[?session_ev(activated) | Events], Action}}}, Context};
        {error, _} = Error ->
            error({Error, Context})
    end.

handle_proxy_result(#'ProxyResult'{intent = {_, Intent}, trx = Trx, next_state = ProxyState}, St) ->
    case bind_transaction(Trx, St) of
        {ok, Events1} ->
            {What, {Events2, Action}} = handle_proxy_intent(Intent, ProxyState, St),
            {What, {Events1 ++ Events2, Action}};
        {error, Error} ->
            fail(Error, St)
    end.

bind_transaction(undefined, _St) ->
    % no transaction yet
    {ok, []};
bind_transaction(Trx, {#domain_InvoicePayment{id = PaymentID, trx = undefined}, _}) ->
    % got transaction, nothing bound so far
    {ok, [?payment_ev(?payment_bound(PaymentID, Trx))]};
bind_transaction(Trx, {#domain_InvoicePayment{trx = Trx}, _}) ->
    % got the same transaction as one which has been bound previously
    {ok, []};
bind_transaction(Trx, {#domain_InvoicePayment{id = PaymentID, trx = TrxWas}, _}) ->
    % got transaction which differs from the bound one
    % verify against proxy contracts
    case Trx#domain_TransactionInfo.id of
        ID when ID =:= TrxWas#domain_TransactionInfo.id ->
            {ok, [?payment_ev(?payment_bound(PaymentID, Trx))]};
        _ ->
            {error, construct_error(<<"proxy_contract_violated">>)}
    end.

handle_proxy_intent(#'FinishIntent'{status = {ok, _}}, _ProxyState, St) ->
    PaymentID = get_payment_id(St),
    Target = get_target(St),
    Events = [?payment_ev(?payment_status_changed(PaymentID, Target))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}};

handle_proxy_intent(#'FinishIntent'{status = {failure, Error}}, _ProxyState, St) ->
    fail(construct_error(Error), St);

handle_proxy_intent(#'SleepIntent'{timer = Timer}, ProxyState, _St) ->
    Action = hg_machine_action:set_timer(Timer),
    Events = [?session_ev({proxy_state_changed, ProxyState})],
    {next, {Events, Action}};

handle_proxy_intent(
    #'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction},
    ProxyState, St
) ->
    Action = try_set_timer(Timer, hg_machine_action:set_tag(Tag)),
    Events = [
        ?session_ev({proxy_state_changed, ProxyState}),
        ?session_ev(suspended)
        | try_emit_interaction_event(UserInteraction, St)
    ],
    {next, {Events, Action}}.

try_set_timer(undefined, Action) ->
    Action;
try_set_timer(Timer, Action) ->
    hg_machine_action:set_timer(Timer, Action).

try_emit_interaction_event(undefined, _St) ->
    [];
try_emit_interaction_event(UserInteraction, St) ->
    [?payment_ev(?payment_interaction_requested(get_payment_id(St), UserInteraction))].

handle_exception(#'TryLater'{e = Error}, St) ->
    case retry(St) of
        {wait, Timeout, Events} ->
            Action = hg_machine_action:set_timeout(Timeout),
            {next, {Events, Action}};
        finish ->
            fail(construct_error(Error), St)
    end.

handle_error(_Error, St) ->
    Error = construct_error(<<"internal_error">>),
    fail(Error, St).

retry({_Payment, #{retry := Retry}}) ->
    case genlib_retry:next_step(Retry) of
        {wait, Timeout, RetryNext} ->
            {wait, Timeout div 1000, [?session_ev({proxy_retry_changed, RetryNext})]};
        finish ->
            finish
    end.

fail(Error, St) ->
    Events = [?payment_ev(?payment_status_changed(get_payment_id(St), ?failed(Error)))],
    Action = hg_machine_action:new(),
    {done, {Events, Action}}.

construct_retry_strategy(_Target) ->
    Timecap = 30000,
    Timeout = 10000,
    genlib_retry:timecap(Timecap, genlib_retry:linear(infinity, Timeout)).

construct_proxy_context({Payment, Session}, Options = #{proxy := Proxy}) ->
    #'Context'{
        session = construct_session(Session),
        payment = construct_payment_info(Payment, Options),
        options = Proxy#domain_Proxy.options
    }.

construct_session(#{target := Target, proxy_state := ProxyState}) ->
    #'Session'{
        target = Target,
        state = ProxyState
    }.

construct_payment_info(Payment, #{invoice := Invoice}) ->
    #'PaymentInfo'{
        invoice = Invoice,
        payment = Payment
    }.

construct_error(#'Error'{code = Code, description = Description}) ->
    construct_error(Code, Description);
construct_error(Code) when is_binary(Code) ->
    construct_error(Code, undefined).

construct_error(Code, Description) ->
    #domain_OperationError{code = Code, description = Description}.

%%

get_payment_id({Payment, _State}) ->
    Payment#domain_InvoicePayment.id.

get_status({_Payment, #{status := Status}}) ->
    Status.

get_target({_Payment, #{target := Target}}) ->
    Target.

%%

-spec merge_event(ev(), st()) -> st().

merge_event(?payment_ev(Event), St) ->
    merge_public_event(Event, St);
merge_event(?session_ev(Event), St) ->
    merge_session_event(Event, St).

merge_public_event(?payment_started(Payment), undefined) ->
    {Payment, undefined};
merge_public_event(?payment_bound(_, Trx), {Payment, State}) ->
    {Payment#domain_InvoicePayment{trx = Trx}, State};
merge_public_event(?payment_status_changed(_, Status), {Payment, State}) ->
    {Payment#domain_InvoicePayment{status = Status}, State}.

%% TODO session_finished?
merge_session_event({started, Target}, {Payment, _}) ->
    {Payment, create_session(Target)};
merge_session_event({proxy_state_changed, ProxyState}, {Payment, Session}) ->
    {Payment, Session#{proxy_state => ProxyState}};
merge_session_event({proxy_retry_changed, Retry}, {Payment, Session}) ->
    {Payment, Session#{retry => Retry}};
merge_session_event(activated, {Payment, Session}) ->
    {Payment, Session#{status => active}};
merge_session_event(suspended, {Payment, Session}) ->
    {Payment, Session#{status => suspended}}.

create_session(Target) ->
    #{
        target => Target,
        status => active,
        proxy_state => undefined,
        retry => construct_retry_strategy(Target)
    }.

%%

-define(SERVICE, {hg_proxy_provider_thrift, 'ProviderProxy'}).

issue_process_call(ProxyContext, Opts, Context) ->
    issue_call({?SERVICE, 'ProcessPayment', [ProxyContext]}, Opts, Context).

issue_callback_call(Payload, ProxyContext, Opts, Context) ->
    issue_call({?SERVICE, 'HandlePaymentCallback', [Payload, ProxyContext]}, Opts, Context).

issue_call(Call, #{proxy := Proxy}, Context = #{client_context := ClientContext}) ->
    {Result, ClientContext1} = woody_client:call_safe(ClientContext, Call, get_call_options(Proxy)),
    {Result, Context#{client_context := ClientContext1}}.

get_call_options(#domain_Proxy{url = Url}) ->
    #{url => Url}.
