-module(hg_invoice_payment).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").
-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

%% API

-export([process/4]).

%% Machine callbacks

% -behaviour(hg_machine).

% -export([init/2]).
% -export([process_signal/2]).
% -export([process_call/2]).

%%

-record(st, {
    stage       :: process_payment | capture_payment,
    proxy_ref   :: hg_domain_thrift:'ProxyRef'(),
    proxy_state :: binary() | undefined
}).

-type st() :: #st{}.

-type invoice()     :: hg_domain_thrift:'Invoice'().
-type payment()     :: hg_domain_thrift:'InvoicePayment'().
-type payment_trx() :: hg_domain_thrift:'TransactionInfo'() | undefined.
-type error()       :: hg_domain_thrift:'OperationError'().

-spec process(payment(), invoice(), binary() | undefined, hg_machine:context()) ->
    {{ok, payment_trx()}, hg_machine:context()} |
    {{{error, error()}, payment_trx()}, hg_machine:context()} |
    {{{next, hg_machine_action:t(), binary()}, payment_trx()}, hg_machine:context()}.

process(Payment, Invoice, undefined, Context) ->
    process_(Payment, Invoice, construct_state(Payment, Invoice), Context);
process(Payment, Invoice, St, Context) when is_binary(St) ->
    process_(Payment, Invoice, unmarshal_st(St), Context).

process_(Payment, Invoice, St = #st{}, Context) ->
    Proxy = get_proxy(Invoice, St),
    PaymentInfo = construct_payment_info(Payment, Invoice, Proxy, St),
    handle_state(Proxy, PaymentInfo, St, Context).

handle_state(Proxy, PaymentInfo, St = #st{stage = process_payment}, Context0) ->
    % FIXME: dirty simulation of one-phase payment through the two-phase interaction
    case handle_process_result(process_payment(Proxy, PaymentInfo, Context0), St) of
        {{ok, Trx}, Context} ->
            NextSt = St#st{stage = capture_payment, proxy_state = undefined},
            {{{next, hg_machine_action:instant(), marshal_st(NextSt)}, Trx}, Context};
        Result ->
            Result
    end;
handle_state(Proxy, PaymentInfo, St = #st{stage = capture_payment}, Context) ->
    handle_process_result(capture_payment(Proxy, PaymentInfo, Context), St).

handle_process_result({#'ProcessResult'{intent = {_, Intent}, trx = Trx, next_state = ProxyStateNext}, Context}, St) ->
    handle_process_result(Intent, Trx, St#st{proxy_state = ProxyStateNext}, Context).

handle_process_result(#'FinishIntent'{status = {ok, _}}, Trx, _St, Context) ->
    {{ok, Trx}, Context};
handle_process_result(#'FinishIntent'{status = {failure, Error}}, Trx, _St, Context) ->
    {{{error, map_error(Error)}, Trx}, Context};
handle_process_result(#'SleepIntent'{timer = Timer}, Trx, StNext, Context) ->
    {{{next, hg_machine_action:set_timer(Timer), marshal_st(StNext)}, Trx}, Context}.

get_proxy(#domain_Invoice{domain_revision = Revision}, #st{proxy_ref = Ref}) ->
    hg_domain:get(Revision, Ref).

construct_payment_info(Payment, Invoice, Proxy, #st{proxy_state = ProxyState}) ->
    #'PaymentInfo'{
        invoice = Invoice,
        payment = Payment,
        options = Proxy#domain_Proxy.options,
        state   = ProxyState
    }.

map_error(#'Error'{code = Code, description = Description}) ->
    #domain_OperationError{code = Code, description = Description}.

%%

construct_state(Payment, Invoice) ->
    #st{
        stage     = process_payment,
        proxy_ref = select_proxy(Payment, Invoice)
    }.

select_proxy(_, _) ->
    % FIXME: turbo routing
    #domain_ProxyRef{id = 1}.

-spec marshal_st(st()) -> binary().

marshal_st(St) ->
    term_to_binary(St).

-spec unmarshal_st(binary()) -> st().

unmarshal_st(St) ->
    binary_to_term(St).

%% Proxy provider client

-define(SERVICE, {hg_proxy_provider_thrift, 'ProviderProxy'}).

-type process_payment_result() :: hg_proxy_provider_thrift:'ProcessResult'().

-spec process_payment(
    hg_domain_thrift:'Proxy'(),
    hg_proxy_provider_thrift:'PaymentInfo'(),
    woody_client:context()
) ->
    {process_payment_result(), woody_client:context()}.
process_payment(Proxy, PaymentInfo, Context) ->
    call(Context, Proxy, {?SERVICE, 'ProcessPayment', [PaymentInfo]}).

-spec capture_payment(
    hg_domain_thrift:'Proxy'(),
    hg_proxy_provider_thrift:'PaymentInfo'(),
    woody_client:context()
) ->
    {process_payment_result(), woody_client:context()}.
capture_payment(Proxy, PaymentInfo, Context) ->
    call(Context, Proxy, {?SERVICE, 'CapturePayment', [PaymentInfo]}).

call(Context = #{client_context := ClientContext0}, Proxy, Call) ->
    Endpoint = get_call_options(Proxy),
    try woody_client:call(ClientContext0, Call, Endpoint) of
        {{ok, Result = #'ProcessResult'{}}, ClientContext} ->
            {Result, Context#{client_context => ClientContext}}
    catch
        % TODO: support retry strategies
        {#'TryLater'{e = _Error}, ClientContext} ->
            Result = #'ProcessResult'{intent = {sleep, #'SleepIntent'{timer = {timeout, 10}}}},
            {Result, Context#{client_context => ClientContext}}
    end.

get_call_options(#domain_Proxy{url = Url}) ->
    #{url => Url}.
