-module(hg_invoice_payment).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").
-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

%% API

-export([process/3]).

%% Machine callbacks

% -behaviour(hg_machine).

% -export([init/2]).
% -export([process_signal/2]).
% -export([process_call/2]).

%%

-record(st, {
    stage       :: process_payment | capture_payment,
    proxy_ref   :: hg_domain_thrift:'ProxyRef'(),
    proxy_state :: binary() | undefined,
    context     :: woody_client:context()
}).

-opaque st() :: #st{}.
-export_type([st/0]).

-type invoice()     :: hg_domain_thrift:'Invoice'().
-type payment()     :: hg_domain_thrift:'InvoicePayment'().
-type payment_trx() :: hg_domain_thrift:'TransactionInfo'() | undefined.
-type error()       :: hg_domain_thrift:'OperationError'().

-spec process(payment(), invoice(), st() | undefined) ->
    {ok, payment_trx()} |
    {{error, error()}, payment_trx()} |
    {{next, hg_machine_action:t(), st()}, payment_trx()}.

process(Payment, Invoice, undefined) ->
    process(Payment, Invoice, construct_state(Payment, Invoice));
process(Payment, Invoice, St = #st{}) ->
    Proxy = get_proxy(Invoice, St),
    PaymentInfo = construct_payment_info(Payment, Invoice, Proxy, St),
    process_(Proxy, PaymentInfo, St).

process_(Proxy, PaymentInfo, St = #st{stage = process_payment, context = Context}) ->
    % FIXME: dirty simulation of one-phase payment through the two-phase interaction
    case handle_process_result(process_payment(Proxy, PaymentInfo, Context), St) of
        {ok, Trx} ->
            NextSt = St#st{stage = capture_payment, proxy_state = undefined},
            {{next, hg_machine_action:instant(), NextSt}, Trx};
        Result ->
            Result
    end;
process_(Proxy, PaymentInfo, St = #st{stage = capture_payment, context = Context}) ->
    handle_process_result(capture_payment(Proxy, PaymentInfo, Context), St).

handle_process_result(#'ProcessResult'{intent = {_, Intent}, trx = Trx, next_state = ProxyStateNext}, St) ->
    handle_process_result(Intent, Trx, St#st{proxy_state = ProxyStateNext}).

handle_process_result(#'FinishIntent'{status = {ok, _}}, Trx, _) ->
    {ok, Trx};
handle_process_result(#'FinishIntent'{status = {failure, Error}}, Trx, _) ->
    {{error, map_error(Error)}, Trx};
handle_process_result(#'SleepIntent'{timer = Timer}, Trx, StNext) ->
    {{next, hg_machine_action:set_timer(Timer), StNext}, Trx}.

get_proxy(#'Invoice'{domain_revision = Revision}, #st{proxy_ref = Ref}) ->
    hg_domain:get(Revision, Ref).

construct_payment_info(Payment, Invoice, Proxy, #st{proxy_state = ProxyState}) ->
    #'PaymentInfo'{
        invoice = Invoice,
        payment = Payment,
        options = Proxy#'Proxy'.options,
        state   = ProxyState
    }.

map_error(#'Error'{code = Code, description = Description}) ->
    #'OperationError'{code = Code, description = Description}.

%%

construct_state(Payment, Invoice) ->
    #st{
        stage     = process_payment,
        proxy_ref = select_proxy(Payment, Invoice),
        context   = construct_context()
    }.

construct_context() ->
    ReqID = genlib_format:format_int_base(genlib_time:ticks(), 62),
    woody_client:new_context(ReqID, hg_woody_event_handler).

select_proxy(_, _) ->
    % FIXME: turbo routing
    #'ProxyRef'{id = 1}.

%% Proxy provider client

-define(SERVICE, {hg_proxy_provider_thrift, 'ProviderProxy'}).

-type process_payment_result() :: hg_proxy_provider_thrift:'ProcessResult'().

-spec process_payment(
    hg_domain_thrift:'Proxy'(),
    hg_proxy_provider_thrift:'PaymentInfo'(),
    woody_client:context()
) ->
    process_payment_result().
process_payment(Proxy, PaymentInfo, Context) ->
    call(Context, Proxy, {?SERVICE, 'ProcessPayment', [PaymentInfo]}).

-spec capture_payment(
    hg_domain_thrift:'Proxy'(),
    hg_proxy_provider_thrift:'PaymentInfo'(),
    woody_client:context()
) ->
    process_payment_result().
capture_payment(Proxy, PaymentInfo, Context) ->
    call(Context, Proxy, {?SERVICE, 'CapturePayment', [PaymentInfo]}).

call(Context, Proxy, Call) ->
    Endpoint = get_call_options(Proxy),
    try woody_client:call(Context, Call, Endpoint) of
        {{ok, Result = #'ProcessResult'{}}, _} ->
            Result
    catch
        % TODO: support retry strategies
        {#'TryLater'{e = _Error}, _} ->
            #'ProcessResult'{intent = {sleep, #'SleepIntent'{timer = {timeout, 10}}}}
    end.

get_call_options(#'Proxy'{url = Url}) ->
    #{url => Url}.
