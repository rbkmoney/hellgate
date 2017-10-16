-module(hg_dummy_provider).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).

-export([get_callback_url/0]).
-export([construct_silent_callback/1]).

%% cowboy http callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%

-define(COWBOY_PORT, 9988).

-define(sleep(To),
    {sleep, #'SleepIntent'{timer = {timeout, To}}}).
-define(suspend(Tag, To, UI),
    {suspend, #'SuspendIntent'{tag = Tag, timeout = {timeout, To}, user_interaction = UI}}).
-define(finish(),
    {finish, #'FinishIntent'{status = {success, #'Success'{}}}}).

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/provider/dummy", {dmsl_proxy_provider_thrift, 'ProviderProxy'}}.

-spec get_http_cowboy_spec() -> #{}.

get_http_cowboy_spec() ->
    Dispatch = cowboy_router:compile([{'_', [{"/", ?MODULE, []}]}]),
    #{
        listener_ref => ?MODULE,
        acceptors_count => 10,
        transport_opts => [{port, ?COWBOY_PORT}],
        proto_opts => [{env, [{dispatch, Dispatch}]}]
    }.

%%

-define(LAY_LOW_BUDDY   , <<"lay low buddy">>).

-type form() :: #{binary() => binary() | true}.

-spec construct_silent_callback(form()) -> form().

construct_silent_callback(Form) ->
    Form#{<<"payload">> => ?LAY_LOW_BUDDY}.

%%

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("hellgate/include/payment_events.hrl").

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'ProcessPayment',
    [#prxprv_Context{
        session = #prxprv_Session{target = ?refunded(), state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    process_refund(State, PaymentInfo, Opts);

handle_function(
    'ProcessPayment',
    [#prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts);

handle_function(
    'HandlePaymentCallback',
    [Payload, #prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    handle_payment_callback(Payload, Target, State, PaymentInfo, Opts).

process_payment(?processed(), undefined, _, _) ->
    sleep(1, <<"sleeping">>);
process_payment(?processed(), <<"sleeping">>, PaymentInfo, _) ->
    finish(get_payment_id(PaymentInfo));

process_payment(?captured(), undefined, PaymentInfo, _Opts) ->
    case get_payment_tool_type(PaymentInfo) of
        {bank_card, with_tds} ->
            Tag = hg_utils:unique_id(),
            Uri = get_callback_url(),
            UserInteraction = {
                'redirect',
                {
                    'post_request',
                    #'BrowserPostRequest'{uri = Uri, form = #{<<"tag">> => Tag}}
                }
            },
            suspend(Tag, 2, <<"suspended">>, UserInteraction);
        {bank_card, without_tds} ->
            %% simple workflow without 3DS
            sleep(1, <<"sleeping">>);
        {payment_terminal, euroset} ->
            %% workflow for euroset terminal, similar to 3DS workflow
            SPID = get_short_payment_id(PaymentInfo),
            UserInteraction = {payment_terminal_reciept, #'PaymentTerminalReceipt'{
               short_payment_id = SPID,
               due = get_invoice_due_date(PaymentInfo)
            }},
            suspend(SPID, 2, <<"suspended">>, UserInteraction)
    end;
process_payment(?captured(), <<"sleeping">>, PaymentInfo, _) ->
    finish(get_payment_id(PaymentInfo));

process_payment(?cancelled(), _, PaymentInfo, _) ->
    finish(get_payment_id(PaymentInfo)).

handle_payment_callback(?LAY_LOW_BUDDY, ?captured(), <<"suspended">>, _PaymentInfo, _Opts) ->
    respond(<<"sure">>, #prxprv_CallbackProxyResult{
        intent     = undefined,
        next_state = <<"suspended">>
    });
handle_payment_callback(Tag, ?captured(), <<"suspended">>, PaymentInfo, _Opts) ->
    {{ok, PaymentInfo}, _} = get_payment_info(Tag),
    respond(<<"sure">>, #prxprv_CallbackProxyResult{
        intent     = ?sleep(1),
        next_state = <<"sleeping">>
    }).

process_refund(undefined, PaymentInfo, _) ->
    finish(hg_utils:construct_complex_id([get_payment_id(PaymentInfo), get_refund_id(PaymentInfo)])).

finish(TrxID) ->
    #prxprv_ProxyResult{
        intent = ?finish(),
        trx    = #domain_TransactionInfo{id = TrxID, extra = #{}}
    }.

sleep(Timeout, State) ->
    #prxprv_ProxyResult{
        intent     = ?sleep(Timeout),
        next_state = State
    }.

suspend(Tag, Timeout, State, UserInteraction) ->
    #prxprv_ProxyResult{
        intent     = ?suspend(Tag, Timeout, UserInteraction),
        next_state = State
    }.

respond(Response, CallbackResult) ->
    #prxprv_CallbackResult{
        response   = Response,
        result     = CallbackResult
    }.

get_payment_id(#prxprv_PaymentInfo{payment = Payment}) ->
    #prxprv_InvoicePayment{id = PaymentID} = Payment,
    PaymentID.

get_refund_id(#prxprv_PaymentInfo{refund = Refund}) ->
    #prxprv_InvoicePaymentRefund{id = RefundID} = Refund,
    RefundID.

get_payment_tool_type(#prxprv_PaymentInfo{payment = Payment}) ->
    Token3DS = hg_ct_helper:bank_card_tds_token(),
    #prxprv_InvoicePayment{payer = #domain_Payer{payment_tool = PaymentTool}} = Payment,
    case PaymentTool of
        {'bank_card', #domain_BankCard{token = Token3DS}} ->
            {bank_card, with_tds};
        {'bank_card', _} ->
            {bank_card, without_tds};
        {'payment_terminal', #domain_PaymentTerminal{terminal_type = euroset}} ->
            {payment_terminal, euroset}
    end.

get_short_payment_id(#prxprv_PaymentInfo{invoice = Invoice, payment = Payment}) ->
    <<(Invoice#prxprv_Invoice.id)/binary, ".", (Payment#prxprv_InvoicePayment.id)/binary>>.

get_invoice_due_date(#prxprv_PaymentInfo{invoice = Invoice}) ->
    Invoice#prxprv_Invoice.due.

%%

-spec init(atom(), cowboy_req:req(), list()) -> {ok, cowboy_req:req(), state}.

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-spec handle(cowboy_req:req(), state) -> {ok, cowboy_req:req(), state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = handle_user_interaction_response(Method, Req2),
    {ok, Req3, State}.

-spec terminate(term(), cowboy_req:req(), state) -> ok.

terminate(_Reason, _Req, _State) ->
    ok.

-spec get_callback_url() -> binary().

get_callback_url() ->
    genlib:to_binary("http://127.0.0.1:" ++ integer_to_list(?COWBOY_PORT)).

handle_user_interaction_response(<<"POST">>, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Form = maps:from_list(cow_qs:parse_qs(Body)),
    Tag = maps:get(<<"tag">>, Form),
    Payload = maps:get(<<"payload">>, Form, Tag),
    RespCode = callback_to_hell(Tag, Payload),
    cowboy_req:reply(RespCode, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<>>, Req2);
handle_user_interaction_response(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

callback_to_hell(Tag, Payload) ->
    case hg_client_api:call(
        proxy_host_provider, 'ProcessCallback', [Tag, Payload],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ) of
        {{ok, _Response}, _} ->
            200;
        {{error, _}, _} ->
            500;
        {{exception, _}, _} ->
            500
    end.

get_payment_info(Tag) ->
    hg_client_api:call(
        proxy_host_provider, 'GetPayment', [Tag],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ).
