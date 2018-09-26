-module(hg_dummy_provider).

-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).

-export([get_callback_url/0]).
-export([construct_silent_callback/1]).

-export([make_payment_tool/1]).

%% cowboy http callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%

-define(COWBOY_PORT, 9988).

-define(sleep(To, UI),
    {sleep, #'prxprv_SleepIntent'{timer = {timeout, To}, user_interaction = UI}}).
-define(suspend(Tag, To, UI),
    {suspend, #'prxprv_SuspendIntent'{tag = Tag, timeout = {timeout, To}, user_interaction = UI}}).
-define(finish(Status),
    {finish, #'prxprv_FinishIntent'{status = Status}}).
-define(success(),
    {success, #'prxprv_Success'{}}).
-define(failure(Failure),
    {failure, Failure}).
-define(recurrent_token_finish(Token),
    {finish, #'prxprv_RecurrentTokenFinishIntent'{status = {success, #'prxprv_RecurrentTokenSuccess'{token = Token}}}}).
-define(recurrent_token_finish_w_failure(Failure),
    {finish, #'prxprv_RecurrentTokenFinishIntent'{status = {failure, Failure}}}).

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

-define(REC_TOKEN, <<"rec_token">>).

-type form() :: #{binary() => binary() | true}.

-spec construct_silent_callback(form()) -> form().

construct_silent_callback(Form) ->
    Form#{<<"payload">> => ?LAY_LOW_BUDDY}.

-type failure_scenario_step() :: good | temp | fail | error.
-type failure_scenario() :: [failure_scenario_step()].
%%

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("hellgate/include/payment_events.hrl").

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'GenerateToken',
    [#prxprv_RecurrentTokenContext{
        session = #prxprv_RecurrentTokenSession{state = State},
        token_info = TokenInfo,
        options = _
    }],
    Opts
) ->
    generate_token(State, TokenInfo, Opts);

handle_function(
    'HandleRecurrentTokenCallback',
    [Payload, #prxprv_RecurrentTokenContext{
        session = #prxprv_RecurrentTokenSession{state = State},
        token_info = TokenInfo,
        options = _
    }],
    Opts
) ->
    handle_token_callback(Payload, State, TokenInfo, Opts);

handle_function(
    'ProcessPayment',
    [#prxprv_PaymentContext{
        session = #prxprv_Session{target = ?refunded(), state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    process_refund(State, PaymentInfo, Opts);

handle_function(
    'ProcessPayment',
    [#prxprv_PaymentContext{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts);

handle_function(
    'HandlePaymentCallback',
    [Payload, #prxprv_PaymentContext{
        session = #prxprv_Session{target = Target, state = State},
        payment_info = PaymentInfo,
        options = _
    }],
    Opts
) ->
    handle_payment_callback(Payload, Target, State, PaymentInfo, Opts).

%
% Recurrent tokens
%

generate_token(undefined, #prxprv_RecurrentTokenInfo{payment_tool = RecurrentPaytool}, _Opts) ->
    case get_recurrent_paytool_scenario(RecurrentPaytool) of
        forbidden ->
            #prxprv_RecurrentTokenProxyResult{
                intent = ?recurrent_token_finish_w_failure(#'domain_Failure'{code = <<"forbidden">>})
            };
        unexpected_failure ->
            error(unexpected_failure);
        _ ->
            token_sleep(1, <<"sleeping">>)
    end;
generate_token(<<"sleeping">>, #prxprv_RecurrentTokenInfo{payment_tool = RecurrentPaytool}, _Opts) ->
    case get_recurrent_paytool_scenario(RecurrentPaytool) of
        {preauth_3ds, Timeout} ->
            Tag = generate_recurent_tag(),
            Uri = get_callback_url(),
            UserInteraction = {
                'redirect',
                {
                    'post_request',
                    #'BrowserPostRequest'{uri = Uri, form = #{<<"tag">> => Tag}}
                }
            },
            token_suspend(Tag, Timeout, <<"suspended">>, UserInteraction);
        no_preauth ->
            token_sleep(1, <<"finishing">>)
    end;
generate_token(<<"finishing">>, TokenInfo, _Opts) ->
    Token = ?REC_TOKEN,
    token_finish(TokenInfo, Token).

handle_token_callback(_Tag, <<"suspended">>, TokenInfo, _Opts) ->
    Token = ?REC_TOKEN,
    token_respond(<<"sure">>, token_finish(TokenInfo, Token)).

token_finish(#prxprv_RecurrentTokenInfo{payment_tool = PaymentTool}, Token) ->
    #prxprv_RecurrentTokenProxyResult{
        intent = ?recurrent_token_finish(Token),
        trx    = #domain_TransactionInfo{id = PaymentTool#prxprv_RecurrentPaymentTool.id, extra = #{}}
    }.

token_sleep(Timeout, State) ->
    #prxprv_RecurrentTokenProxyResult{
        intent     = ?sleep(Timeout, undefined),
        next_state = State
    }.

token_suspend(Tag, Timeout, State, UserInteraction) ->
    #prxprv_RecurrentTokenProxyResult{
        intent     = ?suspend(Tag, Timeout, UserInteraction),
        next_state = State
    }.

token_respond(Response, CallbackResult) ->
    #prxprv_RecurrentTokenCallbackResult{
        response   = Response,
        result     = CallbackResult
    }.

%
% Payments
%

process_payment(?processed(), undefined, PaymentInfo, _) ->
    case get_payment_info_scenario(PaymentInfo) of
        {preauth_3ds, Timeout} ->
            Tag = generate_payment_tag(),
            Uri = get_callback_url(),
            UserInteraction = {
                'redirect',
                {
                    'post_request',
                    #'BrowserPostRequest'{uri = Uri, form = #{<<"tag">> => Tag}}
                }
            },
            suspend(Tag, Timeout, <<"suspended">>, UserInteraction);
        no_preauth ->
            %% simple workflow without 3DS
            sleep(1, <<"sleeping">>);
        preauth_3ds_offsite ->
            %% user interaction in sleep intent
            Uri = get_callback_url(),
            UserInteraction = {
                'redirect',
                {
                    'post_request',
                    #'BrowserPostRequest'{
                        uri = Uri,
                        form = #{
                            <<"invoice_id">> => get_invoice_id(PaymentInfo),
                            <<"payment_id">> => get_payment_id(PaymentInfo)
                        }
                    }
                }
            },
            sleep(1, <<"sleeping_with_user_interaction">>, UserInteraction);
        terminal ->
            %% workflow for euroset terminal, similar to 3DS workflow
            SPID = get_short_payment_id(PaymentInfo),
            UserInteraction = {payment_terminal_reciept, #'PaymentTerminalReceipt'{
               short_payment_id = SPID,
               due = get_invoice_due_date(PaymentInfo)
            }},
            suspend(SPID, 2, <<"suspended">>, UserInteraction);
        digital_wallet ->
            %% simple workflow
            sleep(1, <<"sleeping">>);
        recurrent ->
            %% simple workflow without 3DS
            sleep(1, <<"sleeping">>);
        unexpected_failure ->
            sleep(1, <<"sleeping">>, undefined, get_payment_id(PaymentInfo));
        {temporary_unavailability, _Scenario} ->
            sleep(0, <<"sleeping">>)
    end;
process_payment(?processed(), <<"sleeping">>, PaymentInfo, _) ->
    case get_payment_info_scenario(PaymentInfo) of
        unexpected_failure ->
            error(unexpected_failure);
        {temporary_unavailability, Scenario} ->
            process_failure_scenario(PaymentInfo, Scenario, get_payment_id(PaymentInfo));
        _ ->
            finish(?success(), get_payment_id(PaymentInfo))
    end;
process_payment(?processed(), <<"sleeping_with_user_interaction">>, PaymentInfo, _) ->
    Key = {get_invoice_id(PaymentInfo), get_payment_id(PaymentInfo)},
    case get_transaction_state(Key) of
        processed ->
            finish(?success(), get_payment_id(PaymentInfo));
        {pending, Count} when Count > 2 ->
            Failure = payproc_errors:construct('PaymentFailure',
                {authorization_failed, {unknown, #payprocerr_GeneralFailure{}}}),
            finish(?failure(Failure));
        {pending, Count} ->
            set_transaction_state(Key, {pending, Count + 1}),
            sleep(1, <<"sleeping_with_user_interaction">>);
        undefined ->
            set_transaction_state(Key, {pending, 1}),
            sleep(1, <<"sleeping_with_user_interaction">>)
    end;

process_payment(?captured(), undefined, PaymentInfo, _Opts) ->
    case get_payment_info_scenario(PaymentInfo) of
        {temporary_unavailability, Scenario} ->
            process_failure_scenario(PaymentInfo, Scenario, get_payment_id(PaymentInfo));
        _ ->
            finish(?success(), get_payment_id(PaymentInfo))
    end;

process_payment(?cancelled(), _, PaymentInfo, _) ->
    finish(?success(), get_payment_id(PaymentInfo)).

handle_payment_callback(?LAY_LOW_BUDDY, ?processed(), <<"suspended">>, _PaymentInfo, _Opts) ->
    respond(<<"sure">>, #prxprv_PaymentCallbackProxyResult{
        intent     = undefined,
        next_state = <<"suspended">>
    });
handle_payment_callback(Tag, ?processed(), <<"suspended">>, PaymentInfo, _Opts) ->
    {{ok, PaymentInfo}, _} = get_payment_info(Tag),
    respond(<<"sure">>, #prxprv_PaymentCallbackProxyResult{
        intent     = ?sleep(1, undefined),
        next_state = <<"sleeping">>
    }).

-spec do_failure_scenario_step(failure_scenario(), term()) -> failure_scenario_step().
do_failure_scenario_step(Scenario, Key) ->
    Step = case get_transaction_state(Key) of
        {scenario_step, S} ->
            S;
        undefined ->
            1
    end,
    set_transaction_state(Key, {scenario_step, Step + 1}),
    get_failure_scenario_step(Scenario, Step).

-spec get_failure_scenario_step(failure_scenario(), Index :: pos_integer()) -> failure_scenario_step().
get_failure_scenario_step(Scenario, Step) when Step > length(Scenario) ->
    good;
get_failure_scenario_step(Scenario, Step) ->
    lists:nth(Step, Scenario).

process_refund(undefined, PaymentInfo, _) ->
    case get_payment_info_scenario(PaymentInfo) of
        {temporary_unavailability, Scenario} ->
            PaymentId = hg_utils:construct_complex_id([get_payment_id(PaymentInfo), get_refund_id(PaymentInfo)]),
            process_failure_scenario(PaymentInfo, Scenario, PaymentId);
        _ ->
            finish(?success(), get_payment_id(PaymentInfo))
    end.

process_failure_scenario(PaymentInfo, Scenario, PaymentId) ->
    Key = {get_invoice_id(PaymentInfo), get_payment_id(PaymentInfo)},
    case do_failure_scenario_step(Scenario, Key) of
        good ->
            finish(?success(), PaymentId);
        temp ->
            Failure = payproc_errors:construct('PaymentFailure',
                {authorization_failed, {temporarily_unavailable, #payprocerr_GeneralFailure{}}}),
            finish(?failure(Failure));
        fail ->
            Failure = payproc_errors:construct('PaymentFailure',
                {authorization_failed, {unknown, #payprocerr_GeneralFailure{}}}),
            finish(?failure(Failure));
        error ->
            error(planned_scenario_error)
    end.

finish(Status, TrxID) ->
    #prxprv_PaymentProxyResult{
        intent = ?finish(Status),
        trx    = #domain_TransactionInfo{id = TrxID, extra = #{}}
    }.

finish(Status) ->
    #prxprv_PaymentProxyResult{
        intent = ?finish(Status)
    }.

sleep(Timeout, State) ->
    sleep(Timeout, State, undefined).

sleep(Timeout, State, UserInteraction) ->
    #prxprv_PaymentProxyResult{
        intent     = ?sleep(Timeout, UserInteraction),
        next_state = State
    }.

sleep(Timeout, State, UserInteraction, TrxID) ->
    #prxprv_PaymentProxyResult{
        intent     = ?sleep(Timeout, UserInteraction),
        trx        = #domain_TransactionInfo{id = TrxID, extra = #{}},
        next_state = State
    }.

suspend(Tag, Timeout, State, UserInteraction) ->
    #prxprv_PaymentProxyResult{
        intent     = ?suspend(Tag, Timeout, UserInteraction),
        next_state = State
    }.

respond(Response, CallbackResult) ->
    #prxprv_PaymentCallbackResult{
        response   = Response,
        result     = CallbackResult
    }.

get_payment_id(#prxprv_PaymentInfo{payment = Payment}) ->
    Payment#prxprv_InvoicePayment.id.

get_refund_id(#prxprv_PaymentInfo{refund = Refund}) ->
    Refund#prxprv_InvoicePaymentRefund.id.

get_invoice_id(#prxprv_PaymentInfo{invoice = Invoice}) ->
    Invoice#prxprv_Invoice.id.

get_payment_info_scenario(
    #prxprv_PaymentInfo{payment = #prxprv_InvoicePayment{payment_resource = Resource}}
) ->
    get_payment_resource_scenario(Resource).

get_payment_resource_scenario({disposable_payment_resource, PaymentResource}) ->
    PaymentTool = get_payment_tool(PaymentResource),
    get_payment_tool_scenario(PaymentTool);
get_payment_resource_scenario({recurrent_payment_resource, _}) ->
    recurrent.

get_recurrent_paytool_scenario(#prxprv_RecurrentPaymentTool{payment_resource = PaymentResource}) ->
    PaymentTool = get_payment_tool(PaymentResource),
    get_payment_tool_scenario(PaymentTool).

get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"no_preauth">>}}) ->
    no_preauth;
get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"preauth_3ds:timeout=", Timeout/binary>>}}) ->
    {preauth_3ds, erlang:binary_to_integer(Timeout)};
get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"preauth_3ds_offsite">>}}) ->
    preauth_3ds_offsite;
get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"forbidden">>}}) ->
    forbidden;
get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"unexpected_failure">>}}) ->
    unexpected_failure;
get_payment_tool_scenario({'bank_card', #domain_BankCard{token = <<"scenario_",
                                                                   BinScenario/binary>>}}) ->
    Scenario = decode_failure_scenario(BinScenario),
    {temporary_unavailability, Scenario};
get_payment_tool_scenario({'payment_terminal', #domain_PaymentTerminal{terminal_type = euroset}}) ->
    terminal;
get_payment_tool_scenario({'digital_wallet', #domain_DigitalWallet{provider = qiwi}}) ->
    digital_wallet.

-spec make_payment_tool(PaymenToolCode) -> PaymenTool when
    PaymenToolCode :: atom() | {temporary_unavailability, failure_scenario()},
    PaymenTool :: {hg_domain_thrift:'PaymentTool'(), hg_domain_thrift:'PaymentSessionID'()}.

make_payment_tool(no_preauth) ->
    make_simple_payment_tool(<<"no_preauth">>, visa);
make_payment_tool(preauth_3ds) ->
    make_payment_tool({preauth_3ds, 3});
make_payment_tool({preauth_3ds, Timeout}) ->
    TimeoutBin = erlang:integer_to_binary(Timeout),
    make_simple_payment_tool(<<"preauth_3ds:timeout=", TimeoutBin/binary>>, visa);
make_payment_tool(preauth_3ds_offsite) ->
    make_simple_payment_tool(<<"preauth_3ds_offsite">>, jcb);
make_payment_tool(forbidden) ->
    make_simple_payment_tool(<<"forbidden">>, visa);
make_payment_tool(unexpected_failure) ->
    make_simple_payment_tool(<<"unexpected_failure">>, visa);
make_payment_tool({scenario, Scenario}) ->
    BinScenario = encode_failure_scenario(Scenario),
    make_simple_payment_tool(<<"scenario_", BinScenario/binary>>, visa);
make_payment_tool(terminal) ->
    {
        {payment_terminal, #domain_PaymentTerminal{
            terminal_type = euroset
        }},
        <<>>
    };
make_payment_tool(digital_wallet) ->
    {
        {digital_wallet, #domain_DigitalWallet{
            provider = qiwi,
            id       = <<"+79876543210">>
        }},
        <<>>
    };
make_payment_tool(tokenized_bank_card) ->
    make_simple_payment_tool(<<"no_preauth">>, visa, applepay).

make_simple_payment_tool(Token, PaymentSystem) ->
    make_simple_payment_tool(Token, PaymentSystem, undefined).

make_simple_payment_tool(Token, PaymentSystem, TokenProvider) ->
    construct_payment_tool_and_session(Token, PaymentSystem, <<"424242">>, <<"4242">>, TokenProvider, <<"SESSION42">>).

construct_payment_tool_and_session(Token, PaymentSystem, Bin, Pan, TokenProvider, Session) ->
    {
        {bank_card, #domain_BankCard{
            token          = Token,
            payment_system = PaymentSystem,
            bin            = Bin,
            masked_pan     = Pan,
            token_provider = TokenProvider
        }},
        Session
    }.

get_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.

get_short_payment_id(#prxprv_PaymentInfo{invoice = Invoice, payment = Payment}) ->
    <<(Invoice#prxprv_Invoice.id)/binary, ".", (Payment#prxprv_InvoicePayment.id)/binary>>.

get_invoice_due_date(#prxprv_PaymentInfo{invoice = Invoice}) ->
    Invoice#prxprv_Invoice.due.

-spec encode_failure_scenario(failure_scenario()) -> binary().

encode_failure_scenario(Scenario) ->
    << <<(encode_failure_scenario_step(S)):8>> || S <- Scenario >>.

-spec decode_failure_scenario(binary()) -> failure_scenario().

decode_failure_scenario(BinScenario) ->
    [decode_failure_scenario_step(B) || <<B:8>> <= BinScenario].

-spec encode_failure_scenario_step(failure_scenario_step()) -> byte().

encode_failure_scenario_step(good) ->
    $g;
encode_failure_scenario_step(temp) ->
    $t;
encode_failure_scenario_step(fail) ->
    $f;
encode_failure_scenario_step(error) ->
    $e.

-spec decode_failure_scenario_step(byte()) -> failure_scenario_step().

decode_failure_scenario_step($g) ->
    good;
decode_failure_scenario_step($t) ->
    temp;
decode_failure_scenario_step($f) ->
    fail;
decode_failure_scenario_step($e) ->
    error.

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
    RespCode = case maps:get(<<"tag">>, Form, undefined) of
        %% sleep intent
        undefined ->
            InvoiceID = maps:get(<<"invoice_id">>, Form),
            PaymentID = maps:get(<<"payment_id">>, Form),
            set_transaction_state({InvoiceID, PaymentID}, processed),
            200;
        %% suspend intent
        Tag ->
            Payload = maps:get(<<"payload">>, Form, Tag),
            callback_to_hell(Tag, Payload)
    end,
    cowboy_req:reply(RespCode, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<>>, Req2);
handle_user_interaction_response(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

callback_to_hell(Tag, Payload) ->
    % This case emulate precisely current proxy behaviour. HOLY MACKEREL!
    Fun = case Tag of
        <<"payment-", _Rest/binary>> ->
            'ProcessPaymentCallback';
        <<"recurrent-", _Rest/binary>> ->
            'ProcessRecurrentTokenCallback';
        % FIXME adhoc for old tests, probably can be safely removed
        _ ->
            'ProcessPaymentCallback'
    end,
    case hg_client_api:call(
        proxy_host_provider, Fun, [Tag, Payload],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ) of
        {{ok, _Response}, _} ->
            200;
        {{error, _}, _} ->
            500;
        {{exception, #'InvalidRequest'{}}, _} ->
            400
    end.

generate_payment_tag() ->
    Tag = hg_utils:unique_id(),
    <<"payment-", Tag/binary>>.

generate_recurent_tag() ->
    Tag = hg_utils:unique_id(),
    <<"recurrent-", Tag/binary>>.

get_payment_info(Tag) ->
    hg_client_api:call(
        proxy_host_provider, 'GetPayment', [Tag],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ).

set_transaction_state(Key, Value) ->
    hg_kv_store:put(Key, Value).

get_transaction_state(Key) ->
    hg_kv_store:get(Key).
