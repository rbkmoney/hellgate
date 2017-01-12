-module(hg_dummy_provider).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).

%% cowboy http callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%

-define(COWBOY_PORT, 9988).

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

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("hellgate/include/invoice_events.hrl").

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'ProcessPayment',
    [#prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment = PaymentInfo,
        options = _
    }],
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts);

handle_function(
    'HandlePaymentCallback',
    [Payload, #prxprv_Context{
        session = #prxprv_Session{target = Target, state = State},
        payment = PaymentInfo,
        options = _
    }],
    Opts
) ->
    handle_callback(Payload, Target, State, PaymentInfo, Opts).

process_payment(?processed(), undefined, _, _) ->
    sleep(1, <<"sleeping">>);
process_payment(?processed(), <<"sleeping">>, PaymentInfo, _) ->
    finish(PaymentInfo);

process_payment(?captured(), undefined, PaymentInfo, _Opts) ->
    Token3DS = hg_ct_helper:bank_card_tds_token(),
    case get_payment_token(PaymentInfo) of
        Token3DS ->
            Tag = hg_utils:unique_id(),
            Uri = genlib:to_binary("http://127.0.0.1:" ++ integer_to_list(?COWBOY_PORT)),
            UserInteraction = {
                'redirect',
                {
                    'post_request',
                    #'BrowserPostRequest'{uri = Uri, form = #{<<"tag">> => Tag}}
                }
            },
            suspend(Tag, 2, <<"suspended">>, UserInteraction);
        _ ->
            %% simple workflow without 3DS
            sleep(1, <<"sleeping">>)
    end;
process_payment(?captured(), <<"sleeping">>, PaymentInfo, _) ->
    finish(PaymentInfo).

handle_callback(<<"payload">>, ?captured(), <<"suspended">>, _PaymentInfo, _Opts) ->
    respond(<<"sure">>, sleep(1, <<"sleeping">>)).

finish(#prxprv_PaymentInfo{payment = Payment}) ->
    #prxprv_ProxyResult{
        intent = {finish, #'FinishIntent'{status = {success, #'Success'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#prxprv_InvoicePayment.id}
    }.

sleep(Timeout, State) ->
    #prxprv_ProxyResult{
        intent     = {sleep, #'SleepIntent'{timer = {timeout, Timeout}}},
        next_state = State
    }.

suspend(Tag, Timeout, State, UserInteraction) ->
    #prxprv_ProxyResult{
        intent     = {suspend, #'SuspendIntent'{
            tag     = Tag,
            timeout = {timeout, Timeout},
            user_interaction = UserInteraction
        }},
        next_state = State
    }.

respond(Response, Result) ->
    #prxprv_CallbackResult{
        response = Response,
        result = Result
    }.

get_payment_token(#prxprv_PaymentInfo{payment = Payment}) ->
    #prxprv_InvoicePayment{payer = #domain_Payer{payment_tool = PaymentTool}} = Payment,
    {'bank_card', #domain_BankCard{token = Token}} = PaymentTool,
    Token.

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

handle_user_interaction_response(<<"POST">>, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Form = cow_qs:parse_qs(Body),
    {_, Tag} = lists:keyfind(<<"tag">>, 1, Form),
    RespCode = callback_to_hell(Tag),
    cowboy_req:reply(RespCode, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<>>, Req2);
handle_user_interaction_response(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

callback_to_hell(Tag) ->
    case hg_client_api:call(
        proxy_host_provider, 'ProcessCallback', [Tag, <<"payload">>],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ) of
        {{ok, _Response}, _} ->
            200;
        {{error, _}, _} ->
            500;
        {{exception, _}, _} ->
            500
    end.
