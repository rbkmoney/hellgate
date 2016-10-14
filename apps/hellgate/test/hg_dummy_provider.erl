-module(hg_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).

%% cowboy http callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/provider/dummy", {dmsl_proxy_provider_thrift, 'ProviderProxy'}}.

%%

-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("hellgate/include/invoice_events.hrl").

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), #{}) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function(
    'ProcessPayment',
    {#'Context'{
        session = #'Session'{target = Target, state = State},
        payment = PaymentInfo,
        options = _
    }},
    Context,
    Opts
) ->
    process_payment(Target, State, PaymentInfo, Opts, Context);

handle_function(
    'HandlePaymentCallback',
    {Payload, #'Context'{
        session = #'Session'{target = Target, state = State},
        payment = PaymentInfo,
        options = _
    }},
    Context,
    Opts
) ->
    handle_callback(Payload, Target, State, PaymentInfo, Opts, Context).

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


process_payment(?processed(), undefined, _, _, Context) ->
    {{ok, sleep(1, <<"sleeping">>)}, Context};
process_payment(?processed(), <<"sleeping">>, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context};

process_payment(?captured(), undefined, PaymentInfo, _Opts, Context) ->
    Token3DS = hg_ct_helper:bank_card_tds_token(),
    case get_payment_token(PaymentInfo) of
        Token3DS ->
            Tag = hg_utils:unique_id(),
            %% We pass tag as uri, but in actual workflow proxy should pack it somehow for payform
            UserInteraction = {'redirect', {'get_request', #'BrowserGetRequest'{uri = Tag}}},
            {{ok, suspend(Tag, 3, <<"suspended">>, UserInteraction)}, Context};
        _ ->
            %% simple workflow without 3DS
            {{ok, sleep(1, <<"sleeping">>)}, Context}
    end;
process_payment(?captured(), <<"sleeping">>, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context}.

handle_callback(<<"payload">>, ?captured(), <<"suspended">>, _PaymentInfo, _Opts, Context) ->
    {{ok, respond(<<"sure">>, sleep(1, <<"sleeping">>))}, Context}.

finish(#'PaymentInfo'{payment = Payment}) ->
    #'ProxyResult'{
        intent = {finish, #'FinishIntent'{status = {ok, #'Ok'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#domain_InvoicePayment.id}
    }.

sleep(Timeout, State) ->
    #'ProxyResult'{
        intent     = {sleep, #'SleepIntent'{timer = {timeout, Timeout}}},
        next_state = State
    }.

suspend(Tag, Timeout, State, UserInteraction) ->
    #'ProxyResult'{
        intent     = {suspend, #'SuspendIntent'{
            tag     = Tag,
            timeout = {timeout, Timeout},
            user_interaction = UserInteraction
        }},
        next_state = State
    }.

respond(Response, Result) ->
    #'CallbackResult'{
        response = Response,
        result = Result
    }.

get_payment_token(#'PaymentInfo'{payment = Payment}) ->
    #domain_InvoicePayment{payer = #domain_Payer{payment_tool = PaymentTool}} = Payment,
    {'bank_card', #domain_BankCard{token = Token}} = PaymentTool,
    Token.

handle_user_interaction_response(<<"POST">>, Req) ->
    {ok, Tag, _Garbage} = cowboy_req:body(Req),
    RespCode = callback_to_hell(Tag),
    cowboy_req:reply(RespCode, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<"">>, Req);
handle_user_interaction_response(<<"GET">>, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

callback_to_hell(Tag) ->
    case hg_client_api:call(
        proxy_host_provider, 'ProcessCallback', [Tag, <<"payload">>],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ) of
        {ok, _} -> 200;
        {{ok, _}, _} -> 200;
        {{error, _}, _} -> 500;
        {{exception, _}, _} -> 500;
        %% WTF IS THIS?!
        _ -> 666
    end.