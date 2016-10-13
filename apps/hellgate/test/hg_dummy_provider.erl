-module(hg_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).

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

process_payment(?processed(), undefined, _, _, Context) ->
    {{ok, sleep(1, <<"sleeping">>)}, Context};
process_payment(?processed(), <<"sleeping">>, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context};

process_payment(?captured(), undefined, _, Opts, Context) ->
    Tag = hg_utils:unique_id(),
    _Pid = spawn(fun () -> callback(Tag, <<"payload">>, <<"sure">>, Opts, 1000) end),
    {{ok, suspend(Tag, 3, <<"suspended">>)}, Context};
process_payment(?captured(), <<"sleeping">>, PaymentInfo, _, Context) ->
    {{ok, finish(PaymentInfo)}, Context}.

handle_callback(<<"payload">>, ?captured(), <<"suspended">>, _, _, Context) ->
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

suspend(Tag, Timeout, State) ->
    #'ProxyResult'{
        intent     = {suspend, #'SuspendIntent'{
            tag     = Tag,
            timeout = {timeout, Timeout}
        }},
        next_state = State
    }.

respond(Response, Result) ->
    #'CallbackResult'{
        response = Response,
        result = Result
    }.

callback(Tag, Payload, Expect, #{hellgate_root_url := RootUrl}, Timeout) ->
    _ = timer:sleep(Timeout),
    {ok, Expect} = hg_client_api:call(
        proxy_host_provider, 'ProcessCallback', [Tag, Payload],
        hg_client_api:new(RootUrl)
    ).
