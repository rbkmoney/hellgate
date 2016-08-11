-module(hg_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

-behaviour(hg_test_proxy).

-export([get_child_spec/2]).
-export([get_url/2]).

%%

-spec get_child_spec(inet:hostname() | inet:ip_address(), inet:port_number()) ->
    supervisor:child_spec().

get_child_spec(Host, Port) ->
    {Path, Service} = get_service_spec(),
    woody_server:child_spec(
        ?MODULE,
        #{
            ip => hg_utils:get_hostname_ip(Host),
            port => Port,
            net_opts => [],
            event_handler => hg_woody_event_handler,
            handlers => [{Path, {Service, ?MODULE, []}}]
        }
    ).

-spec get_url(inet:hostname() | inet:ip_address(), inet:port_number()) ->
    woody_t:url().

get_url(Host, Port) ->
    {Path, _Service} = get_service_spec(),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), Path]).

get_service_spec() ->
    {"/test/proxy/provider/dummy", {hg_proxy_provider_thrift, 'ProviderProxy'}}.

%%

-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('ProcessPayment', {#'PaymentInfo'{state = undefined}}, Context, _Opts) ->
    {{ok, sleep(1, <<"sleeping">>)}, Context};
handle_function('ProcessPayment', {#'PaymentInfo'{state = <<"sleeping">>} = PaymentInfo}, Context, _Opts) ->
    {{ok, finish(PaymentInfo)}, Context};

handle_function('CapturePayment', {PaymentInfo}, Context, _Opts) ->
    {{ok, finish(PaymentInfo)}, Context};

handle_function('CancelPayment', {PaymentInfo}, Context, _Opts) ->
    {{ok, finish(PaymentInfo)}, Context}.

finish(#'PaymentInfo'{payment = Payment}) ->
    #'ProcessResult'{
        intent = {finish, #'FinishIntent'{status = {ok, #'Ok'{}}}},
        trx    = #domain_TransactionInfo{id = Payment#domain_InvoicePayment.id}
    }.

sleep(Timeout, State) ->
    #'ProcessResult'{
        intent     = {sleep, #'SleepIntent'{timer = {timeout, Timeout}}},
        next_state = State
    }.

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.
