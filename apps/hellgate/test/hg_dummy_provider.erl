-module(hg_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

-export([get_child_spec/2]).
-export([get_url/2]).

%%

-type host() :: string() | inet:ip_address().
-type port_num() :: 1024..65535.

-spec get_child_spec(host(), port_num()) -> supervisor:child_spec().

get_child_spec(Host, Port) ->
    {Name, Path, Service} = get_service_spec(),
    woody_server:child_spec(
        Name,
        #{
            ip => hg_utils:get_hostname_ip(Host),
            port => Port,
            net_opts => [],
            event_handler => hg_woody_event_handler,
            handlers => [{Path, {Service, ?MODULE, []}}]
        }
    ).

-spec get_url(host(), port_num()) -> woody_t:url().

get_url(Host, Port) ->
    {_Name, Path, _Service} = get_service_spec(),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), Path]).

get_service_spec() ->
    {?MODULE, "/test/proxy/provider/dummy",
        {hg_proxy_provider_thrift, 'ProviderProxy'}}.

%%

-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {ok, term()} | no_return().

handle_function('ProcessPayment', {#'PaymentInfo'{state = undefined}}, _Context, _Opts) ->
    {ok, sleep(1, <<"sleeping">>)};
handle_function('ProcessPayment', {#'PaymentInfo'{state = <<"sleeping">>} = PaymentInfo}, _Context, _Opts) ->
    {ok, finish(PaymentInfo)};

handle_function('CapturePayment', {PaymentInfo}, _Context, _Opts) ->
    {ok, finish(PaymentInfo)};

handle_function('CancelPayment', {PaymentInfo}, _Context, _Opts) ->
    {ok, finish(PaymentInfo)}.

finish(#'PaymentInfo'{payment = Payment}) ->
    #'ProcessResult'{
        intent = {finish, #'FinishIntent'{status = {ok, #'Ok'{}}}},
        trx    = #'TransactionInfo'{id = Payment#'InvoicePayment'.id}
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
