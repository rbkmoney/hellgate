-module(hg_dummy_provider).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%%

-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {ok, term()} | no_return().

handle_function('ProcessPayment', {#'PaymentInfo'{state = undefined}}, _Context, _Opts) ->
    {ok, sleep(3, <<"sleeping">>)};
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
