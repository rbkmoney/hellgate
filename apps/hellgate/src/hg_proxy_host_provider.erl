%%% Host handler for provider proxies
%%%
%%% TODO
%%%  - designate an exception when specified tag is missing

-module(hg_proxy_host_provider).

-include_lib("damsel/include/dmsl_proxy_provider_thrift.hrl").

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-type tag() :: dmsl_base_thrift:'Tag'().
-type callback() :: dmsl_proxy_provider_thrift:'Callback'().
-type callback_response() :: dmsl_proxy_provider_thrift:'CallbackResponse'().
-type callback_name() ::
    'ProcessPaymentCallback'
    | 'ProcessRecurrentTokenCallback'
    | 'GetPayment'.

-spec handle_function(callback_name(), {tag()} | {tag(), callback()}, hg_woody_wrapper:handler_opts()) ->
    term() | no_return().
handle_function('ProcessPaymentCallback', {Tag, Callback}, _) ->
    handle_callback_result(hg_invoice:process_callback(Tag, {provider, Callback}));
handle_function('ProcessRecurrentTokenCallback', {Tag, Callback}, _) ->
    handle_callback_result(hg_recurrent_paytool:process_callback(Tag, {provider, Callback}));
handle_function('GetPayment', {Tag}, _) ->
    case hg_invoice:get({tag, Tag}) of
        {ok, InvoiceSt} ->
            case hg_invoice:get_payment({tag, Tag}, InvoiceSt) of
                {ok, PaymentSt} ->
                    Opts = hg_invoice:get_payment_opts(InvoiceSt),
                    hg_invoice_payment:construct_payment_info(PaymentSt, Opts);
                {error, notfound} ->
                    hg_woody_wrapper:raise(#prxprv_PaymentNotFound{})
            end;
        {error, notfound} ->
            hg_woody_wrapper:raise(#prxprv_PaymentNotFound{})
    end.

-spec handle_callback_result
    ({ok, callback_response()}) -> callback_response();
    ({error, any()}) -> no_return().
handle_callback_result({ok, Response}) ->
    Response;
handle_callback_result({error, invalid_callback}) ->
    hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Invalid callback">>]});
handle_callback_result({error, notfound}) ->
    hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Not found">>]});
handle_callback_result({error, Reason}) ->
    error(Reason).
