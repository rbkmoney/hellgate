%%% Host handler for provider proxies
%%%
%%% TODO
%%%  - designate an exception when specified tag is missing

-module(hg_proxy_host_provider).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-type tag()          :: dmsl_proxy_thrift:'CallbackTag'().
-type callback()     :: dmsl_proxy_thrift:'Callback'().
-type response()     :: dmsl_proxy_thrift:'CallbackResponse'().
-type payment_info() :: dmsl_proxy_provider_thrift:'PaymentInfo'().

-spec handle_function
    ('ProcessCallback', [tag() | callback()], hg_woody_wrapper:handler_opts()) ->
        response() | no_return();
    ('GetPayment', [tag()], hg_woody_wrapper:handler_opts()) ->
        payment_info() | no_return().

handle_function('ProcessCallback', [Tag, Callback], _) ->
    case hg_invoice:process_callback(Tag, {provider, Callback}) of
        {ok, Response} ->
            Response;
        {error, invalid_callback} ->
            hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Invalid callback">>]});
        {error, notfound} ->
            hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Not found">>]});
        {error, Reason} ->
            error(Reason)
    end;

handle_function('GetPayment', [Tag], _) ->
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
