-module(hg_api_invoicing_handler).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {ok, term()} | no_return().

handle_function('Create', {UserInfo, InvoiceParams}, Context, _Opts) ->
    hg_invoice:create(UserInfo, InvoiceParams, opts(Context));

handle_function('Get', {UserInfo, InvoiceID}, Context, _Opts) ->
    hg_invoice:get(UserInfo, InvoiceID, opts(Context));

handle_function('GetEvents', {UserInfo, InvoiceID, Range}, Context, _Opts) ->
    hg_invoice:get_events(UserInfo, InvoiceID, Range, opts(Context));

handle_function('StartPayment', {UserInfo, InvoiceID, PaymentParams}, Context, _Opts) ->
    hg_invoice:start_payment(UserInfo, InvoiceID, PaymentParams, opts(Context));

handle_function('GetPayment', {UserInfo, PaymentID}, Context, _Opts) ->
    hg_invoice:get_payment(UserInfo, PaymentID, opts(Context));

handle_function('Fulfill', {UserInfo, InvoiceID, Reason}, Context, _Opts) ->
    hg_invoice:fulfill(UserInfo, InvoiceID, Reason, opts(Context));

handle_function('Void', {UserInfo, InvoiceID, Reason}, Context, _Opts) ->
    hg_invoice:void(UserInfo, InvoiceID, Reason, opts(Context)).

opts(Context) ->
    #{context => Context}.

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.
