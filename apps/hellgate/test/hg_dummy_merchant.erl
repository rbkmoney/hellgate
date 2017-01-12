-module(hg_dummy_merchant).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/merchant/dummy", {dmsl_proxy_merchant_thrift, 'MerchantProxy'}}.

%%

-include_lib("dmsl/include/dmsl_proxy_merchant_thrift.hrl").
-include_lib("hellgate/include/invoice_events.hrl").

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'HandleInvoiceEvent',
    [#prxmerch_Context{
        session = #prxmerch_Session{event = Event, state = State},
        invoice = InvoiceInfo,
        options = _
    }],
    Opts
) ->
    handle_invoice_event(Event, State, InvoiceInfo, Opts).

handle_invoice_event(_Event, undefined, _InvoiceInfo, _Opts) ->
    sleep(1, <<"sleeping">>);
handle_invoice_event(_Event, <<"sleeping">>, _InvoiceInfo, _Opts) ->
    finish().

finish() ->
    #prxmerch_ProxyResult{
        intent     = {finish, #prxmerch_FinishIntent{}},
        next_state = <<>>
    }.

sleep(Timeout, State) ->
    #prxmerch_ProxyResult{
        intent     = {sleep, #prxmerch_SleepIntent{timer = {timeout, Timeout}}},
        next_state = State
    }.
