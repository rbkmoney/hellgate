-module(hg_proto).

-export([get_service/1]).

-export([get_service_spec/1]).
-export([get_service_spec/2]).

-export_type([service/0]).
-export_type([service_spec/0]).

%%

-define(VERSION_PREFIX, "/v1").

-type service()      :: woody:service().
-type service_spec() :: {Path :: string(), service()}.

-spec get_service(Name :: atom()) -> service().

get_service(party_management) ->
    {dmsl_payment_processing_thrift, 'PartyManagement'};
get_service(invoicing) ->
    {dmsl_payment_processing_thrift, 'Invoicing'};
get_service(invoice_templating) ->
    {dmsl_payment_processing_thrift, 'InvoiceTemplating'};
get_service(customer_management) ->
    {dmsl_payment_processing_thrift, 'CustomerManagement'};
get_service(payment_processing_eventsink) ->
    {dmsl_payment_processing_thrift, 'EventSink'};
get_service(recurrent_paytools) ->
    {dmsl_payment_processing_thrift, 'RecurrentPaymentTools'};
get_service(recurrent_paytools_eventsink) ->
    {dmsl_payment_processing_thrift, 'RecurrentPaymentToolEventSink'};
get_service(proxy_provider) ->
    {dmsl_proxy_provider_thrift, 'ProviderProxy'};
get_service(proxy_inspector) ->
    {dmsl_proxy_inspector_thrift, 'InspectorProxy'};
get_service(proxy_host_provider) ->
    {dmsl_proxy_provider_thrift, 'ProviderProxyHost'};
get_service(accounter) ->
    {dmsl_accounter_thrift, 'Accounter'};
get_service(automaton) ->
    {mg_proto_state_processing_thrift, 'Automaton'};
get_service(processor) ->
    {mg_proto_state_processing_thrift, 'Processor'};
get_service(eventsink) ->
    {mg_proto_state_processing_thrift, 'EventSink'}.

-spec get_service_spec(Name :: atom()) -> service_spec().

get_service_spec(Name) ->
    get_service_spec(Name, #{}).

-spec get_service_spec(Name :: atom(), Opts :: #{namespace => binary()}) -> service_spec().

get_service_spec(Name = party_management, #{}) ->
    {?VERSION_PREFIX ++ "/processing/partymgmt", get_service(Name)};
get_service_spec(Name = invoicing, #{}) ->
    {?VERSION_PREFIX ++ "/processing/invoicing", get_service(Name)};
get_service_spec(Name = invoice_templating, #{}) ->
    {?VERSION_PREFIX ++ "/processing/invoice_templating", get_service(Name)};
get_service_spec(Name = customer_management, #{}) ->
    {?VERSION_PREFIX ++ "/processing/customer_management", get_service(Name)};
get_service_spec(Name = payment_processing_eventsink, #{}) ->
    {?VERSION_PREFIX ++ "/processing/eventsink", get_service(Name)};
get_service_spec(Name = recurrent_payment_tools, #{}) ->
    {?VERSION_PREFIX ++ "/processing/recpaytools", get_service(Name)};
get_service_spec(Name = recurrent_payment_tools_eventsink, #{}) ->
    {?VERSION_PREFIX ++ "/processing/recpaytools/eventsink", get_service(Name)};
get_service_spec(Name = processor, #{namespace := Ns}) when is_binary(Ns) ->
    {?VERSION_PREFIX ++ "/stateproc/" ++ binary_to_list(Ns), get_service(Name)};
get_service_spec(Name = proxy_host_provider, #{}) ->
    {?VERSION_PREFIX ++ "/proxyhost/provider", get_service(Name)}.
