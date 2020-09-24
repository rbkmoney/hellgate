-module(hg_dummy_inspector).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).


-include_lib("damsel/include/dmsl_proxy_inspector_thrift.hrl").
-include_lib("hellgate/include/invoice_events.hrl").

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/inspector/dummy", {dmsl_proxy_inspector_thrift, 'InspectorProxy'}}.


-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'InspectPayment',
    {#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"risk_score">> := RiskScore
        }
    }},
    _Options
) ->
    binary_to_atom(RiskScore, utf8);

handle_function(
    'InspectPayment',
    {#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"link_state">> := <<"unexpected_failure">>
        }
    }},
    _Options
) ->
    erlang:error(test_error);
handle_function(
    'InspectPayment',
    {#proxy_inspector_Context{
        payment = #proxy_inspector_PaymentInfo{
            payment = #proxy_inspector_InvoicePayment{
                id = PaymentID
            },
            invoice = #proxy_inspector_Invoice{
                id = InvoiceID
            }
        },
        options = #{
            <<"link_state">> := <<"temporary_failure">>
        }
    }},
    _Options
) ->
    case is_already_failed(InvoiceID, PaymentID) of
        false ->
            ok = set_failed(InvoiceID, PaymentID),
            erlang:error(test_error);
        true ->
            low
    end;
handle_function(
    'InspectPayment',
    {#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"link_state">> := _LinkState
        }
    }},
    _Options
) ->
    timer:sleep(10000),
    high.

-define(temp_failure_key(InvoiceID, PaymentID), {temporary_failure_inspector, InvoiceID, PaymentID}).

is_already_failed(InvoiceID, PaymentID) ->
    case hg_kv_store:get(?temp_failure_key(InvoiceID, PaymentID)) of
        undefined ->
            false;
        failed ->
            true
    end.

set_failed(InvoiceID, PaymentID) ->
    hg_kv_store:put(?temp_failure_key(InvoiceID, PaymentID), failed).
