-module(hg_dummy_inspector).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).


-include_lib("dmsl/include/dmsl_proxy_inspector_thrift.hrl").
-include_lib("hellgate/include/invoice_events.hrl").

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/inspector/dummy", {dmsl_proxy_inspector_thrift, 'InspectorProxy'}}.


-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'InspectPayment',
    [#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"risk_score">> := RiskScore
        }
    }],
    _Options
) ->
    binary_to_atom(RiskScore, utf8);

handle_function(
    'InspectPayment',
    [#proxy_inspector_Context{
        payment = _PaymentInfo,
        options = #{
            <<"link_state">> := _LinkState
        }
    }],
    _Options
) ->
    erlang:error(test_error).












