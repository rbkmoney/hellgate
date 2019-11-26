-module(hg_dummy_fault_detector).

-behaviour(woody_server_thrift_handler).

-export([child_spec/0]).

-export([handle_function/4]).

-include_lib("fault_detector_proto/include/fd_proto_fault_detector_thrift.hrl").

-spec child_spec() ->
    term().
child_spec() ->
    woody_server:child_spec(
      ?MODULE,
      #{
         handlers => [{"/", {{fd_proto_fault_detector_thrift, 'FaultDetector'}, ?MODULE}}],
         event_handler => scoper_woody_event_handler,
         ip => {127, 0, 0, 1},
         port => 20001
        }
     ).

-spec handle_function(woody:func(), woody:args(), _, hg_woody_wrapper:handler_opts()) ->
    {ok, term()} | no_return().

handle_function(
    'GetStatistics',
    _Args,
    _Context,
    _Options
 ) ->
    {ok, [
          #fault_detector_ServiceStatistics{
             service_id = <<"hellgate_service.provider_conversion.200">>,
             failure_rate = 0.9,
             operations_count = 10,
             error_operations_count = 9,
             overtime_operations_count = 0,
             success_operations_count = 1
            },
          #fault_detector_ServiceStatistics{
             service_id = <<"hellgate_service.provider_conversion.201">>,
             failure_rate = 0.1,
             operations_count = 10,
             error_operations_count = 1,
             overtime_operations_count = 0,
             success_operations_count = 9
            },
          #fault_detector_ServiceStatistics{
             service_id = <<"hellgate_service.adapter_availability.200">>,
             failure_rate = 0.9,
             operations_count = 10,
             error_operations_count = 9,
             overtime_operations_count = 0,
             success_operations_count = 1
            },
          #fault_detector_ServiceStatistics{
             service_id = <<"hellgate_service.adapter_availability.201">>,
             failure_rate = 0.1,
             operations_count = 10,
             error_operations_count = 1,
             overtime_operations_count = 0,
             success_operations_count = 9
            }
         ]};

handle_function(_Function, _Args, _Context, _Options) ->
    timer:sleep(3000),
    {ok, undefined}.
