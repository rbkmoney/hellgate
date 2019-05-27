%%% Fault detector interaction

-module(hg_fault_detector_client).

-include_lib("fault_detector_proto/include/fd_proto_fault_detector_thrift.hrl").

-define(service_config(SW, OTL, PAS), #fault_detector_ServiceConfig{
     sliding_window       = SW,
     operation_time_limit = OTL,
     pre_aggregation_size = PAS
}).

-define(operation(OpId, State), #fault_detector_Operation{
     operation_id = OpId,
     state        = State
}).

-define(state_start(TimeStamp),  #fault_detector_Start{ time_start = TimeStamp }).
-define(state_error(TimeStamp),  #fault_detector_Error{ time_end   = TimeStamp }).
-define(state_finish(TimeStamp), #fault_detector_Finish{ time_end  = TimeStamp }).

-export([build_config/0]).
-export([build_config/2]).
-export([build_config/3]).

-export([build_service_id/2]).
-export([build_operation_id/1]).

-export([init_service/1]).
-export([init_service/2]).

-export([get_statistics/1]).

-export([register_operation/3]).
-export([register_operation/4]).

-type operation_status()        :: start | finish | error.
-type service_stats()           :: fd_proto_fault_detector_thrift:'ServiceStatistics'().
-type service_id()              :: fd_proto_fault_detector_thrift:'ServiceId'().
-type operation_id()            :: fd_proto_fault_detector_thrift:'OperationId'().
-type service_config()          :: fd_proto_fault_detector_thrift:'ServiceConfig'().
-type sliding_window()          :: fd_proto_fault_detector_thrift:'Milliseconds'().
-type operation_time_limit()    :: fd_proto_fault_detector_thrift:'Milliseconds'().
-type pre_aggregation_size()    :: fd_proto_fault_detector_thrift:'Seconds'() | undefined.

-type fd_service_type()         :: adapter_availability.

%% API

%%------------------------------------------------------------------------------
%% @doc
%% `build_config/0` creates a default config that can be used  with
%% `init_service/2` and `register_operation/4`.
%%
%% The default values can be adjusted via sys.config.
%%
%% Config
%% `SlidingWindow`: pick operations from SlidingWindow milliseconds.
%%      Default: 60000
%% `OpTimeLimit`: expected operation execution time, in milliseconds.
%%      Default: 10000
%% `PreAggrSize`: time interval for data preaggregation, in seconds.
%%      Default: 2
%% @end
%%------------------------------------------------------------------------------
-spec build_config() ->
    service_config().
build_config() ->
    EnvFDConfig     = genlib_app:env(hellgate, fault_detector, #{}),
    SlidingWindow   = genlib_map:get(sliding_window,       EnvFDConfig, 60000),
    OpTimeLimit     = genlib_map:get(operation_time_limit, EnvFDConfig, 10000),
    PreAggrSize     = genlib_map:get(pre_aggregation_size, EnvFDConfig, 2),
    ?service_config(SlidingWindow, OpTimeLimit, PreAggrSize).

%%------------------------------------------------------------------------------
%% @doc
%% `build_config/2` receives the length of the sliding windown and the operation
%% time limit as arguments. The config can then be used with `init_service/2`
%% and `register_operation/4`.
%% @end
%%------------------------------------------------------------------------------
-spec build_config(sliding_window(), operation_time_limit()) ->
    service_config().
build_config(SlidingWindow, OpTimeLimit) ->
    ?service_config(SlidingWindow, OpTimeLimit, undefined).

%%------------------------------------------------------------------------------
%% @doc
%% `build_config/3` is analogous to `build_config/2` but also receives
%% the optional pre-aggregation size argument.
%% @end
%%------------------------------------------------------------------------------
-spec build_config(sliding_window(), operation_time_limit(), pre_aggregation_size()) ->
    service_config().
build_config(SlidingWindow, OpTimeLimit, PreAggrSize) ->
    ?service_config(SlidingWindow, OpTimeLimit, PreAggrSize).

%%------------------------------------------------------------------------------
%% @doc
%% `build_service_id/2` is a helper function for building service IDs
%% @end
%%------------------------------------------------------------------------------
-spec build_service_id(fd_service_type(), binary()) ->
    binary().
build_service_id(ServiceType, ID) ->
    do_build_service_id(ServiceType, ID).

%%------------------------------------------------------------------------------
%% @doc
%% `build_operation_id_id/3` is a helper function for building operation IDs
%% @end
%%------------------------------------------------------------------------------
-spec build_operation_id(fd_service_type()) -> binary().
build_operation_id(ServiceType) ->
    do_build_operation_id(ServiceType).

%%------------------------------------------------------------------------------
%% @doc
%% `init_service/1` receives a service id and initialises a fault detector
%% service for it, allowing you to aggregate availability statistics via
%% `register_operation/3` and `register_operation/4` and fetch it using the
%% `get_statistics/1` function.
%% @end
%%------------------------------------------------------------------------------
-spec init_service(service_id()) ->
    {ok, initialised} | {error, any()}.
init_service(ServiceId) ->
    ServiceConfig = build_config(),
    call('InitService', [ServiceId, ServiceConfig]).

%%------------------------------------------------------------------------------
%% @doc
%% `init_service/2` is analogous to `init_service/1` but also receives
%% configuration for the fault detector service created by `build_config/3`.
%% @end
%%------------------------------------------------------------------------------
-spec init_service(service_id(), service_config()) ->
    {ok, initialised} | {error, any()}.
init_service(ServiceId, ServiceConfig) ->
    call('InitService', [ServiceId, ServiceConfig]).

%%------------------------------------------------------------------------------
%% @doc
%% `get_statistics/1` receives a list of service ids and returns a
%% list of statistics on the services' reliability.
%%
%% Returns an empty list if the fault detector itself is unavailable. Services
%% not initialised in the fault detector will not be in the list.
%% @end
%%------------------------------------------------------------------------------
-spec get_statistics([service_id()]) -> [service_stats()].
get_statistics(ServiceIds) when is_list(ServiceIds) ->
    call('GetStatistics', [ServiceIds]).

%%------------------------------------------------------------------------------
%% @doc
%% `register_operation/3` receives a service id, an operation id and an
%% operation status which is one of the following atoms: `start`, `finish`, `error`,
%% respectively for registering a start and either a successful or an erroneous
%% end of an operation. The data is then used to aggregate statistics on a
%% service's availability that is accessible via `get_statistics/1`.
%% @end
%%------------------------------------------------------------------------------
-spec register_operation(operation_status(), service_id(), operation_id()) ->
    {ok, registered} | {error, not_found} | {error, any()}.
register_operation(Status, ServiceId, OperationId) ->
    ServiceConfig = build_config(),
    register_operation(Status, ServiceId, OperationId, ServiceConfig).

%%------------------------------------------------------------------------------
%% @doc
%% `register_operation/4` is analogous to `register_operation/3` but also
%% receives configuration for the fault detector service created
%% by `build_config/3`.
%% @end
%%------------------------------------------------------------------------------
-spec register_operation(operation_status(), service_id(), operation_id(), service_config()) ->
    {ok, registered} | {error, not_found} | {error, any()}.
register_operation(Status, ServiceId, OperationId, ServiceConfig) ->
    OperationState = case Status of
        start  -> {Status, ?state_start(hg_datetime:format_now())};
        error  -> {Status, ?state_error(hg_datetime:format_now())};
        finish -> {Status, ?state_finish(hg_datetime:format_now())}
    end,
    Operation = ?operation(OperationId, OperationState),
    call('RegisterOperation', [ServiceId, Operation, ServiceConfig]).

%% PRIVATE

call(Function, Args) ->
    ServiceUrls = genlib_app:env(hellgate, services),
    Url         = genlib:to_binary(maps:get(fault_detector, ServiceUrls)),
    Opts        = #{url => Url},
    EnvFDConfig = genlib_app:env(hellgate, fault_detector, #{}),
    Timeout     = genlib_map:get(timeout, EnvFDConfig, infinity),
    Deadline    = woody_deadline:from_timeout(Timeout),
    do_call(Function, Args, Opts, Deadline).

do_call('InitService', Args, Opts, Deadline) ->
    try hg_woody_wrapper:call(fault_detector, 'InitService', Args, Opts, Deadline) of
        {ok, _Result} -> {ok, initialised}
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason
            when Class =:= resource_unavailable orelse
                 Class =:= result_unknown ->
            [ServiceId | _] = Args,
            ErrorText = "Unable to init service ~p in fault detector, ~p:~p",
            _ = lager:warning(ErrorText, [ServiceId, error, Reason]),
            {error, Reason};
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            [ServiceId | _] = Args,
            ErrorText = "Unable to init service ~p in fault detector, ~p:~p",
            _ = lager:error(ErrorText, [ServiceId, error, Reason]),
            {error, Reason}
    end;
do_call('GetStatistics', Args, Opts, Deadline) ->
    try hg_woody_wrapper:call(fault_detector, 'GetStatistics', Args, Opts, Deadline) of
        {ok, Stats} -> Stats
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason
            when Class =:= resource_unavailable orelse
                 Class =:= result_unknown ->
            [ServiceIds | _] = Args,
            String = "Unable to get statistics for services ~p from fault detector, ~p:~p",
            _ = lager:warning(String, [ServiceIds, error, Reason]),
            [];
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            [ServiceIds | _] = Args,
            String = "Unable to get statistics for services ~p from fault detector, ~p:~p",
            _ = lager:error(String, [ServiceIds, error, Reason]),
            []
    end;
do_call('RegisterOperation', Args, Opts, Deadline) ->
    try hg_woody_wrapper:call(fault_detector, 'RegisterOperation', Args, Opts, Deadline) of
        {ok, _Result} ->
            {ok, registered};
        {exception, #fault_detector_ServiceNotFoundException{}} ->
            {error, not_found}
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason
            when Class =:= resource_unavailable orelse
                 Class =:= result_unknown ->
            [ServiceId, OperationId | _] = Args,
            ErrorText = "Unable to register operation ~p for service ~p in fault detector, ~p:~p",
            _ = lager:warning(ErrorText, [OperationId, ServiceId, error, Reason]),
            {error, Reason};
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            [ServiceId, OperationId | _] = Args,
            ErrorText = "Unable to register operation ~p for service ~p in fault detector, ~p:~p",
            _ = lager:error(ErrorText, [OperationId, ServiceId, error, Reason]),
            {error, Reason}
    end.

do_build_service_id(adapter_availability, ID) ->
    hg_utils:construct_complex_id([
        <<"hellgate_service">>,
        <<"adapter_availability">>,
        ID
    ]).

do_build_operation_id(adapter_availability) ->
    hg_utils:construct_complex_id([
        <<"hellgate_operation">>,
        <<"adapter_availability">>,
        hg_utils:unique_id()
    ]).
