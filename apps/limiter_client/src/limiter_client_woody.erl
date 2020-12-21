-module(limiter_client_woody).

-export([call/4]).

-define(APP, limiter_client).

%%
-type client_opts() :: #{
    url := woody:url(),
    %% See hackney:request/5 for available transport options.
    transport_opts => woody_client_thrift_http_transport:transport_options()
}.

-spec call(atom(), woody:func(), woody:args(), woody_context:ctx()) -> woody:result().
call(Service, Function, Args, Context0) ->
    Deadline = get_service_deadline(),
    Context1 = set_deadline(Deadline, Context0),
    Retry = get_service_retry(Function),
    EventHandler = scoper_woody_event_handler,
    call(Service, Function, Args, Context1, EventHandler, Retry).

call(Service, Function, Args, Context, EventHandler, Retry) ->
    Options = get_service_options(Service),
    Request = {{dmsl_proto_limiter_thrift, Service}, Function, Args},
    try
        woody_client:call(
            Request,
            Options#{event_handler => EventHandler},
            Context
        )
    catch
        error:{woody_error, {_Source, Class, _Details}} = Error when
            Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            NextRetry = apply_retry_strategy(Retry, Error, Context),
            call(Service, Function, Args, Context, EventHandler, NextRetry)
    end.

-spec get_service_options(atom()) -> client_opts().
get_service_options(Service) ->
    construct_opts(maps:get(Service, genlib_app:env(?APP, services))).

construct_opts(Opts = #{url := Url}) ->
    Opts#{url := genlib:to_binary(Url)};
construct_opts(Url) ->
    #{url => genlib:to_binary(Url)}.

apply_retry_strategy(Retry, Error, Context) ->
    apply_retry_step(genlib_retry:next_step(Retry), woody_context:get_deadline(Context), Error).

apply_retry_step(finish, _, Error) ->
    erlang:error(Error);
apply_retry_step({wait, Timeout, Retry}, undefined, _) ->
    ok = timer:sleep(Timeout),
    Retry;
apply_retry_step({wait, Timeout, Retry}, Deadline0, Error) ->
    Deadline1 = woody_deadline:from_unixtime_ms(
        woody_deadline:to_unixtime_ms(Deadline0) - Timeout
    ),
    case woody_deadline:is_reached(Deadline1) of
        true ->
            % no more time for retries
            erlang:error(Error);
        false ->
            ok = timer:sleep(Timeout),
            Retry
    end.

get_service_deadline() ->
    case genlib_app:env(?APP, deadline, undefined) of
        Timeout when is_integer(Timeout) andalso Timeout >= 0 ->
            woody_deadline:from_timeout(Timeout);
        undefined ->
            undefined
    end.

set_deadline(Deadline, Context) ->
    case woody_context:get_deadline(Context) of
        undefined ->
            woody_context:set_deadline(Deadline, Context);
        _AlreadySet ->
            Context
    end.

get_service_retry(Function) ->
    FunctionReties = genlib_app:env(?APP, retries, #{}),
    DefaultRetry = maps:get('_', FunctionReties, finish),
    maps:get(Function, FunctionReties, DefaultRetry).
