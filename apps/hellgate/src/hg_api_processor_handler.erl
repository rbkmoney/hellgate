-module(hg_api_processor_handler).
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export([handle_error/4]).

%%

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-type func() :: 'processSignal' | 'processCall'.

-spec handle_function(func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {ok, term()} | no_return().

handle_function('processSignal', {Args}, Context, _Opts) ->
    #'SignalArgs'{signal = {_Type, Signal}, history = History} = Args,
    {ok, hg_machine:dispatch_signal(Signal, unmarshal_history(History), opts(Context))};

handle_function('processCall', {Args}, Context, _Opts) ->
    #'CallArgs'{call = Payload, history = History} = Args,
    {ok, hg_machine:dispatch_call(Payload, unmarshal_history(History), opts(Context))}.

unmarshal_history(undefined) ->
    [];
unmarshal_history(History) ->
    [{ID, Body} || #'Event'{id = ID, body = Body} <- History].

opts(Context) ->
    #{context => Context}.

-spec handle_error(woody_t:func(), term(), woody_client:context(), []) ->
    _.

handle_error(_Function, _Reason, _Context, _Opts) ->
    ok.
