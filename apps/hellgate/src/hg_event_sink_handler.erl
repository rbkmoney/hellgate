-module(hg_event_sink_handler).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type event_id() :: dmsl_base_thrift:'EventID'().
-type event()    :: dmsl_payment_processing_thrift:'Event'().

-spec handle_function
    ('GetEvents', woody:args(), hg_woody_wrapper:handler_opts()) ->
        [event()] | no_return();
    ('GetLastEventID', woody:args(), hg_woody_wrapper:handler_opts()) ->
        event_id() | no_return().

handle_function('GetEvents', [#payproc_EventRange{'after' = After, limit = Limit}], _Opts) ->
    case hg_event_sink:get_events(<<"payproc">>, After, Limit) of
        {ok, Events} ->
            publish_events(Events);
        {error, event_not_found} ->
            throw(#payproc_EventNotFound{})
    end;

handle_function('GetLastEventID', [], _Opts) ->
    % TODO handle thrift exceptions here
    case hg_event_sink:get_last_event_id(<<"payproc">>) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#payproc_NoLastEvent{})
    end.

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, Ns, SourceID, {EventID, Dt, Payload}}) ->
    hg_event_provider:publish_event(Ns, ID, SourceID, {EventID, Dt, hg_msgpack_marshalling:unmarshal(Payload)}).
