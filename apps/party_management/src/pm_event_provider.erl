-module(pm_event_provider).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type source_event() :: _.
-type public_event() :: {source(), payload()}.
-type source()   :: dmsl_payment_processing_thrift:'EventSource'().
-type payload()  :: dmsl_payment_processing_thrift:'EventPayload'().

-export_type([public_event/0]).

-callback publish_event(pm_machine:id(), source_event()) ->
    public_event().

-export([publish_event/4]).

%%

-type event_id() :: dmsl_base_thrift:'EventID'().
-type event()    :: dmsl_payment_processing_thrift:'Event'().

-spec publish_event(pm_machine:ns(), event_id(), pm_machine:id(), pm_machine:event()) ->
    event().

publish_event(Ns, EventID, MachineID, {ID, Dt, Ev}) ->
    Module = pm_machine:get_handler_module(Ns),
    {Source, Payload} = Module:publish_event(MachineID, Ev),
    #payproc_Event{
        id         = EventID,
        source     = Source,
        created_at = Dt,
        payload    = Payload,
        sequence   = ID
    }.
