-module(hg_event_provider).

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-type public_event() :: {source(), sequence(), payload()}.

-type source()   :: hg_payment_processing_thrift:'EventSource'().
-type sequence() :: pos_integer().
-type payload()  :: hg_payment_processing_thrift:'EventPayload'().

-export_type([public_event/0]).

-callback publish_event(hg_machine:id(), hg_machine:event()) ->
    {true, public_event()} | false.

-export([publish_event/4]).

%%

-type event_id() :: hg_base_thrift:'EventID'().
-type event()    :: hg_payment_processing_thrift:'Event'().

-spec publish_event(hg_machine:ns(), event_id(), hg_machine:id(), hg_machine:event()) ->
    {true, event()} | false.

publish_event(Ns, EventID, MachineID, {_ID, Dt, Ev}) ->
    Module = hg_machine:get_handler_module(Ns),
    case Module:publish_event(MachineID, Ev) of
        {true, {Source, Sequence, Payload}} ->
            {true, #payproc_Event{
                id         = EventID,
                source     = Source,
                created_at = Dt,
                sequence   = Sequence,
                payload    = Payload
            }};
        false ->
            false
    end.
