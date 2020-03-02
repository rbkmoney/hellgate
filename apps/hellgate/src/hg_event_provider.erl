-module(hg_event_provider).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type source_event() :: _.
-type public_event() :: {source(), payload()}.
-type source()   :: dmsl_payment_processing_thrift:'EventSource'().
-type payload()  :: dmsl_payment_processing_thrift:'EventPayload'().

-export_type([public_event/0]).

-callback publish_event(hg_machine:id(), source_event()) ->
    public_event().

-export([publish_event/4]).

%%

-type event_id() :: dmsl_base_thrift:'EventID'().
-type event()    :: dmsl_payment_processing_thrift:'Event'().

-spec publish_event(hg_machine:ns(), event_id(), hg_machine:id(), hg_machine:event()) ->
    event().

publish_event(Ns, EventID, MachineID, {ID, Dt, Ev}) ->
    Module = get_handler_module(Ns),
    {Source, Payload} = Module:publish_event(MachineID, Ev),
    #payproc_Event{
        id         = EventID,
        source     = Source,
        created_at = Dt,
        payload    = Payload,
        sequence   = ID
    }.

get_handler_module(NS) ->
    Machines = [hg_machine, pm_machine],
    {value, Handler} = search_and_return(
        fun (Machine) ->
            try Machine:get_handler_module(NS) of
                Result ->
                    {true, Result}
            catch
                error:badarg ->
                    false
            end
        end,
        Machines),
    Handler.

-spec search_and_return(Pred, List) -> {value, Value} | false when
    Pred :: fun((T) -> {true, Value} | false),
    List :: [T],
    Value :: T | any().
search_and_return(Pred, [Head | Tail]) ->
    case Pred(Head) of
        {true, Result} ->
            {value, Result};
        false ->
            search_and_return(Pred, Tail)
    end;
search_and_return(Pred, []) when is_function(Pred, 1) ->
    false.
