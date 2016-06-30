-module(hg_history).
-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

-export([wrap/3]).
-export([unwrap/1]).

%%

-type payload() :: term().

-spec wrap([payload()], module(), hg_machine:history(payload())) ->
    hg_state_processing_thrift:'EventBodies'().

wrap(Events, Module, History) ->
    wrap_events(Module, length(History) + 1, Events).

wrap_events(Module, Seq, [Ev | Rest]) ->
    [wrap_event(Module, Seq, Ev) | wrap_events(Module, Seq + 1, Rest)];
wrap_events(_, _, []) ->
    [].

wrap_event(Module, Seq, EventInner) ->
    marshal_term({Module, Seq, EventInner}).

-spec unwrap(hg_state_processing_thrift:'History'()) ->
    [{module(), hg_machine:event(payload())}].

unwrap(History) ->
    [unwrap_event(E) || E <- History].

unwrap_event(#'Event'{id = ID, machine_id = Source, created_at = Dt, event_payload = Payload}) ->
    {Module, Seq, EventInner} = unmarshal_term(Payload),
    {Module, {ID, Source, Dt, Seq, EventInner}}.

%%

marshal_term(V) ->
    term_to_binary(V).

unmarshal_term(B) ->
    binary_to_term(B).
