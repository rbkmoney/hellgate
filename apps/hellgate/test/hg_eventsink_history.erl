-module(hg_eventsink_history).

-export([assert_total_order/1]).

%%

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type history() :: [dmsl_payment_processing_thrift:'Event'()].

-define(event(ID, Source, Payload),
    #payproc_Event{
        id = ID,
        source = Source,
        payload = Payload
    }
).

-spec assert_total_order(history()) -> ok | no_return().

assert_total_order([]) ->
    ok;

assert_total_order([?event(ID, _, _) | Rest]) ->
    _ = lists:foldl(
        fun (?event(ID1, _, _), ID0) ->
            true = ID1 > ID0,
            ID1
        end,
        ID,
        Rest
    ),
    ok.
