-module(hg_eventsink_history).

-export([assert_total_order/1]).
-export([assert_contiguous_sequences/1]).

%%

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type history() :: [dmsl_payment_processing_thrift:'Event'()].

-define(event(ID, Source, Seq, Payload),
    #payproc_Event{
        id = ID,
        source = Source,
        sequence = Seq,
        payload = Payload
    }
).

-spec assert_total_order(history()) -> ok | no_return().

assert_total_order([?event(ID, _, _, _) | Rest]) ->
    _ = lists:foldl(
        fun (?event(ID1, _, _, _), ID0) ->
            true = ID1 > ID0,
            ID1
        end,
        ID,
        Rest
    ),
    ok.

-spec assert_contiguous_sequences(history()) -> ok | no_return().

assert_contiguous_sequences(Events) ->
    InvoiceSeqs = orddict:to_list(lists:foldl(
        fun (?event(_ID, InvoiceID, Seq, _), Acc) ->
            orddict:update(InvoiceID, fun (Seqs) -> Seqs ++ [Seq] end, [Seq], Acc)
        end,
        orddict:new(),
        Events
    )),
    lists:foreach(
        fun (E = {InvoiceID, Seqs}) ->
            SeqsExpected = lists:seq(1, length(Seqs)),
            {InvoiceID, SeqsExpected} = E
        end,
        InvoiceSeqs
    ).
