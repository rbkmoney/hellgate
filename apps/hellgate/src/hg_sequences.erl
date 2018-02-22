-module(hg_sequences).

-export([get_current/1]).
-export([get_next/1]).

-type seq_id() :: seq_proto_sequences_thrift:'SequenceId'().
-type value() :: seq_proto_sequences_thrift:'Value'().

-spec get_current(seq_id()) ->
    value().

get_current(SeqID) ->
    {ok, Value} = issue_call('GetCurrent', [SeqID]),
    Value.

-spec get_next(seq_id()) ->
    value().

get_next(SeqID) ->
    {ok, Value} = issue_call('GetNext', [SeqID]),
    Value.

issue_call(Func, Args) ->
    hg_woody_wrapper:call(sequences, Func, Args).