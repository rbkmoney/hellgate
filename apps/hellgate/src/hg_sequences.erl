-module(hg_sequences).

-export([get_next/1]).

-include_lib("bender_proto/include/bender_thrift.hrl").
-include_lib("bender_proto/include/msgpack_thrift.hrl").

-type seq_id() :: binary().
-type value() :: bender_thrift:'InternalID'().

-spec get_next(seq_id()) ->
    value().

get_next(SeqID) ->
    Hash = erlang:phash2(SeqID),
    WoodyCtx = #{},
    {ok, Value} = gen_by_sequence(gen_idemotency_key(), SeqID, Hash, WoodyCtx),
    Value.

gen_idemotency_key() ->
    Random = crypto:strong_rand_bytes(16),
    genlib_format:format_int_base(binary:decode_unsigned(Random), 62).

gen_by_sequence(IdempotentKey, SequenceID, Hash, WoodyContext) ->
    Sequence = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
    generate_id(IdempotentKey, Sequence, Hash, WoodyContext).

generate_id(Key, BenderSchema, Hash, _) ->
    Context = hg_msgpack_marshalling:marshal(#{
        <<"version">>     => 1,
        <<"params_hash">> => Hash
    }),
    Args = [Key, BenderSchema, Context],
    R = issue_call('GenerateID', Args),
    Result = case R of
        {ok, #bender_GenerationResult{internal_id = InternalID, context = undefined}} ->
            {ok, InternalID};
        {ok, #bender_GenerationResult{internal_id = InternalID, context = Ctx}}       ->
            #{<<"params_hash">> := BenderHash} = mg_msgpack_marshalling:unmarshal(Ctx),
            {ok, InternalID, BenderHash}
    end,
    case Result of
        {ok, ID}         -> {ok, ID};
        {ok, ID, Hash}   -> {ok, ID};
        {ok, ID, _Other} -> {error, {external_id_conflict, ID}}
    end.

issue_call(Func, Args) ->
    hg_woody_wrapper:call(bender, Func, Args).
