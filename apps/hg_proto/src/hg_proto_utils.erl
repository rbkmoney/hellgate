-module(hg_proto_utils).

-export([serialize/2]).
-export([deserialize/2]).

-export([record_to_proplist/2]).

%%

%% TODO: move it to the thrift runtime lib?

-type thrift_type() ::
    thrift_base_type() |
    thrift_collection_type() |
    thrift_enum_type() |
    thrift_struct_type().

-type thrift_base_type() ::
    bool   |
    double |
    i8     |
    i16    |
    i32    |
    i64    |
    string.

-type thrift_collection_type() ::
    {list, thrift_type()} |
    {set, thrift_type()} |
    {map, thrift_type(), thrift_type()}.

-type thrift_enum_type() ::
    {enum, thrift_type_ref()}.

-type thrift_struct_type() ::
    {struct, thrift_struct_flavor(), thrift_type_ref()}.

-type thrift_struct_flavor() :: struct | union | exception.

-type thrift_type_ref() :: {module(), Name :: atom()}.

%%

-spec serialize(thrift_type(), term()) -> {ok, binary()} | {error, any()}.

serialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:write(Proto, {Type, Data}) of
        {NewProto, ok} ->
            {_, Result} = thrift_protocol:close_transport(NewProto),
            Result;
        {_NewProto, {error, _Reason} = Error} ->
            Error
    end.

-spec deserialize(thrift_type(), binary()) -> {ok, term()} | {error, any()}.

deserialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(Data),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:read(Proto, Type) of
        {_NewProto, {ok, Result}} ->
            {ok, Result};
        {_NewProto, {error, _Reason} = Error} ->
            Error
    end.

new_protocol(Trans) ->
    thrift_binary_protocol:new(Trans, [{strict_read, true}, {strict_write, true}]).

%%

-spec record_to_proplist(Record :: tuple(), RecordInfo :: [atom()]) -> [{atom(), _}].

record_to_proplist(Record, RecordInfo) ->
    element(1, lists:foldl(
        fun (RecordField, {L, N}) ->
            case element(N, Record) of
                V when V /= undefined ->
                    {[{RecordField, V} | L], N + 1};
                undefined ->
                    {L, N + 1}
            end
        end,
        {[], 1 + 1},
        RecordInfo
    )).
