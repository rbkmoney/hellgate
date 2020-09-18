-module(hg_proto_utils).

-export([serialize/2]).
-export([deserialize/2]).

-export([serialize_function_args/2]).
-export([deserialize_function_args/2]).

-export([serialize_function_reply/2]).
-export([deserialize_function_reply/2]).

-export([serialize_function_exception/2]).
-export([deserialize_function_exception/2]).

-export([record_to_proplist/2]).

%% Types

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
    {struct, thrift_struct_flavor(), thrift_type_ref() | thrift_struct_def()}.

-type thrift_struct_flavor() :: struct | union | exception.

-type thrift_type_ref() :: {module(), Name :: atom()}.

-type thrift_struct_def() :: list({
    Tag :: pos_integer(),
    Requireness :: required | optional | undefined,
    Type :: thrift_struct_type(),
    Name :: atom(),
    Default :: any()
}).

-type thrift_fun_ref() :: {Service :: atom(), Function :: atom()}.
-type thrift_fun_full_ref() :: {module(), thrift_fun_ref()}.
-type thrift_exception() :: tuple().

-export_type([thrift_type/0]).
-export_type([thrift_exception/0]).
-export_type([thrift_fun_full_ref/0]).
-export_type([thrift_fun_ref/0]).

%% API

-spec serialize_function_args(thrift_fun_full_ref(), list(term())) ->
    binary().

serialize_function_args({Module, {Service, Function}}, Args) when is_list(Args) ->
    ArgsType = Module:function_info(Service, Function, params_type),
    ArgsRecord = erlang:list_to_tuple([args | Args]),
    serialize(ArgsType, ArgsRecord).

-spec serialize_function_reply(thrift_fun_full_ref(), term()) ->
    binary().

serialize_function_reply({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, reply_type),
    serialize(ArgsType, Data).

-spec serialize_function_exception(thrift_fun_full_ref(), thrift_exception()) ->
    binary().

serialize_function_exception(FunctionRef, Exception) ->
    ExceptionType = get_fun_exception_type(FunctionRef),
    Name = find_exception_name(ExceptionType, Exception),
    serialize(ExceptionType, {Name, Exception}).

-spec serialize(thrift_type(), term()) -> binary().

serialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:write(Proto, {Type, Data}) of
        {NewProto, ok} ->
            {_, {ok, Result}} = thrift_protocol:close_transport(NewProto),
            Result;
        {_NewProto, {error, Reason}} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

-spec deserialize(thrift_type(), binary()) ->
    term().

deserialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(Data),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:read(Proto, Type) of
        {_NewProto, {ok, Result}} ->
            Result;
        {_NewProto, {error, Reason}} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

-spec deserialize_function_args(thrift_fun_full_ref(), binary()) ->
    list(term()).

deserialize_function_args({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, params_type),
    Args = deserialize(ArgsType, Data),
    erlang:tuple_to_list(Args).

-spec deserialize_function_reply(thrift_fun_full_ref(), binary()) ->
    term().

deserialize_function_reply({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, reply_type),
    deserialize(ArgsType, Data).

-spec deserialize_function_exception(thrift_fun_full_ref(), binary()) ->
    thrift_exception().

deserialize_function_exception(FunctionRef, Data) ->
    ExceptionType = get_fun_exception_type(FunctionRef),
    {_Name, Exception} = deserialize(ExceptionType, Data),
    Exception.

%% Internals

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

-spec get_fun_exception_type(thrift_fun_full_ref()) ->
    thrift_type().

get_fun_exception_type({Module, {Service, Function}}) ->
    DeclaredType = Module:function_info(Service, Function, exceptions),
    % В сгенерированном коде исключения объявлены как структура.
    % Для удобства работы, преобразуем тип в union.
    {struct, struct, Exceptions} = DeclaredType,
    {struct, union, Exceptions}.

-spec find_exception_name(thrift_type(), thrift_exception()) ->
    Name :: atom().

find_exception_name(Type, Exception) ->
    RecordName = erlang:element(1, Exception),
    {struct, union, Variants} = Type,
    do_find_exception_name(Variants, RecordName).

-spec do_find_exception_name(thrift_struct_def(), atom()) ->
    Name :: atom().

do_find_exception_name([], RecordName) ->
    erlang:error({thrift, {unknown_exception, RecordName}});
do_find_exception_name([{_Tag, _Req, Type, Name, _Default} | Tail], RecordName) ->
    {struct, exception, {Module, Exception}} = Type,
    case Module:record_name(Exception) of
        TypeRecordName when TypeRecordName =:= RecordName ->
            Name;
        _Other ->
            do_find_exception_name(Tail, RecordName)
    end.

