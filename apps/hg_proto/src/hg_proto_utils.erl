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
    bool |
    double |
    i8 |
    i16 |
    i32 |
    i64 |
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

-spec serialize_function_args(thrift_fun_full_ref(), woody:args()) -> binary().
serialize_function_args({Module, {Service, Function}}, Args) when is_tuple(Args) ->
    ArgsType = Module:function_info(Service, Function, params_type),
    serialize(ArgsType, Args).

-spec serialize_function_reply(thrift_fun_full_ref(), term()) -> binary().
serialize_function_reply({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, reply_type),
    serialize(ArgsType, Data).

-spec serialize_function_exception(thrift_fun_full_ref(), thrift_exception()) -> binary().
serialize_function_exception(FunctionRef, Exception) ->
    ExceptionType = get_fun_exception_type(FunctionRef),
    Name = find_exception_name(FunctionRef, Exception),
    serialize(ExceptionType, {Name, Exception}).

-spec serialize(thrift_type(), term()) -> binary().
serialize(Type, Data) ->
    Codec0 = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Codec0, Type, Data) of
        {ok, Codec1} ->
            thrift_strict_binary_codec:close(Codec1);
        {error, Reason} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

-spec deserialize(thrift_type(), binary()) -> term().
deserialize(Type, Data) ->
    Codec0 = thrift_strict_binary_codec:new(Data),
    case thrift_strict_binary_codec:read(Codec0, Type) of
        {ok, Result, Codec1} ->
            case thrift_strict_binary_codec:close(Codec1) of
                <<>> ->
                    Result;
                Leftovers ->
                    erlang:error({thrift, {protocol, {excess_binary_data, Leftovers}}})
            end;
        {error, Reason} ->
            erlang:error({thrift, {protocol, Reason}})
    end.

-spec deserialize_function_args(thrift_fun_full_ref(), binary()) -> woody:args().
deserialize_function_args({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, params_type),
    deserialize(ArgsType, Data).

-spec deserialize_function_reply(thrift_fun_full_ref(), binary()) -> term().
deserialize_function_reply({Module, {Service, Function}}, Data) ->
    ArgsType = Module:function_info(Service, Function, reply_type),
    deserialize(ArgsType, Data).

-spec deserialize_function_exception(thrift_fun_full_ref(), binary()) -> thrift_exception().
deserialize_function_exception(FunctionRef, Data) ->
    ExceptionType = get_fun_exception_type(FunctionRef),
    {_Name, Exception} = deserialize(ExceptionType, Data),
    Exception.

%%

-spec record_to_proplist(Record :: tuple(), RecordInfo :: [atom()]) -> [{atom(), _}].
record_to_proplist(Record, RecordInfo) ->
    element(
        1,
        lists:foldl(
            fun(RecordField, {L, N}) ->
                case element(N, Record) of
                    V when V /= undefined ->
                        {[{RecordField, V} | L], N + 1};
                    undefined ->
                        {L, N + 1}
                end
            end,
            {[], 1 + 1},
            RecordInfo
        )
    ).

-spec get_fun_exception_type(thrift_fun_full_ref()) -> thrift_type().
get_fun_exception_type({Module, {Service, Function}}) ->
    DeclaredType = Module:function_info(Service, Function, exceptions),
    % В сгенерированном коде исключения объявлены как структура.
    % Для удобства работы, преобразуем тип в union.
    {struct, struct, Exceptions} = DeclaredType,
    {struct, union, Exceptions}.

-spec find_exception_name(thrift_fun_full_ref(), thrift_exception()) -> Name :: atom().
find_exception_name({Module, {Service, Function}}, Exception) ->
    case thrift_processor_codec:match_exception({Module, Service}, Function, Exception) of
        {ok, {_Type, Name}} ->
            Name;
        {error, bad_exception} ->
            erlang:error({thrift, {unknown_exception, Exception}})
    end.
