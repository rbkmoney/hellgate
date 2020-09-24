-module(mg_msgpack_marshalling).

-include_lib("mg_proto/include/mg_proto_msgpack_thrift.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-export_type([value/0]).
-export_type([msgpack_value/0]).

-type value() :: term().

-type msgpack_value() ::
    undefined |
    boolean() |
    list() |
    map() |
    binary() |
    {bin, binary()} |
    integer() |
    float().

-spec marshal(msgpack_value()) -> mg_proto_msgpack_thrift:'Value'().
marshal(undefined) ->
    {nl, #mg_msgpack_Nil{}};
marshal(Boolean) when is_boolean(Boolean) ->
    {b, Boolean};
marshal(Integer) when is_integer(Integer) ->
    {i, Integer};
marshal(Float) when is_float(Float) ->
    {flt, Float};
marshal(String) when is_binary(String) ->
    {str, String};
marshal({bin, Binary}) ->
    {bin, Binary};
marshal(Array) when is_list(Array) ->
    {arr, lists:map(fun marshal/1, Array)};
marshal(Object) when is_map(Object) ->
    {obj,
        maps:fold(
            fun(K, V, Acc) ->
                maps:put(marshal(K), marshal(V), Acc)
            end,
            #{},
            Object
        )}.

-spec unmarshal(mg_proto_msgpack_thrift:'Value'()) -> msgpack_value().
unmarshal({nl, #mg_msgpack_Nil{}}) ->
    undefined;
unmarshal({b, Boolean}) ->
    Boolean;
unmarshal({i, Integer}) ->
    Integer;
unmarshal({flt, Float}) ->
    Float;
unmarshal({str, String}) ->
    String;
unmarshal({bin, Binary}) ->
    {bin, Binary};
unmarshal({obj, Object}) ->
    maps:fold(fun(K, V, Acc) -> maps:put(unmarshal(K), unmarshal(V), Acc) end, #{}, Object);
unmarshal({arr, Array}) ->
    lists:map(fun unmarshal/1, Array).
