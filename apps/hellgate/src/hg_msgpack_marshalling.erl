-module(hg_msgpack_marshalling).
-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API
-export([marshal/1]).
-export([unmarshal/1]).

-export([marshal/2]).
-export([unmarshal/2]).

-export_type([value/0]).

-type value() :: term().

%%

-spec marshal(value()) ->
    dmsl_msgpack_thrift:'Value'().
marshal(undefined) ->
    {nl, #msgpack_Nil{}};
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
marshal(Object) when is_map(Object) ->
    {obj, maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(K), marshal(V), Acc)
        end,
        #{},
        Object
    )};
marshal(Array) when is_list(Array) ->
    {arr, lists:map(fun marshal/1, Array)}.

-spec unmarshal(dmsl_msgpack_thrift:'Value'()) ->
    value().
unmarshal({nl, #msgpack_Nil{}}) ->
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

%%

-include_lib("dmsl/include/dmsl_json_thrift.hrl").

-spec marshal
    (json, dmsl_json_thrift:'Value'()) -> value().

marshal(json, {nl, _})                -> undefined;
marshal(json, {b, B})                 -> B;
marshal(json, {i, I})                 -> I;
marshal(json, {flt, F})               -> F;
marshal(json, {str, S})               -> S;
marshal(json, {obj, O})               -> maps:map(fun (_, V) -> marshal(json, V) end, O);
marshal(json, {arr, A})               -> lists:map(fun (V)   -> marshal(json, V) end, A).

-spec unmarshal
    (json, value()) -> dmsl_json_thrift:'Value'().

unmarshal(json, undefined)            -> {nl, #json_Null{}};
unmarshal(json, B) when is_boolean(B) -> {b, B};
unmarshal(json, I) when is_integer(I) -> {i, I};
unmarshal(json, F) when is_float(F)   -> {flt, F};
unmarshal(json, S) when is_binary(S)  -> {str, S};
unmarshal(json, O) when is_map(O)     -> {obj, maps:map(fun (_, V) -> unmarshal(json, V) end, O)};
unmarshal(json, A) when is_list(A)    -> {arr, lists:map(fun (V)   -> unmarshal(json, V) end, A)}.
