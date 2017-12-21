-module(hg_party_marshalling).

-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-spec marshal(term()) -> hg_msgpack_marshalling:msgpack_value().

marshal(Atom) when is_atom(Atom) ->
    [<<":atom:">>, atom_to_binary(Atom, utf8)];
marshal(Tuple) when is_tuple(Tuple) ->
    [<<":tuple:">>, lists:map(fun marshal/1, tuple_to_list(Tuple))];
marshal(List) when is_list(List) ->
    [<<":list:">>, lists:map(fun marshal/1, List)];
marshal(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(K), marshal(V), Acc)
        end,
        #{},
        Map
    );
marshal(V) ->
    V.

-spec unmarshal(hg_msgpack_marshalling:msgpack_value()) -> term().

unmarshal([<<":atom:">>, Atom]) ->
    binary_to_existing_atom(Atom, utf8);
unmarshal([<<":tuple:">>, Tuple]) ->
    list_to_tuple(lists:map(fun unmarshal/1, Tuple));
unmarshal([<<":list:">>, List])->
    lists:map(fun unmarshal/1, List);
unmarshal(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> maps:put(unmarshal(K), unmarshal(V), Acc) end, #{}, Map);
unmarshal(V) ->
    V.
