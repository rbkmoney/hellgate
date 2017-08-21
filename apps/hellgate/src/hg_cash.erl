-module(hg_cash).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-type cash() :: dmsl_domain_thrift:'Cash'().

%% Marshalling

-include("msgpack_marshalling.hrl").

-spec marshal(cash()) ->
    term().

marshal(Cash) ->
    marshal(cash, Cash).

marshal(cash, ?cash(Amount, ?currency(SymbolicCode))) ->
    ?WRAP_VERSION_DATA(2, [Amount, {str, SymbolicCode}]).

%% Unmarshalling

-spec unmarshal(term()) -> cash().

unmarshal(Cash) ->
    unmarshal(cash, Cash).

unmarshal(Type, #{{str, "version"} := Version, {str, "data"} := Data}) ->
    unmarshal(Version, Type, Data).

unmarshal(2, cash, [Amount, {str, SymbolicCode}]) ->
    ?cash(Amount, ?currency(?BIN(SymbolicCode))).