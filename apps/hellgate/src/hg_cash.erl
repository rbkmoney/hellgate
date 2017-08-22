-module(hg_cash).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-type cash() :: dmsl_domain_thrift:'Cash'().

%% Marshalling

-spec marshal(cash()) ->
    term().

marshal(Cash) ->
    marshal(cash, Cash).

marshal(cash, ?cash(Amount, ?currency(SymbolicCode))) ->
    [1, [Amount, SymbolicCode]].

%% Unmarshalling

-spec unmarshal(term()) ->
    cash().

unmarshal(Cash) ->
    unmarshal(cash, Cash).

unmarshal(cash, [1, [Amount, SymbolicCode]]) ->
    ?cash(Amount, ?currency(SymbolicCode)).