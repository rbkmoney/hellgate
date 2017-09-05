-module(hg_cash).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-type cash() :: dmsl_domain_thrift:'Cash'().

%% Marshalling

-spec marshal(cash()) ->
    hg_msgpack_marshalling:value().

marshal(Cash) ->
    marshal(cash, Cash).

marshal(cash, ?cash(Amount, SymbolicCode)) ->
    [2, [Amount, SymbolicCode]].

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    cash().

unmarshal(Cash) ->
    unmarshal(cash, Cash).

unmarshal(cash, [2, [Amount, SymbolicCode]]) ->
    ?cash(Amount, SymbolicCode);

unmarshal(cash, [1, {'domain_Cash', Amount, {'domain_CurrencyRef', SymbolicCode}}]) ->
    ?cash(Amount, SymbolicCode).
