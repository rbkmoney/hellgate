-module(pm_cash).
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([add/2]).
-export([sub/2]).
-export([marshal/1]).
-export([unmarshal/1]).

-type cash() :: dmsl_domain_thrift:'Cash'().

%% Simple arithmetics

-spec add(cash(), cash()) ->
    cash().

add(?cash(Amount1, Curr), ?cash(Amount2, Curr)) ->
    ?cash(Amount1 + Amount2, Curr);

add(_, _) ->
    error(badarg).

-spec sub(cash(), cash()) ->
    cash().

sub(?cash(Amount1, Curr), ?cash(Amount2, Curr)) ->
    ?cash(Amount1 - Amount2, Curr);

sub(_, _) ->
    error(badarg).

%% Marshalling

-spec marshal(cash()) ->
    pm_msgpack_marshalling:value().

marshal(Cash) ->
    marshal(cash, Cash).

marshal(cash, ?cash(Amount, SymbolicCode)) ->
    [2, [Amount, SymbolicCode]];

marshal(cash, undefined) ->
    undefined.

%% Unmarshalling

-spec unmarshal(pm_msgpack_marshalling:value()) ->
    cash().

unmarshal(Cash) ->
    unmarshal(cash, Cash).

unmarshal(cash, [2, [Amount, SymbolicCode]]) ->
    ?cash(Amount, SymbolicCode);

unmarshal(cash, [1, {'domain_Cash', Amount, {'domain_CurrencyRef', SymbolicCode}}]) ->
    ?cash(Amount, SymbolicCode);

unmarshal(cash, undefined) ->
    undefined.