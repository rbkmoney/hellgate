%%% Rational number arithmetic subset
%%%
%%% Rounding rule: remainder strictly greater than half of the quotient.

-module(hg_rational).

%%

-type t() :: {integer(), pos_integer()}.

-export([new/1]).
-export([new/2]).
-export([to_integer/1]).

-export([mul/2]).

%%

-spec new(integer()) -> t().

new(I) ->
    {I, 1}.

-spec new(integer(), neg_integer() | pos_integer()) -> t().

new(P, Q) when Q > 0 ->
    {P, Q};
new(P, Q) when Q < 0 ->
    {-P, -Q};
new(_, 0) ->
    error(badarg).

-spec to_integer(t()) -> integer().

to_integer({P, Q}) when P > 0 ->
    P div Q + if 2 * (P rem Q) > Q -> 1; true -> 0 end;
to_integer({P, Q}) ->
    -to_integer({-P, Q}).

-spec mul(t(), t()) -> t().

mul({P1, Q1}, {P2, Q2}) ->
    {P1 * P2, Q1 * Q2}.
