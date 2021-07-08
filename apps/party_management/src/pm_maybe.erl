-module(pm_maybe).

-export([apply/2]).
-export([apply/3]).

-export([get_defined/1]).
-export([get_defined/2]).

-type maybe(T) ::
    undefined | T.

-export_type([maybe/1]).

-spec apply(fun((T) -> U), maybe(T)) -> maybe(U).
apply(Fun, Arg) ->
    pm_maybe:apply(Fun, Arg, undefined).

-spec apply(fun((T) -> U), maybe(T), Default) -> U | Default.
apply(Fun, Arg, _Default) when Arg =/= undefined ->
    Fun(Arg);
apply(_Fun, undefined, Default) ->
    Default.

-spec get_defined([maybe(T)]) -> T | no_return().
get_defined([]) ->
    erlang:error(badarg);
get_defined([Value | _Tail]) when Value =/= undefined ->
    Value;
get_defined([undefined | Tail]) ->
    get_defined(Tail).

-spec get_defined(maybe(T), maybe(T)) -> T | no_return().
get_defined(V1, V2) ->
    get_defined([V1, V2]).
