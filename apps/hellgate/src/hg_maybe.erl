-module(hg_maybe).

-export([apply/2]).
-export([apply/3]).


-spec apply(fun(), Arg :: undefined | term()) ->
    term().
apply(Fun, Arg) ->
    hg_maybe:apply(Fun, Arg, undefined).

-spec apply(fun(), Arg :: undefined | term(), Default :: term()) ->
    term().
apply(Fun, Arg, _Default) when Arg =/= undefined ->
    Fun(Arg);
apply(_Fun, undefined, Default) ->
    Default.
