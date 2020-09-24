-module(hg_timings).

-type event()         :: atom().
-type name()          :: atom().
-type reftime()       :: integer().
-type milliseconds()  :: non_neg_integer().

-opaque t() :: {
    #{event() => reftime()},
    #{name()  => milliseconds()}
}.

-export_type([t/0]).

-export([new/0]).
-export([mark/2]).
-export([mark/3]).
-export([accrue/4]).

-export([merge/2]).
-export([merge/1]).

-export([to_map/1]).

%%

-spec new() -> t().

new() ->
    {#{}, #{}}.

-spec mark(event(), reftime()) -> t().

mark(Event, RefTime) ->
    mark(Event, RefTime, new()).

-spec mark(event(), reftime(), t()) -> t().

mark(Event, RefTime, {Events, Timings}) ->
    {Events#{Event => RefTime}, Timings}.

-spec accrue(name(), event(), reftime(), t()) -> t().

accrue(Name, Event, CurrentTime, {Events, Timings}) ->
    RefTime = maps:get(Event, Events, CurrentTime),
    Timing = erlang:max(0, CurrentTime - RefTime),
    {Events, Timings#{Name => maps:get(Name, Timings, 0) + Timing}}.

-spec merge(t(), t()) -> t().

merge({Events1, Timings1}, {Events2, Timings2}) ->
    {
        merge_with(fun erlang:max/2, 0, Events1, Events2),
        merge_with(fun erlang:'+'/2, 0, Timings1, Timings2)
    }.

merge_with(Op, I, M1, M2) ->
    maps:merge(M2, maps:map(
        fun (K, V1) ->
            Op(V1, maps:get(K, M2, I))
        end,
        M1
    )).

-spec merge([t()]) -> t().

merge(Ts) ->
    lists:foldl(fun merge/2, new(), Ts).

-spec to_map(t()) -> #{name() => milliseconds()}.

to_map({_Events, Timings}) ->
    Timings.
