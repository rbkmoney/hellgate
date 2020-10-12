-module(pm_party_cache).

%% API
-export([cache_child_spec/2]).
-export([get_party/1]).
-export([update_party/2]).

-export_type([cache_options/0]).

-type cache_options() :: #{
    type => set | ordered_set,
    policy => lru | mru,
    memory => integer(),
    size => integer(),
    n => integer(),
    %% seconds
    ttl => integer(),
    check => integer(),
    stats => function() | {module(), atom()},
    heir => atom() | pid()
}.

-define(CACHE_NS, party).

-spec cache_child_spec(atom(), cache_options()) -> supervisor:child_spec().
cache_child_spec(ChildID, Options) ->
    #{
        id => ChildID,
        start => {cache, start_link, [?CACHE_NS, cache_options(Options)]},
        restart => permanent,
        type => supervisor
    }.

-spec get_party({party_id(), party_revision_param()}) -> not_found | {ok, st()}.
get_party(Key) ->
    case cache:get(?CACHE_NS, Key) of
        undefined ->
            not_found;
        Value ->
            {ok, Value}
    end.

-spec update_party({party_id(), party_revision_param()}, st()) -> ok.
update_party(Key, Value) ->
    cache:put(?CACHE_NS, Key, Value).

-spec cache_options(cache_options()) -> list().
cache_options(Options) ->
    maps:to_list(Options).
