-module(pm_party_cache).

%% API
-export([cache_child_spec/2]).
-export([get_party/1]).
-export([update_party/2]).

-export_type([cache_options/0]).

%% see `cache:start_link/1`
-type cache_options() :: #{
    type => set | ordered_set,
    policy => lru | mru,
    memory => integer(),
    size => integer(),
    n => integer(),
    ttl => integer(),
    check => integer()
}.

-type party_revision() :: pm_party:party_revision().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type party_st() :: pm_party_machine:st().

-define(CACHE_NS, party).

-spec cache_child_spec(atom(), cache_options()) -> supervisor:child_spec().
cache_child_spec(ChildID, Options) ->
    #{
        id => ChildID,
        start => {cache, start_link, [?CACHE_NS, cache_options(Options)]},
        restart => permanent,
        type => supervisor
    }.

-spec get_party({party_id(), party_revision()}) -> not_found | {ok, party_st()}.
get_party(Key) ->
    case cache:get(?CACHE_NS, Key) of
        undefined ->
            not_found;
        Value ->
            {ok, Value}
    end.

-spec update_party({party_id(), party_revision()}, party_st()) -> ok.
update_party(Key, Value) ->
    cache:put(?CACHE_NS, Key, Value).

-spec cache_options(cache_options()) -> list().
cache_options(Options) ->
    Opt0 = genlib_map:compact(#{
        type => genlib_map:get(type, Options),
        policy => genlib_map:get(policy, Options),
        memory => genlib_map:get(memory, Options),
        size => genlib_map:get(size, Options),
        n => genlib_map:get(n, Options),
        ttl => genlib_map:get(ttl, Options),
        check => genlib_map:get(check, Options)
    }),
    maps:to_list(Opt0).
