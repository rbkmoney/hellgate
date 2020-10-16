-module(pm_party_cache).

%% API
-export([cache_child_spec/2]).
-export([get_party/2]).
-export([update_party/3]).

-export_type([cache_options/0]).

%% see `cache:start_link/1`
-type cache_options() :: #{
    type => set | ordered_set,
    policy => lru | mru,
    % bytes
    memory => integer(),
    % number of items
    size => integer(),
    % number of items
    n => integer(),
    % seconds
    ttl => integer(),
    % seconds
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

-spec get_party(party_id(), party_revision()) -> not_found | {ok, party_st()}.
get_party(PartyID, PartyRevision) ->
    case cache:get(?CACHE_NS, {PartyID, PartyRevision}) of
        undefined ->
            not_found;
        Value ->
            {ok, Value}
    end.

-spec update_party(party_id(), party_revision(), party_st()) -> ok.
update_party(PartyID, PartyRevision, Value) ->
    cache:put(?CACHE_NS, {PartyID, PartyRevision}, Value).

-spec cache_options(cache_options()) -> list().
cache_options(Options) ->
    KeyList = [type, policy, memory, size, n, ttl, check],
    Opt0 = maps:with(KeyList, Options),
    maps:to_list(Opt0).
