-module(pm_ct_domain).

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([reset/1]).
-export([commit/2]).

-export([with/2]).

%%

-type revision() :: pm_domain:revision().
-type object() :: pm_domain:object().

-spec reset(revision()) -> revision() | no_return().
reset(ToRevision) ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(ToRevision),
    pm_domain:upsert(maps:values(Domain)).

-spec commit(revision(), dmt_client:commit()) -> revision() | no_return().
commit(Revision, Commit) ->
    dmt_client:commit(Revision, Commit).

-spec with(object() | [object()], fun((revision()) -> R)) -> R | no_return().
with(NewObjects, Fun) ->
    WasRevision = pm_domain:head(),
    Revision = pm_domain:upsert(WasRevision, NewObjects),
    try
        Fun(Revision)
    after
        reset(WasRevision)
    end.
