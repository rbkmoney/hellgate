%%% Domain config interfaces
%%%
%%% TODO
%%%  - Use proper reflection instead of blind pattern matching when (un)wrapping
%%%    domain objects

-module(hg_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

%%

-export([head/0]).
-export([get/1]).
-export([get/2]).
-export([find/2]).
-export([exists/2]).
-export([commit/2]).
-export([reset/1]).

-export([insert/1]).
-export([update/1]).
-export([upsert/1]).
-export([remove/1]).
-export([cleanup/0]).

%%

-type revision() :: dmt_client:version().
-type ref() :: dmsl_domain_thrift:'Reference'().
-type object() :: dmsl_domain_thrift:'DomainObject'().
-type data() :: _.

-export_type([revision/0]).
-export_type([ref/0]).
-export_type([object/0]).
-export_type([data/0]).

-spec head() -> revision().
head() ->
    dmt_client:get_last_version().

-spec get(ref()) -> data() | no_return().
get(Ref) ->
    get(latest, Ref).

-spec get(dmt_client:version(), ref()) -> data() | no_return().
get(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object(Revision, Ref))
    catch
        throw:#'ObjectNotFound'{} ->
            error({object_not_found, {Revision, Ref}})
    end.

-spec find(revision(), ref()) -> data() | notfound.
find(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object(Revision, Ref))
    catch
        throw:#'ObjectNotFound'{} ->
            notfound
    end.

-spec exists(revision(), ref()) -> boolean().
exists(Revision, Ref) ->
    try
        _ = dmt_client:checkout_object(Revision, Ref),
        true
    catch
        throw:#'ObjectNotFound'{} ->
            false
    end.

extract_data({_Tag, {_Name, _Ref, Data}}) ->
    Data.

-spec commit(revision(), dmt_client:commit()) -> revision() | no_return().
commit(Revision, Commit) ->
    dmt_client:commit(Revision, Commit).

-spec reset(revision()) -> revision() | no_return().
reset(ToRevision) ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(ToRevision),
    upsert(maps:values(Domain)).

%% convenience shortcuts, use carefully

-spec insert(object() | [object()]) -> revision() | no_return().
insert(ObjectOrMany) ->
    dmt_client:insert(ObjectOrMany).

-spec update(object() | [object()]) -> revision() | no_return().
update(NewObjectOrMany) ->
    dmt_client:update(NewObjectOrMany).

-spec upsert(object() | [object()]) -> revision() | no_return().
upsert(NewObjectOrMany) ->
    dmt_client:upsert(NewObjectOrMany).

-spec remove(object() | [object()]) -> revision() | no_return().
remove(ObjectOrMany) ->
    dmt_client:remove(ObjectOrMany).

-spec cleanup() -> revision() | no_return().
cleanup() ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(latest),
    remove(maps:values(Domain)).
