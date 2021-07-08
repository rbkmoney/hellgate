%%% Domain config interfaces
%%%
%%% TODO
%%%  - Use proper reflection instead of blind pattern matching when (un)wrapping
%%%    domain objects

-module(pm_domain).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

%%

-export([head/0]).
-export([all/1]).
-export([get/2]).
-export([find/2]).
-export([exists/2]).

-export([insert/1]).
-export([update/1]).
-export([cleanup/0]).

%%

-type revision() :: pos_integer().
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

-spec all(revision()) -> dmsl_domain_thrift:'Domain'().
all(Revision) ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(Revision),
    Domain.

-spec get(revision(), ref()) -> data() | no_return().
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

-spec commit(revision(), dmt_client:commit()) -> ok | no_return().
commit(Revision, Commit) ->
    Revision = dmt_client:commit(Revision, Commit) - 1,
    _ = pm_domain:all(Revision + 1),
    ok.

-spec insert(object() | [object()]) -> ok | no_return().
insert(Object) when not is_list(Object) ->
    insert([Object]);
insert(Objects) ->
    Commit = #'Commit'{
        ops = [
            {insert, #'InsertOp'{
                object = Object
            }}
            || Object <- Objects
        ]
    },
    commit(head(), Commit).

-spec update(object() | [object()]) -> ok | no_return().
update(NewObject) when not is_list(NewObject) ->
    update([NewObject]);
update(NewObjects) ->
    Revision = head(),
    Commit = #'Commit'{
        ops = [
            {update, #'UpdateOp'{
                old_object = {Tag, {ObjectName, Ref, OldData}},
                new_object = NewObject
            }}
            || NewObject = {Tag, {ObjectName, Ref, _Data}} <- NewObjects,
               OldData <- [get(Revision, {Tag, Ref})]
        ]
    },
    commit(Revision, Commit).

-spec remove([object()]) -> ok | no_return().
remove(Objects) ->
    Commit = #'Commit'{
        ops = [
            {remove, #'RemoveOp'{
                object = Object
            }}
            || Object <- Objects
        ]
    },
    commit(head(), Commit).

-spec cleanup() -> ok | no_return().
cleanup() ->
    Domain = all(head()),
    remove(maps:values(Domain)).
