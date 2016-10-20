%%% Domain config interfaces
%%%
%%% TODO
%%%  - Use proper reflection instead of blind pattern matching when (un)wrapping
%%%    domain objects

-module(hg_domain).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

%%

-export([head/0]).
-export([all/1]).
-export([get/2]).
-export([commit/2]).

-export([insert/1]).
-export([update/1]).
-export([remove/1]).
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
    #'Snapshot'{version = Version} = dmt_client:checkout({head, #'Head'{}}),
    Version.

-spec all(revision()) -> dmsl_domain_thrift:'Domain'().

all(Revision) ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout({version, Revision}),
    Domain.

-spec get(revision(), ref()) -> data().

get(Revision, Ref) ->
    try
        #'VersionedObject'{object = Object} = dmt_client:checkout_object({version, Revision}, Ref),
        {_Tag, {_Name, _Ref, Data}} = Object,
        Data
    catch
        throw:object_not_found ->
            error({object_not_found, {Revision, Ref}})
    end.

-spec commit(revision(), dmt:commit()) -> ok.

commit(Revision, Commit) ->
    Revision = dmt_client:commit(Revision, Commit) - 1,
    _ = hg_domain:all(Revision + 1),
    ok.

%% convenience shortcuts, use carefully

-spec insert(object() | [object()]) -> ok.

insert(Object) when not is_list(Object) ->
    insert([Object]);
insert(Objects) ->
    Commit = #'Commit'{
        ops = [
            {insert, #'InsertOp'{
                object = Object
            }} ||
                Object <- Objects
        ]
    },
    commit(head(), Commit).

-spec update(object() | [object()]) -> ok.

update(NewObject) when not is_list(NewObject) ->
    update([NewObject]);
update(NewObjects) ->
    Commit = #'Commit'{
        ops = [
            {update, #'UpdateOp'{
                old_object = {Tag, {ObjectName, Ref, OldData}},
                new_object = NewObject
            }}
                || NewObject = {Tag, {ObjectName, Ref, _Data}} <- NewObjects,
                    OldData <- [get(head(), {Tag, Ref})]
        ]
    },
    commit(head(), Commit).

-spec remove(object() | [object()]) -> ok.

remove(Object) when not is_list(Object) ->
    remove([Object]);
remove(Objects) ->
    Commit = #'Commit'{
        ops = [
            {remove, #'RemoveOp'{
                object = Object
            }} ||
                Object <- Objects
        ]
    },
    commit(head(), Commit).

-spec cleanup() -> ok.

cleanup() ->
    Domain = all(head()),
    remove(maps:values(Domain)).
