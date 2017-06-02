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
-export([find/2]).
-export([commit/2]).
-export([reset/1]).

-export([insert/1]).
-export([update/1]).
-export([upsert/1]).
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

-spec get(revision(), ref()) -> data() | no_return().

get(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object({version, Revision}, Ref))
    catch
        throw:object_not_found ->
            error({object_not_found, {Revision, Ref}})
    end.

-spec find(revision(), ref()) -> data() | notfound.

find(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object({version, Revision}, Ref))
    catch
        throw:object_not_found ->
            notfound
    end.

extract_data(#'VersionedObject'{object = {_Tag, {_Name, _Ref, Data}}}) ->
    Data.

-spec commit(revision(), dmt_client:commit()) -> ok | no_return().

commit(Revision, Commit) ->
    Revision = dmt_client:commit(Revision, Commit) - 1,
    _ = hg_domain:all(Revision + 1),
    ok.

-spec reset(revision()) -> ok | no_return().

reset(Revision) ->
    upsert(maps:values(all(Revision))).

%% convenience shortcuts, use carefully

-spec insert(object() | [object()]) -> ok | no_return().

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

-spec upsert(object() | [object()]) -> ok | no_return().

upsert(NewObject) when not is_list(NewObject) ->
    upsert([NewObject]);
upsert(NewObjects) ->
    Revision = head(),
    Commit = #'Commit'{
        ops = lists:foldl(
            fun (NewObject = {Tag, {ObjectName, Ref, NewData}}, Ops) ->
                case find(Revision, {Tag, Ref}) of
                    NewData ->
                        Ops;
                    notfound ->
                        [{insert, #'InsertOp'{
                            object = NewObject
                        }} | Ops];
                    OldData ->
                        [{update, #'UpdateOp'{
                            old_object = {Tag, {ObjectName, Ref, OldData}},
                            new_object = NewObject
                        }} | Ops]
                end
            end,
            [],
            NewObjects
        )
    },
    commit(Revision, Commit).

-spec remove(object() | [object()]) -> ok | no_return().

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

-spec cleanup() -> ok | no_return().

cleanup() ->
    Domain = all(head()),
    remove(maps:values(Domain)).
