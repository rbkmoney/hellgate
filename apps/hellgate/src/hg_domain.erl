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
-export([get/1]).
-export([cleanup/0]).

%%

-type revision() :: pos_integer().
-type ref() :: _.
-type data() :: _.
-type object() :: ref().

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
    #'VersionedObject'{object = {_Tag, {_Name, _Ref, Data}}} = dmt_client:checkout_object({version, Revision}, Ref),
    Data.

-spec commit(revision(), dmt:commit()) -> ok.

commit(Revision, Commit) ->
    Revision = dmt_client:commit(Revision, Commit) - 1,
    _ = hg_domain:all(Revision + 1),
    ok.

%% convenience shortcuts, use carefully

-spec get(ref()) -> data().

get(Ref) ->
    get(head(), Ref).

-spec insert({ref(), data()}) -> ok.

insert(Object) ->
    Commit = #'Commit'{
        ops = [
            {insert, #'InsertOp'{
                object = Object
            }}
        ]
    },
    commit(head(), Commit).

-spec update({ref(), data()}) -> ok.

update({Tag, {ObjectName, Ref, _Data}} = NewObject) ->
    OldData = get(head(), {Tag, Ref}),
    Commit = #'Commit'{
        ops = [
            {update, #'UpdateOp'{
                old_object = {Tag, {ObjectName, Ref, OldData}},
                new_object = NewObject
            }}
        ]
    },
    commit(head(), Commit).

-spec remove([object()]) -> ok.

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