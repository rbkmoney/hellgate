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

%%

-type revision() :: pos_integer().
-type ref() :: _.
-type data() :: _.

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
    Tag = tag(Ref),
    #'VersionedObject'{object = {Tag, Obj}} = dmt_client:checkout_object({version, Revision}, tag_ref(Ref)),
    get_data(Obj).

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

insert({Ref, Data}) ->
    Commit = #'Commit'{
        ops = [
            {insert, #'InsertOp'{
                object = domain_obj(Ref, Data)
            }}
        ]
    },
    commit(head(), Commit).

-spec update({ref(), data()}) -> ok.

update({Ref, NewData}) ->
    OldData = get(head(), Ref),
    Commit = #'Commit'{
        ops = [
            {update, #'UpdateOp'{
                old_object = domain_obj(Ref, OldData),
                new_object = domain_obj(Ref, NewData)
            }}
        ]
    },
    commit(head(), Commit).


%% todo: use dmt_core's intraspection
tag(Ref) ->
    RecName = element(1, Ref),
    {struct, union, Fields} = dmsl_domain_thrift:struct_info('Reference'),
    Field = lists:keyfind({struct, struct, {dmsl_domain_thrift, struct_name(RecName)}}, 3, Fields),
    element(4, Field).

tag_ref(Ref) ->
    {tag(Ref), Ref}.

domain_obj(Ref) ->
    {struct, union, Fields} = dmsl_domain_thrift:struct_info('DomainObject'),
    {_, _, {_, _, {dmsl_domain_thrift, DomainObject}}, _, _} = lists:keyfind(tag(Ref), 4, Fields),
    DomainObject.

domain_obj(Ref, Data) ->
    {tag(Ref), {rec_name(domain_obj(Ref)), Ref, Data}}.

get_data(Object) ->
    element(3, Object).

struct_name([$d, $o, $m, $a, $i, $n, $_ | Rest]) ->
    list_to_atom(Rest);
struct_name(RecordName) ->
    struct_name(atom_to_list(RecordName)).

rec_name(StructName) ->
    list_to_atom("domain_" ++ atom_to_list(StructName)).
