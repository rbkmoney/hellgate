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
-export([get/2]).
-export([find/2]).
-export([exists/2]).

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

-spec get(revision(), ref()) -> data() | no_return().

get(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object({version, Revision}, Ref))
    catch
        throw:#'ObjectNotFound'{} ->
            error({object_not_found, {Revision, Ref}})
    end.

-spec find(revision(), ref()) -> data() | notfound.

find(Revision, Ref) ->
    try
        extract_data(dmt_client:checkout_object({version, Revision}, Ref))
    catch
        throw:#'ObjectNotFound'{} ->
            notfound
    end.

-spec exists(revision(), ref()) -> boolean().

exists(Revision, Ref) ->
    try
        _ = dmt_client:checkout_object({version, Revision}, Ref),
        true
    catch
        throw:#'ObjectNotFound'{} ->
            false
    end.

extract_data(#'VersionedObject'{object = {_Tag, {_Name, _Ref, Data}}}) ->
    Data.

