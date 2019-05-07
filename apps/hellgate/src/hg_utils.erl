-module(hg_utils).

-export([unique_id/0]).
-export([uid/1]).
-export([logtag_process/2]).
-export([unwrap_result/1]).
-export([construct_complex_id/1]).

-export([select_defined/2]).
-export([select_defined/1]).

-export([format_reason/1]).

%%

-spec unique_id() -> dmsl_base_thrift:'ID'().

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

-spec uid(undefined | dmsl_base_thrift:'ID'()) -> dmsl_base_thrift:'ID'().

uid(undefined) ->
    unique_id();
uid(ID) when is_binary(ID) ->
    ID.

%%

-spec logtag_process(atom(), any()) -> ok.

logtag_process(Key, Value) when is_atom(Key) ->
    logger:update_process_metadata(#{Key => Value}).

%%

-spec construct_complex_id([binary() | {atom(), binary()}]) -> binary().

construct_complex_id(L) ->
    genlib_string:join($., lists:map(
        fun
            ({Tag, ID}) -> [atom_to_binary(Tag, utf8), <<"-">>, ID];
            (ID       ) -> ID
        end,
        L
    )).

%%

-spec select_defined(T | undefined, T | undefined) -> T | undefined.

select_defined(V1, V2) ->
    select_defined([V1, V2]).

-spec select_defined([T | undefined]) -> T | undefined.

select_defined([V | _]) when V /= undefined ->
    V;
select_defined([undefined | Vs]) ->
    select_defined(Vs);
select_defined([]) ->
    undefined.

%%

-spec unwrap_result
    ({ok, T}) -> T;
    ({error, _}) -> no_return().

unwrap_result({ok, V}) ->
    V;
unwrap_result({error, E}) ->
    error(E).

-spec format_reason(atom()) -> binary().
%% TODO: fix this dirty hack
format_reason(V) ->
    genlib:to_binary(V).