-module(hg_utils).

-export([unique_id/0]).
-export([logtag_process/2]).
-export([unwrap_result/1]).
-export([construct_complex_id/1]).

-export([select_defined/2]).
-export([select_defined/1]).

%%

-spec unique_id() -> dmsl_base_thrift:'ID'().

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

%%

-spec logtag_process(atom(), any()) -> ok.

logtag_process(Key, Value) when is_atom(Key) ->
    % TODO preformat into binary?
    lager:md(orddict:store(Key, Value, lager:md())).

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
