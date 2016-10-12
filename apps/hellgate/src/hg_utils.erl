-module(hg_utils).

-export([unique_id/0]).
-export([logtag_process/2]).
-export([unwrap_result/1]).

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

-spec unwrap_result
    ({ok, T}) -> T;
    ({error, _}) -> no_return().

unwrap_result({ok, V}) ->
    V;
unwrap_result({error, E}) ->
    error(E).
