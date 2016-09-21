-module(hg_utils).

-export([unique_id/0]).
-export([logtag_process/2]).
-export([unwrap_result/1]).
-export([get_hostname_ip/1]).

%%

-spec unique_id() -> hg_base_thrift:'ID'().

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

%%

-spec get_hostname_ip(Hostname | IP) -> IP when
    Hostname :: string(),
    IP :: inet:ip_address().

get_hostname_ip(Host) ->
    % TODO: respect preferred address family
    case inet:getaddr(Host, inet) of
        {ok, IP} ->
            IP;
        {error, Error} ->
            exit(Error)
    end.
