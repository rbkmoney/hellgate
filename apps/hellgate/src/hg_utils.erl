-module(hg_utils).

-export([unique_id/0]).
-export([shift_datetime/2]).
-export([logtag_process/2]).
-export([get_hostname_ip/1]).

%%

-spec unique_id() -> hg_base_thrift:'ID'().

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

%%

-type seconds() :: integer().

-type dt() :: calendar:datetime() | hg_base_thrift:'Timestamp'().

-spec shift_datetime(dt(), seconds()) -> dt().

shift_datetime(Dt, Seconds) when is_binary(Dt) ->
    format_dt(shift_datetime(parse_dt(Dt), Seconds));
shift_datetime(Dt = {_, _}, Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Dt) + Seconds).

format_dt(Dt) ->
    genlib_format:format_datetime_iso8601(Dt).
parse_dt(Dt) ->
    genlib_format:parse_datetime_iso8601(Dt).

%%

-spec logtag_process(atom(), any()) -> ok.

logtag_process(Key, Value) when is_atom(Key) ->
    % TODO preformat into binary?
    lager:md(orddict:store(Key, Value, lager:md())).

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
