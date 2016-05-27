-module(hg_utils).

-export([shift_datetime/2]).
-export([get_hostname_ip/1]).

%%

-type seconds() :: integer().
-type datetime_iso8601() :: binary().

-type dt() :: calendar:datetime() | datetime_iso8601().

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

-include_lib("kernel/include/inet.hrl").

-spec get_hostname_ip(Hostname | IP) -> IP when
    Hostname :: string(),
    IP :: inet:ip_address().

get_hostname_ip(IP) when tuple_size(IP) == 4 ->
    IP;

get_hostname_ip(Host) ->
    case inet:gethostbyname(Host) of
        {ok, #hostent{h_addr_list = [IP | _]}} ->
            IP;
        {error, Error} ->
            exit(Error)
    end.
