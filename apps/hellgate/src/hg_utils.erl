-module(hg_utils).

-export([get_hostname_ip/1]).

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
