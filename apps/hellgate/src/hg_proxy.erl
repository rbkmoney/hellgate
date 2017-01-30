-module(hg_proxy).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-export([get_call_options/2]).

%%


-spec get_call_options(dmsl_domain_thrift:'Proxy'(), hg_domain:revision()) ->
    woody_client:options().

get_call_options(#domain_Proxy{ref = ProxyRef}, Revision) ->
    ProxyDef = hg_domain:get(Revision, {proxy, ProxyRef}),
    construct_call_options(ProxyDef).

construct_call_options(#domain_ProxyDefinition{url = Url, transport_options = TransOpts}) ->
    maps:merge(#{url => Url}, construct_transport_options(TransOpts)).

construct_transport_options(undefined) ->
    #{};
construct_transport_options(#domain_TransportOptions{
    connect_timeout = ConnectTimeout,
    receive_timeout = ReceiveTimeout
}) ->
    #{
        connect_timeout => ConnectTimeout,
        recv_timeout    => ReceiveTimeout
    }.
