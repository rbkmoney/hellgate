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

construct_call_options(#domain_ProxyDefinition{url = Url}) ->
    #{url => Url, transport_opts => construct_transport_options()}.

construct_transport_options() ->
    construct_transport_options(genlib_app:env(hellgate, proxy_opts, #{})).

construct_transport_options(#{transport_opts := TransportOpts = #{}}) ->
    maps:to_list(maps:with([connect_timeout, recv_timeout], TransportOpts));
construct_transport_options(#{}) ->
    [].
