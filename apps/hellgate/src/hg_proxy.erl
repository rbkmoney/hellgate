-module(hg_proxy).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-export([get_call_options/2]).

%%


-spec get_call_options(dmsl_domain_thrift:'Proxy'(), hg_domain:revision()) ->
    hg_woody_wrapper:client_opts().

get_call_options(#domain_Proxy{ref = ProxyRef}, Revision) ->
    ProxyDef = hg_domain:get(Revision, {proxy, ProxyRef}),
    construct_call_options(ProxyDef).

construct_call_options(#domain_ProxyDefinition{url = Url}) ->
    construct_transport_options(#{url => Url}).

construct_transport_options(Opts) ->
    construct_transport_options(Opts, genlib_app:env(hellgate, proxy_opts, #{})).

construct_transport_options(Opts, #{transport_opts := TransportOpts = #{}}) ->
    Fields = [connect_timeout, recv_timeout, pool, max_connections],
    Opts#{transport_opts => maps:with(Fields, TransportOpts)};
construct_transport_options(Opts, #{}) ->
    Opts.
