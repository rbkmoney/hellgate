-module(hg_test_proxy).

-type host() :: inet:hostname() | inet:ip_address().

-callback get_child_spec(host(), inet:port_number()) -> supervisor:child_spec().
-callback get_url(host(), inet:port_number()) -> woody_t:url().

-export([get_child_spec/3]).
-export([get_url/3]).

%%

-spec get_child_spec(module(), host(), inet:port_number()) ->
    supervisor:child_spec().

get_child_spec(Module, Host, Port) ->
    Module:get_child_spec(Host, Port).

-spec get_url(module(), host(), inet:port_number()) ->
    supervisor:child_spec().

get_url(Module, Host, Port) ->
    Module:get_url(Host, Port).
