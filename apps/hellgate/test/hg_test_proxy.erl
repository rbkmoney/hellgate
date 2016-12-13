-module(hg_test_proxy).

-type ip() :: string().

-callback get_service_spec() -> hg_proto:service_spec().

-export([get_child_spec/3]).
-export([get_child_spec/4]).
-export([get_url/3]).

%%

-spec get_child_spec(module(), ip(), inet:port_number()) ->
    supervisor:child_spec().

get_child_spec(Module, Host, Port) ->
    get_child_spec(Module, Host, Port, #{}).

-spec get_child_spec(module(), ip(), inet:port_number(), #{}) ->
    supervisor:child_spec().

get_child_spec(Module, IPStr, Port, Args) ->
    {ok, IP} = inet:parse_address(IPStr),
    {Path, Service} = Module:get_service_spec(),
    woody_server:child_spec(
        {?MODULE, Module},
        #{
            ip => IP,
            port => Port,
            net_opts => [],
            event_handler => hg_woody_event_handler,
            handlers => [{Path, {Service, hg_woody_wrapper, Args#{handler => Module}}}]
        }
    ).

-spec get_url(module(), ip(), inet:port_number()) ->
    supervisor:child_spec().

get_url(Module, Host, Port) ->
    {Path, _Service} = Module:get_service_spec(),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), Path]).
