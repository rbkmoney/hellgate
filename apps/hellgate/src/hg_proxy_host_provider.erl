-module(hg_proxy_host_provider).
-include_lib("hg_proto/include/hg_proxy_provider_thrift.hrl").

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-type tag()      :: hg_base_thrift:'Tag'().
-type callback() :: hg_proxy_provider_thrift:'Callback'().

-spec handle_function('ProcessCallback', {tag(), callback()}, woody_client:context(), _) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('ProcessCallback', {Tag, Callback}, Context, _) ->
    map_error(hg_invoice:process_callback(Tag, {provider, Callback}, Context)).

map_error({{error, Reason}, Context}) ->
    % TODO rather hacky
    throw({#'InvalidRequest'{errors = [genlib:to_binary(Reason)]}, Context});
map_error({Ok, Context}) ->
    {Ok, Context}.
