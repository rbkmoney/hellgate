%%% Host handler for provider proxies
%%%
%%% TODO
%%%  - designate an exception when specified tag is missing

-module(hg_proxy_host_provider).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-type tag()      :: dmsl_proxy_thrift:'CallbackTag'().
-type callback() :: dmsl_proxy_thrift:'Callback'().

-spec handle_function('ProcessCallback', [Args], hg_woody_wrapper:handler_opts()) ->
    term() | no_return()
    when Args :: tag() | callback().

handle_function('ProcessCallback', [Tag, Callback], _) ->
    map_error(hg_invoice:process_callback(Tag, {provider, Callback})).

map_error({ok, Response}) ->
    Response;
map_error({error, invalid_callback}) ->
    hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Invalid callback">>]});
map_error({error, notfound}) ->
    hg_woody_wrapper:raise(#'InvalidRequest'{errors = [<<"Not found">>]});
map_error({error, Reason}) ->
    error(Reason).
