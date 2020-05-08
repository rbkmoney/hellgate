-module(pm_woody_client).

%% API
-export([new/1]).

-type opts() :: #{
    url := url(),
    event_handler => event_handler(),
    transport_opts => transport_opts()
}.

-spec new(woody:url() | opts()) ->
    client().

new(Opts = #{url := _}) ->
    EventHandlerOpts = genlib_app:env(ff_server, scoper_event_handler_options, #{}),
    maps:merge(
        #{
            event_handler => {scoper_woody_event_handler, EventHandlerOpts}
        },
        maps:with([url, event_handler, transport_opts], Opts)
    );
new(Url) when is_binary(Url); is_list(Url) ->
    new(#{
        url => genlib:to_binary(Url)
    }).
