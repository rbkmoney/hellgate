-module(pm_woody_client).

%% API
-export([new/1]).

-type url()            :: woody:url().
-type event_handler()  :: woody:ev_handler().
-type transport_opts() :: woody_client_thrift_http_transport:transport_options().

-type client() :: #{
    url            := url(),
    event_handler  := event_handler(),
    transport_opts => transport_opts()
}.

-type opts() :: #{
    url := url(),
    event_handler => event_handler(),
    transport_opts => transport_opts()
}.

-spec new(woody:url() | opts()) ->
    client().

new(Opts = #{url := _}) ->
    EventHandlerOpts = genlib_app:env(party_management, scoper_event_handler_options, #{}),
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
