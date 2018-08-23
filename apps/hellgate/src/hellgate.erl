%%% @doc Public API, supervisor and application startup.
%%% @end

-module(hellgate).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

%%
%% API
%%
-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() ->
    ok.
stop() ->
    application:stop(?MODULE).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    MachineHandlers = [
        hg_party_machine,
        hg_invoice,
        hg_invoice_template,
        hg_customer,
        hg_recurrent_paytool
    ],
    PartyClient = party_client:create(),
    Opts = #{party_client => PartyClient},
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [
            hg_machine:get_child_spec(MachineHandlers),
            get_api_child_spec(MachineHandlers, Opts),
            party_client:child_spec(party_client, PartyClient)
        ]
    }}.

get_api_child_spec(MachineHandlers, Opts) ->
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    woody_server:child_spec(
        ?MODULE,
        #{
            ip            => Ip,
            port          => genlib_app:env(?MODULE, port, 8022),
            net_opts      => genlib_app:env(?MODULE, net_opts, []),
            event_handler => scoper_woody_event_handler,
            handlers      => hg_machine:get_service_handlers(MachineHandlers) ++ [
                construct_service_handler(party_management             , hg_party_woody_handler, Opts),
                construct_service_handler(invoicing                    , hg_invoice            , Opts),
                construct_service_handler(invoice_templating           , hg_invoice_template   , Opts),
                construct_service_handler(customer_management          , hg_customer           , Opts),
                construct_service_handler(recurrent_paytool            , hg_recurrent_paytool  , Opts),
                construct_service_handler(recurrent_paytool_eventsink  , hg_recurrent_paytool  , Opts),
                construct_service_handler(proxy_host_provider          , hg_proxy_host_provider, Opts),
                construct_service_handler(payment_processing_eventsink , hg_event_sink_handler , Opts)
            ],
            additional_routes => [erl_health_handle:get_route(HealthCheckers)]
        }
    ).

construct_service_handler(Name, Module, Opts) ->
    FullOpts = maps:merge(#{handler => Module}, Opts),
    {Path, Service} = hg_proto:get_service_spec(Name),
    {Path, {Service, {hg_woody_wrapper, FullOpts}}}.

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
