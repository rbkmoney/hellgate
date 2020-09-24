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

-define(DEFAULT_HANDLING_TIMEOUT, 30000).  % 30 seconds

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
        hg_invoice,
        hg_invoice_template,
        hg_customer,
        hg_recurrent_paytool
    ],
    PMMachineHandlers = [
        pm_party_machine
    ],
    PartyClient = party_client:create_client(),
    DefaultTimeout = genlib_app:env(hellgate, default_woody_handling_timeout, ?DEFAULT_HANDLING_TIMEOUT),
    Opts = #{
        party_client => PartyClient,
        default_handling_timeout => DefaultTimeout
    },
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [
            party_client:child_spec(party_client, PartyClient),
            hg_machine:get_child_spec(MachineHandlers),
            pm_machine:get_child_spec(PMMachineHandlers),
            get_api_child_spec(MachineHandlers, PMMachineHandlers, Opts)
        ]
    }}.

get_api_child_spec(MachineHandlers, PMMachineHandlers, Opts) ->
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    HealthRoutes = construct_health_routes(genlib_app:env(?MODULE, health_check, #{})),
    EventHandlerOpts = genlib_app:env(?MODULE, scoper_event_handler_options, #{}),
    woody_server:child_spec(
        ?MODULE,
        #{
            ip            => Ip,
            port          => genlib_app:env(?MODULE, port, 8022),
            transport_opts => genlib_app:env(?MODULE, transport_opts, #{}),
            protocol_opts => genlib_app:env(?MODULE, protocol_opts, #{}),
            event_handler => {scoper_woody_event_handler, EventHandlerOpts},
            handlers      => hg_machine:get_service_handlers(MachineHandlers, Opts) ++
                             pm_machine:get_service_handlers(PMMachineHandlers, Opts) ++ [
                construct_service_handler_pm(claim_committer           , pm_claim_committer_handler, Opts),
                construct_service_handler_pm(party_management          , pm_party_handler          , Opts),
                construct_service_handler(invoicing                    , hg_invoice                , Opts),
                construct_service_handler(invoice_templating           , hg_invoice_template       , Opts),
                construct_service_handler(customer_management          , hg_customer               , Opts),
                construct_service_handler(recurrent_paytool            , hg_recurrent_paytool      , Opts),
                construct_service_handler(recurrent_paytool_eventsink  , hg_recurrent_paytool      , Opts),
                construct_service_handler(proxy_host_provider          , hg_proxy_host_provider    , Opts),
                construct_service_handler(payment_processing_eventsink , hg_event_sink_handler     , Opts)
            ],
            additional_routes => HealthRoutes,
            shutdown_timeout => genlib_app:env(?MODULE, shutdown_timeout, 0)
        }
    ).

construct_health_routes(Check) ->
    [erl_health_handle:get_route(enable_health_logging(Check))].

enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun (_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

construct_service_handler(Name, Module, Opts) ->
    FullOpts = maps:merge(#{handler => Module}, Opts),
    {Path, Service} = hg_proto:get_service_spec(Name),
    {Path, {Service, {hg_woody_wrapper, FullOpts}}}.

construct_service_handler_pm(Name, Module, Opts) ->
    FullOpts = maps:merge(#{handler => Module}, Opts),
    {Path, Service} = pm_proto:get_service_spec(Name),
    {Path, {Service, {pm_woody_wrapper, FullOpts}}}.

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
