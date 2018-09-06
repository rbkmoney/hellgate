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
        hg_party_machine,
        hg_invoice,
        hg_invoice_template,
        hg_customer,
        hg_recurrent_paytool
    ],
    {ok, {
        #{strategy => one_for_all, intensity => 6, period => 30},
        [
            hg_machine:get_child_spec(MachineHandlers),
            get_api_child_spec(MachineHandlers)
        ]
    }}.

get_api_child_spec(MachineHandlers) ->
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
                construct_service_handler(party_management             , hg_party_woody_handler),
                construct_service_handler(invoicing                    , hg_invoice            ),
                construct_service_handler(invoice_templating           , hg_invoice_template   ),
                construct_service_handler(customer_management          , hg_customer           ),
                construct_service_handler(recurrent_paytool            , hg_recurrent_paytool  ),
                construct_service_handler(recurrent_paytool_eventsink  , hg_recurrent_paytool  ),
                construct_service_handler(proxy_host_provider          , hg_proxy_host_provider),
                construct_service_handler(payment_processing_eventsink , hg_event_sink_handler )
            ],
            additional_routes => [erl_health_handle:get_route(HealthCheckers)]
        }
    ).

construct_service_handler(Name, Module) ->
    Timeout = genlib_app:env(hellgate, default_woody_handling_timeout, ?DEFAULT_HANDLING_TIMEOUT),
    Opts = #{
        handler => Module,
        default_handling_timeout => Timeout
    },
    construct_service_handler(Name, hg_woody_wrapper, Opts).

construct_service_handler(Name, Module, Opts) ->
    {Path, Service} = hg_proto:get_service_spec(Name),
    {Path, {Service, {Module, Opts}}}.

%% Application callbacks

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
