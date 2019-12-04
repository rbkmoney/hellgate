-module(hg_woody_wrapper).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).
-export_type([handler_opts/0]).
-export_type([client_opts/0]).

-type handler_opts() :: #{
    handler := module(),
    party_client => party_client:client(),
    default_handling_timeout => timeout(),
    user_identity => undefined | woody_user_identity:user_identity()
}.

-type client_opts() :: #{
    url            := woody:url(),
    transport_opts => [{_, _}]
}.

-define(DEFAULT_HANDLING_TIMEOUT, 30000).  % 30 seconds

%% Callbacks

-callback(handle_function(woody:func(), woody:args(), handler_opts()) ->
    term() | no_return()).

%% API

-export([call/3]).
-export([call/4]).
-export([call/5]).
-export([raise/1]).

-export([get_service_options/1]).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), handler_opts()) ->
    {ok, term()} | no_return().

handle_function(Func, Args, WoodyContext0, #{handler := Handler} = Opts) ->
    WoodyContext = ensure_woody_deadline_set(WoodyContext0, Opts),
    ok = hg_context:save(create_context(WoodyContext, Opts)),
    try
        Result = Handler:handle_function(
            Func,
            Args,
            Opts
        ),
        {ok, Result}
    catch
        throw:Reason ->
            raise(Reason)
    after
        hg_context:cleanup()
    end.

-spec call(atom(), woody:func(), list()) ->
    term().

call(ServiceName, Function, Args) ->
    Opts = get_service_options(ServiceName),
    Deadline = undefined,
    call(ServiceName, Function, Args, Opts, Deadline).

-spec call(atom(), woody:func(), list(), client_opts()) ->
    term().

call(ServiceName, Function, Args, Opts) ->
    Deadline = undefined,
    call(ServiceName, Function, Args, Opts, Deadline).

-spec call(atom(), woody:func(), list(), client_opts(), woody_deadline:deadline()) ->
    term().

call(ServiceName, Function, Args, Opts, Deadline) ->
    Service = get_service_modname(ServiceName),
    Context = hg_context:get_woody_context(hg_context:load()),
    Request = {Service, Function, Args},
    woody_client:call(
        Request,
        Opts#{event_handler => {scoper_woody_event_handler, genlib_app:env(hellgate, event_handler_opts, #{})}},
        attach_deadline(Deadline, Context)
    ).

-spec get_service_options(atom()) ->
    client_opts().
get_service_options(ServiceName) ->
    construct_opts(maps:get(ServiceName, genlib_app:env(hellgate, services))).

-spec attach_deadline(woody_deadline:deadline(), woody_context:ctx()) -> woody_context:ctx().

attach_deadline(undefined, Context) ->
    Context;
attach_deadline(Deadline, Context) ->
    woody_context:set_deadline(Deadline, Context).

-spec raise(term()) ->
    no_return().

raise(Exception) ->
    woody_error:raise(business, Exception).

%% Internal functions

construct_opts(Opts = #{url := Url}) ->
    Opts#{url := genlib:to_binary(Url)};
construct_opts(Url) ->
    #{url => genlib:to_binary(Url)}.

-spec get_service_modname(atom()) ->
    {module(), atom()}.

get_service_modname(ServiceName) ->
    hg_proto:get_service(ServiceName).

create_context(WoodyContext, Opts) ->
    ContextOptions = #{
        woody_context => WoodyContext
    },
    Context = hg_context:create(ContextOptions),
    configure_party_client(Context, Opts).

configure_party_client(Context0, #{party_client := PartyClient}) ->
    DefaultUserInfo = #{id => <<"hellgate">>, realm => <<"service">>},
    Context1 = set_default_party_user_identity(DefaultUserInfo, Context0),
    hg_context:set_party_client(PartyClient, Context1);
configure_party_client(Context, _Opts) ->
    Context.

set_default_party_user_identity(UserInfo, Context) ->
    PartyClientContext0 = hg_context:get_party_client_context(Context),
    PartyClientContext1 = case party_client_context:get_user_info(PartyClientContext0) of
        undefined ->
            party_client_context:set_user_info(UserInfo, PartyClientContext0);
        _UserInfo ->
            PartyClientContext0
    end,
    hg_context:set_party_client_context(PartyClientContext1, Context).

-spec ensure_woody_deadline_set(woody_context:ctx(), handler_opts()) ->
    woody_context:ctx().

ensure_woody_deadline_set(WoodyContext, Opts) ->
    case woody_context:get_deadline(WoodyContext) of
        undefined ->
            DefaultTimeout = maps:get(default_handling_timeout, Opts, ?DEFAULT_HANDLING_TIMEOUT),
            Deadline = woody_deadline:from_timeout(DefaultTimeout),
            woody_context:set_deadline(Deadline, WoodyContext);
        _Other ->
            WoodyContext
    end.
