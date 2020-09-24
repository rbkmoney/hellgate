-module(pm_context).

-export([create/0]).
-export([create/1]).
-export([save/1]).
-export([load/0]).
-export([cleanup/0]).

-export([get_woody_context/1]).
-export([set_user_identity/2]).

-opaque context() :: #{
    woody_context := woody_context(),
    user_identity => user_identity()
}.

-type options() :: #{
    user_identity => user_identity(),
    woody_context => woody_context()
}.

-export_type([context/0]).
-export_type([options/0]).

%% Internal types

-type user_identity() :: woody_user_identity:user_identity().
-type woody_context() :: woody_context:ctx().

%% TODO change when moved to separate app
-define(REGISTRY_KEY, {p, l, stored_hg_context}).

%% API

-spec create() -> context().
create() ->
    create(#{}).

-spec create(options()) -> context().
create(Options0) ->
    ensure_woody_context_exists(Options0).

-spec save(context()) -> ok.
save(Context) ->
    true =
        try
            gproc:reg(?REGISTRY_KEY, Context)
        catch
            error:badarg ->
                gproc:set_value(?REGISTRY_KEY, Context)
        end,
    ok.

-spec load() -> context() | no_return().
load() ->
    gproc:get_value(?REGISTRY_KEY).

-spec cleanup() -> ok.
cleanup() ->
    true = gproc:unreg(?REGISTRY_KEY),
    ok.

-spec get_woody_context(context()) -> woody_context().
get_woody_context(Context) ->
    #{woody_context := WoodyContext} = ensure_woody_user_info_set(Context),
    WoodyContext.

-spec set_user_identity(user_identity(), context()) -> context().
set_user_identity(Identity, Context) ->
    Context#{user_identity => Identity}.

%% Internal functions

-spec ensure_woody_context_exists(options()) -> options().
ensure_woody_context_exists(#{woody_context := _WoodyContext} = Options) ->
    Options;
ensure_woody_context_exists(Options) ->
    Options#{woody_context => woody_context:new()}.

-spec ensure_woody_user_info_set(context()) -> context().
ensure_woody_user_info_set(#{user_identity := Identity, woody_context := WoodyContext} = Context) ->
    NewWoodyContext = woody_user_identity:put(Identity, WoodyContext),
    Context#{woody_context := NewWoodyContext};
ensure_woody_user_info_set(Context) ->
    Context.
