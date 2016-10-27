-module(hg_context).

%% API
-export([set/1]).
-export([get/0]).
-export([update/1]).
-export([cleanup/0]).

-spec set(Context :: term()) -> ok | no_return().

set(Context) ->
    true = gproc:reg({p, l, context}, Context),
    ok.

-spec get() -> term() | no_return().

get() ->
    gproc:get_value({p, l, context}).

-spec update(Context :: term()) -> ok | no_return().

update(Context) ->
    true = gproc:set_value({p, l, context}, Context),
    ok.

-spec cleanup() -> ok | no_return().

cleanup() ->
    true = gproc:unreg({p, l, context}),
    ok.