-module(hg_log_scope).

%% API
-export([new_scope/0]).
-export([set_values/1]).
-export([remove_value/1]).
-export([cleanup/0]).
-export([merge/0]).


-define(TAG, log_scopes).

-spec new_scope() -> _.
new_scope() ->
    try
        Scopes = gproc:get_value({p, l, ?TAG}),
        gproc:set_value({p, l, ?TAG}, [#{} | Scopes])
    catch
        error:badarg ->
            gproc:reg({p, l, ?TAG}, [#{}])
    end.

-spec cleanup() -> _.

cleanup() ->
    case gproc:get_value({p, l, ?TAG}) of
        [_] ->
            true = gproc:set_value({p, l, ?TAG}, [#{}]);
        [_Current | Rest] ->
            true = gproc:set_value({p, l, ?TAG}, Rest)
    end,
    ok.

-spec set_values(_) -> _.

set_values(Meta) ->
    [Current | Rest] = gproc:get_value({p, l, ?TAG}),
    true = gproc:set_value({p, l, ?TAG}, [maps:merge(Current, Meta)| Rest]),
    ok.


-spec remove_value(_) -> _.

remove_value(Key) ->
    [Current0 | Rest] = gproc:get_value({p, l, ?TAG}),
    true = gproc:set_value({p, l, ?TAG}, [maps:remove(Key, Current0) | Rest]),
    ok.

merge() ->
    Scopes = try
        gproc:get_value({p, l, ?TAG})
    catch
        error:badarg ->
            [#{}]
    end,
    lists:foldl(
        fun(M, Acc) ->
            maps:merge(M, Acc)
        end,
        #{},
        Scopes
    ).

