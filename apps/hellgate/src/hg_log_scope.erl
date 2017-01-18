-module(hg_log_scope).

%% API
-export([new/1]).
-export([set_meta/1]).
-export([remove_meta/1]).
-export([clean/0]).

-type meta() :: #{atom() => any()}.
-type scope_name() :: atom().

-define(TAG, log_scopes).

-spec new(scope_name()) -> ok.

new(Name) ->
    case get_scope_names() of
        {ok, Scopes} ->
            set_scope_names([Name | Scopes]);
        error ->
            init_scope_names([Name])
    end,
    lager:md(lists:keystore(Name, 1, lager:md(), {Name, #{}})).

-spec clean() -> ok.

clean() ->
    case get_scope_names() of
        {ok, []} ->
            ok;
        {ok, [Current | Rest]} ->
            lager:md(
                lists:keydelete(Current, 1, lager:md())
            ),
            set_scope_names(Rest);
        error ->
            ok
    end.


-spec set_meta(meta()) -> ok.

set_meta(Meta) ->
    {ok, [Name | _]} = get_scope_names(),
    Current = case lists:keyfind(Name, 1, lager:md()) of
        {Name, C} ->
            C;
        false ->
            #{}
    end,
    lager:md(
        lists:keystore(Name, 1, lager:md(), {Name, maps:merge(Current, Meta)})
    ).


-spec remove_meta([atom()]) -> ok.

remove_meta(Keys) ->
    {ok, [Name | _]} = get_scope_names(),
    NewMeta = case lists:keyfind(Name, 1, lager:md()) of
        {Name, C} ->
            maps:filter(
                fun(K, _V) ->
                    not lists:member(K, Keys)
                end,
                C
            );
        false ->
            #{}
    end,
    lager:md(
        lists:keystore(Name, 1, lager:md(), {Name, NewMeta})
    ).

init_scope_names(Names) ->
    gproc:reg({p, l, ?TAG}, Names).

get_scope_names() ->
    try
        Scopes = gproc:get_value({p, l, ?TAG}),
        {ok, Scopes}
    catch
        error:badarg ->
            error
    end.

set_scope_names(Names) ->
    gproc:set_value({p, l, ?TAG}, Names).
