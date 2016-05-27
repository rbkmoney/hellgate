-module(hg_domain).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").

%%

-export([head/0]).
-export([all/1]).
-export([get/2]).

%%

-type version() :: _.
-type ref() :: _.
-type data() :: _.

-spec head() -> version().

head() ->
    42.

-spec all(version()) -> hg_domain_thrift:'Domain'().

all(_Version) ->
    get_fixture().

-spec get(version(), ref()) -> data().

get(Version, Ref) ->
    % FIXME: the dirtiest hack you'll ever see
    Name = type_to_name(Ref),
    case maps:get({Name, Ref}, all(Version), undefined) of
        {Name, {_, Ref, Data}} ->
            Data;
        undefined ->
            undefined
    end.

type_to_name(#'CurrencyRef'{}) ->
    currency;
type_to_name(#'ProxyRef'{}) ->
    proxy.

%%

-define(
    object(ObjectName, Ref, Data),
    {type_to_name(Ref), Ref} => {type_to_name(Ref), {ObjectName, Ref, Data}}
).

get_fixture() ->
    #{
        ?object('CurrencyObject',
            #'CurrencyRef'{symbolic_code = <<"RUR">>},
            #'Currency'{name = <<"Russian rubles">>, numeric_code = 643, symbolic_code = <<"RUR">>, exponent = 2}
        )
    }.
