-module(hg_domain).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").

%%

-export([head/0]).
-export([all/1]).
-export([get/2]).

%%

-type revision() :: pos_integer().
-type ref() :: _.
-type data() :: _.

-spec head() -> revision().

head() ->
    42.

-spec all(revision()) -> hg_domain_thrift:'Domain'().

all(_Revision) ->
    get_fixture().

-spec get(revision(), ref()) -> data().

get(Revision, Ref) ->
    % FIXME: the dirtiest hack you'll ever see
    Name = type_to_name(Ref),
    case maps:get({Name, Ref}, all(Revision), undefined) of
        {Name, {_, Ref, Data}} ->
            Data;
        undefined ->
            undefined
    end.

type_to_name(#domain_CurrencyRef{}) ->
    currency;
type_to_name(#domain_GlobalsRef{}) ->
    globals;
type_to_name(#domain_PartyPrototypeRef{}) ->
    party_prototype;
type_to_name(#domain_ProxyRef{}) ->
    proxy.

%%

-define(
    object(ObjectName, Ref, Data),
    {type_to_name(Ref), Ref} => {type_to_name(Ref), {ObjectName, Ref, Data}}
).

get_fixture() ->
    #{
        ?object('Globals',
            #domain_GlobalsRef{},
            #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{
                    id = 42
                }
            }
        ),
        ?object('PartyPrototype',
            #domain_PartyPrototypeRef{
                id = 42
            },
            #domain_PartyPrototype{
                shop = #domain_ShopPrototype{},
                default_services = #domain_ShopServices{}
            }
        ),
        ?object('CurrencyObject',
            #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            #domain_Currency{
                name = <<"Russian rubles">>,
                numeric_code = 643,
                symbolic_code = <<"RUB">>,
                exponent = 2
            }
        ),
        ?object('ProxyObject',
            #domain_ProxyRef{id = 1},
            #domain_ProxyDefinition{
                url     = genlib_app:env(hellgate, provider_proxy_url),
                options = genlib_app:env(hellgate, provider_proxy_options, #{})
            }
        )
    }.
