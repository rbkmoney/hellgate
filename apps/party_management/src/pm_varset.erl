-module(pm_varset).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([encode_varset/1]).
-export([decode_varset/2]).
-export([decode_varset/1]).

-export_type([varset/0]).

-type varset() :: #{
    category => dmsl_domain_thrift:'CategoryRef'(),
    currency => dmsl_domain_thrift:'CurrencyRef'(),
    cost => dmsl_domain_thrift:'Cash'(),
    payment_tool => dmsl_domain_thrift:'PaymentTool'(),
    party_id => dmsl_domain_thrift:'PartyID'(),
    shop_id => dmsl_domain_thrift:'ShopID'(),
    payout_method => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    p2p_tool => dmsl_domain_thrift:'P2PTool'()
}.

-type encoded_varset() :: dmsl_payment_processing_thrift:'Varset'().

-spec encode_varset(varset()) -> encoded_varset().
encode_varset(Varset) ->
    #payproc_Varset{
        category = genlib_map:get(category, Varset),
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        p2p_tool = genlib_map:get(p2p_tool, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        identification_level = genlib_map:get(identification_level, Varset),
        party_id = genlib_map:get(party_id, Varset),
        shop_id = genlib_map:get(shop_id, Varset)
    }.

-spec decode_varset(encoded_varset(), varset()) -> varset().
decode_varset(Varset, VS) ->
    VS#{
        category => Varset#payproc_Varset.category,
        currency => Varset#payproc_Varset.currency,
        cost => Varset#payproc_Varset.amount,
        payment_tool => prepare_payment_tool_var(
            Varset#payproc_Varset.payment_method,
            Varset#payproc_Varset.payment_tool
        ),
        payout_method => Varset#payproc_Varset.payout_method,
        wallet_id => Varset#payproc_Varset.wallet_id,
        p2p_tool => Varset#payproc_Varset.p2p_tool,
        identification_level => Varset#payproc_Varset.identification_level,
        shop_id => Varset#payproc_Varset.shop_id,
        party_id => Varset#payproc_Varset.party_id
    }.

-spec decode_varset(encoded_varset()) -> varset().
decode_varset(Varset) ->
    genlib_map:compact(#{
        category => Varset#payproc_Varset.category,
        currency => Varset#payproc_Varset.currency,
        cost => Varset#payproc_Varset.amount,
        payment_tool => prepare_payment_tool_var(
            Varset#payproc_Varset.payment_method,
            Varset#payproc_Varset.payment_tool
        ),
        payout_method => Varset#payproc_Varset.payout_method,
        wallet_id => Varset#payproc_Varset.wallet_id,
        p2p_tool => Varset#payproc_Varset.p2p_tool,
        identification_level => Varset#payproc_Varset.identification_level,
        shop_id => Varset#payproc_Varset.shop_id,
        party_id => Varset#payproc_Varset.party_id
    }).

prepare_payment_tool_var(_PaymentMethodRef, PaymentTool) when PaymentTool /= undefined ->
    PaymentTool;
prepare_payment_tool_var(PaymentMethodRef = #domain_PaymentMethodRef{}, _PaymentTool) ->
    pm_payment_tool:create_from_method(PaymentMethodRef);
prepare_payment_tool_var(undefined, undefined) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec encode_decode_test() -> _.

encode_decode_test() ->
    Varset = #{
        category => #domain_CategoryRef{id = 1},
        currency => #domain_CurrencyRef{symbolic_code = <<"RUB">>},
        cost => #domain_Cash{
            amount = 20,
            currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>}
        },
        payment_tool =>
            {digital_wallet, #domain_DigitalWallet{
                provider_deprecated = qiwi,
                id = <<"digital_wallet_id">>
            }},
        payout_method => #domain_PayoutMethodRef{id = any},
        wallet_id => <<"wallet_id">>,
        p2p_tool => #domain_P2PTool{
            sender =
                {digital_wallet, #domain_DigitalWallet{
                    provider_deprecated = qiwi,
                    id = <<"digital_wallet_id">>
                }},
            receiver =
                {digital_wallet, #domain_DigitalWallet{
                    provider_deprecated = qiwi,
                    id = <<"digital_wallet_id">>
                }}
        },
        identification_level => full,
        shop_id => <<"shop_id">>,
        party_id => <<"party_id">>
    },
    Varset = decode_varset(encode_varset(Varset)).

-endif.
