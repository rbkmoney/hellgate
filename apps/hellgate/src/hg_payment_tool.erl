%%% Payment tools

-module(hg_payment_tool).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([has_any_payment_method/2]).
-export([unmarshal/1]).

%%

-type t() :: dmsl_domain_thrift:'PaymentTool'().
-type method() :: dmsl_domain_thrift:'PaymentMethodRef'().

-spec has_any_payment_method(t(), ordsets:ordset(method())) -> boolean().
has_any_payment_method(PaymentTool, SupportedMethods) ->
    not ordsets:is_disjoint(get_possible_methods(PaymentTool), SupportedMethods).

-spec get_possible_methods(t()) -> ordsets:ordset(method()).
get_possible_methods(
    {bank_card, #domain_BankCard{payment_system_deprecated = PS, is_cvv_empty = true} = BankCard}
) ->
    ordsets:add_element(
        #domain_PaymentMethodRef{id = {empty_cvv_bank_card_deprecated, PS}},
        get_possible_bank_card_methods(BankCard)
    );
get_possible_methods(
    {bank_card, #domain_BankCard{payment_system_deprecated = PS, token_provider_deprecated = undefined} = BankCard}
) ->
    ordsets:add_element(
        #domain_PaymentMethodRef{id = {bank_card_deprecated, PS}},
        get_possible_bank_card_methods(BankCard)
    );
get_possible_methods(
    {bank_card,
        #domain_BankCard{
            payment_system_deprecated = PaymentSystem,
            token_provider_deprecated = TokenProvider,
            tokenization_method = none
        } = BankCard}
) when PaymentSystem /= undefined andalso TokenProvider /= undefined ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {bank_card_deprecated, PaymentSystem}},
        create_payment_method_ref(BankCard)
    ]);
get_possible_methods(
    {bank_card,
        #domain_BankCard{
            payment_system_deprecated = PaymentSystem,
            token_provider_deprecated = TokenProvider
        } = BankCard}
) when PaymentSystem /= undefined andalso TokenProvider /= undefined ->
    ordsets:from_list([
        #domain_PaymentMethodRef{
            id =
                {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
                    payment_system_deprecated = PaymentSystem,
                    token_provider_deprecated = TokenProvider,
                    tokenization_method = undefined
                }}
        },
        get_possible_bank_card_methods(BankCard)
    );
%% ===== payment_terminal
get_possible_methods({payment_terminal, PaymentTerminal}) ->
    filtermap_payment_methods_to_set([
        {payment_terminal_deprecated, PaymentTerminal#domain_PaymentTerminal.terminal_type_deprecated},
        {payment_terminal, PaymentTerminal#domain_PaymentTerminal.payment_service}
    ]);
%% ===== digital_wallet
get_possible_methods({digital_wallet, DigitalWallet}) ->
    filtermap_payment_methods_to_set([
        {digital_wallet_deprecated, DigitalWallet#domain_DigitalWallet.provider_deprecated},
        {digital_wallet, DigitalWallet#domain_DigitalWallet.payment_service}
    ]);
%% ===== crypto_currency
get_possible_methods({crypto_currency_deprecated, CC}) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {crypto_currency_deprecated, CC}}
    ]);
get_possible_methods({crypto_currency, CC}) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {crypto_currency, CC}}
    ]);
%% ===== mobile_commerce
get_possible_methods({mobile_commerce, MobileCommerce}) ->
    filtermap_payment_methods_to_set([
        {mobile_deprecated, MobileCommerce#domain_MobileCommerce.operator_deprecated},
        {mobile, MobileCommerce#domain_MobileCommerce.operator}
    ]).

get_possible_bank_card_methods(BankCard) ->
    filtermap_payment_methods_to_set([
        {bank_card, maybe_legacy_bank_card(BankCard)},
        {bank_card, maybe_bank_card(BankCard)}
    ]).

filtermap_payment_methods_to_set(ItemList) ->
    ordsets:from_list(
        lists:filtermap(
            fun
                ({_K, undefined}) -> false;
                (V) -> {true, #domain_PaymentMethodRef{id = V}}
            end,
            ItemList
        )
    ).

maybe_legacy_bank_card(#domain_BankCard{payment_system_deprecated = PS} = BC) when
    PS /= undefined
->
    #domain_BankCardPaymentMethod{
        payment_system_deprecated = BC#domain_BankCard.payment_system_deprecated,
        is_cvv_empty = genlib:define(BC#domain_BankCard.is_cvv_empty, false),
        token_provider_deprecated = BC#domain_BankCard.token_provider_deprecated,
        tokenization_method = BC#domain_BankCard.tokenization_method
    };
maybe_legacy_bank_card(_) ->
    undefined.

maybe_bank_card(#domain_BankCard{payment_system = PS} = BC) when
    PS /= undefined
->
    #domain_BankCardPaymentMethod{
        payment_system = PS,
        is_cvv_empty = genlib:define(BC#domain_BankCard.is_cvv_empty, false),
        payment_token = BC#domain_BankCard.payment_token,
        tokenization_method = BC#domain_BankCard.tokenization_method
    };
maybe_bank_card(_) ->
    undefined.

%% Unmarshalling

-include("legacy_structures.hrl").

-spec unmarshal(hg_msgpack_marshalling:value()) -> t().
unmarshal(PaymentTool) ->
    unmarshal(payment_tool, PaymentTool).

unmarshal(payment_tool, [3, PMV, V]) ->
    PaymentMethod = payment_method(PMV),
    {PaymentMethod, unmarshal(PaymentMethod, V)};
unmarshal(payment_tool, [2, #{<<"token">> := _} = BankCard]) ->
    {bank_card, unmarshal(bank_card, BankCard)};
unmarshal(payment_tool, [1, ?legacy_bank_card(Token, PaymentSystem, Bin, MaskedPan)]) ->
    BCard = #domain_BankCard{
        token = unmarshal(str, Token),
        bin = unmarshal(str, Bin),
        last_digits = unmarshal(str, MaskedPan)
    },
    {bank_card, set_payment_system(BCard, PaymentSystem)};
unmarshal(payment_tool, [2, TerminalType]) when is_binary(TerminalType) ->
    {payment_terminal, unmarshal(payment_terminal, TerminalType)};
unmarshal(
    bank_card = T,
    #{
        <<"token">> := Token,
        <<"payment_system">> := PaymentSystem,
        <<"bin">> := Bin,
        <<"masked_pan">> := MaskedPan
    } = V
) ->
    TokenProvider = genlib_map:get(<<"token_provider">>, V),
    IssuerCountry = genlib_map:get(<<"issuer_country">>, V),
    BankName = genlib_map:get(<<"bank_name">>, V),
    MD = genlib_map:get(<<"metadata">>, V),
    IsCVVEmpty = genlib_map:get(<<"is_cvv_empty">>, V),
    BCard = #domain_BankCard{
        token = unmarshal(str, Token),
        bin = unmarshal(str, Bin),
        last_digits = unmarshal(str, MaskedPan),
        issuer_country = unmarshal({T, issuer_country}, IssuerCountry),
        bank_name = unmarshal(str, BankName),
        metadata = unmarshal({T, metadata}, MD),
        is_cvv_empty = unmarshal({T, boolean}, IsCVVEmpty)
    },
    set_token_provider(
        set_payment_system(BCard, PaymentSystem),
        TokenProvider
    );
unmarshal(payment_terminal, TerminalType) ->
    case TerminalType of
        <<"euroset">> -> #domain_PaymentTerminal{terminal_type_deprecated = euroset};
        <<"wechat">> -> #domain_PaymentTerminal{terminal_type_deprecated = wechat};
        <<"alipay">> -> #domain_PaymentTerminal{terminal_type_deprecated = alipay};
        <<"zotapay">> -> #domain_PaymentTerminal{terminal_type_deprecated = zotapay};
        <<"qps">> -> #domain_PaymentTerminal{terminal_type_deprecated = qps};
        <<"uzcard">> -> #domain_PaymentTerminal{terminal_type_deprecated = uzcard};
        PSrvRef -> #domain_PaymentTerminal{payment_service = #domain_PaymentServiceRef{id = PSrvRef}}
    end;
unmarshal(digital_wallet, #{
    <<"provider">> := Provider,
    <<"id">> := ID
}) when is_binary(Provider) ->
    {Field, Value} =
        case Provider of
            <<"qiwi">> -> {#domain_DigitalWallet.provider_deprecated, qiwi};
            PSrvRef -> {#domain_DigitalWallet.payment_service, #domain_PaymentServiceRef{id = PSrvRef}}
        end,
    setelement(Field, #domain_DigitalWallet{id = unmarshal(str, ID)}, Value);
unmarshal(crypto_currency, CC) when is_binary(CC) ->
    case CC of
        <<"bitcoin">> -> {crypto_currency_deprecated, bitcoin};
        <<"litecoin">> -> {crypto_currency_deprecated, litecoin};
        <<"bitcoin_cash">> -> {crypto_currency_deprecated, bitcoin_cash};
        <<"ripple">> -> {crypto_currency_deprecated, ripple};
        <<"ethereum">> -> {crypto_currency_deprecated, ethereum};
        <<"zcash">> -> {crypto_currency_deprecated, zcash};
        CryptoCurRef -> {crypto_currency, #domain_CryptoCurrencyRef{id = CryptoCurRef}}
    end;
unmarshal(mobile_commerce, #{
    <<"operator">> := Operator,
    <<"phone">> := #{cc := CC, ctn := Ctn}
}) ->
    PTool = #domain_MobileCommerce{
        phone = #domain_MobilePhone{
            cc = unmarshal(str, CC),
            ctn = unmarshal(str, Ctn)
        }
    },
    {Field, Value} =
        case Operator of
            <<"mts">> -> {#domain_MobileCommerce.operator_deprecated, mts};
            <<"megafone">> -> {#domain_MobileCommerce.operator_deprecated, megafone};
            <<"yota">> -> {#domain_MobileCommerce.operator_deprecated, yota};
            <<"tele2">> -> {#domain_MobileCommerce.operator_deprecated, tele2};
            <<"beeline">> -> {#domain_MobileCommerce.operator_deprecated, beeline};
            BinRef -> {#domain_MobileCommerce.operator, #domain_MobileOperatorRef{id = BinRef}}
        end,
    setelement(Field, PTool, Value);
unmarshal({bank_card, issuer_country}, Residence) when is_binary(Residence) ->
    binary_to_existing_atom(unmarshal(str, Residence), utf8);
unmarshal({bank_card, metadata}, MD) when is_map(MD) ->
    maps:map(fun(_, V) -> hg_msgpack_marshalling:marshal(V) end, MD);
unmarshal({bank_card, boolean}, <<"true">>) ->
    true;
unmarshal({bank_card, boolean}, <<"false">>) ->
    false;
unmarshal(_, Other) ->
    Other.

payment_method(<<"card">>) ->
    bank_card;
payment_method(<<"payterm">>) ->
    payment_terminal;
payment_method(<<"wallet">>) ->
    digital_wallet;
payment_method(<<"crypto_currency">>) ->
    crypto_currency;
payment_method(<<"mobile_commerce">>) ->
    mobile_commerce.

set_payment_system(BCard, PSysBin) when is_binary(PSysBin) ->
    {Field, Value} =
        case PSysBin of
            <<"visa">> -> {#domain_BankCard.payment_system_deprecated, visa};
            <<"mastercard">> -> {#domain_BankCard.payment_system_deprecated, mastercard};
            <<"visaelectron">> -> {#domain_BankCard.payment_system_deprecated, visaelectron};
            <<"maestro">> -> {#domain_BankCard.payment_system_deprecated, maestro};
            <<"forbrugsforeningen">> -> {#domain_BankCard.payment_system_deprecated, forbrugsforeningen};
            <<"dankort">> -> {#domain_BankCard.payment_system_deprecated, dankort};
            <<"amex">> -> {#domain_BankCard.payment_system_deprecated, amex};
            <<"dinersclub">> -> {#domain_BankCard.payment_system_deprecated, dinersclub};
            <<"discover">> -> {#domain_BankCard.payment_system_deprecated, discover};
            <<"unionpay">> -> {#domain_BankCard.payment_system_deprecated, unionpay};
            <<"jcb">> -> {#domain_BankCard.payment_system_deprecated, jcb};
            <<"nspkmir">> -> {#domain_BankCard.payment_system_deprecated, nspkmir};
            PSysRef -> {#domain_BankCard.payment_system, #domain_PaymentSystemRef{id = PSysRef}}
        end,
    setelement(Field, BCard, Value).

set_token_provider(BCard, TokenProvider) when is_binary(TokenProvider) ->
    {Field, Value} =
        case TokenProvider of
            <<"applepay">> -> {#domain_BankCard.token_provider_deprecated, applepay};
            <<"googlepay">> -> {#domain_BankCard.token_provider_deprecated, googlepay};
            <<"samsungpay">> -> {#domain_BankCard.token_provider_deprecated, samsungpay};
            TSrvRef -> {#domain_BankCard.payment_token, #domain_BankCardTokenServiceRef{id = TSrvRef}}
        end,
    setelement(Field, BCard, Value);
set_token_provider(BCard, _) ->
    BCard.
