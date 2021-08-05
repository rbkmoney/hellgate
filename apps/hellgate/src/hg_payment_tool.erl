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
    {bank_card, #domain_BankCard{payment_system_deprecated = PaymentSystem, is_cvv_empty = true} = BankCard}
) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {empty_cvv_bank_card_deprecated, PaymentSystem}},
        create_bank_card_payment_method_ref(BankCard)
    ]);
get_possible_methods(
    {bank_card,
        #domain_BankCard{payment_system_deprecated = PaymentSystem, token_provider_deprecated = undefined} = BankCard}
) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {bank_card_deprecated, PaymentSystem}},
        create_bank_card_payment_method_ref(BankCard)
    ]);
get_possible_methods(
    {bank_card,
        #domain_BankCard{
            payment_system_deprecated = PaymentSystem,
            token_provider_deprecated = TokenProvider,
            tokenization_method = TokenizationMethod
        } = BankCard}
) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{
            id =
                {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
                    payment_system_deprecated = PaymentSystem,
                    token_provider_deprecated = TokenProvider,
                    tokenization_method = TokenizationMethod
                }}
        },
        create_bank_card_payment_method_ref(BankCard)
    ]);
get_possible_methods({payment_terminal, #domain_PaymentTerminal{terminal_type_deprecated = TerminalType}}) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {payment_terminal_deprecated, TerminalType}}
    ]);
get_possible_methods({digital_wallet, #domain_DigitalWallet{provider_deprecated = Provider}}) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {digital_wallet_deprecated, Provider}}
    ]);
get_possible_methods({crypto_currency_deprecated, CC}) ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {crypto_currency_deprecated, CC}}
    ]);
get_possible_methods({mobile_commerce, #domain_MobileCommerce{operator_deprecated = Operator}}) when
    Operator /= undefined
->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {mobile_deprecated, Operator}}
    ]);
get_possible_methods({mobile_commerce, #domain_MobileCommerce{operator = Operator}}) when Operator /= undefined ->
    ordsets:from_list([
        #domain_PaymentMethodRef{id = {mobile = Operator}}
    ]).

create_bank_card_payment_method_ref(#domain_BankCard{
    payment_system_deprecated = PaymentSystem,
    is_cvv_empty = IsCVVEmpty,
    token_provider_deprecated = TokenProvider,
    tokenization_method = TokenizationMethod
}) ->
    #domain_PaymentMethodRef{
        id =
            {bank_card, #domain_BankCardPaymentMethod{
                payment_system_deprecated = PaymentSystem,
                is_cvv_empty = genlib:define(IsCVVEmpty, false),
                token_provider_deprecated = TokenProvider,
                tokenization_method = TokenizationMethod
            }}
    }.

%% Unmarshalling

-include("legacy_structures.hrl").

-spec unmarshal(hg_msgpack_marshalling:value()) -> t().
unmarshal(PaymentTool) ->
    unmarshal(payment_tool, PaymentTool).

unmarshal(payment_tool, [3, PMV, V]) ->
    PaymentMethod = unmarshal(payment_method, PMV),
    {PaymentMethod, unmarshal(PaymentMethod, V)};
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
    #domain_BankCard{
        token = unmarshal(str, Token),
        payment_system_deprecated = unmarshal(payment_system, PaymentSystem),
        bin = unmarshal(str, Bin),
        last_digits = unmarshal(str, MaskedPan),
        token_provider_deprecated = unmarshal({T, token_provider}, TokenProvider),
        issuer_country = unmarshal({T, issuer_country}, IssuerCountry),
        bank_name = unmarshal({T, bank_name}, BankName),
        metadata = unmarshal({T, metadata}, MD),
        is_cvv_empty = unmarshal({T, boolean}, IsCVVEmpty)
    };
unmarshal(payment_terminal = T, TerminalType) ->
    #domain_PaymentTerminal{
        terminal_type_deprecated = unmarshal({T, type}, TerminalType)
    };
unmarshal(digital_wallet = T, #{
    <<"provider">> := Provider,
    <<"id">> := ID
}) ->
    #domain_DigitalWallet{
        provider_deprecated = unmarshal({T, provider}, Provider),
        id = unmarshal(str, ID)
    };
unmarshal(crypto_currency = T, CC) ->
    {crypto_currency_deprecated, unmarshal({T, currency}, CC)};
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
    case Operator of
        <<"mts">> -> PTool#domain_MobileCommerce{operator_deprecated = mts};
        <<"megafone">> -> PTool#domain_MobileCommerce{operator_deprecated = megafone};
        <<"yota">> -> PTool#domain_MobileCommerce{operator_deprecated = yota};
        <<"tele2">> -> PTool#domain_MobileCommerce{operator_deprecated = tele2};
        <<"beeline">> -> PTool#domain_MobileCommerce{operator_deprecated = beeline};
        BinRef -> PTool#domain_MobileCommerce{operator = #domain_MobileOperatorRef{id = BinRef}}
    end;
unmarshal(payment_tool, [2, #{<<"token">> := _} = BankCard]) ->
    {bank_card, unmarshal(bank_card, BankCard)};
unmarshal(payment_tool, [2, TerminalType]) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type_deprecated = unmarshal({payment_terminal, type}, TerminalType)
    }};
unmarshal(payment_tool, [1, ?legacy_bank_card(Token, PaymentSystem, Bin, MaskedPan)]) ->
    {bank_card, #domain_BankCard{
        token = unmarshal(str, Token),
        payment_system_deprecated = unmarshal(payment_system, PaymentSystem),
        bin = unmarshal(str, Bin),
        last_digits = unmarshal(str, MaskedPan)
    }};
unmarshal(payment_method, <<"card">>) ->
    bank_card;
unmarshal(payment_method, <<"payterm">>) ->
    payment_terminal;
unmarshal(payment_method, <<"wallet">>) ->
    digital_wallet;
unmarshal(payment_method, <<"crypto_currency">>) ->
    crypto_currency;
unmarshal(payment_method, <<"mobile_commerce">>) ->
    mobile_commerce;
unmarshal(payment_system, <<"visa">>) ->
    visa;
unmarshal(payment_system, <<"mastercard">>) ->
    mastercard;
unmarshal(payment_system, <<"visaelectron">>) ->
    visaelectron;
unmarshal(payment_system, <<"maestro">>) ->
    maestro;
unmarshal(payment_system, <<"forbrugsforeningen">>) ->
    forbrugsforeningen;
unmarshal(payment_system, <<"dankort">>) ->
    dankort;
unmarshal(payment_system, <<"amex">>) ->
    amex;
unmarshal(payment_system, <<"dinersclub">>) ->
    dinersclub;
unmarshal(payment_system, <<"discover">>) ->
    discover;
unmarshal(payment_system, <<"unionpay">>) ->
    unionpay;
unmarshal(payment_system, <<"jcb">>) ->
    jcb;
unmarshal(payment_system, <<"nspkmir">>) ->
    nspkmir;
unmarshal({bank_card, token_provider}, <<"applepay">>) ->
    applepay;
unmarshal({bank_card, token_provider}, <<"googlepay">>) ->
    googlepay;
unmarshal({bank_card, token_provider}, <<"samsungpay">>) ->
    samsungpay;
unmarshal({bank_card, issuer_country}, Residence) when is_binary(Residence) ->
    binary_to_existing_atom(unmarshal(str, Residence), utf8);
unmarshal({bank_card, bank_name}, Name) when is_binary(Name) ->
    unmarshal(str, Name);
unmarshal({bank_card, metadata}, MD) when is_map(MD) ->
    maps:map(fun(_, V) -> hg_msgpack_marshalling:marshal(V) end, MD);
unmarshal({payment_terminal, type}, <<"euroset">>) ->
    euroset;
unmarshal({payment_terminal, type}, <<"wechat">>) ->
    wechat;
unmarshal({payment_terminal, type}, <<"alipay">>) ->
    alipay;
unmarshal({payment_terminal, type}, <<"zotapay">>) ->
    zotapay;
unmarshal({payment_terminal, type}, <<"qps">>) ->
    qps;
unmarshal({payment_terminal, type}, <<"uzcard">>) ->
    uzcard;
unmarshal({digital_wallet, provider}, <<"qiwi">>) ->
    qiwi;
unmarshal({bank_card, boolean}, <<"true">>) ->
    true;
unmarshal({bank_card, boolean}, <<"false">>) ->
    false;
unmarshal({crypto_currency, currency}, <<"bitcoin">>) ->
    bitcoin;
unmarshal({crypto_currency, currency}, <<"litecoin">>) ->
    litecoin;
unmarshal({crypto_currency, currency}, <<"bitcoin_cash">>) ->
    bitcoin_cash;
unmarshal({crypto_currency, currency}, <<"ripple">>) ->
    ripple;
unmarshal({crypto_currency, currency}, <<"ethereum">>) ->
    ethereum;
unmarshal({crypto_currency, currency}, <<"zcash">>) ->
    zcash;
unmarshal(_, Other) ->
    Other.
