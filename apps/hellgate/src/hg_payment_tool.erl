%%% Payment tools

-module(hg_payment_tool).
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([get_method/1]).
-export([create_from_method/1]).
-export([test_condition/3]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-type t() :: dmsl_domain_thrift:'PaymentTool'().
-type method() :: dmsl_domain_thrift:'PaymentMethodRef'().
-type condition() :: dmsl_domain_thrift:'PaymentToolCondition'().

-spec get_method(t()) -> method().

get_method({bank_card, #domain_BankCard{payment_system = PaymentSystem, is_cvv_empty = true}}) ->
    #domain_PaymentMethodRef{id = {empty_cvv_bank_card_deprecated, PaymentSystem}};
get_method({bank_card, #domain_BankCard{payment_system = PaymentSystem, token_provider = undefined}}) ->
    #domain_PaymentMethodRef{id = {bank_card_deprecated, PaymentSystem}};
get_method({bank_card, #domain_BankCard{
    payment_system = PaymentSystem,
    token_provider = TokenProvider,
    tokenization_method = TokenizationMethod
}}) ->
    #domain_PaymentMethodRef{id = {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
        payment_system = PaymentSystem,
        token_provider = TokenProvider,
        tokenization_method = TokenizationMethod
    }}};
get_method({payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    #domain_PaymentMethodRef{id = {payment_terminal, TerminalType}};
get_method({digital_wallet, #domain_DigitalWallet{provider = Provider}}) ->
    #domain_PaymentMethodRef{id = {digital_wallet, Provider}};
get_method({crypto_currency, CC}) ->
    #domain_PaymentMethodRef{id = {crypto_currency, CC}};
get_method({mobile_commerce, #domain_MobileCommerce{operator = Operator}}) ->
    #domain_PaymentMethodRef{id = {mobile, Operator}}.

-spec create_from_method(method()) -> t().

%% TODO empty strings - ugly hack for dialyzar
create_from_method(#domain_PaymentMethodRef{id = {empty_cvv_bank_card_deprecated, PaymentSystem}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>,
        is_cvv_empty = true
    }};
create_from_method(#domain_PaymentMethodRef{id = {bank_card_deprecated, PaymentSystem}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>
    }};
create_from_method(#domain_PaymentMethodRef{id = {tokenized_bank_card_deprecated, #domain_TokenizedBankCard{
        payment_system = PaymentSystem,
        token_provider = TokenProvider,
        tokenization_method = TokenizationMethod
}}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>,
        token_provider = TokenProvider,
        tokenization_method = TokenizationMethod
    }};
create_from_method(#domain_PaymentMethodRef{id = {payment_terminal, TerminalType}}) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}};
create_from_method(#domain_PaymentMethodRef{id = {digital_wallet, Provider}}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = Provider,
        id = <<"">>
    }};
create_from_method(#domain_PaymentMethodRef{id = {crypto_currency, CC}}) ->
    {crypto_currency, CC}.

%%

-spec test_condition(condition(), t(), hg_domain:revision()) -> boolean() | undefined.

test_condition({bank_card, C}, {bank_card, V = #domain_BankCard{}}, Rev) ->
    test_bank_card_condition(C, V, Rev);
test_condition({payment_terminal, C}, {payment_terminal, V = #domain_PaymentTerminal{}}, Rev) ->
    test_payment_terminal_condition(C, V, Rev);
test_condition({digital_wallet, C}, {digital_wallet, V = #domain_DigitalWallet{}}, Rev) ->
    test_digital_wallet_condition(C, V, Rev);
test_condition({crypto_currency, C}, {crypto_currency, V}, Rev) ->
    test_crypto_currency_condition(C, V, Rev);
test_condition({mobile_commerce, C}, {mobile_commerce, V}, Rev) ->
    test_mobile_commerce_condition(C, V, Rev);
test_condition(_PaymentTool, _Condition, _Rev) ->
    false.

test_bank_card_condition(#domain_BankCardCondition{definition = Def}, V, Rev) when Def /= undefined ->
    test_bank_card_condition_def(Def, V, Rev);
test_bank_card_condition(#domain_BankCardCondition{}, _, _Rev) ->
    true.

% legacy
test_bank_card_condition_def(
    {payment_system_is, Ps},
    #domain_BankCard{payment_system = Ps, token_provider = undefined},
    _Rev
) ->
    true;
test_bank_card_condition_def({payment_system_is, _Ps}, #domain_BankCard{}, _Rev) ->
    false;

test_bank_card_condition_def({payment_system, PaymentSystem}, V, Rev) ->
    test_payment_system_condition(PaymentSystem, V, Rev);
test_bank_card_condition_def({issuer_country_is, IssuerCountry}, V, Rev) ->
    test_issuer_country_condition(IssuerCountry, V, Rev);
test_bank_card_condition_def({issuer_bank_is, BankRef}, V, Rev) ->
    test_issuer_bank_condition(BankRef, V, Rev);
test_bank_card_condition_def(
    {empty_cvv_is, Val},
    #domain_BankCard{is_cvv_empty = Val},
    _Rev
) ->
    true;
%% Для обратной совместимости с картами, у которых нет is_cvv_empty
test_bank_card_condition_def(
    {empty_cvv_is, false},
    #domain_BankCard{is_cvv_empty = undefined},
    _Rev
) ->
    true;
test_bank_card_condition_def({empty_cvv_is, _Val}, #domain_BankCard{}, _Rev) ->
    false.

test_payment_system_condition(
    #domain_PaymentSystemCondition{payment_system_is = Ps, token_provider_is = Tp, tokenization_method_is = TmCond},
    #domain_BankCard{payment_system = Ps, token_provider = Tp, tokenization_method = Tm},
    _Rev
) ->
    test_tokenization_method_condition(TmCond, Tm);
test_payment_system_condition(#domain_PaymentSystemCondition{}, #domain_BankCard{}, _Rev) ->
    false.

test_tokenization_method_condition(undefined, _) ->
    true;
test_tokenization_method_condition(_NotUndefined, undefined) ->
    undefined;
test_tokenization_method_condition(DesiredMethod, ActualMethod) ->
    DesiredMethod == ActualMethod.

test_issuer_country_condition(_Country, #domain_BankCard{issuer_country = undefined}, _Rev) ->
    undefined;
test_issuer_country_condition(Country, #domain_BankCard{issuer_country = TargetCountry}, _Rev) ->
    Country == TargetCountry.

test_issuer_bank_condition(BankRef, #domain_BankCard{bank_name = BankName, bin = BIN}, Rev) ->
    #domain_Bank{binbase_id_patterns = Patterns, bins = BINs} = hg_domain:get(Rev, {bank, BankRef}),
    case {Patterns, BankName} of
        {P, B} when is_list(P) and is_binary(B) ->
            test_bank_card_patterns(Patterns, BankName);
        % TODO т.к. BinBase не обладает полным объемом данных, при их отсутствии мы возвращаемся к проверкам по бинам.
        %      B будущем стоит избавиться от этого.
        {_, _} -> test_bank_card_bins(BIN, BINs)
    end.

test_bank_card_bins(BIN, BINs) ->
    ordsets:is_element(BIN, BINs).

test_bank_card_patterns(Patterns, BankName) ->
    Matches = ordsets:filter(fun(E) -> genlib_wildcard:match(BankName, E) end, Patterns),
    ordsets:size(Matches) > 0.

test_payment_terminal_condition(#domain_PaymentTerminalCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_payment_terminal_condition_def(Def, V, Rev).

test_payment_terminal_condition_def({provider_is, V1}, #domain_PaymentTerminal{terminal_type = V2}, _Rev) ->
    V1 =:= V2.

test_digital_wallet_condition(#domain_DigitalWalletCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_digital_wallet_condition_def(Def, V, Rev).

test_digital_wallet_condition_def({provider_is, V1}, #domain_DigitalWallet{provider = V2}, _Rev) ->
    V1 =:= V2.

test_crypto_currency_condition(#domain_CryptoCurrencyCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_crypto_currency_condition_def(Def, V, Rev).

test_crypto_currency_condition_def({crypto_currency_is, C1}, C2, _Rev) ->
    C1 =:= C2.

test_mobile_commerce_condition(#domain_MobileCommerceCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_mobile_commerce_condition_def(Def, V, Rev).

test_mobile_commerce_condition_def({operator_is, C1}, #domain_MobileCommerce{operator = C2}, _Rev) ->
    C1 =:= C2.

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(t()) ->
    hg_msgpack_marshalling:value().

marshal(PaymentTool) ->
    marshal(payment_tool, PaymentTool).

marshal(payment_tool, {PaymentMethod, V}) ->
    [3, marshal(payment_method, PaymentMethod), marshal(PaymentMethod, V)];

marshal(bank_card = T, #domain_BankCard{} = BankCard) ->
    genlib_map:compact(#{
        <<"token">>             => marshal(str, BankCard#domain_BankCard.token),
        <<"payment_system">>    => marshal({T, payment_system}, BankCard#domain_BankCard.payment_system),
        <<"bin">>               => marshal(str, BankCard#domain_BankCard.bin),
        <<"masked_pan">>        => marshal(str, BankCard#domain_BankCard.last_digits),
        <<"token_provider">>    => marshal({T, token_provider}, BankCard#domain_BankCard.token_provider),
        <<"issuer_country">>    => marshal({T, issuer_country}, BankCard#domain_BankCard.issuer_country),
        <<"bank_name">>         => marshal({T, bank_name}, BankCard#domain_BankCard.bank_name),
        <<"metadata">>          => marshal({T, metadata}, BankCard#domain_BankCard.metadata),
        <<"is_cvv_empty">>      => marshal({T, boolean}, BankCard#domain_BankCard.is_cvv_empty)
    });
marshal(payment_terminal = T, #domain_PaymentTerminal{terminal_type = TerminalType}) ->
    marshal({T, type}, TerminalType);
marshal(digital_wallet = T, #domain_DigitalWallet{} = DigitalWallet) ->
    #{
        <<"provider">> => marshal({T, provider}, DigitalWallet#domain_DigitalWallet.provider),
        <<"id">>       => marshal(str, DigitalWallet#domain_DigitalWallet.id)
    };
marshal(crypto_currency = T, CC) ->
    marshal({T, currency}, CC);

marshal(payment_method, bank_card) ->
    <<"card">>;
marshal(payment_method, payment_terminal) ->
    <<"payterm">>;
marshal(payment_method, digital_wallet) ->
    <<"wallet">>;
marshal(payment_method, crypto_currency) ->
    <<"crypto_currency">>;
marshal(payment_method, mobile_commerce) ->
    <<"mobile_commerce">>;

marshal({bank_card, payment_system}, visa) ->
    <<"visa">>;
marshal({bank_card, payment_system}, mastercard) ->
    <<"mastercard">>;
marshal({bank_card, payment_system}, visaelectron) ->
    <<"visaelectron">>;
marshal({bank_card, payment_system}, maestro) ->
    <<"maestro">>;
marshal({bank_card, payment_system}, forbrugsforeningen) ->
    <<"forbrugsforeningen">>;
marshal({bank_card, payment_system}, dankort) ->
    <<"dankort">>;
marshal({bank_card, payment_system}, amex) ->
    <<"amex">>;
marshal({bank_card, payment_system}, dinersclub) ->
    <<"dinersclub">>;
marshal({bank_card, payment_system}, discover) ->
    <<"discover">>;
marshal({bank_card, payment_system}, unionpay) ->
    <<"unionpay">>;
marshal({bank_card, payment_system}, jcb) ->
    <<"jcb">>;
marshal({bank_card, payment_system}, nspkmir) ->
    <<"nspkmir">>;

marshal({bank_card, token_provider}, applepay) ->
    <<"applepay">>;
marshal({bank_card, token_provider}, googlepay) ->
    <<"googlepay">>;
marshal({bank_card, token_provider}, samsungpay) ->
    <<"samsungpay">>;

marshal({bank_card, issuer_country}, Residence) when is_atom(Residence), Residence /= undefined ->
    marshal(str, atom_to_binary(Residence, utf8));

marshal({bank_card, bank_name}, Name) when is_binary(Name) ->
    marshal(str, Name);

marshal({bank_card, metadata}, MD) when is_map(MD) ->
    maps:map(fun (_, V) -> hg_msgpack_marshalling:unmarshal(V) end, MD);

marshal({payment_terminal, type}, euroset) ->
    <<"euroset">>;

marshal({payment_terminal, type}, wechat) ->
    <<"wechat">>;

marshal({payment_terminal, type}, alipay) ->
    <<"alipay">>;

marshal({payment_terminal, type}, zotapay) ->
    <<"zotapay">>;

marshal({payment_terminal, type}, qps) ->
    <<"qps">>;

marshal({digital_wallet, provider}, qiwi) ->
    <<"qiwi">>;

marshal({bank_card, boolean}, true) ->
    <<"true">>;
marshal({bank_card, boolean}, false) ->
    <<"false">>;

marshal({mobile_commerce, operator}, mts) ->
    <<"mts">>;
marshal({mobile_commerce, operator}, megafone) ->
    <<"megafone">>;
marshal({mobile_commerce, operator}, yota) ->
    <<"yota">>;
marshal({mobile_commerce, operator}, tele2) ->
    <<"tele2">>;
marshal({mobile_commerce, operator}, beeline) ->
    <<"beeline">>;

marshal({crypto_currency, currency}, bitcoin) ->
    <<"bitcoin">>;
marshal({crypto_currency, currency}, litecoin) ->
    <<"litecoin">>;
marshal({crypto_currency, currency}, bitcoin_cash) ->
    <<"bitcoin_cash">>;
marshal({crypto_currency, currency}, ripple) ->
    <<"ripple">>;
marshal({crypto_currency, currency}, ethereum) ->
    <<"ethereum">>;
marshal({crypto_currency, currency}, zcash) ->
    <<"zcash">>;

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    t().

unmarshal(PaymentTool) ->
    unmarshal(payment_tool, PaymentTool).

unmarshal(payment_tool, [3, PMV, V]) ->
    PaymentMethod = unmarshal(payment_method, PMV),
    {PaymentMethod, unmarshal(PaymentMethod, V)};

unmarshal(bank_card = T, #{
    <<"token">>          := Token,
    <<"payment_system">> := PaymentSystem,
    <<"bin">>            := Bin,
    <<"masked_pan">>     := MaskedPan
} = V) ->
    TokenProvider = genlib_map:get(<<"token_provider">>, V),
    IssuerCountry = genlib_map:get(<<"issuer_country">>, V),
    BankName      = genlib_map:get(<<"bank_name">>, V),
    MD            = genlib_map:get(<<"metadata">>, V),
    IsCVVEmpty    = genlib_map:get(<<"is_cvv_empty">>, V),
    #domain_BankCard{
        token            = unmarshal(str, Token),
        payment_system   = unmarshal({T, payment_system}, PaymentSystem),
        bin              = unmarshal(str, Bin),
        last_digits      = unmarshal(str, MaskedPan),
        token_provider   = unmarshal({T, token_provider}, TokenProvider),
        issuer_country   = unmarshal({T, issuer_country}, IssuerCountry),
        bank_name        = unmarshal({T, bank_name}, BankName),
        metadata         = unmarshal({T, metadata}, MD),
        is_cvv_empty     = unmarshal({T, boolean}, IsCVVEmpty)
    };
unmarshal(payment_terminal = T, TerminalType) ->
    #domain_PaymentTerminal{
        terminal_type    = unmarshal({T, type}, TerminalType)
    };
unmarshal(digital_wallet = T, #{
    <<"provider">>       := Provider,
    <<"id">>             := ID
}) ->
    #domain_DigitalWallet{
        provider         = unmarshal({T, provider}, Provider),
        id               = unmarshal(str, ID)
    };
unmarshal(crypto_currency = T, CC) ->
    {crypto_currency, unmarshal({T, currency}, CC)};
unmarshal(mobile_commerce = T, #{
    <<"operator">> := Operator,
    <<"phone">>    := #{cc := CC, ctn := Ctn}
}) ->
    #domain_MobileCommerce{
        operator = unmarshal({T, operator}, Operator),
        phone = #domain_MobilePhone{
            cc = unmarshal(str, CC),
            ctn = unmarshal(str, Ctn)
        }
    };

unmarshal(payment_tool, [2, #{<<"token">>:= _} = BankCard]) ->
    {bank_card, unmarshal(bank_card, BankCard)};
unmarshal(payment_tool, [2, TerminalType]) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type = unmarshal({payment_terminal, type}, TerminalType)
    }};

unmarshal(payment_tool, [1, ?legacy_bank_card(Token, PaymentSystem, Bin, MaskedPan)]) ->
    {bank_card, #domain_BankCard{
        token               = unmarshal(str, Token),
        payment_system      = unmarshal({bank_card, payment_system}, PaymentSystem),
        bin                 = unmarshal(str, Bin),
        last_digits         = unmarshal(str, MaskedPan)
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

unmarshal({bank_card, payment_system}, <<"visa">>) ->
    visa;
unmarshal({bank_card, payment_system}, <<"mastercard">>) ->
    mastercard;
unmarshal({bank_card, payment_system}, <<"visaelectron">>) ->
    visaelectron;
unmarshal({bank_card, payment_system}, <<"maestro">>) ->
    maestro;
unmarshal({bank_card, payment_system}, <<"forbrugsforeningen">>) ->
    forbrugsforeningen;
unmarshal({bank_card, payment_system}, <<"dankort">>) ->
    dankort;
unmarshal({bank_card, payment_system}, <<"amex">>) ->
    amex;
unmarshal({bank_card, payment_system}, <<"dinersclub">>) ->
    dinersclub;
unmarshal({bank_card, payment_system}, <<"discover">>) ->
    discover;
unmarshal({bank_card, payment_system}, <<"unionpay">>) ->
    unionpay;
unmarshal({bank_card, payment_system}, <<"jcb">>) ->
    jcb;
unmarshal({bank_card, payment_system}, <<"nspkmir">>) ->
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
    maps:map(fun (_, V) -> hg_msgpack_marshalling:marshal(V) end, MD);

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

unmarshal({mobile_commerce, operator}, <<"mts">>) ->
    mts;
unmarshal({mobile_commerce, operator}, <<"megafone">>) ->
    megafone;
unmarshal({mobile_commerce, operator}, <<"yota">>) ->
    yota;
unmarshal({mobile_commerce, operator}, <<"tele2">>) ->
    tele2;
unmarshal({mobile_commerce, operator}, <<"beeline">>) ->
    beeline;

unmarshal(_, Other) ->
    Other.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-type testcase() :: {_, fun()}.

-spec legacy_unmarshalling_test_() -> [testcase()].
legacy_unmarshalling_test_() ->
    PT1 = {bank_card, #domain_BankCard{
        token          = <<"abcdefabcdefabcdefabcdef">>,
        payment_system = nspkmir,
        bin            = <<"22002201">>,
        last_digits    = <<"11">>
    }},
    PT2 = {payment_terminal, #domain_PaymentTerminal{
        terminal_type  = euroset
    }},
    [
        ?_assertEqual(PT1, unmarshal(legacy_marshal(2, PT1))),
        ?_assertEqual(PT2, unmarshal(legacy_marshal(2, PT2)))
    ].

legacy_marshal(_Vsn = 2, {bank_card, #domain_BankCard{} = BankCard}) ->
    [2, #{
        <<"token">>          => marshal(str, BankCard#domain_BankCard.token),
        <<"payment_system">> => marshal({bank_card, payment_system}, BankCard#domain_BankCard.payment_system),
        <<"bin">>            => marshal(str, BankCard#domain_BankCard.bin),
        <<"masked_pan">>     => marshal(str, BankCard#domain_BankCard.last_digits)
    }];
legacy_marshal(_Vsn = 2, {payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    [2, marshal({payment_terminal, type}, TerminalType)].

-endif.
