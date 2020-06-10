%%% Payment tools

-module(pm_payment_tool).
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([create_from_method/1]).
-export([test_condition/3]).

%%

-type t() :: dmsl_domain_thrift:'PaymentTool'().
-type method() :: dmsl_domain_thrift:'PaymentMethodRef'().
-type condition() :: dmsl_domain_thrift:'PaymentToolCondition'().

-spec create_from_method(method()) -> t().

%% TODO empty strings - ugly hack for dialyzar
create_from_method(#domain_PaymentMethodRef{id = {empty_cvv_bank_card, PaymentSystem}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>,
        is_cvv_empty = true
    }};
create_from_method(#domain_PaymentMethodRef{id = {bank_card, PaymentSystem}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>
    }};
create_from_method(#domain_PaymentMethodRef{id = {tokenized_bank_card, #domain_TokenizedBankCard{
        payment_system = PaymentSystem,
        token_provider = TokenProvider
}}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        last_digits = <<"">>,
        token_provider = TokenProvider
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

-spec test_condition(condition(), t(), pm_domain:revision()) -> boolean() | undefined.

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
test_bank_card_condition_def({category_is, CategoryRef}, V, Rev) ->
    test_bank_card_category_condition(CategoryRef, V, Rev);
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
    #domain_PaymentSystemCondition{payment_system_is = Ps, token_provider_is = Tp},
    #domain_BankCard{payment_system = Ps, token_provider = Tp},
    _Rev
) ->
    true;
test_payment_system_condition(#domain_PaymentSystemCondition{}, #domain_BankCard{}, _Rev) ->
    false.

test_issuer_country_condition(_Country, #domain_BankCard{issuer_country = undefined}, _Rev) ->
    undefined;
test_issuer_country_condition(Country, #domain_BankCard{issuer_country = TargetCountry}, _Rev) ->
    Country == TargetCountry.

test_issuer_bank_condition(BankRef, #domain_BankCard{bank_name = BankName, bin = BIN}, Rev) ->
    #domain_Bank{binbase_id_patterns = Patterns, bins = BINs} = pm_domain:get(Rev, {bank, BankRef}),
    case {Patterns, BankName} of
        {P, B} when is_list(P) and is_binary(B) ->
            test_bank_card_patterns(Patterns, BankName);
        % TODO т.к. BinBase не обладает полным объемом данных, при их отсутствии мы возвращаемся к проверкам по бинам.
        %      B будущем стоит избавиться от этого.
        {_, _} -> test_bank_card_bins(BIN, BINs)
    end.

test_bank_card_category_condition(CategoryRef, #domain_BankCard{category = Category}, Rev) ->
    #domain_BankCardCategory{
        category_patterns = Patterns
    } = pm_domain:get(Rev, {bank_card_category, CategoryRef}),
    test_bank_card_patterns(Patterns, Category).

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
