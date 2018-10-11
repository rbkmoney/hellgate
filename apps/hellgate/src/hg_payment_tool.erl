%%% Payment tools

-module(hg_payment_tool).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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

get_method({bank_card, #domain_BankCard{payment_system = PaymentSystem, token_provider = undefined}}) ->
    #domain_PaymentMethodRef{id = {bank_card, PaymentSystem}};
get_method({bank_card, #domain_BankCard{payment_system = PaymentSystem, token_provider = TokenProvider}}) ->
    #domain_PaymentMethodRef{id = {tokenized_bank_card, #domain_TokenizedBankCard{
        payment_system = PaymentSystem,
        token_provider = TokenProvider
    }}};
get_method({payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    #domain_PaymentMethodRef{id = {payment_terminal, TerminalType}};
get_method({digital_wallet, #domain_DigitalWallet{provider = Provider}}) ->
    #domain_PaymentMethodRef{id = {digital_wallet, Provider}}.

-spec create_from_method(method()) -> t().

%% TODO empty strings - ugly hack for dialyzar
create_from_method(#domain_PaymentMethodRef{id = {bank_card, PaymentSystem}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        masked_pan = <<"">>
    }};
create_from_method(#domain_PaymentMethodRef{id = {tokenized_bank_card, #domain_TokenizedBankCard{
        payment_system = PaymentSystem,
        token_provider = TokenProvider
}}}) ->
    {bank_card, #domain_BankCard{
        payment_system = PaymentSystem,
        token = <<"">>,
        bin = <<"">>,
        masked_pan = <<"">>,
        token_provider = TokenProvider
    }};
create_from_method(#domain_PaymentMethodRef{id = {payment_terminal, TerminalType}}) ->
    {payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}};
create_from_method(#domain_PaymentMethodRef{id = {digital_wallet, Provider}}) ->
    {digital_wallet, #domain_DigitalWallet{
        provider = Provider,
        id = <<"">>
    }}.

%%

-spec test_condition(condition(), t(), hg_domain:revision()) -> boolean().

test_condition({bank_card, C}, {bank_card, V = #domain_BankCard{}}, Rev) ->
    test_bank_card_condition(C, V, Rev);
test_condition({payment_terminal, C}, {payment_terminal, V = #domain_PaymentTerminal{}}, Rev) ->
    test_payment_terminal_condition(C, V, Rev);
test_condition({digital_wallet, C}, {digital_wallet, V = #domain_DigitalWallet{}}, Rev) ->
    test_digital_wallet_condition(C, V, Rev);
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

test_bank_card_condition_def({bin_in, RangeRef}, #domain_BankCard{bin = BIN}, Rev) ->
    #domain_BankCardBINRange{bins = BINs} = hg_domain:get(Rev, {bank_card_bin_range, RangeRef}),
    ordsets:is_element(BIN, BINs);
test_bank_card_condition_def({payment_system, PaymentSystem}, V, Rev) ->
    test_payment_system_condition(PaymentSystem, V, Rev);
test_bank_card_condition_def({issuer_country_is, IssuerCountry}, V, Rev) ->
    test_issuer_country_condition(IssuerCountry, V, Rev);
test_bank_card_condition_def({issuer_bank_is, BankRef}, #domain_BankCard{bank_name = BankName}, Rev) ->
    #domain_Bank{binbase_id_patterns = Patterns} = hg_domain:get(Rev, {bank, BankRef}),
    Matches = ordsets:filter(fun(E) -> genlib_wildcard:match(BankName, E) end, Patterns),
    ordsets:size(Matches) > 0.

test_payment_system_condition(
    #domain_PaymentSystemCondition{payment_system_is = Ps, token_provider_is = Tp},
    #domain_BankCard{payment_system = Ps, token_provider = Tp},
    _Rev
) ->
    true;
test_payment_system_condition(#domain_PaymentSystemCondition{}, #domain_BankCard{}, _Rev) ->
    false.
test_issuer_country_condition(Country, #domain_BankCard{issuer_country = TargetCountry}, _Rev) ->
    Country == TargetCountry.

test_payment_terminal_condition(#domain_PaymentTerminalCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_payment_terminal_condition_def(Def, V, Rev).

test_payment_terminal_condition_def({provider_is, V1}, #domain_PaymentTerminal{terminal_type = V2}, _Rev) ->
    V1 =:= V2.

test_digital_wallet_condition(#domain_DigitalWalletCondition{definition = Def}, V, Rev) ->
    Def =:= undefined orelse test_digital_wallet_condition_def(Def, V, Rev).

test_digital_wallet_condition_def({provider_is, V1}, #domain_DigitalWallet{provider = V2}, _Rev) ->
    V1 =:= V2.

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
        <<"masked_pan">>        => marshal(str, BankCard#domain_BankCard.masked_pan),
        <<"token_provider">>    => marshal({T, token_provider}, BankCard#domain_BankCard.token_provider)
    });
marshal(payment_terminal = T, #domain_PaymentTerminal{terminal_type = TerminalType}) ->
    marshal({T, type}, TerminalType);
marshal(digital_wallet = T, #domain_DigitalWallet{} = DigitalWallet) ->
    #{
        <<"provider">> => marshal({T, provider}, DigitalWallet#domain_DigitalWallet.provider),
        <<"id">>       => marshal(str, DigitalWallet#domain_DigitalWallet.id)
    };

marshal(payment_method, bank_card) ->
    <<"card">>;
marshal(payment_method, payment_terminal) ->
    <<"payterm">>;
marshal(payment_method, digital_wallet) ->
    <<"wallet">>;

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

marshal({payment_terminal, type}, euroset) ->
    <<"euroset">>;

marshal({digital_wallet, provider}, qiwi) ->
    <<"qiwi">>;

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
    #domain_BankCard{
        token            = unmarshal(str, Token),
        payment_system   = unmarshal({T, payment_system}, PaymentSystem),
        bin              = unmarshal(str, Bin),
        masked_pan       = unmarshal(str, MaskedPan),
        token_provider   = unmarshal({T, token_provider}, TokenProvider)
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
        masked_pan          = unmarshal(str, MaskedPan)
    }};

unmarshal(payment_method, <<"card">>) ->
    bank_card;
unmarshal(payment_method, <<"payterm">>) ->
    payment_terminal;
unmarshal(payment_method, <<"wallet">>) ->
    digital_wallet;

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

unmarshal({payment_terminal, type}, <<"euroset">>) ->
    euroset;

unmarshal({digital_wallet, provider}, <<"qiwi">>) ->
    qiwi;

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
        masked_pan     = <<"11">>
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
        <<"masked_pan">>     => marshal(str, BankCard#domain_BankCard.masked_pan)
    }];
legacy_marshal(_Vsn = 2, {payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    [2, marshal({payment_terminal, type}, TerminalType)].

-endif.
