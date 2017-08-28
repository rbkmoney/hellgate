%%% Payment tools

-module(hg_payment_tool).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-export([get_method/1]).
-export([test_condition/3]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-type t() :: dmsl_domain_thrift:'PaymentTool'().
-type method() :: dmsl_domain_thrift:'PaymentMethodRef'().
-type condition() :: dmsl_domain_thrift:'PaymentToolCondition'().

-spec get_method(t()) -> method().

get_method({bank_card, #domain_BankCard{payment_system = PaymentSystem}}) ->
    #domain_PaymentMethodRef{id = {bank_card, PaymentSystem}};
get_method({payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    #domain_PaymentMethodRef{id = {payment_terminal, TerminalType}}.

%%

-spec test_condition(condition(), t(), hg_domain:revision()) -> boolean().

test_condition({bank_card, C}, {bank_card, V = #domain_BankCard{}}, Rev) ->
    test_bank_card_condition(C, V, Rev);
test_condition(_PaymentTool, _Condition, _Rev) ->
    false.

test_bank_card_condition({payment_system_is, Ps}, #domain_BankCard{payment_system = Ps0}, _Rev) ->
    Ps =:= Ps0;
test_bank_card_condition({bin_in, RangeRef}, #domain_BankCard{bin = BIN}, Rev) ->
    #domain_BankCardBINRange{bins = BINs} = hg_domain:get(Rev, {bank_card_bin_range, RangeRef}),
    ordsets:is_element(BIN, BINs).

-include("legacy_structures.hrl").
%% Marshalling

-spec marshal(t()) ->
    hg_msgpack_marshalling:value().

marshal(PaymentTool) ->
    marshal(payment_tool, PaymentTool).

marshal(payment_tool, {bank_card, #domain_BankCard{} = BankCard}) ->
    [2, #{
        <<"token">>             => marshal(str, BankCard#domain_BankCard.token),
        <<"payment_system">>    => marshal(payment_system, BankCard#domain_BankCard.payment_system),
        <<"bin">>               => marshal(str, BankCard#domain_BankCard.bin),
        <<"masked_pan">>        => marshal(str, BankCard#domain_BankCard.masked_pan)
    }];
marshal(payment_tool, {payment_terminal, #domain_PaymentTerminal{terminal_type = TerminalType}}) ->
    [2, marshal(terminal_type, TerminalType)];

marshal(payment_system, visa) ->
    <<"visa">>;
marshal(payment_system, mastercard) ->
    <<"mastercard">>;
marshal(payment_system, visaelectron) ->
    <<"visaelectron">>;
marshal(payment_system, maestro) ->
    <<"maestro">>;
marshal(payment_system, forbrugsforeningen) ->
    <<"forbrugsforeningen">>;
marshal(payment_system, dankort) ->
    <<"dankort">>;
marshal(payment_system, amex) ->
    <<"amex">>;
marshal(payment_system, dinersclub) ->
    <<"dinersclub">>;
marshal(payment_system, discover) ->
    <<"discover">>;
marshal(payment_system, unionpay) ->
    <<"unionpay">>;
marshal(payment_system, jcb) ->
    <<"jcb">>;
marshal(payment_system, nspkmir) ->
    <<"nspkmir">>;

marshal(terminal_type, euroset) ->
    <<"euroset">>;

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    t().

unmarshal(PaymentTool) ->
    unmarshal(payment_tool, PaymentTool).

unmarshal(payment_tool, [2, #{
    <<"token">>             := Token,
    <<"payment_system">>    := PaymentSystem,
    <<"bin">>               := Bin,
    <<"masked_pan">>        := MaskedPan
}]) ->
    {bank_card, #domain_BankCard{
        token               = unmarshal(str, Token),
        payment_system      = unmarshal(payment_system, PaymentSystem),
        bin                 = unmarshal(str, Bin),
        masked_pan          = unmarshal(str, MaskedPan)
    }};
unmarshal(payment_tool, [2, TerminalType]) ->
    {payment_terminal, #domain_PaymentTerminal{
        terminal_type = unmarshal(terminal_type, TerminalType)
    }};

unmarshal(payment_tool, [1, ?legacy_bank_card(Token, PaymentSystem, Bin, MaskedPan)]) ->
    {bank_card, #domain_BankCard{
        token               = unmarshal(str, Token),
        payment_system      = unmarshal(payment_system, PaymentSystem),
        bin                 = unmarshal(str, Bin),
        masked_pan          = unmarshal(str, MaskedPan)
    }};

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

unmarshal(terminal_type, <<"euroset">>) ->
    euroset;

unmarshal(payment_system, PaymentSystem) when is_atom(PaymentSystem) ->
    PaymentSystem;

unmarshal(_, Other) ->
    Other.