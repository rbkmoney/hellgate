%%% Payment tools

-module(hg_payment_tool).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-export([get_method/1]).
-export([test_condition/3]).

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
