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
    #domain_PaymentMethodRef{id = {bank_card, PaymentSystem}}.

%%

-spec test_condition(t(), condition(), hg_domain:revision()) -> boolean().

test_condition({bank_card, #domain_BankCard{bin = BIN}}, {bank_card_bin_is, RangeRef}, Rev) ->
    #domain_BankCardBINRange{bins = BINs} = hg_domain:get(Rev, {bank_card_bin_range, RangeRef}),
    ordsets:is_element(BIN, BINs);
test_condition(_PaymentTool, _Condition, _Rev) ->
    false.
