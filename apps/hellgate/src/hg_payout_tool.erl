%%% Payout tools

-module(hg_payout_tool).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-export([get_method/1]).

%%

-type payout_tool() :: dmsl_domain_thrift:'PayoutTool'().
-type method()      :: dmsl_domain_thrift:'PayoutMethodRef'().

-spec get_method(payout_tool()) -> method().

get_method(#domain_PayoutTool{payout_tool_info = {russian_bank_account, _}}) ->
    #domain_PayoutMethodRef{id = russian_bank_account};
get_method(#domain_PayoutTool{payout_tool_info = {international_bank_account, _}}) ->
    #domain_PayoutMethodRef{id = international_bank_account}.
