%%% Payout tools

-module(pm_payout_tool).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

%%

-export([create/3]).
-export([get_method/1]).

%%
-type payout_tool()         :: dmsl_domain_thrift:'PayoutTool'().
-type payout_tool_id()      :: dmsl_domain_thrift:'PayoutToolID'().
-type payout_tool_params()  :: dmsl_payment_processing_thrift:'PayoutToolParams'().
-type method()              :: dmsl_domain_thrift:'PayoutMethodRef'().
-type timestamp()           :: dmsl_base_thrift:'Timestamp'().

%%

-spec create(payout_tool_id(), payout_tool_params(), timestamp()) ->
    payout_tool().

create(
    ID,
    #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = ToolInfo
    },
    Timestamp
) ->
    #domain_PayoutTool{
        id = ID,
        created_at = Timestamp,
        currency = Currency,
        payout_tool_info = ToolInfo
    }.

-spec get_method(payout_tool()) -> method().

get_method(#domain_PayoutTool{payout_tool_info = {russian_bank_account, _}}) ->
    #domain_PayoutMethodRef{id = russian_bank_account};
get_method(#domain_PayoutTool{payout_tool_info = {international_bank_account, _}}) ->
    #domain_PayoutMethodRef{id = international_bank_account};
get_method(#domain_PayoutTool{payout_tool_info = {wallet_info, _}}) ->
    #domain_PayoutMethodRef{id = wallet_info}.
