-module(hg_varset).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([prepare_varset/1]).

-type varset() :: pm_selector:varset().

-spec prepare_varset(varset()) -> dmsl_payment_processing_thrift:'Varset'().

prepare_varset(Varset) ->
    #payproc_Varset{
        category = genlib_map:get(category, Varset),
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        payment_method = hg_maybe:apply(
            fun(X) -> hg_payment_tool:get_method(X) end, genlib_map:get(payment_tool, Varset)),
        payout_method = genlib_map:get(payout_method, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        p2p_tool = genlib_map:get(p2p_tool, Varset)
    }.
