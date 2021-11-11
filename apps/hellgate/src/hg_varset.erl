-module(hg_varset).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([prepare_varset/1]).
-export([prepare_contract_terms_varset/1]).
-export([prepare_shop_terms_varset/1]).

-export_type([varset/0]).

-type varset() :: #{
    category => dmsl_domain_thrift:'CategoryRef'(),
    currency => dmsl_domain_thrift:'CurrencyRef'(),
    cost => dmsl_domain_thrift:'Cash'(),
    payment_tool => dmsl_domain_thrift:'PaymentTool'(),
    party_id => dmsl_domain_thrift:'PartyID'(),
    shop_id => dmsl_domain_thrift:'ShopID'(),
    risk_score => dmsl_domain_thrift:'RiskScore'(),
    flow => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()},
    payout_method => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'()
}.

-spec prepare_varset(varset()) -> dmsl_payment_processing_thrift:'Varset'().
prepare_varset(Varset) ->
    #payproc_Varset{
        category = genlib_map:get(category, Varset),
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        identification_level = genlib_map:get(identification_level, Varset),
        party_id = genlib_map:get(party_id, Varset),
        shop_id = genlib_map:get(shop_id, Varset)
    }.

-spec prepare_contract_terms_varset(varset()) -> dmsl_payment_processing_thrift:'ComputeContractTermsVarset'().
prepare_contract_terms_varset(Varset) ->
    #payproc_ComputeContractTermsVarset{
        amount = genlib_map:get(cost, Varset),
        shop_id = genlib_map:get(shop_id, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        bin_data = genlib_map:get(bin_data, Varset)
    }.

-spec prepare_shop_terms_varset(varset()) -> dmsl_payment_processing_thrift:'ComputeShopTermsVarset'().
prepare_shop_terms_varset(Varset) ->
    #payproc_ComputeShopTermsVarset{
        amount = genlib_map:get(cost, Varset),
        payout_method = genlib_map:get(payout_method, Varset),
        payment_tool = genlib_map:get(payment_tool, Varset)
    }.
