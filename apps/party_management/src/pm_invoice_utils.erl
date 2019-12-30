%%% Invoice utils
%%%

-module(pm_invoice_utils).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([validate_currency/2]).

-type currency() :: dmsl_domain_thrift:'CurrencyRef'().
-type shop()     :: dmsl_domain_thrift:'Shop'().

-spec validate_currency(currency(), shop()) -> ok.
validate_currency(Currency, Shop = #domain_Shop{}) ->
    validate_currency_(Currency, get_shop_currency(Shop)).

validate_currency_(Currency, Currency) ->
    ok;
validate_currency_(_, _) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid currency">>]}).

get_shop_currency(#domain_Shop{account = #domain_ShopAccount{currency = Currency}}) ->
    Currency.
