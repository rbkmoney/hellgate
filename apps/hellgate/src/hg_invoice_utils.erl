%%% Invoice utils
%%%

-module(hg_invoice_utils).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([validate_cost/2]).
-export([validate_amount/1]).
-export([validate_currency/2]).
-export([validate_cash_range/1]).
-export([assert_party_accessible/1]).
-export([assert_party_operable/1]).
-export([assert_shop_exists/1]).
-export([assert_shop_operable/1]).

-type amount()     :: dmsl_domain_thrift:'Amount'().
-type currency()   :: dmsl_domain_thrift:'CurrencyRef'().
-type cash()       :: dmsl_domain_thrift:'Cash'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type party()      :: dmsl_domain_thrift:'Party'().
-type shop()       :: dmsl_domain_thrift:'Shop'().
-type party_id()   :: dmsl_domain_thrift:'PartyID'().


-spec validate_cost(cash(), shop()) -> ok.

validate_cost(#domain_Cash{currency = Currency, amount = Amount}, Shop) ->
    _ = validate_amount(Amount),
    _ = validate_currency(Currency, Shop),
    ok.

-spec validate_amount(amount()) -> ok.
validate_amount(Amount) when Amount > 0 ->
    %% TODO FIX THIS ASAP! Amount should be specified in contract terms.
    ok;
validate_amount(_) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid amount">>]}).

-spec validate_currency(currency(), shop()) -> ok.
validate_currency(Currency, Shop = #domain_Shop{}) ->
    validate_currency_(Currency, get_shop_currency(Shop)).

-spec assert_party_accessible(party_id()) -> ok.
assert_party_accessible(PartyID) ->
    UserIdentity = hg_woody_handler_utils:get_user_identity(),
    case hg_access_control:check_user(UserIdentity, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

-spec validate_cash_range(cash_range()) -> ok.
validate_cash_range(#domain_CashRange{
    lower = {LType, #domain_Cash{amount = LAmount, currency = Currency}},
    upper = {UType, #domain_Cash{amount = UAmount, currency = Currency}}
}) when
    LType =/= UType andalso UAmount >= LAmount orelse
    LType =:= UType andalso UAmount > LAmount orelse
    LType =:= UType andalso UType =:= inclusive andalso UAmount == LAmount
->
    ok;
validate_cash_range(_) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid cost range">>]}).


-spec assert_party_operable(party()) -> party().
assert_party_operable(#domain_Party{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_party_unblocked(Blocking),
    _ = assert_party_active(Suspension),
    V.

-spec assert_shop_operable(shop()) -> shop().
assert_shop_operable(#domain_Shop{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_shop_unblocked(Blocking),
    _ = assert_shop_active(Suspension),
    V.

-spec assert_shop_exists(shop() | undefined) -> shop().
assert_shop_exists(#domain_Shop{} = V) ->
    V;
assert_shop_exists(undefined) ->
    throw(#payproc_ShopNotFound{}).


validate_currency_(Currency, Currency) ->
    ok;
validate_currency_(_, _) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid currency">>]}).

get_shop_currency(#domain_Shop{account = #domain_ShopAccount{currency = Currency}}) ->
    Currency.

assert_party_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidPartyStatus{status = {blocking, V}}).

assert_party_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidPartyStatus{status = {suspension, V}}).

assert_shop_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidShopStatus{status = {blocking, V}}).

assert_shop_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidShopStatus{status = {suspension, V}}).
