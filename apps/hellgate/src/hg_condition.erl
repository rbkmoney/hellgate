-module(hg_condition).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("hellgate/include/domain.hrl").

%%

-export([test/3]).

-export([test_cash_range/2]).

%%

-type condition() :: dmsl_domain_thrift:'Condition'().
-type varset()    :: #{}. %% TODO

-spec test(condition(), varset(), hg_domain:revision()) ->
    true | false | undefined.

test({category_is, V1}, #{category := V2}, _) ->
    V1 =:= V2;
test({currency_is, V1}, #{currency := V2}, _) ->
    V1 =:= V2;
test({cost_in, V}, #{cost := C}, _) ->
    test_cash_range(C, V);
test({payment_tool_condition, C}, #{payment_tool := V}, Rev) ->
    hg_payment_tool:test_condition(C, V, Rev);
test({shop_location_is, V}, #{shop := S}, _) ->
    V =:= S#domain_Shop.details#domain_ShopDetails.location;
test({party, V}, #{party := P, shop := S}, _) ->
    test_party(V, P, S);
test(_, #{}, _) ->
    undefined.

test_party(#domain_PartyCondition{id = ID, definition = Def}, P = #domain_Party{id = ID}, S) ->
    test_party_(Def, P, S);
test_party(_, _, _) ->
    false.

test_party_(undefined, _, _) ->
    true;
test_party_({shop_is, ID1}, _, #domain_Shop{id = ID2}) ->
    ID1 =:= ID2.

%%

-spec test_cash_range(dmsl_domain_thrift:'Cash'(), dmsl_domain_thrift:'CashRange'()) ->
    true | false | undefined.

test_cash_range(Cash, #domain_CashRange{lower = Lower, upper = Upper}) ->
    get_product(test_cash_bound(lower, Lower, Cash), test_cash_bound(upper, Upper, Cash)).

test_cash_bound(_, {inclusive, V}, V) ->
    true;
test_cash_bound(lower, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A > Am;
test_cash_bound(upper, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A < Am;
test_cash_bound(_, _, _) ->
    undefined.

%%

get_product(undefined, _) ->
    undefined;
get_product(_, undefined) ->
    undefined;
get_product(A, B) ->
    A and B.
