-module(hg_condition).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("hellgate/include/domain.hrl").

%%

-export([test/3]).

%%

-type condition() :: dmsl_domain_thrift:'Condition'().
-type varset()    :: hg_selector:varset().

-spec test(condition(), varset(), hg_domain:revision()) ->
    true | false | undefined.

test({category_is, V1}, #{category := V2}, _) ->
    V1 =:= V2;
test({currency_is, V1}, #{currency := V2}, _) ->
    V1 =:= V2;
test({cost_in, V}, #{cost := C}, _) ->
    hg_cash_range:is_inside(C, V) =:= within;
test({payment_tool, C}, #{payment_tool := V}, Rev) ->
    hg_payment_tool:test_condition(C, V, Rev);
test({shop_location_is, V}, #{shop := S}, _) ->
    V =:= S#domain_Shop.location;
test({party, V}, #{party := P} = VS, _) ->
    test_party(V, P, genlib_map:get(shop, VS));
test({payout_method_is, V1}, #{payout_method := V2}, _) ->
    V1 =:= V2;
test(_, #{}, _) ->
    undefined.

test_party(#domain_PartyCondition{id = ID, definition = Def}, P = #domain_Party{id = ID}, S) ->
    test_party_(Def, P, S);
test_party(_, _, _) ->
    false.

test_party_(undefined, _, _) ->
    true;
test_party_({shop_is, ID1}, _, #domain_Shop{id = ID2}) ->
    ID1 =:= ID2;
test_party_(_, _, _) ->
    undefined.
