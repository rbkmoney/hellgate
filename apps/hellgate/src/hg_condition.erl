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
    test_cash_range(C, V) =:= within;
test({payment_tool, C}, #{payment_tool := V}, Rev) ->
    hg_payment_tool:test_condition(C, V, Rev);
test({shop_location_is, V}, #{shop := S}, _) ->
    V =:= S#domain_Shop.location;
test({party, V}, #{party := P} = VS, _) ->
    test_party(V, P, genlib_map:get(shop, VS));
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

%%

-spec test_cash_range(dmsl_domain_thrift:'Cash'(), dmsl_domain_thrift:'CashRange'()) ->
    within | {exceeds, lower | upper}.

test_cash_range(Cash, CashRange = #domain_CashRange{lower = Lower, upper = Upper}) ->
    case {
        test_cash_bound(fun erlang:'>'/2, Cash, Lower),
        test_cash_bound(fun erlang:'<'/2, Cash, Upper)
    } of
        {true, true} ->
            within;
        {false, true} ->
            {exceeds, lower};
        {true, false} ->
            {exceeds, upper};
        _ ->
            error({misconfiguration, {'Invalid cash range specified', CashRange, Cash}})
    end.

test_cash_bound(_, V, {inclusive, V}) ->
    true;
test_cash_bound(F, ?cash(A, C), {_, ?cash(Am, C)}) ->
    F(A, Am);
test_cash_bound(_, _, _) ->
    error.
