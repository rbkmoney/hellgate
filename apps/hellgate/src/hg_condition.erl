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
test(_, #{}, _) ->
    undefined.

%%

-spec test_cash_range(dmsl_domain_thrift:'Cash'(), dmsl_domain_thrift:'CashRange'()) ->
    boolean().

test_cash_range(Cash, #domain_CashRange{min = Min, max = Max}) ->
    test_cash_bound(min, Min, Cash) andalso test_cash_bound(max, Max, Cash).

test_cash_bound(_, {inclusive, V}, V) ->
    true;
test_cash_bound(min, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A > Am;
test_cash_bound(max, {_, ?cash(Am, C)}, ?cash(A, C)) ->
    A < Am;
test_cash_bound(_, _, _) ->
    % TODO Non-mathcing currencies, should we error out instead?
    false.
