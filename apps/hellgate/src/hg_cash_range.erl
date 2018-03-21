-module(hg_cash_range).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-export([test_cash/2]).
-export([compare/2]).
-export([intersect/2]).

-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type cash()       :: dmsl_domain_thrift:'Cash'().

-spec test_cash(cash(), cash_range()) ->
    within | {exceeds, lower | upper}.

test_cash(Cash, CashRange = #domain_CashRange{lower = Lower, upper = Upper}) ->
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

-spec intersect(cash_range(), cash_range()) ->
    cash_range() | undefined.

intersect(
    #domain_CashRange{lower = Lower1, upper = Upper1},
    #domain_CashRange{lower = Lower2, upper = Upper2}
) ->
    Lower3 = intersect_bounds(fun erlang:'>'/2, Lower1, Lower2),
    Upper3 = intersect_bounds(fun erlang:'<'/2, Upper1, Upper2),
    case compare_bounds(fun erlang:'<'/2, Lower3, Upper3) of
        true ->
            #domain_CashRange{lower = Lower3, upper = Upper3};
        false ->
            undefined
    end.

-spec compare(cash_range(), cash_range()) ->
    true | false.

compare(
    #domain_CashRange{lower = Lower1, upper = Upper1},
    #domain_CashRange{lower = Lower2, upper = Upper2}
) ->
    compare_bounds(fun erlang:'>'/2, Lower1, Lower2) and
        compare_bounds(fun erlang:'<'/2, Upper1, Upper2).

%%

intersect_bounds(F, Lower1, Lower2) ->
    case compare_bounds(F, Lower1, Lower2) of
        true ->
            Lower1;
        false ->
            Lower2
    end.

compare_bounds(_, {exclusive, Cash}, {exclusive, Cash}) ->
    true;
compare_bounds(F, {_, Cash}, Bound) ->
    test_cash_bound(F, Cash, Bound) == true orelse false.

test_cash_bound(_, V, {inclusive, V}) ->
    true;
test_cash_bound(F, ?cash(A, C), {_, ?cash(Am, C)}) ->
    F(A, Am);
test_cash_bound(_, _, _) ->
    error.


%% Marshalling

-spec marshal(cash_range()) ->
    hg_msgpack_marshalling:value().

marshal(CashRange) ->
    marshal(cash_range, CashRange).

marshal(cash_range, #domain_CashRange{
    lower = Lower,
    upper = Upper
}) ->
    [2, [marshal(cash_bound, Lower), marshal(cash_bound, Upper)]];

marshal(cash_bound, {Exclusiveness, Cash}) ->
    [marshal(exclusiveness, Exclusiveness), hg_cash:marshal(Cash)];

marshal(exclusiveness, inclusive) ->
    <<"inclusive">>;
marshal(exclusiveness, exclusive) ->
    <<"exclusive">>.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    cash_range().

unmarshal(CashRange) ->
    unmarshal(cash_range, CashRange).

unmarshal(cash_range, [2, [Lower, Upper]]) ->
    #domain_CashRange{
        lower = unmarshal(cash_bound, Lower),
        upper = unmarshal(cash_bound, Upper)
    };

unmarshal(cash_bound, [Exclusiveness, Cash]) ->
    {unmarshal(exclusiveness, Exclusiveness), hg_cash:unmarshal(Cash)};

unmarshal(exclusiveness, <<"inclusive">>) ->
    inclusive;
unmarshal(exclusiveness, <<"exclusive">>) ->
    exclusive;

unmarshal(cash_range, [1, {'domain_CashRange', Upper, Lower}]) ->
    #domain_CashRange{
        lower = unmarshal(cash_bound_legacy, Lower),
        upper = unmarshal(cash_bound_legacy, Upper)
    };

unmarshal(cash_bound_legacy, {Exclusiveness, Cash}) when
    Exclusiveness == exclusive; Exclusiveness == inclusive
->
    {Exclusiveness, hg_cash:unmarshal([1, Cash])}.
