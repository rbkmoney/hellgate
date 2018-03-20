-module(hg_cash_range).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include("domain.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-export([test_inclusive_intersection/2]).

-type cash_range() :: dmsl_domain_thrift:'CashRange'().

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


-spec test_inclusive_intersection(cash_range(), cash_range()) ->
    true | false.

test_inclusive_intersection(
    #domain_CashRange{lower = Lower1, upper = Upper1},
    #domain_CashRange{lower = Lower2, upper = Upper2}
) ->
    case {
        test_cash_bound(fun erlang:'>'/2, Lower1, Lower2),
        test_cash_bound(fun erlang:'<'/2, Upper1, Upper2)
    } of
        {true, true} ->
            true;
        {_, _} ->
            false
    end.

test_cash_bound(_, {_, Cash}, {inclusive, Cash}) ->
    true;
test_cash_bound(_, {exclusive, Cash}, {exclusive, Cash}) ->
    true;
test_cash_bound(F, {_, ?cash(Amount1, Currency)}, {_, ?cash(Amount2, Currency)}) ->
    F(Amount1, Amount2);
test_cash_bound(_, _, _) ->
    false.