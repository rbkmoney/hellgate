%%% Domain selectors manipulation
%%%
%%% TODO
%%%  - Manipulating predicates w/o respect to their struct infos is dangerous
%%%  - Decide on semantics
%%%     - First satisfiable predicate wins?
%%%       If not, it would be harder to join / overlay selectors
%%%  - Domain revision is out of place. An `Opts`, anyone?

-module(hg_selector).

%%

-type t() ::
    dmsl_domain_thrift:'CashLimitSelector'() |
    dmsl_domain_thrift:'CashFlowSelector'() |
    dmsl_domain_thrift:'PaymentMethodSelector'() |
    dmsl_domain_thrift:'ProviderSelector'() |
    dmsl_domain_thrift:'TerminalSelector'() |
    dmsl_domain_thrift:'SystemAccountSetSelector'() |
    dmsl_domain_thrift:'ExternalAccountSetSelector'().

-type value() ::
    _. %% FIXME

-type varset() :: #{
    %% TODO
}.

-export([fold/3]).
-export([collect/1]).
-export([reduce/3]).

%%

-spec fold(FoldWith :: fun((Value :: _, Acc) -> Acc), Acc, t()) ->
    Acc when
        Acc :: term().

fold(FoldWith, Acc, {value, V}) ->
    FoldWith(V, Acc);
fold(FoldWith, Acc, {decisions, Ps}) ->
    fold_decisions(FoldWith, Acc, Ps).

fold_decisions(FoldWith, Acc, [{_Type, _, S} | Rest]) ->
    fold_decisions(FoldWith, fold(FoldWith, Acc, S), Rest);
fold_decisions(_, Acc, []) ->
    Acc.

-spec collect(t()) ->
    [value()].

collect(S) ->
    fold(fun (V, Acc) -> [V | Acc] end, [], S).

-spec reduce(t(), varset(), hg_domain:revision()) ->
    t().

reduce({value, _} = V, _, _) ->
    V;
reduce({decisions, Ps}, VS, Rev) ->
    case reduce_decisions(Ps, VS, Rev) of
        [{_Type, true, S} | _] ->
            S;
        Ps1 ->
            {decisions, Ps1}
    end;
reduce({predicates, Ps}, VS, Rev) ->
    reduce({decisions, Ps}, VS, Rev).


reduce_decisions([{Type, V, S} | Rest], VS, Rev) ->
    case reduce_predicate(V, VS, Rev) of
        false ->
            reduce_decisions(Rest, VS, Rev);
        V1 ->
            case reduce(S, VS, Rev) of
                {decisions, []} ->
                    reduce_decisions(Rest, VS, Rev);
                S1 ->
                    [{Type, V1, S1} | reduce_decisions(Rest, VS, Rev)]
            end
    end;
reduce_decisions([], _, _) ->
    [].

reduce_predicate({condition, C0}, VS, Rev) ->
    case reduce_condition(C0, VS, Rev) of
        B when is_boolean(B) ->
            B;
        C1 ->
            {condition, C1}
    end;

reduce_predicate({is_not, P0}, VS, Rev) ->
    case reduce_predicate(P0, VS, Rev) of
        B when is_boolean(B) ->
            not B;
        P1 ->
            {is_not, P1}
    end;

reduce_predicate({all_of, Ps}, VS, Rev) ->
    reduce_combination(false, Ps, VS, Rev, []);

reduce_predicate({any_of, Ps}, VS, Rev) ->
    reduce_combination(true, Ps, VS, Rev, []).

reduce_combination(Fix, [P | Ps], VS, Rev, PAcc) ->
    case reduce_predicate(P, VS, Rev) of
        Fix ->
            Fix;
        B when is_boolean(B) ->
            reduce_combination(Fix, Ps, VS, Rev, PAcc);
        P1 ->
            reduce_combination(Fix, Ps, VS, Rev, [P1 | PAcc])
    end;
reduce_combination(_, [], _, _, []) ->
    false;
reduce_combination(_, [], _, _, PAcc) ->
    PAcc.

reduce_condition({category_is, V1}, #{category := V2}, _) ->
    V1 =:= V2;
reduce_condition({currency_is, V1}, #{currency := V2}, _) ->
    V1 =:= V2;
reduce_condition({payment_tool_condition, C}, #{payment_tool := V}, Rev) ->
    hg_payment_tool:test_condition(C, V, Rev);
reduce_condition(C, #{}, _) ->
    % Irreducible, return as is
    C.
