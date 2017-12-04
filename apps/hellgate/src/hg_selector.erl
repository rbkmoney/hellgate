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
    dmsl_domain_thrift:'CurrencySelector'() |
    dmsl_domain_thrift:'CategorySelector'() |
    dmsl_domain_thrift:'CashLimitSelector'() |
    dmsl_domain_thrift:'CashFlowSelector'() |
    dmsl_domain_thrift:'PaymentMethodSelector'() |
    dmsl_domain_thrift:'ProviderSelector'() |
    dmsl_domain_thrift:'TerminalSelector'() |
    dmsl_domain_thrift:'SystemAccountSetSelector'() |
    dmsl_domain_thrift:'ExternalAccountSetSelector'() |
    dmsl_domain_thrift:'HoldLifetimeSelector'() |
    dmsl_domain_thrift:'CashValueSelector'().

-type value() ::
    _. %% FIXME

-type varset() :: #{
    residence   => dmsl_domain_thrift:'Residence'(),
    category    => dmsl_domain_thrift:'CategoryRef'(),
    currency    => dmsl_domain_thrift:'CurrencyRef'(),
    cost        => dmsl_domain_thrift:'Cash'(),
    payment_tool=> dmsl_domain_thrift:'PaymentTool'(),
    party       => dmsl_domain_thrift:'Party'(),
    shop        => dmsl_domain_thrift:'Shop'(),
    risk_score  => dmsl_domain_thrift:'RiskScore'(),
    flow        => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()}
}.

-export_type([varset/0]).

-export([fold/3]).
-export([collect/1]).
-export([reduce/3]).

-define(const(Bool), {constant, Bool}).

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
        [{_Type, ?const(true), S} | _] ->
            S;
        Ps1 ->
            {decisions, Ps1}
    end.

reduce_decisions([{Type, V, S} | Rest], VS, Rev) ->
    case reduce_predicate(V, VS, Rev) of
        ?const(false) ->
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

reduce_predicate(?const(B), _, _) ->
    ?const(B);

reduce_predicate({condition, C0}, VS, Rev) ->
    case reduce_condition(C0, VS, Rev) of
        ?const(B) ->
            ?const(B);
        C1 ->
            {condition, C1}
    end;

reduce_predicate({is_not, P0}, VS, Rev) ->
    case reduce_predicate(P0, VS, Rev) of
        ?const(B) ->
            ?const(not B);
        P1 ->
            {is_not, P1}
    end;

reduce_predicate({all_of, Ps}, VS, Rev) ->
    reduce_combination(all_of, false, Ps, VS, Rev, []);

reduce_predicate({any_of, Ps}, VS, Rev) ->
    reduce_combination(any_of, true, Ps, VS, Rev, []).

reduce_combination(Type, Fix, [P | Ps], VS, Rev, PAcc) ->
    case reduce_predicate(P, VS, Rev) of
        ?const(Fix) ->
            ?const(Fix);
        ?const(_) ->
            reduce_combination(Type, Fix, Ps, VS, Rev, PAcc);
        P1 ->
            reduce_combination(Type, Fix, Ps, VS, Rev, [P1 | PAcc])
    end;
reduce_combination(_, Fix, [], _, _, []) ->
    ?const(not Fix);
reduce_combination(Type, _, [], _, _, PAcc) ->
    {Type, lists:reverse(PAcc)}.

reduce_condition(C, VS, Rev) ->
    case hg_condition:test(C, VS, Rev) of
        B when is_boolean(B) ->
            ?const(B);
        undefined ->
            % Irreducible, return as is
            C
    end.
