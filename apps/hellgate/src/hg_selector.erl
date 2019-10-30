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
    dmsl_domain_thrift:'CashValueSelector'() |
    dmsl_domain_thrift:'CumulativeLimitSelector'() |
    dmsl_domain_thrift:'TimeSpanSelector'() |
    dmsl_domain_thrift:'P2PProviderSelector'().

-type value() ::
    _. %% FIXME

-type varset() :: #{
    category        => dmsl_domain_thrift:'CategoryRef'(),
    currency        => dmsl_domain_thrift:'CurrencyRef'(),
    cost            => dmsl_domain_thrift:'Cash'(),
    payment_tool    => dmsl_domain_thrift:'PaymentTool'(),
    party_id        => dmsl_domain_thrift:'PartyID'(),
    shop_id         => dmsl_domain_thrift:'ShopID'(),
    risk_score      => dmsl_domain_thrift:'RiskScore'(),
    flow            => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()},
    payout_method   => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id       => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    p2p_tool        => dmsl_domain_thrift:'P2PTool'()
}.

-type predicate() :: dmsl_domain_thrift:'Predicate'().

-export_type([varset/0]).

-export([fold/3]).
-export([collect/1]).
-export([reduce/3]).
-export([reduce_to_value/3]).
-export([reduce_predicate/3]).

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


-spec reduce_to_value(t(), varset(), hg_domain:revision()) -> value() | no_return().

reduce_to_value(Selector, VS, Revision) ->
    case reduce(Selector, VS, Revision) of
        {value, Value} ->
            Value;
        _ ->
            error({misconfiguration, {'Can\'t reduce selector to value', Selector, VS, Revision}})
    end.

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

-spec reduce_predicate(predicate(), varset(), hg_domain:revision()) ->
    predicate().

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-spec test() -> _.

-spec p2p_provider_test() -> _.
p2p_provider_test() ->
    BankCardCondition = #domain_BankCardCondition{definition = {issuer_country_is, rus}},
    BankCardCondition2 = #domain_BankCardCondition{definition = {issuer_country_is, usa}},
    P2PCondition1 = #domain_P2PToolCondition{
        sender_is = {bank_card, BankCardCondition},
        receiver_is = {bank_card, BankCardCondition}
    },
    P2PCondition2 = #domain_P2PToolCondition{
        sender_is = {payment_tool, {bank_card, BankCardCondition}},
        receiver_is = {payment_tool, {bank_card, BankCardCondition2}}
    },
    P2PProviderSelector = {decisions, [
        #domain_P2PProviderDecision{
            if_ = {condition, {p2p_tool, P2PCondition1}},
            then_ = {value, [#domain_ProviderRef{id = 1}]}
        },
        #domain_P2PProviderDecision{
            if_ = {condition, {p2p_tool, P2PCondition2}},
            then_ = {value, [#domain_ProviderRef{id = 2}]}
        }
    ]},
    BankCard1 = #domain_BankCard{
        token          = <<"TOKEN1">>,
        payment_system = mastercard,
        bin            = <<"888888">>,
        masked_pan     = <<"888">>,
        issuer_country = rus
    },
    BankCard2 = #domain_BankCard{
        token          = <<"TOKEN2">>,
        payment_system = mastercard,
        bin            = <<"777777">>,
        masked_pan     = <<"777">>,
        issuer_country = rus
    },
    Vs = #{
        p2p_tool => #domain_P2PTool{
            sender   = {bank_card, BankCard1},
            receiver = {bank_card, BankCard2}
        }
    },
    ?assertEqual([{domain_ProviderRef, 1}], reduce_to_value(P2PProviderSelector, Vs, 1)).

-spec p2p_allow_test() -> _.
p2p_allow_test() ->
    FunGenCard = fun(PS, Country) -> #domain_BankCard{
        token          = <<"TOKEN1">>,
        payment_system = PS,
        bin            = <<"888888">>,
        masked_pan     = <<"888">>,
        issuer_country = Country}
    end,
    FunGenVS = fun(PS1, PS2) -> #{p2p_tool => #domain_P2PTool{
            sender   = {bank_card, FunGenCard(PS1, rus)},
            receiver = {bank_card, FunGenCard(PS2, rus)}
        }}
    end,
    Condition = #domain_BankCardCondition{definition = {payment_system_is, visa}},
    CardCondition1 = #domain_P2PToolCondition{
        sender_is = {bank_card, Condition},
        receiver_is = {bank_card, Condition}
    },
    Predicate = {any_of, [{condition, {p2p_tool, CardCondition1}}]},
    VS1 = FunGenVS(nspkmir, visa),
    Allow = reduce_predicate(Predicate, VS1, 1),
    ?assertEqual({constant, false}, Allow),

    VS2 = FunGenVS(visa, visa),
    Allow2 = reduce_predicate(Predicate, VS2, 1),
    ?assertEqual({constant, true}, Allow2).

-endif.
