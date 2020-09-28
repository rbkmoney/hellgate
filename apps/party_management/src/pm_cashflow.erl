%%% Cash flow computations
%%%
%%% TODO
%%%  - reduction raises suspicions
%%%     - should we consider posting with the same source and destination invalid?
%%%     - did we get rid of splicing for good?
%%%  - we should probably validate final cash flow somewhere here

-module(pm_cashflow).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type account() :: dmsl_domain_thrift:'CashFlowAccount'().
-type account_id() :: dmsl_domain_thrift:'AccountID'().
-type account_map() :: #{account() => account_id()}.
-type context() :: dmsl_domain_thrift:'CashFlowContext'().
-type cash_flow() :: dmsl_domain_thrift:'CashFlow'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().

%%

-export([finalize/3]).

%%

-define(posting(Source, Destination, Volume, Details), #domain_CashFlowPosting{
    source = Source,
    destination = Destination,
    volume = Volume,
    details = Details
}).

-define(final_posting(Source, Destination, Volume, Details), #domain_FinalCashFlowPosting{
    source = Source,
    destination = Destination,
    volume = Volume,
    details = Details
}).

-spec finalize(cash_flow(), context(), account_map()) -> final_cash_flow() | no_return().
finalize(CF, Context, AccountMap) ->
    compute_postings(CF, Context, AccountMap).

compute_postings(CF, Context, AccountMap) ->
    [
        ?final_posting(
            construct_final_account(Source, AccountMap),
            construct_final_account(Destination, AccountMap),
            compute_volume(Volume, Context),
            Details
        )
        || ?posting(Source, Destination, Volume, Details) <- CF
    ].

construct_final_account(AccountType, AccountMap) ->
    #domain_FinalCashFlowAccount{
        account_type = AccountType,
        account_id = resolve_account(AccountType, AccountMap)
    }.

resolve_account(AccountType, AccountMap) ->
    case AccountMap of
        #{AccountType := V} ->
            V;
        #{} ->
            error({misconfiguration, {'Cash flow account can not be mapped', {AccountType, AccountMap}}})
    end.

%%

-define(fixed(Cash),
    {fixed, #domain_CashVolumeFixed{cash = Cash}}
).

-define(share(P, Q, Of, RoundingMethod),
    {share, #domain_CashVolumeShare{'parts' = ?rational(P, Q), 'of' = Of, 'rounding_method' = RoundingMethod}}
).

-define(product(Fun, CVs),
    {product, {Fun, CVs}}
).

-define(rational(P, Q), #'Rational'{p = P, q = Q}).

compute_volume(?fixed(Cash), _Context) ->
    Cash;
compute_volume(?share(P, Q, Of, RoundingMethod), Context) ->
    compute_parts_of(P, Q, resolve_constant(Of, Context), RoundingMethod);
compute_volume(?product(Fun, CVs) = CV0, Context) ->
    case ordsets:size(CVs) of
        N when N > 0 ->
            compute_product(Fun, ordsets:to_list(CVs), CV0, Context);
        0 ->
            error({misconfiguration, {'Cash volume product over empty set', CV0}})
    end.

compute_parts_of(P, Q, Cash = #domain_Cash{amount = Amount}, RoundingMethod) ->
    Cash#domain_Cash{
        amount = genlib_rational:round(
            genlib_rational:mul(
                genlib_rational:new(Amount),
                genlib_rational:new(P, Q)
            ),
            get_rounding_method(RoundingMethod)
        )
    }.

compute_product(Fun, [CV | CVRest], CV0, Context) ->
    lists:foldl(
        fun(CVN, CVMin) -> compute_product(Fun, CVN, CVMin, CV0, Context) end,
        compute_volume(CV, Context),
        CVRest
    ).

compute_product(Fun, CV, CVMin = #domain_Cash{amount = AmountMin, currency = Currency}, CV0, Context) ->
    case compute_volume(CV, Context) of
        #domain_Cash{amount = Amount, currency = Currency} ->
            CVMin#domain_Cash{amount = compute_product_fun(Fun, AmountMin, Amount)};
        _ ->
            error({misconfiguration, {'Cash volume product over volumes of different currencies', CV0}})
    end.

compute_product_fun(min_of, V1, V2) ->
    erlang:min(V1, V2);
compute_product_fun(max_of, V1, V2) ->
    erlang:max(V1, V2).

resolve_constant(Constant, Context) ->
    case Context of
        #{Constant := V} ->
            V;
        #{} ->
            error({misconfiguration, {'Cash flow constant not found', {Constant, Context}}})
    end.

get_rounding_method(undefined) ->
    round_half_away_from_zero;
get_rounding_method(round_half_towards_zero) ->
    round_half_towards_zero;
get_rounding_method(round_half_away_from_zero) ->
    round_half_away_from_zero.
