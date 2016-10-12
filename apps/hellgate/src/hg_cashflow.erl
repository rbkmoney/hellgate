%%% Cash flow computations
%%%
%%% TODO
%%%  - reduction raises suspicions
%%%     - it's not a bijection, therefore there's no definitive way to map the
%%%       set of postings to the original cash flow
%%%     - should we consider posting with the same source and destination invalid?

-module(hg_cashflow).
-include_lib("hg_proto/include/hg_domain_thrift.hrl").

-type t() :: hg_domain_thrift:'CashFlow'().

-type amount() :: hg_domain_thrift:'Amount'().
-type context() :: hg_domain_thrift:'CashFlowContext'().
-type accounts() :: #{_ => _}. %% FIXME
-type posting() :: {Source :: _, Destination :: _, amount()}. %% FIXME

-export_type([t/0]).

%%

-export([join/2]).
-export([compute/3]).

%%

-spec join(t(), t()) ->
    t().

join(CF1, CF2) ->
    CF1 ++ CF2.

-spec compute(t(), context(), accounts()) ->
    {ok, [posting()]} | {error, _}. %% FIXME

compute(CF, Context, Accounts) ->
    try {ok, reduce_postings(compute_postings(CF, Context, Accounts))} catch
        Reason ->
            {error, Reason}
    end.

compute_postings(CF, Context, Accounts) ->
    [compute_posting(Posting, Context, Accounts) || Posting <- CF].

compute_posting(Posting, Context, Accounts) ->
    Source = resolve_account(Posting#domain_CashFlowPosting.source, Accounts),
    Destination = resolve_account(Posting#domain_CashFlowPosting.destination, Accounts),
    Amount = compute_amount(Posting#domain_CashFlowPosting.volume, Context),
    {Source, Destination, Amount}.

resolve_account(Account, Accounts) ->
    case Accounts of
        #{Account := V} ->
            V;
        #{} ->
            throw({account_not_found, Account})
    end.

compute_amount({fixed, #domain_CashVolumeFixed{amount = Amount}}, _Context) ->
    Amount;
compute_amount({share, #domain_CashVolumeShare{parts = Parts, 'of' = Of}}, Context) ->
    compute_parts_of(Parts, resolve_constant(Of, Context)).

resolve_constant(Constant, Context) ->
    case Context of
        #{Constant := V} ->
            V;
        #{} ->
            throw({constant_not_found, Constant})
    end.

compute_parts_of(#'Rational'{p = P, q = Q}, Amount) ->
    hg_rational:to_integer(hg_rational:mul(hg_rational:new(Amount), hg_rational:new(P, Q))).

reduce_postings([Posting | Rest0]) ->
    {Ps, Rest} = reduce_posting(Posting, Rest0),
    Ps ++ reduce_postings(Rest);
reduce_postings([]) ->
    [].

reduce_posting({Source, Source, _}, Rest) ->
    % Posting where source and destination are the same is redundant.
    {[], Rest};
reduce_posting({Source, Destination, Amount}, Rest) ->
    % Postings with the same source and destination pairs are the same should be
    % accumulated together.
    {Ps, Rest1} = lists:splitwith(fun (S, D, _) -> S == Source andalso D == Destination end, Rest),
    AmountCumulative = lists:foldl(fun ({_, _, A}, Acc) -> A + Acc end, Amount, Ps),
    {[{Source, Destination, AmountCumulative}], Rest1}.
