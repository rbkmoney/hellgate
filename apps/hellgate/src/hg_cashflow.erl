%%% Cash flow computations
%%%
%%% TODO
%%%  - reduction raises suspicions
%%%     - it's not a bijection, therefore there's no definitive way to map the
%%%       set of postings to the original cash flow
%%%     - should we consider posting with the same source and destination invalid?

-module(hg_cashflow).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type amount() :: dmsl_domain_thrift:'Amount'().
-type currency() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type context() :: dmsl_domain_thrift:'CashFlowContext'().
-type account() :: dmsl_domain_thrift:'CashFlowAccount'().

-type posting(A, V, C) :: {A, A, V, C}.

-type t() :: [posting(account(), amount(), currency())].

-export_type([t/0]).
-export_type([account/0]).

%%

-export([compute/3]).

%%

-spec compute(dmsl_domain_thrift:'CashFlow'(), dmsl_domain_thrift:'CurrencyRef'(), context()) ->
    t() | no_return().

compute(CF, CurrencyRef, Context) ->
    Currency = CurrencyRef#domain_CurrencyRef.symbolic_code,
    try splice_postings(compute_postings(CF, Currency, Context)) catch
        Reason ->
            error(Reason) % FIXME
    end.

-define(posting(Source, Destination, Volume),
    #domain_CashFlowPosting{source = Source, destination = Destination, volume = Volume}).
-define(fixed(Amount),
    {fixed, #domain_CashVolumeFixed{amount = Amount}}).
-define(rational(P, Q),
    #'Rational'{p = P, q = Q}).
-define(share(P, Q, Of),
    {share, #domain_CashVolumeShare{'parts' = ?rational(P, Q), 'of' = Of}}).

compute_postings(CF, Currency, Context) ->
    [
        {Source, Destination, compute_amount(Volume, Context), Currency} ||
            ?posting(Source, Destination, Volume) <- CF
    ].

compute_amount(?fixed(Amount), _Context) ->
    Amount;
compute_amount(?share(P, Q, Of), Context) ->
    compute_parts_of(P, Q, resolve_constant(Of, Context)).

compute_parts_of(P, Q, Amount) ->
    hg_rational:to_integer(hg_rational:mul(hg_rational:new(Amount), hg_rational:new(P, Q))).

resolve_constant(Constant, Context) ->
    case Context of
        #{Constant := V} ->
            V;
        #{} ->
            throw({constant_not_found, Constant})
    end.

splice_postings([Posting | Rest0]) ->
    {Ps, Rest} = splice_posting(Posting, Rest0),
    Ps ++ splice_postings(Rest);
splice_postings([]) ->
    [].

splice_posting({S0, D0, A0, C0}, Rest) ->
    % Postings with the same source and destination pairs should be accumulated
    % together.
    {Ps, Rest1} = lists:splitwith(fun ({S, D, _, C}) -> S == S0 andalso D == D0 andalso C == C0 end, Rest),
    ASum = lists:foldl(fun ({_, _, A, _}, Acc) -> A + Acc end, A0, Ps),
    {[{S0, D0, ASum, C0}], Rest1}.
