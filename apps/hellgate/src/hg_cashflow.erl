%%% Cash flow computations
%%%
%%% TODO
%%%  - reduction raises suspicions
%%%     - it's not a bijection, therefore there's no definitive way to map the
%%%       set of postings to the original cash flow
%%%     - should we consider posting with the same source and destination invalid?

-module(hg_cashflow).
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-type t() :: dmsl_domain_thrift:'CashFlow'().

% -type amount() :: dmsl_domain_thrift:'Amount'().
-type context() :: dmsl_domain_thrift:'CashFlowContext'().
% -type accounts() :: #{_ => _}. %% FIXME
% -type posting() :: {Source :: _, Destination :: _, amount()}. %% FIXME

-export_type([t/0]).

%%

-export([join/2]).
-export([compute/2]).

%%

-spec join(t(), t()) ->
    t().

join(CF1, CF2) ->
    CF1 ++ CF2.

-spec compute(t(), context()) ->
    % {ok, [posting()]} | {error, _}. %% FIXME
    {ok, t()} | {error, _}. %% FIXME

compute(CF, Context) ->
    try {ok, splice_postings(compute_postings(CF, Context))} catch
        Reason ->
            {error, Reason}
    end.

compute_postings(CF, Context) ->
    [compute_posting(Posting, Context) || Posting <- CF].

compute_posting(Posting, Context) ->
    % Source = resolve_account(Posting#domain_CashFlowPosting.source, Accounts),
    % Destination = resolve_account(Posting#domain_CashFlowPosting.destination, Accounts),
    Amount = compute_amount(Posting#domain_CashFlowPosting.volume, Context),
    % {Source, Destination, Amount}.
    Posting#domain_CashFlowPosting{volume = construct_volume(Amount)}.

% resolve_account(Account, Accounts) ->
%     case Accounts of
%         #{Account := V} ->
%             V;
%         #{} ->
%             throw({account_not_found, Account})
%     end.

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

splice_postings([Posting | Rest0]) ->
    {Ps, Rest} = splice_posting(Posting, Rest0),
    Ps ++ splice_postings(Rest);
splice_postings([]) ->
    [].

splice_posting(
    Posting = #domain_CashFlowPosting{source = Source, destination = Destination, volume = Volume},
    Rest
) ->
    % Postings with the same source and destination pairs should be accumulated
    % together.
    {Ps, Rest1} = lists:splitwith(
        fun (#domain_CashFlowPosting{source = S, destination = D}) ->
            S == Source andalso D == Destination
        end,
        Rest
    ),
    AmountCumulative = lists:foldl(
        fun (#domain_CashFlowPosting{volume = V}, Acc) -> get_amount(V) + Acc end,
        get_amount(Volume), Ps
    ),
    {[Posting#domain_CashFlowPosting{volume = construct_volume(AmountCumulative)}], Rest1}.

get_amount({fixed, #domain_CashVolumeFixed{amount = Amount}}) ->
    Amount.

construct_volume(Amount) ->
    {fixed, #domain_CashVolumeFixed{amount = Amount}}.
