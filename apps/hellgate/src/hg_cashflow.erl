%%% Cash flow computations
%%%
%%% TODO
%%%  - reduction raises suspicions
%%%     - should we consider posting with the same source and destination invalid?
%%%     - did we get rid of splicing for good?
%%%  - we should probably validate final cash flow somewhere here

-module(hg_cashflow).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type account() :: dmsl_domain_thrift:'CashFlowAccount'().
-type account_id() :: dmsl_domain_thrift:'AccountID'().
-type account_map() :: #{account() => account_id()}.
-type context() :: dmsl_domain_thrift:'CashFlowContext'().
-type cash_flow() :: dmsl_domain_thrift:'CashFlow'().
-type cash_flow_posting() :: dmsl_domain_thrift:'CashFlowPosting'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type cash_volume() :: dmsl_domain_thrift:'CashVolume'().
-type final_cash_flow_account() :: dmsl_domain_thrift:'FinalCashFlowAccount'().

-type shop() :: dmsl_domain_thrift:'Shop'().
-type party() :: hg_party:party().
-type route() :: dmsl_domain_thrift:'PaymentRoute'().
-type posting_context() :: #{
    shop := shop(),
    party := party(),
    route := route()
}.

%%

-export([finalize/3]).
-export([revert/1]).
-export([add_cashflow_posting_context/4]).

-export([compute_volume/2]).

-export([get_partial_remainders/1]).

-export([marshal/1]).
-export([unmarshal/1]).

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

-spec compute_postings(cash_flow(), context(), account_map()) -> final_cash_flow() | no_return().
compute_postings(CF, Context, AccountMap) ->
    [
        ?final_posting(
            construct_final_account(Source, AccountMap, PostingContext),
            construct_final_account(Destination, AccountMap, PostingContext),
            compute_volume(Volume, Context),
            Details
        )
        || {?posting(Source, Destination, Volume, Details), PostingContext} <- CF
    ].

-spec construct_final_account(account(), account_map(), posting_context()) -> final_cash_flow_account() | no_return().
construct_final_account(AccountType, AccountMap, PostingContext) ->
    #domain_FinalCashFlowAccount{
        account_type = AccountType,
        account_id = resolve_account(AccountType, AccountMap),
        transaction_account = construct_transaction_account(AccountType, PostingContext)
    }.

construct_transaction_account({merchant, MerchantFlowAccount}, #{party := Party, shop := Shop}) ->
    #domain_Party{id = PartyID} = Party,
    #domain_Shop{id = ShopID} = Shop,
    AccountOwner = #domain_MerchantTransactionAccountOwner{
        party_id = PartyID,
        shop_id = ShopID
    },
    {merchant, #domain_MerchantTransactionAccount{
        type = MerchantFlowAccount,
        owner = AccountOwner
    }};
construct_transaction_account({provider, ProviderFlowAccount}, #{route := Route}) ->
    #domain_PaymentRoute{
        provider = ProviderRef,
        terminal = TerminalRef
    } = Route,
    AccountOwner = #domain_ProviderTransactionAccountOwner{
        provider_ref = ProviderRef,
        terminal_ref = TerminalRef
    },
    {provider, #domain_ProviderTransactionAccount{
        type = ProviderFlowAccount,
        owner = AccountOwner
    }};
construct_transaction_account({system, SystemFlowAccount}, _) ->
    {system, #domain_SystemTransactionAccount{
        type = SystemFlowAccount
    }};
construct_transaction_account({external, ExternalFlowAccount}, _) ->
    {external, #domain_ExternalTransactionAccount{
        type = ExternalFlowAccount
    }}.

-spec resolve_account(account(), account_map()) -> account_id() | no_return().
resolve_account(AccountType, AccountMap) ->
    case AccountMap of
        #{AccountType := V} ->
            V;
        #{} ->
            error({misconfiguration, {'Cash flow account can not be mapped', {AccountType, AccountMap}}})
    end.

%%

-spec revert(final_cash_flow()) -> final_cash_flow().
revert(CF) ->
    [
        ?final_posting(Destination, Source, Volume, revert_details(Details))
        || ?final_posting(Source, Destination, Volume, Details) <- CF
    ].

revert_details(undefined) ->
    undefined;
revert_details(Details) ->
    % TODO looks gnarly
    <<"Revert '", Details/binary, "'">>.

-spec add_cashflow_posting_context(cash_flow(), party(), shop(), route()) -> [{cash_flow_posting(), posting_context()}].
add_cashflow_posting_context(CashFlow, Party, Shop, Route) ->
    Context = #{
        party => Party,
        shop => Shop,
        route => Route
    },
    lists:map(fun(A) -> {A, Context} end, CashFlow).

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

-spec compute_volume(cash_volume(), context()) -> cash() | no_return().
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

%%

-include("domain.hrl").

-spec get_partial_remainders(final_cash_flow()) -> #{account() => cash()}.
get_partial_remainders(CashFlow) ->
    lists:foldl(
        fun(?final_posting(Source, Destination, Volume, _), Acc) ->
            decrement_remainder(Source, Volume, increment_remainder(Destination, Volume, Acc))
        end,
        #{},
        CashFlow
    ).

increment_remainder(AccountType, Cash, Acc) ->
    modify_remainder(AccountType, Cash, Acc).

decrement_remainder(AccountType, ?cash(Amount, Currency), Acc) ->
    modify_remainder(AccountType, ?cash(-Amount, Currency), Acc).

modify_remainder(#domain_FinalCashFlowAccount{account_type = AccountType}, ?cash(Amount, Currency), Acc) ->
    maps:update_with(
        AccountType,
        fun(?cash(A, C)) when C == Currency ->
            ?cash(A + Amount, Currency)
        end,
        ?cash(Amount, Currency),
        Acc
    ).

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(final_cash_flow()) -> hg_msgpack_marshalling:value().
marshal(CashFlow) ->
    marshal(final_cash_flow, CashFlow).

marshal(final_cash_flow, CashFlow) ->
    [2, [marshal(final_cash_flow_posting, CashFlowPosting) || CashFlowPosting <- CashFlow]];
marshal(final_cash_flow_posting, #domain_FinalCashFlowPosting{} = CashFlowPosting) ->
    genlib_map:compact(#{
        <<"source">> =>
            marshal(final_cash_flow_account, CashFlowPosting#domain_FinalCashFlowPosting.source),
        <<"destination">> =>
            marshal(final_cash_flow_account, CashFlowPosting#domain_FinalCashFlowPosting.destination),
        <<"volume">> =>
            hg_cash:marshal(CashFlowPosting#domain_FinalCashFlowPosting.volume),
        <<"details">> =>
            marshal(str, CashFlowPosting#domain_FinalCashFlowPosting.details)
    });
marshal(final_cash_flow_account, #domain_FinalCashFlowAccount{} = CashFlowAccount) ->
    #{
        <<"type">> =>
            marshal(account_type, CashFlowAccount#domain_FinalCashFlowAccount.account_type),
        <<"id">> =>
            marshal(str, CashFlowAccount#domain_FinalCashFlowAccount.account_id)
    };
marshal(account_type, {merchant, settlement}) ->
    [<<"merchant">>, <<"settlement">>];
marshal(account_type, {merchant, guarantee}) ->
    [<<"merchant">>, <<"guarantee">>];
marshal(account_type, {provider, settlement}) ->
    [<<"provider">>, <<"settlement">>];
marshal(account_type, {system, settlement}) ->
    [<<"system">>, <<"settlement">>];
marshal(account_type, {system, subagent}) ->
    [<<"system">>, <<"subagent">>];
marshal(account_type, {external, income}) ->
    [<<"external">>, <<"income">>];
marshal(account_type, {external, outcome}) ->
    [<<"external">>, <<"outcome">>];
marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) -> final_cash_flow().
unmarshal(CashFlow) ->
    unmarshal(final_cash_flow, CashFlow).

unmarshal(final_cash_flow, [_, CashFlow]) ->
    [unmarshal(final_cash_flow_posting, CashFlowPosting) || CashFlowPosting <- CashFlow];
unmarshal(
    final_cash_flow_posting,
    #{
        <<"source">> := Source,
        <<"destination">> := Destination,
        <<"volume">> := Volume
    } = CashFlow
) ->
    Details = maps:get(<<"details">>, CashFlow, undefined),
    #domain_FinalCashFlowPosting{
        source = unmarshal(final_cash_flow_account, Source),
        destination = unmarshal(final_cash_flow_account, Destination),
        volume = hg_cash:unmarshal(Volume),
        details = unmarshal(str, Details)
    };
unmarshal(
    final_cash_flow_posting,
    ?legacy_final_cash_flow_posting(Source, Destination, Volume, Details)
) ->
    #domain_FinalCashFlowPosting{
        source = unmarshal(final_cash_flow_account, Source),
        destination = unmarshal(final_cash_flow_account, Destination),
        volume = hg_cash:unmarshal([1, Volume]),
        details = unmarshal(str, Details)
    };
unmarshal(final_cash_flow_account, #{
    <<"type">> := AccountType,
    <<"id">> := AccountId
}) ->
    #domain_FinalCashFlowAccount{
        account_type = unmarshal(account_type, AccountType),
        account_id = unmarshal(str, AccountId)
    };
unmarshal(
    final_cash_flow_account,
    ?legacy_final_cash_flow_account(AccountType, AccountId)
) ->
    #domain_FinalCashFlowAccount{
        account_type = unmarshal(account_type, AccountType),
        account_id = unmarshal(str, AccountId)
    };
unmarshal(account_type, [<<"merchant">>, <<"settlement">>]) ->
    {merchant, settlement};
unmarshal(account_type, [<<"merchant">>, <<"guarantee">>]) ->
    {merchant, guarantee};
unmarshal(account_type, [<<"provider">>, <<"settlement">>]) ->
    {provider, settlement};
unmarshal(account_type, [<<"system">>, <<"settlement">>]) ->
    {system, settlement};
unmarshal(account_type, [<<"system">>, <<"subagent">>]) ->
    {system, subagent};
unmarshal(account_type, [<<"external">>, <<"income">>]) ->
    {external, income};
unmarshal(account_type, [<<"external">>, <<"outcome">>]) ->
    {external, outcome};
unmarshal(account_type, {AccType1, AccType2} = AccType) when is_atom(AccType1), is_atom(AccType2) ->
    AccType;
unmarshal(_, Other) ->
    Other.
