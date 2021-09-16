%%% Accounting
%%%
%%% TODO
%%%  - Brittle posting id assignment, it should be a level upper, maybe even in
%%%    `hg_cashflow`.
%%%  - Stuff cash flow details in the posting description fields.

-module(hg_accounting).

-export([get_account/1]).
-export([get_balance/1]).
-export([get_balance/2]).
-export([create_account/1]).
-export([create_account/2]).
-export([collect_account_map/6]).
-export([collect_merchant_account_map/2]).
-export([collect_provider_account_map/3]).
-export([collect_system_account_map/4]).
-export([collect_external_account_map/4]).

-export([hold/2]).
-export([plan/2]).
-export([commit/2]).
-export([rollback/2]).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-type amount() :: dmsl_domain_thrift:'Amount'().
-type currency_code() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id() :: dmsl_accounter_thrift:'AccountID'().
-type plan_id() :: dmsl_accounter_thrift:'PlanID'().
-type batch_id() :: dmsl_accounter_thrift:'BatchID'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().
-type batch() :: {batch_id(), final_cash_flow()}.
-type clock() :: shumpune_shumpune_thrift:'Clock'().

-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type shop() :: dmsl_domain_thrift:'Shop'().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type provider() :: dmsl_domain_thrift:'Provider'().
-type varset() :: hg_varset:varset().
-type revision() :: hg_domain:revision().

-export_type([batch/0]).

-type account() :: #{
    account_id => account_id(),
    currency_code => currency_code()
}.

-type balance() :: #{
    account_id => account_id(),
    own_amount => amount(),
    min_available_amount => amount(),
    max_available_amount => amount()
}.

-spec get_account(account_id()) -> account().
get_account(AccountID) ->
    case call_accounter('GetAccountByID', {AccountID}) of
        {ok, Result} ->
            construct_account(AccountID, Result);
        {exception, #shumpune_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec get_balance(account_id()) -> balance().
get_balance(AccountID) ->
    get_balance(AccountID, {latest, #shumpune_LatestClock{}}).

-spec get_balance(account_id(), clock()) -> balance().
get_balance(AccountID, Clock) ->
    case call_accounter('GetBalanceByID', {AccountID, Clock}) of
        {ok, Result} ->
            construct_balance(AccountID, Result);
        {exception, #shumpune_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec create_account(currency_code()) -> account_id().
create_account(CurrencyCode) ->
    create_account(CurrencyCode, undefined).

-spec create_account(currency_code(), binary() | undefined) -> account_id().
create_account(CurrencyCode, Description) ->
    case call_accounter('CreateAccount', {construct_prototype(CurrencyCode, Description)}) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            % FIXME
            error({accounting, Exception})
    end.

-spec collect_account_map(payment(), shop(), payment_institution(), provider(), varset(), revision()) -> map().
collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision) ->
    Map0 = collect_merchant_account_map(Shop, #{}),
    Map1 = collect_provider_account_map(Payment, Provider, Map0),
    Map2 = collect_system_account_map(Payment, PaymentInstitution, Revision, Map1),
    collect_external_account_map(Payment, VS, Revision, Map2).

-spec collect_merchant_account_map(shop(), map()) -> map().
collect_merchant_account_map(#domain_Shop{account = MerchantAccount}, Acc) ->
    Acc#{
        {merchant, settlement} => MerchantAccount#domain_ShopAccount.settlement,
        {merchant, guarantee} => MerchantAccount#domain_ShopAccount.guarantee
    }.

-spec collect_provider_account_map(payment(), provider(), map()) -> map().
collect_provider_account_map(Payment, #domain_Provider{accounts = ProviderAccounts}, Acc) ->
    Currency = get_currency(get_payment_cost(Payment)),
    ProviderAccount = hg_payment_institution:choose_provider_account(Currency, ProviderAccounts),
    Acc#{
        {provider, settlement} => ProviderAccount#domain_ProviderAccount.settlement
    }.

-spec collect_system_account_map(payment(), payment_institution(), revision(), map()) -> map().
collect_system_account_map(Payment, PaymentInstitution, Revision, Acc) ->
    Currency = get_currency(get_payment_cost(Payment)),
    SystemAccount = hg_payment_institution:get_system_account(Currency, Revision, PaymentInstitution),
    Acc#{
        {system, settlement} => SystemAccount#domain_SystemAccount.settlement,
        {system, subagent} => SystemAccount#domain_SystemAccount.subagent
    }.

-spec collect_external_account_map(payment(), varset(), revision(), map()) -> map().
collect_external_account_map(Payment, VS, Revision, Acc) ->
    Currency = get_currency(get_payment_cost(Payment)),
    case hg_payment_institution:choose_external_account(Currency, VS, Revision) of
        #domain_ExternalAccount{income = Income, outcome = Outcome} ->
            Acc#{
                {external, income} => Income,
                {external, outcome} => Outcome
            };
        undefined ->
            Acc
    end.

construct_prototype(CurrencyCode, Description) ->
    #shumpune_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

%%
-spec plan(plan_id(), [batch()]) -> clock().
plan(_PlanID, []) ->
    error(badarg);
plan(_PlanID, Batches) when not is_list(Batches) ->
    error(badarg);
plan(PlanID, Batches) ->
    lists:foldl(
        fun(Batch, _) -> hold(PlanID, Batch) end,
        undefined,
        Batches
    ).

-spec hold(plan_id(), batch()) -> clock().
hold(PlanID, Batch) ->
    do('Hold', construct_plan_change(PlanID, Batch)).

-spec commit(plan_id(), [batch()]) -> clock().
commit(PlanID, Batches) ->
    do('CommitPlan', construct_plan(PlanID, Batches)).

-spec rollback(plan_id(), [batch()]) -> clock().
rollback(PlanID, Batches) ->
    do('RollbackPlan', construct_plan(PlanID, Batches)).

do(Op, Plan) ->
    case call_accounter(Op, {Plan}) of
        {ok, Clock} ->
            Clock;
        {exception, Exception} ->
            % FIXME
            error({accounting, Exception})
    end.

construct_plan_change(PlanID, {BatchID, Cashflow}) ->
    #shumpune_PostingPlanChange{
        id = PlanID,
        batch = #shumpune_PostingBatch{
            id = BatchID,
            postings = collect_postings(Cashflow)
        }
    }.

construct_plan(PlanID, Batches) ->
    #shumpune_PostingPlan{
        id = PlanID,
        batch_list = [
            #shumpune_PostingBatch{
                id = BatchID,
                postings = collect_postings(Cashflow)
            }
         || {BatchID, Cashflow} <- Batches
        ]
    }.

collect_postings(Cashflow) ->
    [
        #shumpune_Posting{
            from_id = Source,
            to_id = Destination,
            amount = Amount,
            currency_sym_code = CurrencyCode,
            description = construct_posting_description(Details)
        }
     || #domain_FinalCashFlowPosting{
            source = #domain_FinalCashFlowAccount{account_id = Source},
            destination = #domain_FinalCashFlowAccount{account_id = Destination},
            details = Details,
            volume = #domain_Cash{
                amount = Amount,
                currency = #domain_CurrencyRef{symbolic_code = CurrencyCode}
            }
        } <- Cashflow
    ].

construct_posting_description(Details) when is_binary(Details) ->
    Details;
construct_posting_description(undefined) ->
    <<>>.

%%

construct_account(
    AccountID,
    #shumpune_Account{
        currency_sym_code = CurrencyCode
    }
) ->
    #{
        account_id => AccountID,
        currency_code => CurrencyCode
    }.

construct_balance(
    AccountID,
    #shumpune_Balance{
        own_amount = OwnAmount,
        min_available_amount = MinAvailableAmount,
        max_available_amount = MaxAvailableAmount
    }
) ->
    #{
        account_id => AccountID,
        own_amount => OwnAmount,
        min_available_amount => MinAvailableAmount,
        max_available_amount => MaxAvailableAmount
    }.

%%

call_accounter(Function, Args) ->
    hg_woody_wrapper:call(accounter, Function, Args).

get_payment_cost(#domain_InvoicePayment{cost = Cost}) ->
    Cost.

get_currency(#domain_Cash{currency = Currency}) ->
    Currency.
