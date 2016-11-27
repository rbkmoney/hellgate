%%% Accounting
%%%
%%% TODO
%%%  - Brittle posting id assignment, it should be a level upper, maybe even in
%%%    `hg_cashflow`.
%%%  - Stuff cash flow details in the posting description fields.

-module(hg_accounting).

-export([get_account/1]).
-export([create_account/1]).
-export([create_account/2]).

-export([plan/4]).
-export([commit/3]).
-export([rollback/3]).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-type amount()        :: dmsl_domain_thrift:'Amount'().
-type currency_code() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id()    :: dmsl_accounter_thrift:'AccountID'().
-type plan_id()       :: dmsl_accounter_thrift:'PlanID'().
-type batch_id()      :: dmsl_accounter_thrift:'BatchID'().

-type account() :: #{
    account_id => account_id(),
    own_amount => amount(),
    min_available_amount => amount(),
    currency_code => currency_code()
}.

-spec get_account(AccountID :: integer()) ->
    account().

get_account(AccountID) ->
    try
        Result = call_accounter('GetAccountByID', [AccountID]),
        construct_account(AccountID, Result)
    catch
        {exception, #accounter_AccountNotFound{}} ->
            throw(#payproc_AccountNotFound{})
    end.

-spec create_account(currency_code()) ->
    account_id().

create_account(CurrencyCode) ->
    create_account(CurrencyCode, undefined).

-spec create_account(currency_code(), binary() | undefined) ->
    account_id().

create_account(CurrencyCode, Description) ->
    call_accounter('CreateAccount', [construct_prototype(CurrencyCode, Description)]).

construct_prototype(CurrencyCode, Description) ->
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

%%
-type batch() :: [{batch_id(), hg_cashflow:t()}].
-type accounts_map() :: #{hg_cashflow:account() => account_id()}.
-type accounts_state() :: #{hg_cashflow:account() => account()}.

-spec plan(plan_id(), batch_id(), hg_cashflow:t(), accounts_map()) ->
    accounts_state().

plan(PlanID, BatchID, Cashflow, AccountMap) ->
    do('Hold', construct_plan_change(PlanID, BatchID, Cashflow, AccountMap), AccountMap).

-spec commit(plan_id(), [batch()], accounts_map()) ->
    accounts_state().

commit(PlanID, Batches, AccountMap) ->
    do('CommitPlan', construct_plan(PlanID, Batches, AccountMap), AccountMap).

-spec rollback(plan_id(), [batch()], accounts_map()) ->
    accounts_state().

rollback(PlanID, Batches, AccountMap) ->
    do('RollbackPlan', construct_plan(PlanID, Batches, AccountMap), AccountMap).

do(Op, Plan, AccountMap) ->
    try
        PlanLog = call_accounter(Op, [Plan]),
        collect_accounts_state(PlanLog, AccountMap)
    catch
        Exception ->
            error(Exception) % FIXME
    end.

construct_plan_change(PlanID, BatchID, Cashflow, AccountMap) ->
    #accounter_PostingPlanChange{
        id = PlanID,
        batch = #accounter_PostingBatch{
            id = BatchID,
            postings = collect_postings(Cashflow, AccountMap)
        }
    }.

construct_plan(PlanID, Batches, AccountMap) ->
    #accounter_PostingPlan{
        id    = PlanID,
        batch_list = [
            #accounter_PostingBatch{
                id = BatchID,
                postings = collect_postings(Cashflow, AccountMap)
            }
        || {BatchID, Cashflow} <- Batches]
    }.

collect_postings(Cashflow, AccountMap) ->
    [
        #accounter_Posting{
            from_id           = resolve_account(Source, AccountMap),
            to_id             = resolve_account(Destination, AccountMap),
            amount            = Amount,
            currency_sym_code = CurrencyCode,
            description       = <<>>
        } ||
            {Source, Destination, Amount, CurrencyCode} <-
             Cashflow
    ].

resolve_account(Account, Accounts) ->
    case Accounts of
        #{Account := V} ->
            V;
        #{} ->
            error({account_not_found, Account}) % FIXME
    end.

collect_accounts_state(
    #accounter_PostingPlanLog{affected_accounts = Affected},
    AccountMap
) ->
    maps:map(
        fun (_Account, AccountID) ->
            construct_account(AccountID, maps:get(AccountID, Affected))
        end,
        AccountMap
    ).

%%

construct_account(
    AccountID,
    #accounter_Account{
        own_amount = OwnAmount,
        currency_sym_code = CurrencyCode,
        min_available_amount = MinAvailableAmount
    }
) ->
    #{
        account_id => AccountID,
        own_amount => OwnAmount,
        min_available_amount => MinAvailableAmount,
        currency_code => CurrencyCode
    }.

%%

call_accounter(Function, Args) ->
    hg_woody_wrapper:call('Accounter', Function, Args).
