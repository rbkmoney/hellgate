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

-export([plan/2]).
-export([commit/2]).
-export([rollback/2]).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-type amount()          :: dmsl_domain_thrift:'Amount'().
-type currency_code()   :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id()      :: dmsl_accounter_thrift:'AccountID'().
-type plan_id()         :: dmsl_accounter_thrift:'PlanID'().
-type batch_id()        :: dmsl_accounter_thrift:'BatchID'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().
-type batch()           :: {batch_id(), final_cash_flow()}.

-export_type([batch/0]).

-type account() :: #{
    account_id => account_id(),
    own_amount => amount(),
    min_available_amount => amount(),
    currency_code => currency_code()
}.

-spec get_account(AccountID :: integer()) ->
    account().

get_account(AccountID) ->
    case call_accounter('GetAccountByID', [AccountID]) of
        {ok, Result} ->
            construct_account(AccountID, Result);
        {exception, #accounter_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec create_account(currency_code()) ->
    account_id().

create_account(CurrencyCode) ->
    create_account(CurrencyCode, undefined).

-spec create_account(currency_code(), binary() | undefined) ->
    account_id().

create_account(CurrencyCode, Description) ->
    case call_accounter('CreateAccount', [construct_prototype(CurrencyCode, Description)]) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            error({accounting, Exception}) % FIXME
    end.

construct_prototype(CurrencyCode, Description) ->
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

%%
-type accounts_state() :: #{account_id() => account()}.

-spec plan(plan_id(), batch()) ->
    accounts_state().

plan(PlanID, Batch) ->
    do('Hold', construct_plan_change(PlanID, Batch)).

-spec commit(plan_id(), [batch()]) ->
    accounts_state().

commit(PlanID, Batches) ->
    do('CommitPlan', construct_plan(PlanID, Batches)).

-spec rollback(plan_id(), [batch()]) ->
    accounts_state().

rollback(PlanID, Batches) ->
    do('RollbackPlan', construct_plan(PlanID, Batches)).

do(Op, Plan) ->
    case call_accounter(Op, [Plan]) of
        {ok, PlanLog} ->
            collect_accounts_state(PlanLog);
        {exception, Exception} ->
            error({accounting, Exception}) % FIXME
    end.

construct_plan_change(PlanID, {BatchID, Cashflow}) ->
    #accounter_PostingPlanChange{
        id = PlanID,
        batch = #accounter_PostingBatch{
            id = BatchID,
            postings = collect_postings(Cashflow)
        }
    }.

construct_plan(PlanID, Batches) ->
    #accounter_PostingPlan{
        id    = PlanID,
        batch_list = [
            #accounter_PostingBatch{
                id = BatchID,
                postings = collect_postings(Cashflow)
            }
        || {BatchID, Cashflow} <- Batches]
    }.

collect_postings(Cashflow) ->
    [
        #accounter_Posting{
            from_id           = Source,
            to_id             = Destination,
            amount            = Amount,
            currency_sym_code = CurrencyCode,
            description       = construct_posting_description(Details)
        }
        || #domain_FinalCashFlowPosting{
            source      = #domain_FinalCashFlowAccount{account_id = Source},
            destination = #domain_FinalCashFlowAccount{account_id = Destination},
            details     = Details,
            volume      = #domain_Cash{
                amount      = Amount,
                currency    = #domain_CurrencyRef{symbolic_code = CurrencyCode}
            }
        } <- Cashflow
    ].

construct_posting_description(Details) when is_binary(Details) ->
    Details;
construct_posting_description(undefined) ->
    <<>>.

collect_accounts_state(#accounter_PostingPlanLog{affected_accounts = Affected}) ->
    maps:map(
        fun (AccountID, Account) ->
            construct_account(AccountID, Account)
        end,
        Affected
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
