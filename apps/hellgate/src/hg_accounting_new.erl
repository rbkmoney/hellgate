%%% Accounting for shumaich
%%%
%%% TODO
%%%  - Brittle posting id assignment, it should be a level upper, maybe even in
%%%    `hg_cashflow`.
%%%  - Stuff cash flow details in the posting description fields.

-module(hg_accounting_new).

-export([get_account/1]).
-export([get_account/2]).

-export([get_balance/1]).
-export([get_balance/2]).

-export([create_account/1]).

-export([collect_account_map/6]).
-export([collect_merchant_account_map/2]).
-export([collect_provider_account_map/3]).
-export([collect_system_account_map/4]).
-export([collect_external_account_map/4]).

-export([hold/3]).
-export([hold/4]).

-export([plan/3]).
-export([plan/4]).

-export([commit/3]).
-export([commit/4]).

-export([rollback/3]).
-export([rollback/4]).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("shumaich_proto/include/shumaich_shumaich_thrift.hrl").

-type amount() :: dmsl_domain_thrift:'Amount'().
-type currency_code() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id() :: dmsl_accounter_thrift:'AccountID'().
-type plan_id() :: dmsl_accounter_thrift:'PlanID'().
-type batch_id() :: dmsl_accounter_thrift:'BatchID'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().
-type batch() :: {batch_id(), final_cash_flow()}.
-type clock() :: dmsl_domain_thrift:'AccounterClock'().

-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type shop() :: dmsl_domain_thrift:'Shop'().
-type payment_institution() :: dmsl_domain_thrift:'PaymentInstitution'().
-type provider() :: dmsl_domain_thrift:'Provider'().
-type varset() :: pm_selector:varset().
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
    max_available_amount => amount(),
    clock => clock()
}.

-define(DEFAULT_RETRY_STRATEGY, {exponential, 10, 1.1, 100}).

-spec get_account(account_id()) -> account().
get_account(AccountID) ->
    get_account(AccountID, undefined).

-spec get_account(account_id(), undefined | clock()) -> account().
get_account(AccountID, Clock) ->
    case call_accounter('GetAccountByID', {AccountID, to_accounter_clock(Clock)}) of
        {ok, Result} ->
            construct_account(AccountID, Result);
        {exception, #shumaich_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec get_balance(account_id()) -> balance().
get_balance(AccountID) ->
    get_balance(AccountID, undefined).

-spec get_balance(account_id(), undefined | clock()) -> balance().
get_balance(AccountID, Clock) ->
    case call_accounter('GetBalanceByID', {AccountID, to_accounter_clock(Clock)}) of
        {ok, Result} ->
            construct_balance(AccountID, Result);
        {exception, #shumaich_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec create_account(currency_code()) -> account_id().
create_account(_CurrencyCode) ->
    WoodyCtx = hg_context:get_woody_context(hg_context:load()),
    % FIXME: placeholder, the sequence id should probably be passed externally
    %        not sure about the minimum too
    hg_utils:gen_sequence(<<"create_shumaich_account">>, WoodyCtx, #{minimum => 10000}).

-spec collect_account_map(payment(), shop(), payment_institution(), provider(), varset(), revision()) -> map().
collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision) ->
    hg_accounting:collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision).

-spec collect_merchant_account_map(shop(), map()) -> map().
collect_merchant_account_map(Shop, Acc) ->
    hg_accounting:collect_merchant_account_map(Shop, Acc).

-spec collect_provider_account_map(payment(), provider(), map()) -> map().
collect_provider_account_map(Payment, Provider, Acc) ->
    hg_accounting:collect_provider_account_map(Payment, Provider, Acc).

-spec collect_system_account_map(payment(), payment_institution(), revision(), map()) -> map().
collect_system_account_map(Payment, PaymentInstitution, Revision, Acc) ->
    hg_accounting:collect_system_account_map(Payment, PaymentInstitution, Revision, Acc).

-spec collect_external_account_map(payment(), varset(), revision(), map()) -> map().
collect_external_account_map(Payment, VS, Revision, Acc) ->
    hg_accounting:collect_external_account_map(Payment, VS, Revision, Acc).

%%
-spec plan(plan_id(), [batch()], hg_datetime:timestamp()) -> clock().
plan(_PlanID, [], _Timestamp) ->
    error(badarg);
plan(_PlanID, Batches, _Timestamp) when not is_list(Batches) ->
    error(badarg);
plan(PlanID, Batches, Timestamp) ->
    lists:foldl(
        fun(Batch, ClockAcc) -> hold(PlanID, Batch, Timestamp, ClockAcc) end,
        undefined,
        Batches
    ).

-spec plan(plan_id(), [batch()], hg_datetime:timestamp(), clock()) -> clock().
plan(_PlanID, [], _Timestamp, _Clock) ->
    error(badarg);
plan(_PlanID, Batches, _Timestamp, _Clock) when not is_list(Batches) ->
    error(badarg);
plan(PlanID, Batches, Timestamp, Clock) ->
    lists:foldl(
        fun(Batch, ClockAcc) -> hold(PlanID, Batch, Timestamp, ClockAcc) end,
        Clock,
        Batches
    ).

-spec hold(plan_id(), batch(), hg_datetime:timestamp()) -> clock().
hold(PlanID, Batch, Timestamp) ->
    do('Hold', construct_plan_change(PlanID, Batch, Timestamp)).

-spec hold(plan_id(), batch(), hg_datetime:timestamp(), clock() | undefined) -> clock().
hold(PlanID, Batches, Timestamp, Clock) ->
    AccounterClock = to_accounter_clock(Clock),
    do('Hold', construct_plan_change(PlanID, Batches, Timestamp), AccounterClock).

-spec commit(plan_id(), [batch()], hg_datetime:timestamp()) -> clock().
commit(PlanID, Batches, Timestamp) ->
    do('CommitPlan', construct_plan(PlanID, Batches, Timestamp)).

-spec commit(plan_id(), [batch()], hg_datetime:timestamp(), clock() | undefined) -> clock().
commit(PlanID, Batches, Timestamp, Clock) ->
    AccounterClock = to_accounter_clock(Clock),
    do('CommitPlan', construct_plan(PlanID, Batches, Timestamp), AccounterClock).

-spec rollback(plan_id(), [batch()], hg_datetime:timestamp()) -> clock().
rollback(PlanID, Batches, Timestamp) ->
    do('RollbackPlan', construct_plan(PlanID, Batches, Timestamp)).

-spec rollback(plan_id(), [batch()], hg_datetime:timestamp(), clock() | undefined) -> clock().
rollback(PlanID, Batches, Timestamp, Clock) ->
    AccounterClock = to_accounter_clock(Clock),
    do('RollbackPlan', construct_plan(PlanID, Batches, Timestamp), AccounterClock).

do(Op, Plan) ->
    do(Op, Plan, {latest, #shumaich_LatestClock{}}).

do(Op, Plan, PreviousClock) ->
    case call_accounter(Op, {Plan, PreviousClock}) of
        {ok, Clock} ->
            to_domain_clock(Clock);
        {exception, Exception} ->
            % FIXME
            error({accounting, Exception})
    end.

construct_plan_change(PlanID, {BatchID, Cashflow}, Timestamp) ->
    #shumaich_PostingPlanChange{
        id = PlanID,
        creation_time = Timestamp,
        batch = #shumaich_PostingBatch{
            id = BatchID,
            postings = collect_postings(Cashflow)
        }
    }.

construct_plan(PlanID, Batches, Timestamp) ->
    #shumaich_PostingPlan{
        id = PlanID,
        creation_time = Timestamp,
        batch_list = [
            #shumaich_PostingBatch{
                id = BatchID,
                postings = collect_postings(Cashflow)
            }
            || {BatchID, Cashflow} <- Batches
        ]
    }.

collect_postings(Cashflow) ->
    [
        #shumaich_Posting{
            from_account = #shumaich_Account{id = Source, currency_symbolic_code = CurrencyCode},
            to_account = #shumaich_Account{id = Destination, currency_symbolic_code = CurrencyCode},
            amount = Amount,
            currency_symbolic_code = CurrencyCode,
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
    #shumaich_Account{
        currency_symbolic_code = CurrencyCode
    }
) ->
    #{
        account_id => AccountID,
        currency_code => CurrencyCode
    }.

construct_balance(
    AccountID,
    #shumaich_Balance{
        own_amount = OwnAmount,
        min_available_amount = MinAvailableAmount,
        max_available_amount = MaxAvailableAmount,
        clock = Clock
    }
) ->
    genlib_map:compact(#{
        account_id => AccountID,
        own_amount => OwnAmount,
        min_available_amount => MinAvailableAmount,
        max_available_amount => MaxAvailableAmount,
        clock => to_domain_clock(Clock)
    }).

%%

call_accounter(Function, Args) ->
    %% Really not sure what to best do when we run out of retries
    try
        call_with_retry(Function, Args)
    catch
        throw:{error, no_more_retries} ->
            error({accounter, not_ready})
    end.

call_with_retry(Function, Args) ->
    hg_retry:call_with_retry(
        fun() ->
            case hg_woody_wrapper:call(accounter_new, Function, Args) of
                {ok, _} = Ok ->
                    {return, Ok};
                {exception, #shumaich_NotReady{}} ->
                    retry;
                {exception, _} = Exception ->
                    {return, Exception}
            end
        end,
        get_retry_strategy(Function)
    ).

to_domain_clock({latest, #shumaich_LatestClock{}}) ->
    undefined;
to_domain_clock({vector, #shumaich_VectorClock{state = State}}) ->
    {vector, #domain_VectorClock{state = State}}.

to_accounter_clock(undefined) ->
    {latest, #shumaich_LatestClock{}};
to_accounter_clock({vector, #domain_VectorClock{state = State}}) ->
    {vector, #shumaich_VectorClock{state = State}}.

get_retry_strategy(Function) ->
    PolicyConfig = genlib_app:env(hellgate, accounter_retry_policy, #{}),
    hg_retry:new_strategy(maps:get(Function, PolicyConfig, ?DEFAULT_RETRY_STRATEGY)).
