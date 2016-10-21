%%% Accounting
%%%
%%% TODO
%%%  - Brittle posting id assignment, it should be a level upper, maybe even in
%%%    `hg_cashflow`.
%%%  - Stuff cash flow details in the posting description fields.

-module(hg_accounting).

-export([get_account/2]).
-export([create_account/2]).
-export([create_account/3]).

-export([plan/4]).
-export([commit/4]).
-export([rollback/4]).

-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-type amount()        :: dmsl_domain_thrift:'Amount'().
-type currency_code() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id()    :: dmsl_accounter_thrift:'AccountID'().
-type plan_id()       :: dmsl_accounter_thrift:'PlanID'().

-type account() :: #{
    account_id => account_id(),
    own_amount => amount(),
    available_amount => amount(),
    currency_code => currency_code()
}.

-spec get_account(AccountID :: integer(), woody_client:context())->
    {account(), woody_client:context()}.

get_account(AccountID, Context0) ->
    {Result, Context} = call_accounter('GetAccountByID', [AccountID], Context0),
    {construct_account(AccountID, Result), Context}.

-spec create_account(currency_code(), woody_client:context()) ->
    {account_id(), woody_client:context()}.

create_account(CurrencyCode, Context0) ->
    create_account(CurrencyCode, undefined, Context0).

-spec create_account(currency_code(), binary() | undefined, woody_client:context()) ->
    {account_id(), woody_client:context()}.

create_account(CurrencyCode, Description, Context0) ->
    call_accounter('CreateAccount', [construct_prototype(CurrencyCode, Description)], Context0).

construct_prototype(CurrencyCode, Description) ->
    #accounter_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

%%

-type accounts_map() :: #{hg_cashflow:account() => account_id()}.
-type accounts_state() :: #{hg_cashflow:account() => account()}.

-spec plan(plan_id(), hg_cashflow:t(), accounts_map(), woody_client:context()) ->
    {accounts_state(), woody_client:context()}.

plan(PlanID, Cashflow, AccountMap, Context0) ->
    do('Hold', construct_plan(PlanID, Cashflow, AccountMap), AccountMap, Context0).

-spec commit(plan_id(), hg_cashflow:t(), accounts_map(), woody_client:context()) ->
    {accounts_state(), woody_client:context()}.

commit(PlanID, Cashflow, AccountMap, Context0) ->
    do('CommitPlan', construct_plan(PlanID, Cashflow, AccountMap), AccountMap, Context0).

-spec rollback(plan_id(), hg_cashflow:t(), accounts_map(), woody_client:context()) ->
    {accounts_state(), woody_client:context()}.

rollback(PlanID, Cashflow, AccountMap, Context0) ->
    do('RollbackPlan', construct_plan(PlanID, Cashflow, AccountMap), AccountMap, Context0).

do(Op, Plan, AccountMap, Context0) ->
    try
        {PlanLog, Context} = call_accounter(Op, [Plan], Context0),
        {collect_accounts_state(PlanLog, AccountMap), Context}
    catch
        Exception ->
            error(Exception) % FIXME
    end.

construct_plan(PlanID, Cashflow, AccountMap) ->
    #accounter_PostingPlan{
        id    = PlanID,
        batch = collect_postings(Cashflow, AccountMap)
    }.

collect_postings(Cashflow, AccountMap) ->
    [
        #accounter_Posting{
            id                = ID,
            from_id           = resolve_account(Source, AccountMap),
            to_id             = resolve_account(Destination, AccountMap),
            amount            = Amount,
            currency_sym_code = CurrencyCode,
            description       = <<>>
        } ||
            {ID, {Source, Destination, Amount, CurrencyCode}} <-
                lists:zip(lists:seq(1, length(Cashflow)), Cashflow)
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
        available_amount = AvailableAmount,
        currency_sym_code = CurrencyCode
    }
) ->
    #{
        account_id => AccountID,
        own_amount => OwnAmount,
        available_amount => AvailableAmount,
        currency_code => CurrencyCode
    }.

%%

call_accounter(Function, Args, Context) ->
    Url = genlib_app:env(hellgate, accounter_service_url),
    Service = {dmsl_accounter_thrift, 'Accounter'},
    woody_client:call(Context, {Service, Function, Args}, #{url => Url}).
