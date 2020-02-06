%%% Accounting
%%%
%%% TODO
%%%  - Brittle posting id assignment, it should be a level upper, maybe even in
%%%    `pm_cashflow`.
%%%  - Stuff cash flow details in the posting description fields.

-module(pm_accounting).

-export([get_account/1]).
-export([get_balance/1]).
-export([get_balance/2]).
-export([create_account/1]).
-export([create_account/2]).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

-type amount()          :: dmsl_domain_thrift:'Amount'().
-type currency_code()   :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type account_id()      :: dmsl_accounter_thrift:'AccountID'().
-type batch_id()        :: dmsl_accounter_thrift:'BatchID'().
-type final_cash_flow() :: dmsl_domain_thrift:'FinalCashFlow'().
-type batch()           :: {batch_id(), final_cash_flow()}.
-type clock()           :: shumpune_shumpune_thrift:'Clock'().

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

-spec get_account(account_id()) ->
    account().

get_account(AccountID) ->
    case call_accounter('GetAccountByID', [AccountID]) of
        {ok, Result} ->
            construct_account(AccountID, Result);
        {exception, #shumpune_AccountNotFound{}} ->
            hg_woody_wrapper:raise(#payproc_AccountNotFound{})
    end.

-spec get_balance(account_id()) ->
    balance().

get_balance(AccountID) ->
    get_balance(AccountID, {latest, #shumpune_LatestClock{}}).

-spec get_balance(account_id(), clock()) ->
    balance().

get_balance(AccountID, Clock) ->
    case call_accounter('GetBalanceByID', [AccountID, Clock]) of
        {ok, Result} ->
            construct_balance(AccountID, Result);
        {exception, #shumpune_AccountNotFound{}} ->
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
    #shumpune_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

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
