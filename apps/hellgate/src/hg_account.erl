-module(hg_account).

-export([get_account_by_id/2]).
-export([create_account/2]).
-export([create_account/3]).

-include_lib("hg_proto/include/hg_domain_thrift.hrl").
-include_lib("hg_proto/include/hg_accounter_thrift.hrl").

-type account() :: #accounter_Account{}.
-type account_id() :: hg_domain_thrift:'AccountID'().
-type currency_sym_code() :: hg_domain_thrift:'CurrencySymbolicCode'().

-spec get_account_by_id(AccountID :: integer(), woody_client:context())->
    {{ok, account(), woody_client:context()}}.

get_account_by_id(AccountID, Context0) ->
    {{ok, Account}, Context} = call_accounter('GetAccountByID', [AccountID], Context0),
    {Account, Context}.

-spec create_account(currency_sym_code(), woody_client:context()) ->
    {{ok, account_id(), woody_client:context()}}.

create_account(CurrencySymCode, Context0) ->
    create_account(CurrencySymCode, undefined, Context0).

-spec create_account(currency_sym_code(), binary() | undefined, woody_client:context()) ->
    {{ok, account_id(), woody_client:context()}}.

create_account(CurrencySymCode, Description, Context0) ->
    AccountPrototype = #accounter_AccountPrototype{
        currency_sym_code = CurrencySymCode,
        description = Description
    },
    {{ok, AccountID}, Context} = call_accounter('CreateAccount', [AccountPrototype], Context0),
    {AccountID, Context}.

call_accounter(Function, Args, Context) ->
    Url = genlib_app:env(hellgate, accounter_service_url),
    Service = {hg_accounter_thrift, 'Accounter'},
    woody_client:call(Context, {Service, Function, Args}, #{url => Url}).
