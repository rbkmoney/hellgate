-module(pm_payment_institution).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([get_system_account/4]).
-export([get_realm/1]).
-export([is_live/1]).
%%

-type currency()        :: dmsl_domain_thrift:'CurrencyRef'().
-type varset()          :: pm_selector:varset().
-type revision()        :: pm_domain:revision().
-type payment_inst()    :: dmsl_domain_thrift:'PaymentInstitution'().
-type realm()           :: dmsl_domain_thrift:'PaymentInstitutionRealm'().

%%

-spec get_system_account(currency(), varset(), revision(), payment_inst()) ->
    dmsl_domain_thrift:'SystemAccount'() | no_return().

get_system_account(Currency, VS, Revision, #domain_PaymentInstitution{system_account_set = S}) ->
    SystemAccountSetRef = pm_selector:reduce_to_value(S, VS, Revision),
    SystemAccountSet = pm_domain:get(Revision, {system_account_set, SystemAccountSetRef}),
    case maps:find(Currency, SystemAccountSet#domain_SystemAccountSet.accounts) of
        {ok, Account} ->
            Account;
        error ->
            error({misconfiguration, {'No system account for a given currency', Currency}})
    end.

-spec get_realm(payment_inst()) -> realm().

get_realm(#domain_PaymentInstitution{realm = Realm}) ->
    Realm.

-spec is_live(payment_inst()) -> boolean().

is_live(#domain_PaymentInstitution{realm = Realm}) ->
    Realm =:= live.
