-module(pm_payment_institution).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([reduce_payment_institution/3]).
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

-spec reduce_payment_institution(payment_inst(), varset(), revision()) -> payment_inst().

reduce_payment_institution(PaymentInstitution, VS, Revision) ->
    PaymentInstitution#domain_PaymentInstitution{
        system_account_set = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.system_account_set, VS, Revision),
        default_contract_template = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.default_contract_template, VS, Revision),
        default_wallet_contract_template = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.default_wallet_contract_template, VS, Revision),
        wallet_system_account_set = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.wallet_system_account_set, VS, Revision),
        withdrawal_providers = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.withdrawal_providers, VS, Revision),
        p2p_providers = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.p2p_providers, VS, Revision),
        p2p_inspector = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.p2p_inspector, VS, Revision),
        providers = reduce_if_defined(
            PaymentInstitution#domain_PaymentInstitution.providers, VS, Revision)
    }.

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

reduce_if_defined(Selector, VS, Rev) ->
    pm_maybe:apply(fun(X) -> pm_selector:reduce(X, VS, Rev) end, Selector).
