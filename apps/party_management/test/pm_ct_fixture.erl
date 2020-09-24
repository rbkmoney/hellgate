-module(pm_ct_fixture).

-include("pm_ct_domain.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
%%

-export([construct_currency/1]).
-export([construct_currency/2]).
-export([construct_category/2]).
-export([construct_category/3]).
-export([construct_payment_method/1]).
-export([construct_payout_method/1]).
-export([construct_proxy/2]).
-export([construct_proxy/3]).
-export([construct_inspector/3]).
-export([construct_inspector/4]).
-export([construct_inspector/5]).
-export([construct_contract_template/2]).
-export([construct_contract_template/4]).
-export([construct_provider_account_set/1]).
-export([construct_system_account_set/1]).
-export([construct_system_account_set/3]).
-export([construct_external_account_set/1]).
-export([construct_external_account_set/3]).
-export([construct_business_schedule/1]).
-export([construct_criterion/3]).
-export([construct_term_set_hierarchy/3]).
-export([construct_payment_routing_ruleset/3]).

%%

-type name()        :: binary().
-type category()    :: dmsl_domain_thrift:'CategoryRef'().
-type currency()    :: dmsl_domain_thrift:'CurrencyRef'().
-type proxy()       :: dmsl_domain_thrift:'ProxyRef'().
-type inspector()   :: dmsl_domain_thrift:'InspectorRef'().
-type risk_score()  :: dmsl_domain_thrift:'RiskScore'().
-type template()    :: dmsl_domain_thrift:'ContractTemplateRef'().
-type terms()       :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type lifetime()    :: dmsl_domain_thrift:'Lifetime'() | undefined.
-type payment_routing_ruleset() :: dmsl_domain_thrift:'PaymentRoutingRulesetRef'().

-type system_account_set() :: dmsl_domain_thrift:'SystemAccountSetRef'().
-type external_account_set() :: dmsl_domain_thrift:'ExternalAccountSetRef'().

-type business_schedule() :: dmsl_domain_thrift:'BusinessScheduleRef'().

-type criterion()   :: dmsl_domain_thrift:'CriterionRef'().
-type predicate()   :: dmsl_domain_thrift:'Predicate'().

-type term_set() :: dmsl_domain_thrift:'TermSet'().
-type term_set_hierarchy() :: dmsl_domain_thrift:'TermSetHierarchyRef'().

%%

-define(EVERY, {every, #'ScheduleEvery'{}}).

%%

-spec construct_currency(currency()) ->
    {currency, dmsl_domain_thrift:'CurrencyObject'()}.

construct_currency(Ref) ->
    construct_currency(Ref, 2).

-spec construct_currency(currency(), Exponent :: pos_integer()) ->
    {currency, dmsl_domain_thrift:'CurrencyObject'()}.

construct_currency(?cur(SymbolicCode) = Ref, Exponent) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = SymbolicCode,
            numeric_code = 666,
            symbolic_code = SymbolicCode,
            exponent = Exponent
        }
    }}.

-spec construct_category(category(), name()) ->
    {category, dmsl_domain_thrift:'CategoryObject'()}.

construct_category(Ref, Name) ->
    construct_category(Ref, Name, test).

-spec construct_category(category(), name(), test | live) ->
    {category, dmsl_domain_thrift:'CategoryObject'()}.

construct_category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name = Name,
            description = Name,
            type = Type
        }
    }}.

-spec construct_payment_method(dmsl_domain_thrift:'PaymentMethodRef'()) ->
    {payment_method, dmsl_domain_thrift:'PaymentMethodObject'()}.

construct_payment_method(?pmt(_Type, ?tkz_bank_card(Name, _)) = Ref) when is_atom(Name) ->
    construct_payment_method(Name, Ref);
construct_payment_method(?pmt(_Type, Name) = Ref) when is_atom(Name) ->
    construct_payment_method(Name, Ref);
construct_payment_method(?pmt(_Type, #domain_BankCardPaymentMethod{}) = Ref) ->
    construct_payment_method(Ref#domain_BankCardPaymentMethod.payment_system, Ref).

construct_payment_method(Name, Ref) ->
    Def = erlang:atom_to_binary(Name, unicode),
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name = Def,
            description = Def
        }
    }}.

-spec construct_payout_method(dmsl_domain_thrift:'PayoutMethodRef'()) ->
    {payout_method, dmsl_domain_thrift:'PayoutMethodObject'()}.

construct_payout_method(?pomt(M) = Ref) ->
    Def = erlang:atom_to_binary(M, unicode),
    {payout_method, #domain_PayoutMethodObject{
        ref = Ref,
        data = #domain_PayoutMethodDefinition{
            name = Def,
            description = Def
        }
    }}.

-spec construct_proxy(proxy(), name()) ->
    {proxy, dmsl_domain_thrift:'ProxyObject'()}.

construct_proxy(Ref, Name) ->
    construct_proxy(Ref, Name, #{}).

-spec construct_proxy(proxy(), name(), Opts :: map()) ->
    {proxy, dmsl_domain_thrift:'ProxyObject'()}.

construct_proxy(Ref, Name, Opts) ->
    {proxy, #domain_ProxyObject{
        ref = Ref,
        data = #domain_ProxyDefinition{
            name        = Name,
            description = Name,
            url         = <<>>,
            options     = Opts
        }
    }}.

-spec construct_inspector(inspector(), name(), proxy()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.

construct_inspector(Ref, Name, ProxyRef) ->
    construct_inspector(Ref, Name, ProxyRef, #{}).

-spec construct_inspector(inspector(), name(), proxy(), Additional :: map()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.

construct_inspector(Ref, Name, ProxyRef, Additional) ->
    construct_inspector(Ref, Name, ProxyRef, Additional, undefined).

-spec construct_inspector(inspector(), name(), proxy(), Additional :: map(), risk_score()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.

construct_inspector(Ref, Name, ProxyRef, Additional, FallBackScore) ->
    {inspector, #domain_InspectorObject{
        ref = Ref,
        data = #domain_Inspector{
            name = Name,
            description = Name,
            proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = Additional
            },
            fallback_risk_score = FallBackScore
        }
    }}.

-spec construct_contract_template(template(), terms()) ->
    {contract_template, dmsl_domain_thrift:'ContractTemplateObject'()}.

construct_contract_template(Ref, TermsRef) ->
    construct_contract_template(Ref, TermsRef, undefined, undefined).

-spec construct_contract_template(template(), terms(), ValidSince :: lifetime(), ValidUntil :: lifetime()) ->
    {contract_template, dmsl_domain_thrift:'ContractTemplateObject'()}.

construct_contract_template(Ref, TermsRef, ValidSince, ValidUntil) ->
    {contract_template, #domain_ContractTemplateObject{
        ref = Ref,
        data = #domain_ContractTemplate{
            valid_since = ValidSince,
            valid_until = ValidUntil,
            terms = TermsRef
        }
    }}.

-spec construct_provider_account_set([currency()]) -> dmsl_domain_thrift:'ProviderAccountSet'().

construct_provider_account_set(Currencies) ->
    ok = pm_context:save(pm_context:create()),
    AccountSet = lists:foldl(
        fun (Cur = ?cur(Code), Acc) ->
            Acc#{Cur => ?prvacc(pm_accounting:create_account(Code))}
        end,
        #{},
        Currencies
    ),
    _ = pm_context:cleanup(),
    AccountSet.

-spec construct_system_account_set(system_account_set()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.

construct_system_account_set(Ref) ->
    construct_system_account_set(Ref, <<"Primaries">>, ?cur(<<"RUB">>)).

-spec construct_system_account_set(system_account_set(), name(), currency()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.

construct_system_account_set(Ref, Name, ?cur(CurrencyCode)) ->
    ok = pm_context:save(pm_context:create()),
    SettlementAccountID = pm_accounting:create_account(CurrencyCode),
    SubagentAccountID = pm_accounting:create_account(CurrencyCode),
    pm_context:cleanup(),
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = Name,
            accounts = #{?cur(CurrencyCode) => #domain_SystemAccount{
                settlement = SettlementAccountID,
                subagent = SubagentAccountID
            }}
        }
    }}.

-spec construct_external_account_set(external_account_set()) ->
    {system_account_set, dmsl_domain_thrift:'ExternalAccountSetObject'()}.

construct_external_account_set(Ref) ->
    construct_external_account_set(Ref, <<"Primaries">>, ?cur(<<"RUB">>)).

-spec construct_external_account_set(external_account_set(), name(), currency()) ->
    {system_account_set, dmsl_domain_thrift:'ExternalAccountSetObject'()}.

construct_external_account_set(Ref, Name, ?cur(CurrencyCode)) ->
    ok = pm_context:save(pm_context:create()),
    AccountID1 = pm_accounting:create_account(CurrencyCode),
    AccountID2 = pm_accounting:create_account(CurrencyCode),
    pm_context:cleanup(),
    {external_account_set, #domain_ExternalAccountSetObject{
        ref = Ref,
        data = #domain_ExternalAccountSet{
            name = Name,
            description = Name,
            accounts = #{?cur(<<"RUB">>) => #domain_ExternalAccount{
                income  = AccountID1,
                outcome = AccountID2
            }}
        }
    }}.

-spec construct_business_schedule(business_schedule()) ->
    {business_schedule, dmsl_domain_thrift:'BusinessScheduleObject'()}.

construct_business_schedule(Ref) ->
    {business_schedule, #domain_BusinessScheduleObject{
        ref = Ref,
        data = #domain_BusinessSchedule{
            name = <<"Every day at 7:40">>,
            schedule = #'Schedule'{
                year = ?EVERY,
                month = ?EVERY,
                day_of_month = ?EVERY,
                day_of_week = ?EVERY,
                hour = {on, [7]},
                minute = {on, [40]},
                second = {on, [0]}
            }
        }
    }}.

-spec construct_criterion(criterion(), name(), predicate()) ->
    {criterion, dmsl_domain_thrift:'CriterionObject'()}.

construct_criterion(Ref, Name, Pred) ->
    {criterion, #domain_CriterionObject{
        ref = Ref,
        data = #domain_Criterion{
            name = Name,
            predicate = Pred
        }
    }}.

-spec construct_term_set_hierarchy(term_set_hierarchy(), term_set_hierarchy(), term_set()) ->
    {term_set_hierarchy, dmsl_domain_thrift:'TermSetHierarchyObject'()}.

construct_term_set_hierarchy(Ref, ParentRef, TermSet) ->
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = Ref,
        data = #domain_TermSetHierarchy{
            parent_terms = ParentRef,
            term_sets = [
                #domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }
            ]
        }
    }}.


-spec construct_payment_routing_ruleset(payment_routing_ruleset(), name(), _) ->
    dmsl_domain_thrift:'PaymentRoutingRulesetObject'().

construct_payment_routing_ruleset(Ref, Name, Decisions) ->
    {payment_routing_rules, #domain_PaymentRoutingRulesObject{
        ref = Ref,
        data = #domain_PaymentRoutingRuleset{
            name = Name,
            decisions = Decisions
        }
    }}.
