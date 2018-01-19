-module(hg_ct_fixture).

-include("hg_ct_domain.hrl").
-include_lib("dmsl/include/dmsl_base_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
%%

-export([construct_currency/1]).
-export([construct_currency/2]).
-export([construct_category/2]).
-export([construct_category/3]).
-export([construct_payment_method/1]).
-export([construct_proxy/2]).
-export([construct_proxy/3]).
-export([construct_inspector/3]).
-export([construct_inspector/4]).
-export([construct_contract_template/2]).
-export([construct_contract_template/4]).
-export([construct_provider_account_set/1]).
-export([construct_system_account_set/1]).
-export([construct_system_account_set/3]).
-export([construct_external_account_set/1]).
-export([construct_external_account_set/3]).
-export([construct_cashreg/3]).

%%

-type name()        :: binary().
-type category()    :: dmsl_domain_thrift:'CategoryRef'().
-type currency()    :: dmsl_domain_thrift:'CurrencyRef'().
-type proxy_ref()   :: dmsl_domain_thrift:'ProxyRef'().
-type proxy()       :: dmsl_domain_thrift:'Proxy'().
-type inspector()   :: dmsl_domain_thrift:'InspectorRef'().
-type template()    :: dmsl_domain_thrift:'ContractTemplateRef'().
-type terms()       :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type lifetime()    :: dmsl_domain_thrift:'Lifetime'() | undefined.

-type system_account_set() :: dmsl_domain_thrift:'SystemAccountSetRef'().
-type external_account_set() :: dmsl_domain_thrift:'ExternalAccountSetRef'().

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

construct_payment_method(?pmt(_Type, Name) = Ref) ->
    Def = erlang:atom_to_binary(Name, unicode),
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name = Def,
            description = Def
        }
    }}.

-spec construct_proxy(proxy_ref(), name()) ->
    {proxy, dmsl_domain_thrift:'ProxyObject'()}.

construct_proxy(Ref, Name) ->
    construct_proxy(Ref, Name, #{}).

-spec construct_proxy(proxy_ref(), name(), Opts :: map()) ->
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

-spec construct_inspector(inspector(), name(), proxy_ref()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.

construct_inspector(Ref, Name, ProxyRef) ->
    construct_inspector(Ref, Name, ProxyRef, #{}).

-spec construct_inspector(inspector(), name(), proxy_ref(), Additional :: map()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.

construct_inspector(Ref, Name, ProxyRef, Additional) ->
    {inspector, #domain_InspectorObject{
        ref = Ref,
        data = #domain_Inspector{
            name = Name,
            description = Name,
            proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = Additional
            }
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
    _ = hg_context:set(woody_context:new()),
    AccountSet = lists:foldl(
        fun (Cur = ?cur(Code), Acc) ->
            Acc#{Cur => ?prvacc(hg_accounting:create_account(Code))}
        end,
        #{},
        Currencies
    ),
    _ = hg_context:cleanup(),
    AccountSet.

-spec construct_system_account_set(system_account_set()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.

construct_system_account_set(Ref) ->
    construct_system_account_set(Ref, <<"Primaries">>, ?cur(<<"RUB">>)).

-spec construct_system_account_set(system_account_set(), name(), currency()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.

construct_system_account_set(Ref, Name, ?cur(CurrencyCode)) ->
    _ = hg_context:set(woody_context:new()),
    AccountID = hg_accounting:create_account(CurrencyCode),
    hg_context:cleanup(),
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = Name,
            accounts = #{?cur(CurrencyCode) => #domain_SystemAccount{
                settlement = AccountID
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
    _ = hg_context:set(woody_context:new()),
    AccountID1 = hg_accounting:create_account(CurrencyCode),
    AccountID2 = hg_accounting:create_account(CurrencyCode),
    hg_context:cleanup(),
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

-spec construct_cashreg(proxy_ref(), name(), proxy()) ->
    {cash_register, dmsl_domain_thrift:'CashRegisterObject'()}.

construct_cashreg(Ref, Name, Proxy) ->
    {cash_register, #domain_CashRegisterObject{
        ref = Ref,
        data = #domain_CashRegister{
            name        = Name,
            description = Name,
            proxy       = Proxy
        }
    }}.