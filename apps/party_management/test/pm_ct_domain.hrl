-ifndef(__pm_ct_domain__).
-define(__pm_ct_domain__, 42).

-include("domain.hrl").

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-define(ordset(Es), ordsets:from_list(Es)).

-define(glob(), #domain_GlobalsRef{}).
-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(pomt(M), #domain_PayoutMethodRef{id = M}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(prvtrm(ID), #domain_ProviderTerminalRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(pinst(ID), #domain_PaymentInstitutionRef{id = ID}).
-define(bank(ID), #domain_BankRef{id = ID}).
-define(bussched(ID), #domain_BusinessScheduleRef{id = ID}).
-define(ruleset(ID), #domain_RoutingRulesetRef{id = ID}).
-define(p2pprov(ID), #domain_P2PProviderRef{id = ID}).
-define(wtdrlprov(ID), #domain_WithdrawalProviderRef{id = ID}).
-define(crit(ID), #domain_CriterionRef{id = ID}).
-define(crp(ID), #domain_CashRegisterProviderRef{id = ID}).

-define(cashrng(Lower, Upper), #domain_CashRange{lower = Lower, upper = Upper}).

-define(prvacc(Stl), #domain_ProviderAccount{settlement = Stl}).
-define(partycond(ID, Def), {condition, {party, #domain_PartyCondition{id = ID, definition = Def}}}).

-define(fixed(Amount, Currency),
    {fixed, #domain_CashVolumeFixed{
        cash = #domain_Cash{
            amount = Amount,
            currency = ?currency(Currency)
        }
    }}
).

-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C
    }}
).

-define(share_with_rounding_method(P, Q, C, RM),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C,
        rounding_method = RM
    }}
).

-define(cfpost(A1, A2, V), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V
}).

-define(cfpost(A1, A2, V, D), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V,
    details = D
}).

-define(tkz_bank_card(PaymentSystem, TokenProvider), ?tkz_bank_card(PaymentSystem, TokenProvider, dpan)).

-define(tkz_bank_card(PaymentSystem, TokenProvider, TokenizationMethod), #domain_TokenizedBankCard{
    payment_system = PaymentSystem,
    token_provider = TokenProvider,
    tokenization_method = TokenizationMethod
}).

-define(timeout_reason(), <<"Timeout">>).

-endif.
