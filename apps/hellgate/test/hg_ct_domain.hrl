-ifndef(__hellgate_ct_domain__).
-define(__hellgate_ct_domain__, 42).

-include_lib("hellgate/include/domain.hrl").

-define(ordset(Es),     ordsets:from_list(Es)).

-define(glob(),         #domain_GlobalsRef{}).
-define(cur(ID),        #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T),      #domain_PaymentMethodRef{id = {C, T}}).
-define(cat(ID),        #domain_CategoryRef{id = ID}).
-define(prx(ID),        #domain_ProxyRef{id = ID}).
-define(prv(ID),        #domain_ProviderRef{id = ID}).
-define(trm(ID),        #domain_TerminalRef{id = ID}).
-define(tmpl(ID),       #domain_ContractTemplateRef{id = ID}).
-define(trms(ID),       #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID),        #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID),        #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID),       #domain_InspectorRef{id = ID}).
-define(partyproto(ID), #domain_PartyPrototypeRef{id = ID}).
-define(cashreg(ID),    #domain_CashRegisterRef{id = ID}).

-define(cashrng(Lower, Upper),
    #domain_CashRange{lower = Lower, upper = Upper}).

-define(prvacc(Stl), #domain_ProviderAccount{settlement = Stl}).
-define(partycond(ID, Def), {condition, {party, #domain_PartyCondition{id = ID, definition = Def}}}).

-define(fixed(Amount, Currency),
    {fixed, #domain_CashVolumeFixed{cash = #domain_Cash{
        amount = Amount,
        currency = ?currency(Currency)
    }}}).
-define(share(P, Q, C), {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C}}).

-define(cfpost(A1, A2, V),
    #domain_CashFlowPosting{
        source      = A1,
        destination = A2,
        volume      = V
    }
).

-define(cfpost(A1, A2, V, D),
    #domain_CashFlowPosting{
        source      = A1,
        destination = A2,
        volume      = V,
        details     = D
    }
).

-define(contact_info(EMail),
    ?contact_info(EMail, undefined)).
-define(contact_info(EMail, Phone),
    #domain_ContactInfo{
        email = EMail,
        phone_number = Phone
    }).

-endif.
