-ifndef(__hellgate_ct_domain__).
-define(__hellgate_ct_domain__, 42).

-include_lib("hellgate/include/domain.hrl").

-define(ordset(Es), ordsets:from_list(Es)).

-define(match(Term), erlang:binary_to_term(erlang:term_to_binary(Term))).
-define(glob(), #domain_GlobalsRef{}).
-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(pmt_sys(ID), #domain_PaymentSystemRef{id = ID}).
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
-define(bc_cat(ID), #domain_BankCardCategoryRef{id = ID}).
-define(mob(ID), #domain_MobileOperatorRef{id = ID}).

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

-define(share(P, Q, C), {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C}}).

-define(share_with_rounding_method(P, Q, C, RM),
    {share, #domain_CashVolumeShare{parts = #'Rational'{p = P, q = Q}, 'of' = C, 'rounding_method' = RM}}
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

-define(contact_info(EMail),
    ?contact_info(EMail, undefined)
).

-define(contact_info(EMail, Phone), #domain_ContactInfo{
    email = EMail,
    phone_number = Phone
}).

-define(tkz_bank_card(PaymentSystem, TokenProvider), ?tkz_bank_card(PaymentSystem, TokenProvider, dpan)).

-define(tkz_bank_card(PaymentSystem, TokenProvider, TokenizationMethod), #domain_TokenizedBankCard{
    payment_system_deprecated = PaymentSystem,
    token_provider_deprecated = TokenProvider,
    tokenization_method = TokenizationMethod
}).

-define(timeout_reason(), <<"Timeout">>).

-define(cart(Price, Details), #domain_InvoiceCart{
    lines = [
        #domain_InvoiceLine{
            product = <<"Test">>,
            quantity = 1,
            price = Price,
            metadata = Details
        }
    ]
}).

-define(candidate(Allowed, TerminalRef), #domain_RoutingCandidate{
    allowed = Allowed,
    terminal = TerminalRef
}).

-define(candidate(Descr, Allowed, TerminalRef), #domain_RoutingCandidate{
    description = Descr,
    allowed = Allowed,
    terminal = TerminalRef
}).

-define(candidate(Descr, Allowed, TerminalRef, Priority), #domain_RoutingCandidate{
    description = Descr,
    allowed = Allowed,
    terminal = TerminalRef,
    priority = Priority
}).

-define(delegate(Allowed, RuleSetRef), #domain_RoutingDelegate{
    allowed = Allowed,
    ruleset = RuleSetRef
}).

-define(delegate(Descr, Allowed, RuleSetRef), #domain_RoutingDelegate{
    description = Descr,
    allowed = Allowed,
    ruleset = RuleSetRef
}).

-define(terminal_obj(Ref, PRef), #domain_TerminalObject{
    ref = Ref,
    data = #domain_Terminal{
        name = <<"Payment Terminal">>,
        description = <<"Best terminal">>,
        provider_ref = PRef
    }
}).

-define(provider(ProvisionTermSet), #domain_Provider{
    name = <<"Biba">>,
    description = <<"Payment terminal provider">>,
    proxy = #domain_Proxy{
        ref = ?prx(1),
        additional = #{
            <<"override">> => <<"biba">>
        }
    },
    abs_account = <<"0987654321">>,
    accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
    terms = ProvisionTermSet
}).

-define(payment_terms, #domain_PaymentsProvisionTerms{
    currencies =
        {value,
            ?ordset([
                ?cur(<<"RUB">>)
            ])},
    categories =
        {value,
            ?ordset([
                ?cat(1)
            ])},
    payment_methods =
        {value,
            ?ordset([
                ?pmt(payment_terminal_deprecated, euroset),
                ?pmt(digital_wallet_deprecated, qiwi)
            ])},
    cash_limit =
        {value,
            ?cashrng(
                {inclusive, ?cash(1000, <<"RUB">>)},
                {exclusive, ?cash(10000000, <<"RUB">>)}
            )},
    cash_flow =
        {value, [
            ?cfpost(
                {provider, settlement},
                {merchant, settlement},
                ?share(1, 1, operation_amount)
            ),
            ?cfpost(
                {system, settlement},
                {provider, settlement},
                ?share(21, 1000, operation_amount)
            )
        ]},
    risk_coverage = undefined
}).

-endif.
