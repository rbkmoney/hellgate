-module(pm_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API
-export([reduce_p2p_provider/3]).
-export([reduce_withdrawal_provider/3]).
-export([reduce_payment_provider/3]).
-export([reduce_payment_provider_terminal_terms/4]).

-type p2p_provider()            :: dmsl_domain_thrift:'P2PProvider'().
-type withdrawal_provider()     :: dmsl_domain_thrift:'WithdrawalProvider'().
-type payment_provider()        :: dmsl_domain_thrift:'Provider'().
-type terminal()                :: dmsl_domain_thrift:'Terminal'().
-type payment_provision_terms() :: dmsl_domain_thrift:'PaymentsProvisionTerms'().
-type varset()                  :: pm_selector:varset().
-type domain_revision()         :: pm_domain:revision().

-spec reduce_p2p_provider(p2p_provider(), varset(), domain_revision()) -> p2p_provider().

reduce_p2p_provider(#domain_P2PProvider{p2p_terms = Terms} = Provider, VS, DomainRevision) ->
    Provider#domain_P2PProvider{
        p2p_terms = reduce_p2p_terms(Terms, VS, DomainRevision)
    }.

-spec reduce_withdrawal_provider(withdrawal_provider(), varset(), domain_revision()) -> withdrawal_provider().

reduce_withdrawal_provider(#domain_WithdrawalProvider{withdrawal_terms = Terms} = Provider, VS, DomainRevision) ->
    Provider#domain_WithdrawalProvider{
        withdrawal_terms = reduce_withdrawal_terms(Terms, VS, DomainRevision)
    }.

-spec reduce_payment_provider(payment_provider(), varset(), domain_revision()) -> payment_provider().

reduce_payment_provider(Provider, VS, DomainRevision) ->
    Provider#domain_Provider{
        terminal = pm_selector:reduce(Provider#domain_Provider.terminal, VS, DomainRevision),
        payment_terms = reduce_payment_terms(Provider#domain_Provider.payment_terms, VS, DomainRevision),
        recurrent_paytool_terms = reduce_recurrent_paytool_terms(
            Provider#domain_Provider.recurrent_paytool_terms, VS, DomainRevision
        )
    }.

-spec reduce_payment_provider_terminal_terms(payment_provider(), terminal(), varset(), domain_revision()) ->
    payment_provision_terms().

reduce_payment_provider_terminal_terms(Provider, Terminal, VS, DomainRevision) ->
    ProviderPaymentTerms = Provider#domain_Provider.payment_terms,
    TerminalPaymentTerms = Terminal#domain_Terminal.terms,
    MergedPaymentTerms = merge_payment_terms(ProviderPaymentTerms, TerminalPaymentTerms),
    reduce_payment_terms(MergedPaymentTerms, VS, DomainRevision).

reduce_p2p_terms(#domain_P2PProvisionTerms{} = Terms, VS, Rev) ->
    #domain_P2PProvisionTerms{
        currencies = reduce_if_defined(Terms#domain_P2PProvisionTerms.currencies, VS, Rev),
        cash_limit = reduce_if_defined(Terms#domain_P2PProvisionTerms.cash_limit, VS, Rev),
        cash_flow = reduce_if_defined(Terms#domain_P2PProvisionTerms.cash_flow, VS, Rev),
        fees = reduce_if_defined(Terms#domain_P2PProvisionTerms.fees, VS, Rev)
    }.

reduce_withdrawal_terms(#domain_WithdrawalProvisionTerms{} = Terms, VS, Rev) ->
    #domain_WithdrawalProvisionTerms{
        currencies = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.currencies, VS, Rev),
        payout_methods = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.payout_methods, VS, Rev),
        cash_limit = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.cash_limit, VS, Rev),
        cash_flow = reduce_if_defined(Terms#domain_WithdrawalProvisionTerms.cash_flow, VS, Rev)
    }.

reduce_payment_terms(PaymentTerms, VS, DomainRevision) ->
    #domain_PaymentsProvisionTerms{
        currencies = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.currencies, VS, DomainRevision),
        categories = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.categories, VS, DomainRevision),
        payment_methods = reduce_if_defined(
            PaymentTerms#domain_PaymentsProvisionTerms.payment_methods, VS, DomainRevision
        ),
        cash_limit = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.cash_limit, VS, DomainRevision),
        cash_flow = reduce_if_defined(PaymentTerms#domain_PaymentsProvisionTerms.cash_flow, VS, DomainRevision),
        holds = pm_maybe:apply(
            fun(X) -> reduce_payment_hold_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.holds
        ),
        refunds = pm_maybe:apply(
            fun(X) -> reduce_payment_refund_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.refunds
        ),
        chargebacks = pm_maybe:apply(
            fun(X) -> reduce_payment_chargeback_terms(X, VS, DomainRevision) end,
            PaymentTerms#domain_PaymentsProvisionTerms.chargebacks
        )
    }.

reduce_payment_hold_terms(PaymentHoldTerms, VS, DomainRevision) ->
    #domain_PaymentHoldsProvisionTerms{
        lifetime = reduce_if_defined(PaymentHoldTerms#domain_PaymentHoldsProvisionTerms.lifetime, VS, DomainRevision),
        partial_captures = pm_maybe:apply(
            fun(X) -> reduce_partial_captures_terms(X, VS, DomainRevision) end,
            PaymentHoldTerms#domain_PaymentHoldsProvisionTerms.partial_captures
        )
    }.

reduce_partial_captures_terms(#domain_PartialCaptureProvisionTerms{}, _VS, _DomainRevision) ->
    #domain_PartialCaptureProvisionTerms{}.

reduce_payment_refund_terms(PaymentRefundTerms, VS, DomainRevision) ->
    #domain_PaymentRefundsProvisionTerms{
        cash_flow = reduce_if_defined(
            PaymentRefundTerms#domain_PaymentRefundsProvisionTerms.cash_flow, VS, DomainRevision
        ),
        partial_refunds = pm_maybe:apply(
            fun(X) -> reduce_partial_refunds_terms(X, VS, DomainRevision) end,
            PaymentRefundTerms#domain_PaymentRefundsProvisionTerms.partial_refunds
        )
    }.

reduce_partial_refunds_terms(PartialRefundTerms, VS, DomainRevision) ->
    #domain_PartialRefundsProvisionTerms{
        cash_limit = reduce_if_defined(
            PartialRefundTerms#domain_PartialRefundsProvisionTerms.cash_limit, VS, DomainRevision
        )
    }.

reduce_payment_chargeback_terms(PaymentChargebackTerms, VS, DomainRevision) ->
    #domain_PaymentChargebackProvisionTerms{
        cash_flow = reduce_if_defined(
            PaymentChargebackTerms#domain_PaymentChargebackProvisionTerms.cash_flow, VS, DomainRevision
        )
    }.

reduce_recurrent_paytool_terms(RecurrentPaytoolTerms, VS, DomainRevision) ->
    #domain_RecurrentPaytoolsProvisionTerms{
        cash_value = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.cash_value, VS, DomainRevision
        ),
        categories = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.categories, VS, DomainRevision
        ),
        payment_methods = reduce_if_defined(
            RecurrentPaytoolTerms#domain_RecurrentPaytoolsProvisionTerms.payment_methods, VS, DomainRevision
        )
    }.

merge_payment_terms(
    #domain_PaymentsProvisionTerms{
        currencies      = PCurrencies,
        categories      = PCategories,
        payment_methods = PPaymentMethods,
        cash_limit      = PCashLimit,
        cash_flow       = PCashflow,
        holds           = PHolds,
        refunds         = PRefunds,
        chargebacks     = PChargebacks
    },
    #domain_PaymentsProvisionTerms{
        currencies      = TCurrencies,
        categories      = TCategories,
        payment_methods = TPaymentMethods,
        cash_limit      = TCashLimit,
        cash_flow       = TCashflow,
        holds           = THolds,
        refunds         = TRefunds,
        chargebacks     = TChargebacks
    }
) ->
    #domain_PaymentsProvisionTerms{
        currencies      = pm_utils:select_defined(TCurrencies,     PCurrencies),
        categories      = pm_utils:select_defined(TCategories,     PCategories),
        payment_methods = pm_utils:select_defined(TPaymentMethods, PPaymentMethods),
        cash_limit      = pm_utils:select_defined(TCashLimit,      PCashLimit),
        cash_flow       = pm_utils:select_defined(TCashflow,       PCashflow),
        holds           = pm_utils:select_defined(THolds,          PHolds),
        refunds         = pm_utils:select_defined(TRefunds,        PRefunds),
        chargebacks     = pm_utils:select_defined(TChargebacks,    PChargebacks)
    };
merge_payment_terms(ProviderTerms, TerminalTerms) ->
    pm_utils:select_defined(TerminalTerms, ProviderTerms).

reduce_if_defined(Selector, VS, Rev) ->
    pm_maybe:apply(fun(X) -> pm_selector:reduce(X, VS, Rev) end, Selector).
