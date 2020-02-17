%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/merchant.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/contract.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)

-module(hg_party).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

%% Party support functions

-export([get_party/1]).
-export([get_party_revision/1]).
-export([checkout/2]).

-export([get_contract/2]).

-export([get_terms/3]).
-export([reduce_terms/3]).

-export([get_shop/2]).

-export_type([party/0]).
-export_type([party_revision/0]).
-export_type([party_status/0]).

%%

-type party()                 :: dmsl_domain_thrift:'Party'().
-type party_id()              :: dmsl_domain_thrift:'PartyID'().
-type party_revision()        :: dmsl_domain_thrift:'PartyRevision'().
-type party_status()          :: dmsl_domain_thrift:'PartyStatus'().
-type contract()              :: dmsl_domain_thrift:'Contract'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_template()     :: dmsl_domain_thrift:'ContractTemplate'().
-type shop()                  :: dmsl_domain_thrift:'Shop'().
-type shop_id()               :: dmsl_domain_thrift:'ShopID'().

-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type revision()              :: hg_domain:revision().


%% Interface

-spec get_party(party_id()) ->
    party() | no_return().

get_party(PartyID) ->
    Revision = get_party_revision(PartyID),
    checkout(PartyID, {revision, Revision}).

-spec get_party_revision(party_id()) ->
    party_revision() | no_return().

get_party_revision(PartyID) ->
    {Client, Context} = get_party_client(),
    unwrap_party_result(party_client_thrift:get_revision(PartyID, Client, Context)).

-spec checkout(party_id(), party_client_thrift:party_revision_param()) ->
    party() | no_return().

checkout(PartyID, RevisionParam) ->
    {Client, Context} = get_party_client(),
    unwrap_party_result(party_client_thrift:checkout(PartyID, RevisionParam, Client, Context)).

-spec get_contract(contract_id(), party()) ->
    contract() | undefined.

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    maps:get(ID, Contracts, undefined).

-spec get_terms(contract() | contract_template(), timestamp(), revision()) ->
    dmsl_domain_thrift:'TermSet'() | no_return().

get_terms(#domain_Contract{} = Contract, Timestamp, Revision) ->
    case compute_terms(Contract, Timestamp, Revision) of
        #domain_TermSet{} = Terms ->
            Terms;
        undefined ->
            error({misconfiguration, {'No active TermSet found', Contract#domain_Contract.terms, Timestamp}})
    end;
get_terms(#domain_ContractTemplate{terms = TermSetHierarchyRef}, Timestamp, Revision) ->
    get_term_set(TermSetHierarchyRef, Timestamp, Revision).

-spec get_shop(shop_id(), party()) ->
    shop() | undefined.

get_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

%% Internals

get_party_client() ->
    HgContext = pm_context:load(),
    Client = pm_context:get_party_client(HgContext),
    Context = pm_context:get_party_client_context(HgContext),
    {Client, Context}.

unwrap_party_result({ok, Result}) ->
    Result;
unwrap_party_result({error, Error}) ->
    erlang:throw(Error).

-spec reduce_terms(dmsl_domain_thrift:'TermSet'(), pm_selector:varset(), revision()) ->
    dmsl_domain_thrift:'TermSet'().

%% TODO rework this part for more generic approach
reduce_terms(
    #domain_TermSet{
        payments = PaymentsTerms,
        recurrent_paytools = RecurrentPaytoolTerms,
        payouts = PayoutTerms,
        reports = ReportTerms,
        wallets = WalletTerms
    },
    VS,
    Revision
) ->
    #domain_TermSet{
        payments = pm_maybe:apply(fun(X) -> reduce_payments_terms(X, VS, Revision) end, PaymentsTerms),
        recurrent_paytools = pm_maybe:apply(
            fun(X) -> reduce_recurrent_paytools_terms(X, VS, Revision) end,
            RecurrentPaytoolTerms
        ),
        payouts = pm_maybe:apply(fun(X) -> reduce_payout_terms(X, VS, Revision) end, PayoutTerms),
        reports = pm_maybe:apply(fun(X) -> reduce_reports_terms(X, VS, Revision) end, ReportTerms),
        wallets = pm_maybe:apply(fun(X) -> reduce_wallets_terms(X, VS, Revision) end, WalletTerms)
    }.

reduce_payments_terms(#domain_PaymentsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentsServiceTerms{
        currencies      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.currencies, VS, Rev),
        categories      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.categories, VS, Rev),
        payment_methods = reduce_if_defined(Terms#domain_PaymentsServiceTerms.payment_methods, VS, Rev),
        cash_limit      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.cash_limit, VS, Rev),
        fees            = reduce_if_defined(Terms#domain_PaymentsServiceTerms.fees, VS, Rev),
        holds           = pm_maybe:apply(
            fun(X) -> reduce_holds_terms(X, VS, Rev) end,
            Terms#domain_PaymentsServiceTerms.holds
        ),
        refunds         = pm_maybe:apply(
            fun(X) -> reduce_refunds_terms(X, VS, Rev) end,
            Terms#domain_PaymentsServiceTerms.refunds
        )
    }.

reduce_recurrent_paytools_terms(#domain_RecurrentPaytoolsServiceTerms{} = Terms, VS, Rev) ->
    #domain_RecurrentPaytoolsServiceTerms{
        payment_methods = reduce_if_defined(Terms#domain_RecurrentPaytoolsServiceTerms.payment_methods, VS, Rev)
    }.

reduce_holds_terms(#domain_PaymentHoldsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentHoldsServiceTerms{
        payment_methods  = reduce_if_defined(Terms#domain_PaymentHoldsServiceTerms.payment_methods, VS, Rev),
        lifetime         = reduce_if_defined(Terms#domain_PaymentHoldsServiceTerms.lifetime, VS, Rev),
        partial_captures = Terms#domain_PaymentHoldsServiceTerms.partial_captures
    }.

reduce_refunds_terms(#domain_PaymentRefundsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentRefundsServiceTerms{
        payment_methods     = reduce_if_defined(Terms#domain_PaymentRefundsServiceTerms.payment_methods, VS, Rev),
        fees                = reduce_if_defined(Terms#domain_PaymentRefundsServiceTerms.fees, VS, Rev),
        eligibility_time    = reduce_if_defined(Terms#domain_PaymentRefundsServiceTerms.eligibility_time, VS, Rev),
        partial_refunds     = pm_maybe:apply(
            fun(X) -> reduce_partial_refunds_terms(X, VS, Rev) end,
            Terms#domain_PaymentRefundsServiceTerms.partial_refunds
        )
    }.

reduce_partial_refunds_terms(#domain_PartialRefundsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PartialRefundsServiceTerms{
        cash_limit = reduce_if_defined(Terms#domain_PartialRefundsServiceTerms.cash_limit, VS, Rev)
    }.

reduce_payout_terms(#domain_PayoutsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PayoutsServiceTerms{
        payout_schedules = reduce_if_defined(Terms#domain_PayoutsServiceTerms.payout_schedules, VS, Rev),
        payout_methods   = reduce_if_defined(Terms#domain_PayoutsServiceTerms.payout_methods, VS, Rev),
        cash_limit       = reduce_if_defined(Terms#domain_PayoutsServiceTerms.cash_limit, VS, Rev),
        fees             = reduce_if_defined(Terms#domain_PayoutsServiceTerms.fees, VS, Rev)
    }.

reduce_reports_terms(#domain_ReportsServiceTerms{acts = Acts}, VS, Rev) ->
    #domain_ReportsServiceTerms{
        acts = pm_maybe:apply(fun(X) -> reduce_acts_terms(X, VS, Rev) end, Acts)
    }.

reduce_acts_terms(#domain_ServiceAcceptanceActsTerms{schedules = Schedules}, VS, Rev) ->
    #domain_ServiceAcceptanceActsTerms{
        schedules = reduce_if_defined(Schedules, VS, Rev)
    }.

reduce_wallets_terms(#domain_WalletServiceTerms{} = Terms, VS, Rev) ->
    WithdrawalTerms = Terms#domain_WalletServiceTerms.withdrawals,
    P2PTerms = Terms#domain_WalletServiceTerms.p2p,
    #domain_WalletServiceTerms{
        currencies = reduce_if_defined(Terms#domain_WalletServiceTerms.currencies, VS, Rev),
        wallet_limit = reduce_if_defined(Terms#domain_WalletServiceTerms.wallet_limit, VS, Rev),
        turnover_limit = reduce_if_defined(Terms#domain_WalletServiceTerms.turnover_limit, VS, Rev),
        withdrawals = pm_maybe:apply(fun(X) -> reduce_withdrawals_terms(X, VS, Rev) end, WithdrawalTerms),
        p2p = pm_maybe:apply(fun(X) -> reduce_p2p_terms(X, VS, Rev) end, P2PTerms)
    }.

reduce_withdrawals_terms(#domain_WithdrawalServiceTerms{} = Terms, VS, Rev) ->
    #domain_WithdrawalServiceTerms{
        currencies = reduce_if_defined(Terms#domain_WithdrawalServiceTerms.currencies, VS, Rev),
        cash_limit = reduce_if_defined(Terms#domain_WithdrawalServiceTerms.cash_limit, VS, Rev),
        cash_flow = reduce_if_defined(Terms#domain_WithdrawalServiceTerms.cash_flow, VS, Rev)
    }.

reduce_p2p_terms(#domain_P2PServiceTerms{} = Terms, VS, Rev) ->
    #domain_P2PServiceTerms{
        allow = pm_maybe:apply(fun(X) ->
            pm_selector:reduce_predicate(X, VS, Rev)
        end, Terms#domain_P2PServiceTerms.allow),
        currencies = reduce_if_defined(Terms#domain_P2PServiceTerms.currencies, VS, Rev),
        cash_limit = reduce_if_defined(Terms#domain_P2PServiceTerms.cash_limit, VS, Rev),
        cash_flow = reduce_if_defined(Terms#domain_P2PServiceTerms.cash_flow, VS, Rev),
        fees = reduce_if_defined(Terms#domain_P2PServiceTerms.fees, VS, Rev),
        quote_lifetime = reduce_if_defined(Terms#domain_P2PServiceTerms.quote_lifetime, VS, Rev)
    }.

reduce_if_defined(Selector, VS, Rev) ->
    pm_maybe:apply(fun(X) -> pm_selector:reduce(X, VS, Rev) end, Selector).

compute_terms(#domain_Contract{terms = TermsRef, adjustments = Adjustments}, Timestamp, Revision) ->
    ActiveAdjustments = lists:filter(fun(A) -> is_adjustment_active(A, Timestamp) end, Adjustments),
    % Adjustments are ordered from oldest to newest
    ActiveTermRefs = [TermsRef | [TRef || #domain_ContractAdjustment{terms = TRef} <- ActiveAdjustments]],
    ActiveTermSets = lists:map(
        fun(TRef) ->
            get_term_set(TRef, Timestamp, Revision)
        end,
        ActiveTermRefs
    ),
    merge_term_sets(ActiveTermSets).

is_adjustment_active(
    #domain_ContractAdjustment{created_at = CreatedAt, valid_since = ValidSince, valid_until = ValidUntil},
    Timestamp
) ->
    pm_datetime:between(Timestamp, pm_utils:select_defined(ValidSince, CreatedAt), ValidUntil).


get_term_set(TermsRef, Timestamp, Revision) ->
    #domain_TermSetHierarchy{
        parent_terms = ParentRef,
        term_sets = TimedTermSets
    } = pm_domain:get(Revision, {term_set_hierarchy, TermsRef}),
    TermSet = get_active_term_set(TimedTermSets, Timestamp),
    case ParentRef of
        undefined ->
            TermSet;
        #domain_TermSetHierarchyRef{} ->
            ParentTermSet = get_term_set(ParentRef, Timestamp, Revision),
            merge_term_sets([ParentTermSet, TermSet])
    end.

get_active_term_set(TimedTermSets, Timestamp) ->
    lists:foldl(
        fun(#domain_TimedTermSet{action_time = ActionTime, terms = TermSet}, ActiveTermSet) ->
            case pm_datetime:between(Timestamp, ActionTime) of
                true ->
                    TermSet;
                false ->
                    ActiveTermSet
            end
        end,
        undefined,
        TimedTermSets
    ).

merge_term_sets(TermSets) when is_list(TermSets)->
    lists:foldl(fun merge_term_sets/2, undefined, TermSets).

merge_term_sets(
    #domain_TermSet{
        payments = PaymentTerms1,
        recurrent_paytools = RecurrentPaytoolTerms1,
        payouts = PayoutTerms1,
        reports = Reports1,
        wallets = Wallets1
    },
    #domain_TermSet{
        payments = PaymentTerms0,
        recurrent_paytools = RecurrentPaytoolTerms0,
        payouts = PayoutTerms0,
        reports = Reports0,
        wallets = Wallets0
    }
) ->
    #domain_TermSet{
        payments = merge_payments_terms(PaymentTerms0, PaymentTerms1),
        recurrent_paytools = merge_recurrent_paytools_terms(RecurrentPaytoolTerms0, RecurrentPaytoolTerms1),
        payouts = merge_payouts_terms(PayoutTerms0, PayoutTerms1),
        reports = merge_reports_terms(Reports0, Reports1),
        wallets = merge_wallets_terms(Wallets0, Wallets1)
    };
merge_term_sets(TermSet1, TermSet0) ->
    pm_utils:select_defined(TermSet1, TermSet0).

merge_payments_terms(
    #domain_PaymentsServiceTerms{
        currencies = Curr0,
        categories = Cat0,
        payment_methods = Pm0,
        cash_limit = Al0,
        fees = Fee0,
        holds = Hl0,
        refunds = Rf0
    },
    #domain_PaymentsServiceTerms{
        currencies = Curr1,
        categories = Cat1,
        payment_methods = Pm1,
        cash_limit = Al1,
        fees = Fee1,
        holds = Hl1,
        refunds = Rf1
    }
) ->
    #domain_PaymentsServiceTerms{
        currencies      = pm_utils:select_defined(Curr1, Curr0),
        categories      = pm_utils:select_defined(Cat1, Cat0),
        payment_methods = pm_utils:select_defined(Pm1, Pm0),
        cash_limit      = pm_utils:select_defined(Al1, Al0),
        fees            = pm_utils:select_defined(Fee1, Fee0),
        holds           = merge_holds_terms(Hl0, Hl1),
        refunds         = merge_refunds_terms(Rf0, Rf1)
    };
merge_payments_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_recurrent_paytools_terms(
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = Pm0},
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = Pm1}
) ->
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = pm_utils:select_defined(Pm1, Pm0)};
merge_recurrent_paytools_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_holds_terms(
    #domain_PaymentHoldsServiceTerms{
        payment_methods = Pm0,
        lifetime = Lft0,
        partial_captures = Ptcp0
    },
    #domain_PaymentHoldsServiceTerms{
        payment_methods = Pm1,
        lifetime = Lft1,
        partial_captures = Ptcp1
    }
) ->
    #domain_PaymentHoldsServiceTerms{
        payment_methods  = pm_utils:select_defined(Pm1, Pm0),
        lifetime         = pm_utils:select_defined(Lft1, Lft0),
        partial_captures = pm_utils:select_defined(Ptcp1, Ptcp0)
    };
merge_holds_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_refunds_terms(
    #domain_PaymentRefundsServiceTerms{
        payment_methods = Pm0,
        fees = Fee0,
        eligibility_time = ElTime0,
        partial_refunds = PartRef0
    },
    #domain_PaymentRefundsServiceTerms{
        payment_methods = Pm1,
        fees = Fee1,
        eligibility_time = ElTime1,
        partial_refunds = PartRef1
    }
) ->
    #domain_PaymentRefundsServiceTerms{
        payment_methods     = pm_utils:select_defined(Pm1, Pm0),
        fees                = pm_utils:select_defined(Fee1, Fee0),
        eligibility_time    = pm_utils:select_defined(ElTime1, ElTime0),
        partial_refunds     = merge_partial_refunds_terms(PartRef0, PartRef1)
    };
merge_refunds_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_partial_refunds_terms(
    #domain_PartialRefundsServiceTerms{
        cash_limit  = Cash0
    },
    #domain_PartialRefundsServiceTerms{
        cash_limit  = Cash1
    }
) ->
    #domain_PartialRefundsServiceTerms{
        cash_limit  = pm_utils:select_defined(Cash1, Cash0)
    };
merge_partial_refunds_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_payouts_terms(
    #domain_PayoutsServiceTerms{
        payout_schedules = Ps0,
        payout_methods   = Pm0,
        cash_limit       = Cash0,
        fees             = Fee0
    },
    #domain_PayoutsServiceTerms{
        payout_schedules = Ps1,
        payout_methods   = Pm1,
        cash_limit       = Cash1,
        fees             = Fee1
    }
) ->
    #domain_PayoutsServiceTerms{
        payout_schedules = pm_utils:select_defined(Ps1, Ps0),
        payout_methods   = pm_utils:select_defined(Pm1, Pm0),
        cash_limit       = pm_utils:select_defined(Cash1, Cash0),
        fees             = pm_utils:select_defined(Fee1, Fee0)
    };
merge_payouts_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_reports_terms(
    #domain_ReportsServiceTerms{
        acts = Acts0
    },
    #domain_ReportsServiceTerms{
        acts = Acts1
    }
) ->
    #domain_ReportsServiceTerms{
        acts = merge_service_acceptance_acts_terms(Acts0, Acts1)
    };
merge_reports_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_service_acceptance_acts_terms(
    #domain_ServiceAcceptanceActsTerms{
        schedules = Schedules0
    },
    #domain_ServiceAcceptanceActsTerms{
        schedules = Schedules1
    }
) ->
    #domain_ServiceAcceptanceActsTerms{
        schedules = pm_utils:select_defined(Schedules1, Schedules0)
    };
merge_service_acceptance_acts_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_wallets_terms(
    #domain_WalletServiceTerms{
        currencies = Currencies0,
        wallet_limit = CashLimit0,
        turnover_limit = TurnoverLimit0,
        withdrawals = Withdrawals0,
        p2p = PeerToPeer0
    },
    #domain_WalletServiceTerms{
        currencies = Currencies1,
        wallet_limit = CashLimit1,
        turnover_limit = TurnoverLimit1,
        withdrawals = Withdrawals1,
        p2p = PeerToPeer1
    }
) ->
    #domain_WalletServiceTerms{
        currencies = pm_utils:select_defined(Currencies1, Currencies0),
        wallet_limit = pm_utils:select_defined(CashLimit1, CashLimit0),
        turnover_limit = pm_utils:select_defined(TurnoverLimit1, TurnoverLimit0),
        withdrawals = merge_withdrawals_terms(Withdrawals0, Withdrawals1),
        p2p = merge_p2p_terms(PeerToPeer0, PeerToPeer1)
    };
merge_wallets_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_withdrawals_terms(
    #domain_WithdrawalServiceTerms{
        currencies = Currencies0,
        cash_limit = CashLimit0,
        cash_flow = CashFlow0
    },
    #domain_WithdrawalServiceTerms{
        currencies = Currencies1,
        cash_limit = CashLimit1,
        cash_flow = CashFlow1
    }
) ->
    #domain_WithdrawalServiceTerms{
        currencies = pm_utils:select_defined(Currencies1, Currencies0),
        cash_limit = pm_utils:select_defined(CashLimit1, CashLimit0),
        cash_flow = pm_utils:select_defined(CashFlow1, CashFlow0)
    };
merge_withdrawals_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).

merge_p2p_terms(
    #domain_P2PServiceTerms{
        currencies = Currencies0,
        cash_limit = CashLimit0,
        cash_flow = CashFlow0,
        fees = Fees0
    },
    #domain_P2PServiceTerms{
        currencies = Currencies1,
        cash_limit = CashLimit1,
        cash_flow = CashFlow1,
        fees = Fees1
    }
) ->
    #domain_P2PServiceTerms{
        currencies = pm_utils:select_defined(Currencies1, Currencies0),
        cash_limit = pm_utils:select_defined(CashLimit1, CashLimit0),
        cash_flow = pm_utils:select_defined(CashFlow1, CashFlow0),
        fees = pm_utils:select_defined(Fees1, Fees0)
    };
merge_p2p_terms(Terms0, Terms1) ->
    pm_utils:select_defined(Terms1, Terms0).
