%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/merchant.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/contract.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)

-module(hg_party).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

%% Party support functions

-export([create_party/3]).
-export([blocking/2]).
-export([suspension/2]).

-export([get_contract/2]).
-export([set_contract/2]).
-export([set_new_contract/3]).

-export([get_terms/3]).
-export([reduce_terms/3]).

-export([create_shop/3]).
-export([shop_blocking/3]).
-export([shop_suspension/3]).
-export([set_shop/2]).

-export([get_new_shop_currency/4]).
-export([get_shop/2]).
-export([get_shop_account/2]).
-export([get_account_state/2]).

%% Asserts

-export([assert_party_objects_valid/3]).

%%

-type party()                 :: dmsl_domain_thrift:'Party'().
-type party_id()              :: dmsl_domain_thrift:'PartyID'().
-type contract()              :: dmsl_domain_thrift:'Contract'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_template()     :: dmsl_domain_thrift:'ContractTemplate'().
-type shop()                  :: dmsl_domain_thrift:'Shop'().
-type shop_id()               :: dmsl_domain_thrift:'ShopID'().
-type shop_params()           :: dmsl_payment_processing_thrift:'ShopParams'().
-type currency()              :: dmsl_domain_thrift:'CurrencyRef'().

-type blocking()              :: dmsl_domain_thrift:'Blocking'().
-type suspension()            :: dmsl_domain_thrift:'Suspension'().

-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type revision()              :: hg_domain:revision().


%% Interface

-spec create_party(party_id(), dmsl_domain_thrift:'PartyContactInfo'(), timestamp()) ->
    party().

create_party(PartyID, ContactInfo, Timestamp) ->
    #domain_Party{
        id              = PartyID,
        created_at      = Timestamp,
        revision        = 0,
        contact_info    = ContactInfo,
        blocking        = ?unblocked(Timestamp),
        suspension      = ?active(Timestamp),
        contracts       = #{},
        shops           = #{}
    }.

-spec blocking(blocking(), party()) ->
    party().

blocking(Blocking, Party) ->
    Party#domain_Party{blocking = Blocking}.

-spec suspension(suspension(), party()) ->
    party().

suspension(Suspension, Party) ->
    Party#domain_Party{suspension = Suspension}.

-spec get_contract(contract_id(), party()) ->
    contract() | undefined.

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    maps:get(ID, Contracts, undefined).

-spec set_new_contract(contract(), timestamp(), party()) ->
    party().

set_new_contract(Contract, Timestamp, Party) ->
    set_contract(hg_contract:update_status(Contract, Timestamp), Party).

-spec set_contract(contract(), party()) ->
    party().

set_contract(Contract = #domain_Contract{id = ID}, Party = #domain_Party{contracts = Contracts}) ->
    Party#domain_Party{contracts = Contracts#{ID => Contract}}.

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

-spec create_shop(shop_id(), shop_params(), timestamp()) ->
    shop().

create_shop(ID, ShopParams, Timestamp) ->
    #domain_Shop{
        id              = ID,
        created_at      = Timestamp,
        blocking        = ?unblocked(Timestamp),
        suspension      = ?active(Timestamp),
        category        = ShopParams#payproc_ShopParams.category,
        details         = ShopParams#payproc_ShopParams.details,
        location        = ShopParams#payproc_ShopParams.location,
        contract_id     = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id  = ShopParams#payproc_ShopParams.payout_tool_id
    }.

-spec get_shop(shop_id(), party()) ->
    shop() | undefined.

get_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

-spec set_shop(shop(), party()) ->
    party().

set_shop(Shop = #domain_Shop{id = ID}, Party = #domain_Party{shops = Shops}) ->
    Party#domain_Party{shops = Shops#{ID => Shop}}.

-spec shop_blocking(shop_id(), blocking(), party()) ->
    party().

shop_blocking(ID, Blocking, Party) ->
    Shop = get_shop(ID, Party),
    set_shop(Shop#domain_Shop{blocking = Blocking}, Party).

-spec shop_suspension(shop_id(), suspension(), party()) ->
    party().

shop_suspension(ID, Suspension, Party) ->
    Shop = get_shop(ID, Party),
    set_shop(Shop#domain_Shop{suspension = Suspension}, Party).

-spec get_shop_account(shop_id(), party()) ->
    dmsl_domain_thrift:'ShopAccount'().

get_shop_account(ShopID, Party) ->
    Shop = ensure_shop(get_shop(ShopID, Party)),
    get_shop_account(Shop).

get_shop_account(#domain_Shop{account = undefined}) ->
    throw(#payproc_ShopAccountNotFound{});
get_shop_account(#domain_Shop{account = Account}) ->
    Account.

-spec get_account_state(dmsl_accounter_thrift:'AccountID'(), party()) ->
    dmsl_payment_processing_thrift:'AccountState'().

get_account_state(AccountID, Party) ->
    ok = ensure_account(AccountID, Party),
    Account = hg_accounting:get_account(AccountID),
    #{
        own_amount := OwnAmount,
        min_available_amount := MinAvailableAmount,
        currency_code := CurrencyCode
    } = Account,
    CurrencyRef = #domain_CurrencyRef{
        symbolic_code = CurrencyCode
    },
    Currency = hg_domain:get(hg_domain:head(), {currency, CurrencyRef}),
    #payproc_AccountState{
        account_id = AccountID,
        own_amount = OwnAmount,
        available_amount = MinAvailableAmount,
        currency = Currency
    }.

-spec get_new_shop_currency(shop(), party(), timestamp(), revision()) ->
    currency().

get_new_shop_currency(#domain_Shop{contract_id = ContractID}, Party, Timestamp, Revision) ->
    Currencies = case get_contract(ContractID, Party) of
        undefined ->
            throw({contract_not_exists, ContractID});
        Contract ->
            hg_contract:get_currencies(Contract, Timestamp, Revision)
    end,
    erlang:hd(ordsets:to_list(Currencies)).

%% Internals

get_contract_id(#domain_Contract{id = ContractID}) ->
    ContractID.

ensure_shop(#domain_Shop{} = Shop) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

-spec reduce_terms(dmsl_domain_thrift:'TermSet'(), hg_selector:varset(), revision()) ->
    dmsl_domain_thrift:'TermSet'().

reduce_terms(
    #domain_TermSet{payments = PaymentsTerms, recurrent_paytools = RecurrentPaytoolTerms, payouts = PayoutTerms},
    VS,
    Revision
) ->
    #domain_TermSet{
        payments = reduce_payments_terms(PaymentsTerms, VS, Revision),
        recurrent_paytools = reduce_recurrent_paytools_terms(RecurrentPaytoolTerms, VS, Revision),
        payouts = reduce_payout_terms(PayoutTerms, VS, Revision)
    }.

reduce_payments_terms(#domain_PaymentsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentsServiceTerms{
        currencies      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.currencies, VS, Rev),
        categories      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.categories, VS, Rev),
        payment_methods = reduce_if_defined(Terms#domain_PaymentsServiceTerms.payment_methods, VS, Rev),
        cash_limit      = reduce_if_defined(Terms#domain_PaymentsServiceTerms.cash_limit, VS, Rev),
        fees            = reduce_if_defined(Terms#domain_PaymentsServiceTerms.fees, VS, Rev),
        holds           = reduce_holds_terms(Terms#domain_PaymentsServiceTerms.holds, VS, Rev),
        refunds         = reduce_refunds_terms(Terms#domain_PaymentsServiceTerms.refunds, VS, Rev)
    };
reduce_payments_terms(undefined, _, _) ->
    undefined.

reduce_recurrent_paytools_terms(#domain_RecurrentPaytoolsServiceTerms{} = Terms, VS, Rev) ->
    #domain_RecurrentPaytoolsServiceTerms{
        payment_methods = reduce_if_defined(Terms#domain_RecurrentPaytoolsServiceTerms.payment_methods, VS, Rev)
    };
reduce_recurrent_paytools_terms(undefined, _, _) ->
    undefined.

reduce_holds_terms(#domain_PaymentHoldsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentHoldsServiceTerms{
        payment_methods = reduce_if_defined(Terms#domain_PaymentHoldsServiceTerms.payment_methods, VS, Rev),
        lifetime        = reduce_if_defined(Terms#domain_PaymentHoldsServiceTerms.lifetime, VS, Rev)
    };
reduce_holds_terms(undefined, _, _) ->
    undefined.

reduce_refunds_terms(#domain_PaymentRefundsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PaymentRefundsServiceTerms{
        payment_methods = reduce_if_defined(Terms#domain_PaymentRefundsServiceTerms.payment_methods, VS, Rev),
        fees            = reduce_if_defined(Terms#domain_PaymentRefundsServiceTerms.fees, VS, Rev)
    };
reduce_refunds_terms(undefined, _, _) ->
    undefined.

reduce_payout_terms(#domain_PayoutsServiceTerms{} = Terms, VS, Rev) ->
    #domain_PayoutsServiceTerms{
        payout_schedules = reduce_if_defined(Terms#domain_PayoutsServiceTerms.payout_schedules, VS, Rev),
        payout_methods   = reduce_if_defined(Terms#domain_PayoutsServiceTerms.payout_methods, VS, Rev),
        cash_limit       = reduce_if_defined(Terms#domain_PayoutsServiceTerms.cash_limit, VS, Rev),
        fees             = reduce_if_defined(Terms#domain_PayoutsServiceTerms.fees, VS, Rev)
    };
reduce_payout_terms(undefined, _, _) ->
    undefined.

reduce_if_defined(Selector, VS, Rev) when Selector =/= undefined ->
    hg_selector:reduce(Selector, VS, Rev);
reduce_if_defined(undefined, _, _) ->
    undefined.

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
    hg_datetime:between(Timestamp, hg_utils:select_defined(ValidSince, CreatedAt), ValidUntil).


get_term_set(TermsRef, Timestamp, Revision) ->
    #domain_TermSetHierarchy{
        parent_terms = ParentRef,
        term_sets = TimedTermSets
    } = hg_domain:get(Revision, {term_set_hierarchy, TermsRef}),
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
            case hg_datetime:between(Timestamp, ActionTime) of
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
    #domain_TermSet{payments = PaymentTerms1, recurrent_paytools = RecurrentPaytoolTerms1, payouts = PayoutTerms1},
    #domain_TermSet{payments = PaymentTerms0, recurrent_paytools = RecurrentPaytoolTerms0, payouts = PayoutTerms0}
) ->
    #domain_TermSet{
        payments = merge_payments_terms(PaymentTerms0, PaymentTerms1),
        recurrent_paytools = merge_recurrent_paytools_terms(RecurrentPaytoolTerms0, RecurrentPaytoolTerms1),
        payouts = merge_payouts_terms(PayoutTerms0, PayoutTerms1)
    };
merge_term_sets(TermSet1, TermSet0) ->
    hg_utils:select_defined(TermSet1, TermSet0).

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
        currencies      = hg_utils:select_defined(Curr1, Curr0),
        categories      = hg_utils:select_defined(Cat1, Cat0),
        payment_methods = hg_utils:select_defined(Pm1, Pm0),
        cash_limit      = hg_utils:select_defined(Al1, Al0),
        fees            = hg_utils:select_defined(Fee1, Fee0),
        holds           = merge_holds_terms(Hl0, Hl1),
        refunds         = merge_refunds_terms(Rf0, Rf1)
    };
merge_payments_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_recurrent_paytools_terms(
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = Pm0},
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = Pm1}
) ->
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = hg_utils:select_defined(Pm1, Pm0)};
merge_recurrent_paytools_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_holds_terms(
    #domain_PaymentHoldsServiceTerms{
        payment_methods = Pm0,
        lifetime = Lft0
    },
    #domain_PaymentHoldsServiceTerms{
        payment_methods = Pm1,
        lifetime = Lft1
    }
) ->
    #domain_PaymentHoldsServiceTerms{
        payment_methods = hg_utils:select_defined(Pm1, Pm0),
        lifetime        = hg_utils:select_defined(Lft1, Lft0)
    };
merge_holds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

merge_refunds_terms(
    #domain_PaymentRefundsServiceTerms{
        payment_methods = Pm0,
        fees = Fee0
    },
    #domain_PaymentRefundsServiceTerms{
        payment_methods = Pm1,
        fees = Fee1
    }
) ->
    #domain_PaymentRefundsServiceTerms{
        payment_methods = hg_utils:select_defined(Pm1, Pm0),
        fees            = hg_utils:select_defined(Fee1, Fee0)
    };
merge_refunds_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

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
        payout_schedules = hg_utils:select_defined(Ps1, Ps0),
        payout_methods   = hg_utils:select_defined(Pm1, Pm0),
        cash_limit       = hg_utils:select_defined(Cash1, Cash0),
        fees             = hg_utils:select_defined(Fee1, Fee0)
    };
merge_payouts_terms(Terms0, Terms1) ->
    hg_utils:select_defined(Terms1, Terms0).

ensure_account(AccountID, #domain_Party{shops = Shops}) ->
    case find_shop_account(AccountID, maps:to_list(Shops)) of
        #domain_ShopAccount{} ->
            ok;
        undefined ->
            throw(#payproc_AccountNotFound{})
    end.

find_shop_account(_ID, []) ->
    undefined;
find_shop_account(ID, [{_, #domain_Shop{account = Account}} | Rest]) ->
    case Account of
        #domain_ShopAccount{settlement = ID} ->
            Account;
        #domain_ShopAccount{guarantee = ID} ->
            Account;
        #domain_ShopAccount{payout = ID} ->
            Account;
        _ ->
            find_shop_account(ID, Rest)
    end.

%% Asserts
%% TODO there should be more concise way to express this assertions in terms of preconditions

-spec assert_party_objects_valid(timestamp(), revision(), party()) -> ok | no_return().
-spec assert_shop_contract_valid(shop(), contract(), timestamp(), revision()) -> ok | no_return().
-spec assert_shop_payout_tool_valid(shop(), contract()) -> ok | no_return().

assert_party_objects_valid(Timestamp, Revision, #domain_Party{shops = Shops} = Party) ->
    genlib_map:foreach(
        fun(_ID, Shop) ->
            assert_shop_valid(Shop, Timestamp, Revision, Party)
        end,
        Shops
    ).

assert_shop_valid(Shop, Timestamp, Revision, Party) ->
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
    ok = assert_shop_payout_tool_valid(Shop, Contract).

assert_shop_contract_valid(
    #domain_Shop{id = ID, category = CategoryRef, account = ShopAccount},
    Contract,
    Timestamp,
    Revision
) ->
    #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = CurrencySelector,
            categories = CategorySelector
        }
    } = get_terms(Contract, Timestamp, Revision),
    case ShopAccount of
        #domain_ShopAccount{currency = CurrencyRef} ->
            Currencies = hg_selector:reduce_to_value(CurrencySelector, #{}, Revision),
            _ = ordsets:is_element(CurrencyRef, Currencies) orelse
                raise_contract_terms_violated(
                    ID,
                    get_contract_id(Contract),
                    #domain_TermSet{payments = #domain_PaymentsServiceTerms{currencies = CurrencySelector}}
                );
        undefined ->
            % TODO change to special invalid_changeset error
            throw(#'InvalidRequest'{errors = [<<"Can't create shop without account">>]})
    end,
    Categories = hg_selector:reduce_to_value(CategorySelector, #{}, Revision),
    _ = ordsets:is_element(CategoryRef, Categories) orelse
        raise_contract_terms_violated(
            ID,
            get_contract_id(Contract),
            #domain_TermSet{payments = #domain_PaymentsServiceTerms{categories = CategorySelector}}
        ),
    ok.

-spec raise_contract_terms_violated(shop_id(), contract_id(), dmsl_domain_thrift:'TermSet'()) -> no_return().

raise_contract_terms_violated(ShopID, ContractID, Terms) ->
    hg_claim:raise_invalid_changeset(
        {contract_terms_violated, #payproc_ContractTermsViolated{
            shop_id = ShopID,
            contract_id = ContractID,
            terms = Terms
        }}
    ).

assert_shop_payout_tool_valid(#domain_Shop{payout_tool_id = PayoutToolID}, Contract) ->
    case hg_contract:get_payout_tool(PayoutToolID, Contract) of
        undefined ->
            hg_claim:raise_invalid_changeset({payout_tool_not_exists, PayoutToolID});
        _ ->
            ok
    end.

