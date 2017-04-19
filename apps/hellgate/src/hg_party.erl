%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/merchant.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/contract.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)
%% * Create accounts after shop claim confirmation

-module(hg_party).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

%% Party support functions

-export([create_party/3]).
-export([get_party_id/1]).
-export([blocking/2]).
-export([suspension/2]).

-export([create_contract/4]).
-export([set_new_contract/3]).
-export([is_contract_active/1]).
-export([is_test_contract/3]).
-export([update_contract_status/2]).
-export([get_test_contract_params/2]).
-export([get_test_payout_tool_params/1]).
-export([create_payout_tool/3]).
-export([create_contract_adjustment/4]).

-export([get_contract/2]).
-export([get_contract_id/1]).
-export([get_contract_status/1]).
-export([get_contract_currencies/3]).
-export([get_contract_categories/3]).
-export([get_contract_adjustment/2]).
-export([get_contract_payout_tool/2]).

-export([set_contract_status/3]).
-export([set_contract_adjustment/3]).
-export([set_contract_payout_tool/3]).
-export([set_contract_legal_agreement/3]).

-export([get_payments_service_terms/2]).
-export([get_payments_service_terms/3]).

-export([create_shop/3]).
-export([shop_blocking/3]).
-export([shop_suspension/3]).
-export([set_new_shop/2]).

-export([get_test_shop_params/3]).
-export([get_new_shop_currency/4]).
-export([get_shop/2]).
-export([get_shop_id/1]).
-export([get_shop_account/2]).
-export([get_account_state/2]).

-export([set_shop_category/3]).
-export([set_shop_details/3]).
-export([set_shop_location/3]).
-export([set_shop_contract/4]).
-export([set_shop_payout_tool/3]).
-export([set_shop_proxy/3]).
-export([set_shop_account/3]).

%% Asserts

-export([assert_blocking/2]).
-export([assert_suspension/2]).
-export([assert_shop_blocking/2]).
-export([assert_shop_suspension/2]).
-export([assert_party_objects_valid/3]).

%%

-type party()                 :: dmsl_domain_thrift:'Party'().
-type party_id()              :: dmsl_domain_thrift:'PartyID'().
-type contract()              :: dmsl_domain_thrift:'Contract'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_params()       :: dmsl_payment_processing_thrift:'ContractParams'().
-type contract_status()       :: dmsl_domain_thrift:'ContractStatus'().
-type adjustment()            :: dmsl_domain_thrift:'ContractAdjustment'().
-type adjustment_id()         :: dmsl_domain_thrift:'ContractAdjustmentID'().
-type adjustment_params()     :: dmsl_payment_processing_thrift:'ContractAdjustmentParams'().
-type payout_tool()           :: dmsl_domain_thrift:'PayoutTool'().
-type payout_tool_id()        :: dmsl_domain_thrift:'PayoutToolID'().
-type payout_tool_params()    :: dmsl_payment_processing_thrift:'PayoutToolParams'().
-type shop()                  :: dmsl_domain_thrift:'Shop'().
-type shop_id()               :: dmsl_domain_thrift:'ShopID'().
-type shop_params()           :: dmsl_payment_processing_thrift:'ShopParams'().
-type currency()              :: dmsl_domain_thrift:'CurrencyRef'().
-type category()              :: dmsl_domain_thrift:'CategoryRef'().

-type blocking()              :: dmsl_domain_thrift:'Blocking'().
-type suspension()            :: dmsl_domain_thrift:'Suspension'().

-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type revision()              :: hg_domain:revision().


%% Interface

-spec create_party(party_id(), dmsl_payment_processing_thrift:'PartyParams'(), timestamp()) ->
    party().

create_party(PartyID, #payproc_PartyParams{contact_info = ContactInfo}, Timestamp) ->
    #domain_Party{
        id              = PartyID,
        created_at      = Timestamp,
        contact_info    = ContactInfo,
        blocking        = ?unblocked(<<>>),
        suspension      = ?active(),
        contracts       = #{},
        shops           = #{}
    }.

-spec get_party_id(party()) ->
    party_id().

get_party_id(#domain_Party{id = ID}) ->
    ID.

-spec blocking(blocking(), party()) ->
    party().

blocking(Blocking, Party) ->
    Party#domain_Party{blocking = Blocking}.

-spec suspension(suspension(), party()) ->
    party().

suspension(Suspension, Party) ->
    Party#domain_Party{suspension = Suspension}.

-spec get_contract_id(contract()) ->
    contract_id().

get_contract_id(#domain_Contract{id = ContractID}) ->
    ContractID.

-spec is_contract_active(contract()) ->
    boolean().

is_contract_active(#domain_Contract{status = {active, _}}) ->
    true;
is_contract_active(_) ->
    false.

-spec create_contract(contract_id(), contract_params(), timestamp(), revision()) ->
    contract().

create_contract(ID, Params, Timestamp, Revision) ->
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef
    } = ensure_contract_creation_params(Params, Revision),
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, Revision),
    #domain_Contract{
        id = ID,
        contractor = Contractor,
        created_at = Timestamp,
        valid_since = instantiate_contract_lifetime_bound(ValidSince, Timestamp),
        valid_until = instantiate_contract_lifetime_bound(ValidUntil, Timestamp),
        status = {active, #domain_ContractActive{}},
        terms = TermSetHierarchyRef,
        adjustments = [],
        payout_tools = []
    }.

-spec get_contract(contract_id(), party()) ->
    contract() | undefined.

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    maps:get(ID, Contracts, undefined).

-spec set_new_contract(contract(), timestamp(), party()) ->
    party().

set_new_contract(Contract, Timestamp, Party) ->
    set_contract(update_contract_status(Contract, Timestamp), Party).

-spec get_contract_status(contract()) ->
    contract_status().

get_contract_status(#domain_Contract{status = Status}) ->
    Status.

-spec set_contract_status(contract_id(), contract_status(), party()) ->
    party().

set_contract_status(ContractID, Status, Party) ->
    Contract = get_contract(ContractID, Party),
    set_contract(Contract#domain_Contract{status = Status}, Party).

-spec get_contract_adjustment(adjustment_id(), contract()) ->
    adjustment() | undefined.

get_contract_adjustment(AdjustmentID, #domain_Contract{adjustments = Adjustments}) ->
    case lists:keysearch(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
        {value, Adjustment} ->
            Adjustment;
        false ->
            undefined
    end.

-spec set_contract_adjustment(contract_id(), adjustment(), party()) ->
    party().

set_contract_adjustment(ContractID, Adjustment, Party) ->
    Contract = get_contract(ContractID, Party),
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    set_contract(Contract#domain_Contract{adjustments = Adjustments}, Party).

-spec get_contract_payout_tool(payout_tool_id(), contract()) ->
    payout_tool() | undefined.

get_contract_payout_tool(PayoutToolID, #domain_Contract{payout_tools = PayoutTools}) ->
    case lists:keysearch(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
        {value, PayoutTool} ->
            PayoutTool;
        false ->
            undefined
    end.

-spec set_contract_payout_tool(contract_id(), payout_tool(), party()) ->
    party().

set_contract_payout_tool(ContractID, PayoutTool, Party) ->
    Contract = get_contract(ContractID, Party),
    PayoutTools = Contract#domain_Contract.payout_tools ++ [PayoutTool],
    set_contract(Contract#domain_Contract{payout_tools = PayoutTools}, Party).

-spec set_contract_legal_agreement(contract_id(), dmsl_domain_thrift:'LegalAgreement'(), party()) ->
    party().

set_contract_legal_agreement(ContractID, LegalAgreement, Party) ->
    Contract = get_contract(ContractID, Party),
    set_contract(Contract#domain_Contract{legal_agreement = LegalAgreement}, Party).

-spec get_test_contract_params(binary(), revision()) ->
    {contract_id(), contract_params()}.

get_test_contract_params(Email, Revision) ->
    #domain_ContractPrototype{
        contract_id = ContractID,
        test_contract_template = TemplateRef
    } = get_contract_prototype(Revision),
    {
        ContractID,
        #payproc_ContractParams{
            contractor = {registered_user, #domain_RegisteredUser{email = Email}},
            template = TemplateRef
        }
    }.

-spec get_test_payout_tool_params(revision()) ->
    {payout_tool_id(), payout_tool_params()}.

get_test_payout_tool_params(Revision) ->
    #domain_ContractPrototype{
        payout_tool = #domain_PayoutToolPrototype{
            payout_tool_id = ID,
            payout_tool_info = ToolInfo,
            payout_tool_currency = Currency
        }
    } = get_contract_prototype(Revision),
    {
        ID,
        #payproc_PayoutToolParams{
            currency = Currency,
            tool_info = ToolInfo
        }
    }.

-spec create_payout_tool(payout_tool_id(), payout_tool_params(), timestamp()) ->
    payout_tool().

create_payout_tool(
    ID,
    #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = ToolInfo
    },
    Timestamp
) ->
    #domain_PayoutTool{
        id = ID,
        created_at = Timestamp,
        currency = Currency,
        payout_tool_info = ToolInfo
    }.

-spec create_contract_adjustment(adjustment_id(), adjustment_params(), timestamp(), revision()) ->
    adjustment().

create_contract_adjustment(ID, Params, Timestamp, Revision) ->
    #payproc_ContractAdjustmentParams{
        template = TemplateRef
    } = ensure_adjustment_creation_params(Params, Revision),
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, Revision),
    #domain_ContractAdjustment{
        id = ID,
        created_at = Timestamp,
        valid_since = instantiate_contract_lifetime_bound(ValidSince, Timestamp),
        valid_until = instantiate_contract_lifetime_bound(ValidUntil, Timestamp),
        terms = TermSetHierarchyRef
    }.

-spec get_contract_currencies(contract(), timestamp(), revision()) ->
    ordsets:ordset(currency()) | no_return().

get_contract_currencies(Contract, Timestamp, Revision) ->
    #domain_PaymentsServiceTerms{currencies = CurrencySelector} = get_payments_service_terms(Contract, Timestamp),
    Value = reduce_selector_to_value(CurrencySelector, #{}, Revision),
    case ordsets:size(Value) > 0 of
        true ->
            Value;
        false ->
            error({misconfiguration, {'Empty set in currency selector\'s value', CurrencySelector, Revision}})
    end.

-spec get_contract_categories(contract(), timestamp(), revision()) ->
    ordsets:ordset(category()) | no_return().

get_contract_categories(Contract, Timestamp, Revision) ->
    #domain_PaymentsServiceTerms{categories = CategorySelector} = get_payments_service_terms(Contract, Timestamp),
    Value = reduce_selector_to_value(CategorySelector, #{}, Revision),
    case ordsets:size(Value) > 0 of
        true ->
            Value;
        false ->
            error({misconfiguration, {'Empty set in category selector\'s value', CategorySelector, Revision}})
    end.

-spec update_contract_status(contract(), timestamp()) ->
    contract().

update_contract_status(
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        status = {active, _}
    } = Contract,
    Timestamp
) ->
    case hg_datetime:between(Timestamp, ValidSince, ValidUntil) of
        true ->
            Contract;
        false ->
            Contract#domain_Contract{
                status = {expired, #domain_ContractExpired{}}
            }
    end;
update_contract_status(Contract, _) ->
    Contract.

-spec get_payments_service_terms(shop_id(), party(), timestamp()) ->
    dmsl_domain_thrift:'PaymentsServiceTerms'() | no_return().

get_payments_service_terms(ShopID, Party, Timestamp) ->
    Shop = ensure_shop(get_shop(ShopID, Party)),
    Contract = maps:get(Shop#domain_Shop.contract_id, Party#domain_Party.contracts),
    get_payments_service_terms(Contract, Timestamp).

-spec get_payments_service_terms(contract(), timestamp()) ->
    dmsl_domain_thrift:'PaymentsServiceTerms'() | no_return().

get_payments_service_terms(Contract, Timestamp) ->
    case compute_terms(Contract, Timestamp) of
        #domain_TermSet{payments = PaymentTerms} ->
            PaymentTerms;
        undefined ->
            error({misconfiguration, {'No active TermSet found', Contract#domain_Contract.terms, Timestamp}})
    end.

-spec create_shop(shop_id(), shop_params(), timestamp()) ->
    shop().

create_shop(ID, ShopParams, Timestamp) ->
    #domain_Shop{
        id              = ID,
        created_at      = Timestamp,
        blocking        = ?unblocked(<<>>),
        suspension      = ?active(),
        category        = ShopParams#payproc_ShopParams.category,
        details         = ShopParams#payproc_ShopParams.details,
        location        = ShopParams#payproc_ShopParams.location,
        contract_id     = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id  = ShopParams#payproc_ShopParams.payout_tool_id
    }.

-spec get_test_shop_params(contract_id(), payout_tool_id(), revision()) ->
    {shop_id(), shop_params()}.

get_test_shop_params(ContractID, PayoutToolID, Revision) ->
    ShopPrototype = get_shop_prototype(Revision),
    {
        ShopPrototype#domain_ShopPrototype.shop_id,
        #payproc_ShopParams{
            location = ShopPrototype#domain_ShopPrototype.location,
            category = ShopPrototype#domain_ShopPrototype.category,
            details = ShopPrototype#domain_ShopPrototype.details,
            contract_id = ContractID,
            payout_tool_id = PayoutToolID
        }
    }.

-spec get_shop(shop_id(), party()) ->
    shop() | undefined.

get_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

-spec set_new_shop(shop(), party()) ->
    party().

set_new_shop(Shop, Party) ->
    set_shop(Shop, Party).

-spec set_shop_category(shop_id(), category(), party()) ->
    party().

set_shop_category(ShopID, Category, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{category = Category}, Party).

-spec set_shop_details(shop_id(), dmsl_domain_thrift:'ShopDetails'(), party()) ->
    party().

set_shop_details(ShopID, Details, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{details = Details}, Party).

-spec set_shop_contract(shop_id(), contract_id(), payout_tool_id(), party()) ->
    party().

set_shop_contract(ShopID, ContractID, PayoutToolID, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{contract_id = ContractID, payout_tool_id = PayoutToolID}, Party).

-spec set_shop_payout_tool(shop_id(), payout_tool_id(), party()) ->
    party().

set_shop_payout_tool(ShopID, PayoutToolID, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{payout_tool_id = PayoutToolID}, Party).

-spec set_shop_location(shop_id(), dmsl_domain_thrift:'ShopLocation'(), party()) ->
    party().

set_shop_location(ShopID, Location, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{location = Location}, Party).

-spec set_shop_proxy(shop_id(), dmsl_domain_thrift:'Proxy'(), party()) ->
    party().

set_shop_proxy(ShopID, Proxy, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{proxy = Proxy}, Party).

-spec set_shop_account(shop_id(), dmsl_domain_thrift:'ShopAccount'(), party()) ->
    party().

set_shop_account(ShopID, Account, Party) ->
    Shop = get_shop(ShopID, Party),
    set_shop(Shop#domain_Shop{account = Account}, Party).

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

-spec get_shop_id(shop()) ->
    shop_id().

get_shop_id(#domain_Shop{id = ID}) ->
    ID.

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
            get_contract_currencies(Contract, Timestamp, Revision)
    end,
    erlang:hd(ordsets:to_list(Currencies)).

%% Internals

set_contract(Contract = #domain_Contract{id = ID}, Party = #domain_Party{contracts = Contracts}) ->
    Party#domain_Party{contracts = Contracts#{ID => Contract}}.

set_shop(Shop = #domain_Shop{id = ID}, Party = #domain_Party{shops = Shops}) ->
    Party#domain_Party{shops = Shops#{ID => Shop}}.

ensure_shop(#domain_Shop{} = Shop) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

get_globals(Revision) ->
    hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}).

get_party_prototype(Revision) ->
    Globals = get_globals(Revision),
    hg_domain:get(Revision, {party_prototype, Globals#domain_Globals.party_prototype}).

get_shop_prototype(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.shop.

get_contract_prototype(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.contract.

ensure_contract_creation_params(#payproc_ContractParams{template = TemplateRef} = Params, Revision) ->
    Params#payproc_ContractParams{
        template = ensure_contract_template(TemplateRef, Revision)
    }.

ensure_adjustment_creation_params(#payproc_ContractAdjustmentParams{template = TemplateRef} = Params, Revision) ->
    Params#payproc_ContractAdjustmentParams{
        template = ensure_contract_template(TemplateRef, Revision)
    }.

ensure_contract_template(#domain_ContractTemplateRef{} = TemplateRef, Revision) ->
    try
        _GoodTemplate = get_template(TemplateRef, Revision),
        TemplateRef
    catch
        error:{object_not_found, _} ->
            raise_invalid_request(<<"contract template not found">>)
    end;

ensure_contract_template(undefined, Revision) ->
    get_default_template_ref(Revision).

-spec reduce_selector_to_value(Selector, #{}, revision())
    -> ordsets:ordset(currency()) | ordsets:ordset(category()) | no_return()
    when Selector :: dmsl_domain_thrift:'CurrencySelector'() | dmsl_domain_thrift:'CategorySelector'().

reduce_selector_to_value(Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, Value} ->
            Value;
        _ ->
            error({misconfiguration, {'Can\'t reduce selector to value', Selector, VS, Revision}})
    end.

get_template(TemplateRef, Revision) ->
    hg_domain:get(Revision, {contract_template, TemplateRef}).

get_default_template_ref(Revision) ->
    Globals = get_globals(Revision),
    Globals#domain_Globals.default_contract_template.

instantiate_contract_lifetime_bound(undefined, _) ->
    undefined;
instantiate_contract_lifetime_bound({timestamp, Timestamp}, _) ->
    Timestamp;
instantiate_contract_lifetime_bound({interval, Interval}, Timestamp) ->
    add_interval(Timestamp, Interval).

add_interval(Timestamp, Interval) ->
    #domain_LifetimeInterval{
        years = YY,
        months = MM,
        days = DD
    } = Interval,
    hg_datetime:add_interval(Timestamp, {YY, MM, DD}).

compute_terms(#domain_Contract{terms = TermsRef, adjustments = Adjustments}, Timestamp) ->
    ActiveAdjustments = lists:filter(fun(A) -> is_adjustment_active(A, Timestamp) end, Adjustments),
    % Adjustments are ordered from oldest to newest
    ActiveTermRefs = [TermsRef | [TRef || #domain_ContractAdjustment{terms = TRef} <- ActiveAdjustments]],
    ActiveTermSets = lists:map(
        fun(TRef) ->
            get_term_set(TRef, Timestamp)
        end,
        ActiveTermRefs
    ),
    merge_term_sets(ActiveTermSets).

is_adjustment_active(
    #domain_ContractAdjustment{created_at = CreatedAt, valid_since = ValidSince, valid_until = ValidUntil},
    Timestamp
) ->
    hg_datetime:between(Timestamp, update_if_defined(CreatedAt, ValidSince), ValidUntil).


get_term_set(TermsRef, Timestamp) ->
    % FIXME revision as param?
    Revision = hg_domain:head(),
    #domain_TermSetHierarchy{
        parent_terms = ParentRef,
        term_sets = TimedTermSets
    } = hg_domain:get(Revision, {term_set_hierarchy, TermsRef}),
    TermSet = get_active_term_set(TimedTermSets, Timestamp),
    case ParentRef of
        undefined ->
            TermSet;
        #domain_TermSetHierarchyRef{} ->
            ParentTermSet = get_term_set(ParentRef, Timestamp),
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

merge_term_sets(#domain_TermSet{payments = PaymentTerms1}, #domain_TermSet{payments = PaymentTerms0}) ->
    #domain_TermSet{payments = merge_payments_terms(PaymentTerms0, PaymentTerms1)};
merge_term_sets(undefined, TermSet) ->
    TermSet;
merge_term_sets(TermSet, undefined) ->
    TermSet.

merge_payments_terms(
    #domain_PaymentsServiceTerms{
        currencies = Curr0,
        categories = Cat0,
        payment_methods = Pm0,
        cash_limit = Al0,
        fees = Fee0,
        guarantee_fund = Gf0
    },
    #domain_PaymentsServiceTerms{
        currencies = Curr1,
        categories = Cat1,
        payment_methods = Pm1,
        cash_limit = Al1,
        fees = Fee1,
        guarantee_fund = Gf1
    }
) ->
    #domain_PaymentsServiceTerms{
        currencies = update_if_defined(Curr0, Curr1),
        categories = update_if_defined(Cat0, Cat1),
        payment_methods = update_if_defined(Pm0, Pm1),
        cash_limit = update_if_defined(Al0, Al1),
        fees = update_if_defined(Fee0, Fee1),
        guarantee_fund = update_if_defined(Gf0, Gf1)
    };
merge_payments_terms(undefined, Any) ->
    Any;
merge_payments_terms(Any, undefined) ->
    Any.

update_if_defined(Value, undefined) ->
    Value;
update_if_defined(_, Value) ->
    Value.

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

-spec raise_invalid_request(binary()) ->
    no_return().

raise_invalid_request(Error) ->
    throw(#'InvalidRequest'{errors = [Error]}).

-spec is_test_contract(contract(), timestamp(), revision()) ->
    boolean().

is_test_contract(Contract, Timestamp, Revision) ->
    Categories = get_contract_categories(Contract, Timestamp, Revision),
    {Test, Live} = lists:foldl(
        fun(CategoryRef, {TestFound, LiveFound}) ->
            case hg_domain:get(Revision, {category, CategoryRef}) of
                #domain_Category{type = test} ->
                    {true, LiveFound};
                #domain_Category{type = live} ->
                    {TestFound, true}
            end
        end,
        {false, false},
        ordsets:to_list(Categories)
    ),
    case Test /= Live of
        true ->
            Test;
        false ->
            error({
                misconfiguration,
                {'Test and live category in same term set', Contract#domain_Contract.terms, Timestamp, Revision}
            })
    end.

%% Asserts
%% TODO there should be more concise way to express this assertions in terms of preconditions

-spec assert_blocking(party(), term()) ->       ok | no_return().
-spec assert_suspension(party(), term()) ->     ok | no_return().
-spec assert_shop_blocking(shop(), term()) ->   ok | no_return().
-spec assert_shop_suspension(shop(), term()) -> ok | no_return().
-spec assert_party_objects_valid(timestamp(), revision(), party()) -> ok | no_return().
-spec assert_shop_contract_valid(shop(), contract(), timestamp(), revision()) -> ok | no_return().
-spec assert_shop_payout_tool_valid(shop(), contract()) -> ok | no_return().

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_suspension(#domain_Party{suspension = {Status, _}}, Status) ->
    ok;
assert_suspension(#domain_Party{suspension = Suspension}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {suspension, Suspension}}).

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

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
    #domain_Shop{category = CategoryRef, account = ShopAccount},
    Contract,
    Timestamp,
    Revision
) ->
    #domain_PaymentsServiceTerms{
        currencies = CurrencySelector,
        categories = CategorySelector
    } = get_payments_service_terms(Contract, Timestamp),
    case ShopAccount of
        #domain_ShopAccount{currency = CurrencyRef} ->
            Currencies = reduce_selector_to_value(CurrencySelector, #{}, Revision),
            _ = ordsets:is_element(CurrencyRef, Currencies) orelse
                raise_invalid_request(<<"currency is not permitted by contract">>);
        undefined ->
            ok
    end,
    Categories = reduce_selector_to_value(CategorySelector, #{}, Revision),
    _ = ordsets:is_element(CategoryRef, Categories) orelse
        raise_invalid_request(<<"category is not permitted by contract">>),
    ok.

assert_shop_payout_tool_valid(#domain_Shop{payout_tool_id = PayoutToolID}, Contract) ->
    case get_contract_payout_tool(PayoutToolID, Contract) of
        undefined ->
            throw({payout_tool_not_found, PayoutToolID});
        _ ->
            ok
    end.

