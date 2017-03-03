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

-export([create_party/2]).
-export([get_party_id/1]).

-export([create_contract/3]).
-export([create_test_contract/2]).
-export([create_payout_tool/4]).
-export([create_contract_adjustment/3]).
-export([get_contract/2]).
-export([get_contract_id/1]).
-export([get_contract_currencies/3]).
-export([get_contract_categories/3]).
-export([is_test_contract/3]).

-export([create_shop/4]).
-export([create_test_shop/3]).
-export([get_shop/2]).
-export([get_shop_id/1]).
-export([get_shop_account/2]).
-export([get_account_state/2]).

-export([apply_change/3]).

-export([get_payments_service_terms/2]).
-export([get_payments_service_terms/3]).

%% Asserts

-export([assert_blocking/2]).
-export([assert_suspension/2]).
-export([assert_contract_active/1]).
-export([assert_shop_blocking/2]).
-export([assert_shop_suspension/2]).
-export([assert_shop_update_valid/5]).

%%

-type party()                 :: dmsl_domain_thrift:'Party'().
-type party_id()              :: dmsl_domain_thrift:'PartyID'().
-type contract()              :: dmsl_domain_thrift:'Contract'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_params()       :: dmsl_payment_processing_thrift:'ContractParams'().
-type adjustment_params()     :: dmsl_payment_processing_thrift:'ContractAdjustmentParams'().
-type payout_tool_params()    :: dmsl_payment_processing_thrift:'PayoutToolParams'().
-type shop()                  :: dmsl_domain_thrift:'Shop'().
-type shop_id()               :: dmsl_domain_thrift:'ShopID'().
-type shop_params()           :: dmsl_payment_processing_thrift:'ShopParams'().
-type currency()              :: dmsl_domain_thrift:'CurrencyRef'().
-type category()              :: dmsl_domain_thrift:'CategoryRef'().

-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type revision()              :: hg_domain:revision().


%% Interface

-spec create_party(party_id(), dmsl_payment_processing_thrift:'PartyParams'()) ->
    party().

create_party(PartyID, #payproc_PartyParams{contact_info = ContactInfo}) ->
    #domain_Party{
        id              = PartyID,
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


-spec create_contract(contract_params(), revision(), party()) ->
    contract().

create_contract(Params, Revision, Party) ->
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef
    } = ensure_contract_creation_params(Params, Revision),
    ContractID = get_next_contract_id(Party),
    Contract = instantiate_contract_template(TemplateRef, Revision),
    Contract#domain_Contract{
        id = ContractID,
        contractor = Contractor,
        status = {active, #domain_ContractActive{}},
        adjustments = [],
        payout_tools = []
    }.

-spec create_test_contract(revision(), party()) ->
    contract().

create_test_contract(Revision, Party) ->
    Params = #payproc_ContractParams{template = get_test_template_ref(Revision)},
    create_contract(Params, Revision, Party).

-spec get_contract(contract_id(), party()) ->
    contract().

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    case Contract = maps:get(ID, Contracts, undefined) of
        #domain_Contract{} ->
            Contract;
        undefined ->
            throw(#payproc_ContractNotFound{})
    end.

-spec get_contract_id(contract()) ->
    contract_id().

get_contract_id(#domain_Contract{id = ContractID}) ->
    ContractID.

-spec create_payout_tool(payout_tool_params(), timestamp(), revision(), contract()) ->
    dmsl_domain_thrift:'PayoutTool'().

create_payout_tool(
    #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = ToolInfo
    },
    Timestamp,
    Revision,
    Contract
) ->
    ok = assert_contract_active(Contract),
    ok = assert_contract_live(Contract, Timestamp, Revision),
    ID = get_next_payout_tool_id(Contract),
    #domain_PayoutTool{
        id = ID,
        currency = Currency,
        payout_tool_info = ToolInfo
    }.

-spec create_contract_adjustment(adjustment_params(), revision(), contract()) ->
    dmsl_domain_thrift:'ContractAdjustment'().

create_contract_adjustment(Params, Revision, Contract) ->
    ok = assert_contract_active(Contract),
    #payproc_ContractAdjustmentParams{
        template = TemplateRef
    } = ensure_adjustment_creation_params(Params, Revision),
    ID = get_next_contract_adjustment_id(Contract),
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = instantiate_contract_template(TemplateRef, Revision),
    #domain_ContractAdjustment{
        id = ID,
        valid_since = ValidSince,
        valid_until = ValidUntil,
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

-spec get_payments_service_terms(shop_id(), party(), timestamp()) ->
    dmsl_domain_thrift:'PaymentsServiceTerms'() | no_return().

get_payments_service_terms(ShopID, Party, Timestamp) ->
    Shop = get_shop(ShopID, Party),
    Contract = maps:get(Shop#domain_Shop.contract_id, Party#domain_Party.contracts),
    get_payments_service_terms(Contract, Timestamp).

-spec get_payments_service_terms(dmsl_domain_thrift:'Contract'(), timestamp()) ->
    dmsl_domain_thrift:'PaymentsServiceTerms'() | no_return().

get_payments_service_terms(Contract, Timestamp) ->
    ok = assert_contract_active(Contract),
    case compute_terms(Contract, Timestamp) of
        #domain_TermSet{payments = PaymentTerms} ->
            PaymentTerms;
        undefined ->
            error({misconfiguration, {'No active TermSet found', Contract#domain_Contract.terms, Timestamp}})
    end.

-spec create_shop(shop_params(), timestamp(), revision(), party()) ->
    shop().

create_shop(ShopParams, Timestamp, Revision, Party) ->
    create_shop(ShopParams, ?suspended(), Timestamp, Revision, Party).

create_shop(ShopParams, Suspension, Timestamp, Revision, Party) ->
    ShopID = get_next_shop_id(Party),
    Shop = #domain_Shop{
        id              = ShopID,
        blocking        = ?unblocked(<<>>),
        suspension      = Suspension,
        details         = ShopParams#payproc_ShopParams.details,
        category        = ShopParams#payproc_ShopParams.category,
        contract_id     = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id  = ShopParams#payproc_ShopParams.payout_tool_id,
        proxy           = ShopParams#payproc_ShopParams.proxy
    },
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
    ok = assert_shop_payout_tool_valid(Shop, Contract),
    Shop.

-spec create_test_shop(contract_id(), revision(), party()) ->
    shop().

create_test_shop(ContractID, Revision, Party) ->
    Params = get_shop_prototype_params(ContractID, Revision),
    #domain_Shop{
        id              = get_next_shop_id(Party),
        blocking        = ?unblocked(<<>>),
        suspension      = ?active(),
        details         = Params#payproc_ShopParams.details,
        category        = Params#payproc_ShopParams.category,
        contract_id     = Params#payproc_ShopParams.contract_id,
        payout_tool_id  = Params#payproc_ShopParams.payout_tool_id,
        proxy           = Params#payproc_ShopParams.proxy
    }.

-spec get_shop(shop_id(), party()) ->
    shop().

get_shop(ID, #domain_Party{shops = Shops}) ->
    case maps:get(ID, Shops, undefined) of
        Shop = #domain_Shop{} ->
            Shop;
        undefined ->
            throw(#payproc_ShopNotFound{})
    end.

-spec get_shop_id(shop()) ->
    shop_id().

get_shop_id(#domain_Shop{id = ID}) ->
    ID.

-spec get_shop_account(shop_id(), party()) ->
    dmsl_domain_thrift:'ShopAccount'().

get_shop_account(ShopID, Party) ->
    Shop = get_shop(ShopID, Party),
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

%% Internals

get_shop_prototype_params(ContractID, Revision) ->
    ShopPrototype = get_shop_prototype(Revision),
    #payproc_ShopParams{
        contract_id = ContractID,
        category = ShopPrototype#domain_ShopPrototype.category,
        details = ShopPrototype#domain_ShopPrototype.details
    }.

get_globals(Revision) ->
    hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}).

get_party_prototype(Revision) ->
    Globals = get_globals(Revision),
    hg_domain:get(Revision, {party_prototype, Globals#domain_Globals.party_prototype}).

get_shop_prototype(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.shop.

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

get_test_template_ref(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.test_contract_template.

get_default_template_ref(Revision) ->
    Globals = get_globals(Revision),
    Globals#domain_Globals.default_contract_template.

instantiate_contract_template(TemplateRef, Revision) ->
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, Revision),

    VS = case ValidSince of
        undefined ->
            hg_datetime:format_now();
        {timestamp, TimestampVS} ->
            TimestampVS;
        {interval, IntervalVS} ->
            add_interval(hg_datetime:format_now(), IntervalVS)
    end,
    VU = case ValidUntil of
        undefined ->
            undefined;
        {timestamp, TimestampVU} ->
            TimestampVU;
        {interval, IntervalVU} ->
            add_interval(VS, IntervalVU)
    end,
    #domain_Contract{
        valid_since = VS,
        valid_until = VU,
        terms = TermSetHierarchyRef
    }.

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
    #domain_ContractAdjustment{valid_since = ValidSince, valid_until = ValidUntil},
    Timestamp
) ->
    hg_datetime:between(Timestamp, ValidSince, ValidUntil).

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

get_payout_tool(PayoutToolID, #domain_Contract{payout_tools = PayoutTools}) ->
    case lists:keysearch(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
        {value, PayoutTool} ->
            PayoutTool;
        false ->
            throw(#payproc_PayoutToolNotFound{})
    end.

set_contract(Contract = #domain_Contract{id = ID}, Party = #domain_Party{contracts = Contracts}) ->
    Party#domain_Party{contracts = Contracts#{ID => Contract}}.


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
                % FIXME add special status for expired contracts
                status = {terminated, #domain_ContractTerminated{terminated_at = ValidUntil}}
            }
    end;

update_contract_status(Contract, _) ->
    Contract.

set_shop(Shop = #domain_Shop{id = ID}, Party = #domain_Party{shops = Shops}) ->
    Party#domain_Party{shops = Shops#{ID => Shop}}.

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

%% TODO move all work with changes to hg_party_machine, provide interface for party modification
-spec apply_change(dmsl_payment_processing_thrift:'PartyModification'(), party(), timestamp()) ->
    party().

apply_change({blocking, Blocking}, Party, _) ->
    Party#domain_Party{blocking = Blocking};
apply_change({suspension, Suspension}, Party, _) ->
    Party#domain_Party{suspension = Suspension};
apply_change(?contract_creation(Contract), Party, Timestamp) ->
    set_contract(update_contract_status(Contract, Timestamp), Party);
apply_change(?contract_termination(ID, TerminatedAt, _Reason), Party, _) ->
    Contract = get_contract(ID, Party),
    set_contract(
        Contract#domain_Contract{
            status = {terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}
        },
        Party
    );
apply_change(
    ?contract_legal_agreement_binding(ContractID, LegalAgreement),
    Party = #domain_Party{contracts = Contracts},
    _Timestamp
) ->
    Contract = maps:get(ContractID, Contracts),
    Party#domain_Party{
        contracts = Contracts#{
            ContractID => Contract#domain_Contract{legal_agreement = LegalAgreement}
        }
    };
apply_change(
    ?contract_payout_tool_creation(ContractID, PayoutTool),
    Party = #domain_Party{contracts = Contracts},
    _Timestamp
) ->
    Contract = #domain_Contract{payout_tools = PayoutTools} = maps:get(ContractID, Contracts),
    Party#domain_Party{
        contracts = Contracts#{
            ContractID => Contract#domain_Contract{payout_tools = PayoutTools ++ [PayoutTool]}
        }
    };
apply_change(?contract_adjustment_creation(ID, Adjustment), Party, _) ->
    Contract = get_contract(ID, Party),
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    set_contract(Contract#domain_Contract{adjustments = Adjustments}, Party);
apply_change({shop_creation, Shop}, Party, _) ->
    set_shop(Shop, Party);
apply_change(?shop_modification(ID, V), Party, _) ->
    set_shop(apply_shop_change(V, get_shop(ID, Party)), Party).

apply_shop_change({blocking, Blocking}, Shop) ->
    Shop#domain_Shop{blocking = Blocking};
apply_shop_change({suspension, Suspension}, Shop) ->
    Shop#domain_Shop{suspension = Suspension};
apply_shop_change({update, Update}, Shop) ->
    fold_opt([
        {Update#payproc_ShopUpdate.category,
            fun (V, S) -> S#domain_Shop{category = V} end},
        {Update#payproc_ShopUpdate.details,
            fun (V, S) -> S#domain_Shop{details = V} end},
        {Update#payproc_ShopUpdate.contract_id,
            fun (V, S) -> S#domain_Shop{contract_id = V} end},
        {Update#payproc_ShopUpdate.payout_tool_id,
            fun (V, S) -> S#domain_Shop{payout_tool_id = V} end},
        {Update#payproc_ShopUpdate.proxy,
            fun (V, S) -> S#domain_Shop{proxy = V} end}
    ], Shop);
apply_shop_change(?account_created(ShopAccount), Shop) ->
    Shop#domain_Shop{account = ShopAccount}.

fold_opt([], V) ->
    V;
fold_opt([{undefined, _} | Rest], V) ->
    fold_opt(Rest, V);
fold_opt([{E, Fun} | Rest], V) ->
    fold_opt(Rest, Fun(E, V)).

get_next_contract_id(#domain_Party{contracts = Contracts}) ->
    get_next_id(maps:keys(Contracts)).

get_next_payout_tool_id(#domain_Contract{payout_tools = Tools}) ->
    get_next_id([ID || #domain_PayoutTool{id = ID} <- Tools]).

get_next_contract_adjustment_id(#domain_Contract{adjustments = Adjustments}) ->
    get_next_id([ID || #domain_ContractAdjustment{id = ID} <- Adjustments]).

get_next_shop_id(#domain_Party{shops = Shops}) ->
    get_next_id(maps:keys(Shops)).

get_next_id(IDs) ->
    lists:max([0 | IDs]) + 1.

-spec raise_invalid_request(binary()) ->
    no_return().

raise_invalid_request(Error) ->
    throw(#'InvalidRequest'{errors = [Error]}).


%% Asserts
%% TODO there should be more concise way to express this assertions in terms of preconditions

-spec assert_blocking(party(), term()) ->       ok | no_return().
-spec assert_suspension(party(), term()) ->     ok | no_return().
-spec assert_contract_active(contract()) ->     ok | no_return().
-spec assert_shop_blocking(shop(), term()) ->   ok | no_return().
-spec assert_shop_suspension(shop(), term()) -> ok | no_return().
-spec assert_shop_update_valid(shop_id(), term(), party(), timestamp(), revision()) -> ok | no_return().
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

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    throw(#payproc_InvalidContractStatus{status = Status}).

assert_contract_live(Contract, Timestamp, Revision) ->
    _ = not is_test_contract(Contract, Timestamp, Revision) orelse
        raise_invalid_request(<<"creating payout tool for test contract unavailable">>),
    ok.

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

assert_shop_update_valid(ShopID, Update, Party, Timestamp, Revision) ->
    Shop = apply_shop_change({update, Update}, get_shop(ShopID, Party)),
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
    case is_test_contract(Contract, Timestamp, Revision) of
        true when Shop#domain_Shop.payout_tool_id == undefined ->
            ok;
        true ->
            raise_invalid_request(<<"using payout tool with test shop unavailable">>);
        false ->
            assert_shop_payout_tool_valid(Shop, Contract)
    end.

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
    _PayoutTool = get_payout_tool(PayoutToolID, Contract),
    ok.

