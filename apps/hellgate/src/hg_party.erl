%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/merchant.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/contract.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)
%% * Create accounts after shop claim confirmation

-module(hg_party).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

%% Party support functions

-export([get_payments_service_terms/2]).
-export([get_payments_service_terms/3]).

%%

-type party_id()              :: dmsl_domain_thrift:'PartyID'().
-type party()                 :: dmsl_domain_thrift:'Party'().
-type shop_id()               :: dmsl_domain_thrift:'ShopID'().
-type shop_params()           :: dmsl_payment_processing_thrift:'ShopParams'().
-type shop_update()           :: dmsl_payment_processing_thrift:'ShopUpdate'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_params()       :: dmsl_payment_processing_thrift:'ContractParams'().
-type adjustment_params()     :: dmsl_payment_processing_thrift:'ContractAdjustmentParams'().
-type payout_tool_params()    :: dmsl_payment_processing_thrift:'PayoutToolParams'().
-type claim_id()              :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()                 :: dmsl_payment_processing_thrift:'Claim'().
-type user_info()             :: dmsl_payment_processing_thrift:'UserInfo'().
-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type sequence()              :: pos_integer().
-type legal_agreement()       :: dmsl_domain_thrift:'LegalAgreement'().

-type ev() ::
    {sequence(), public_event() | private_event()}.

new(PartyID, #payproc_PartyParams{contact_info = ContactInfo}) ->
    #domain_Party{
        id              = PartyID,
        contact_info    = ContactInfo,
        blocking        = ?unblocked(<<>>),
        suspension      = ?active(),
        contracts       = #{},
        shops           = #{}
    }.

get_party_id(#domain_Party{id = ID}) ->
    ID.

%%

is_test_contract(Contract, Timestamp, Revision) ->
    Categories = get_contract_categories(Contract, Timestamp, Revision),
    lists:any(
        fun(CategoryRef) ->
            #domain_Category{type = Type} = hg_domain:get(Revision, {category, CategoryRef}),
            Type == test
        end,
        ordsets:to_list(Categories)
    ).

create_contract(
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef,
    },
    Party
) ->
    ContractID = get_next_contract_id(Party),
    Contract = instantiate_contract_template(TemplateRef),
    Contract#domain_Contract{
        id = ContractID,
        contractor = Contractor,
        status = {active, #domain_ContractActive{}},
        adjustments = [],
        payout_tools = []
    }.

get_contract_id(#domain_Contract{id = ContractID}) ->
    ContractID.

get_next_contract_id(#domain_Party{contracts = Contracts}) ->
    get_next_id(maps:keys(Contracts)).

create_payout_tool(
    #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = ToolInfo
    },
    Contract
) ->
    ID = get_next_payout_tool_id(Contract),
    #domain_PayoutTool{
        id = ID,
        currency = Currency,
        payout_tool_info = ToolInfo
    };

get_next_payout_tool_id(#domain_Contract{payout_tools = Tools}) ->
    get_next_id([ID || #domain_PayoutTool{id = ID} <- Tools]).

create_contract_adjustment(#payproc_ContractAdjustmentParams{template = TemplateRef}, Contract) ->
    ID = get_next_contract_adjustment_id(Contract),
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = instantiate_contract_template(TemplateRef),
    #domain_ContractAdjustment{
        id = ID,
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    }.

get_next_contract_adjustment_id(#domain_Contract{adjustments = Adjustments}) ->
    get_next_id([ID || #domain_ContractAdjustment{id = ID} <- Adjustments]).

create_shop(ShopParams, Party) ->
    create_shop(ShopParams, ?suspended(), Party).

create_shop(ShopParams, Suspension, Party) ->
    ShopID = get_next_shop_id(Party),
    #domain_Shop{
        id              = ShopID,
        blocking        = ?unblocked(<<>>),
        suspension      = Suspension,
        details         = ShopParams#payproc_ShopParams.details,
        category        = ShopParams#payproc_ShopParams.category,
        contract_id     = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id  = ShopParams#payproc_ShopParams.payout_tool_id,
        proxy           = ShopParams#payproc_ShopParams.proxy
    }.

get_next_shop_id(#domain_Party{shops = Shops}) ->
    % TODO cache sequences on history collapse
    get_next_id(maps:keys(Shops)).

%%

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

ensure_contract_creation_params(#payproc_ContractParams{template = TemplateRef} = Params) ->
    Params#payproc_ContractParams{
        template = ensure_contract_template(TemplateRef)
    }.

ensure_adjustment_creation_params(#payproc_ContractAdjustmentParams{template = TemplateRef} = Params) ->
    Params#payproc_ContractAdjustmentParams{
        template = ensure_contract_template(TemplateRef)
    }.

ensure_contract_template(#domain_ContractTemplateRef{} = TemplateRef) ->
    try
        _GoodTemplate = get_template(TemplateRef, hg_domain:head()),
        TemplateRef
    catch
        error:{object_not_found, _} ->
            raise_invalid_request(<<"contract template not found">>)
    end;

ensure_contract_template(undefined) ->
    get_default_template_ref(hg_domain:head()).

get_contract_currencies(Contract, Timestamp, Revision) ->
    #domain_PaymentsServiceTerms{currencies = CurrencySelector} = get_payments_service_terms(Contract, Timestamp),
    Value = reduce_selector_to_value(CurrencySelector, #{}, Revision),
    case ordsets:size(Value) > 0 of
        true ->
            Value;
        false ->
            error({misconfiguration, {'Empty set in currency selector\'s value', CurrencySelector, Revision}})
    end.

get_contract_categories(Contract, Timestamp, Revision) ->
    #domain_PaymentsServiceTerms{categories = CategorySelector} = get_payments_service_terms(Contract, Timestamp),
    Value = reduce_selector_to_value(CategorySelector, #{}, Revision),
    case ordsets:size(Value) > 0 of
        true ->
            Value;
        false ->
            error({misconfiguration, {'Empty set in category selector\'s value', CategorySelector, Revision}})
    end.

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

instantiate_contract_template(TemplateRef) ->
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, hg_domain:head()),

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

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    case Contract = maps:get(ID, Contracts, undefined) of
        #domain_Contract{} ->
            Contract;
        undefined ->
            throw(#payproc_ContractNotFound{})
    end.

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

get_shop(ID, #domain_Party{shops = Shops}) ->
    ensure_shop(maps:get(ID, Shops, undefined)).

set_shop(Shop = #domain_Shop{id = ID}, Party = #domain_Party{shops = Shops}) ->
    Party#domain_Party{shops = Shops#{ID => Shop}}.

ensure_shop(Shop = #domain_Shop{}) ->
    Shop;
ensure_shop(undefined) ->
    throw(#payproc_ShopNotFound{}).

get_shop_account(ShopID, St = #st{}) ->
    Shop = get_shop(ShopID, get_party(St)),
    get_shop_account(Shop).

get_shop_account(#domain_Shop{account = undefined}) ->
    throw(#payproc_ShopAccountNotFound{});
get_shop_account(#domain_Shop{account = Account}) ->
    Account.

get_account_state(AccountID, St = #st{}) ->
    ok = ensure_account(AccountID, get_party(St)),
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

apply_change({blocking, Blocking}, Party) ->
    Party#domain_Party{blocking = Blocking};
apply_change({suspension, Suspension}, Party) ->
    Party#domain_Party{suspension = Suspension};
apply_change(?contract_creation(Contract), Party) ->
    set_contract(Contract, Party);
apply_change(?contract_termination(ID, TerminatedAt, _Reason), Party) ->
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
apply_change(?contract_adjustment_creation(ID, Adjustment), Party) ->
    Contract = get_contract(ID, Party),
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    set_contract(Contract#domain_Contract{adjustments = Adjustments}, Party);
apply_change({shop_creation, Shop}, Party) ->
    set_shop(Shop, Party);
apply_change(?shop_modification(ID, V), Party) ->
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

get_next_id(IDs) ->
    lists:max([0 | IDs]) + 1.

raise_invalid_request(Error) ->
    throw(#'InvalidRequest'{errors = [Error]}).

%% TODO there should be more concise way to express this assertions in terms of preconditions

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

assert_shop_modification_allowed(ID, {St, Events}) ->
    % We allow updates to pending shop
    PendingSt = get_pending_st(St),
    _ = assert_shop_unblocked(ID, {PendingSt, Events}).

assert_shop_unblocked(ID, {St, _}) ->
    Shop = get_shop(ID, get_party(St)),
    assert_shop_blocking(Shop, unblocked).

assert_shop_blocked(ID, {St, _}) ->
    Shop = get_shop(ID, get_party(St)),
    assert_shop_blocking(Shop, blocked).

assert_shop_active(ID, {St, _}) ->
    Shop = get_shop(ID, get_party(St)),
    assert_shop_suspension(Shop, active).

assert_shop_suspended(ID, {St, _}) ->
    Shop = get_shop(ID, get_party(St)),
    assert_shop_suspension(Shop, suspended).

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

assert_shop_update_valid(
    ShopID,
    #payproc_ShopUpdate{
        category = NewCategoryRef,
        contract_id = NewContractID,
        payout_tool_id = NewPayoutToolID
    } = Update,
    Party,
    Timestamp,
    Revision
) ->
    Shop = apply_shop_change({update, Update}, get_shop(ShopID, Party)),
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Revision),
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
            Currencies = reduce_selector_to_values(CurrencySelector, #{}, Revision),
            _ = ordsets:is_element(CurrencyRef, Currencies) orelse
                raise_invalid_request(<<"contract currency missmatch">>);
        undefined ->
            ok
    end,
    Categories = reduce_selector_to_values(CategorySelector, #{}, Revision),
    _ = ordsets:is_element(CategoryRef, Categories) orelse
        raise_invalid_request(<<"contract category missmatch">>),
    ok.

assert_shop_payout_tool_valid(#domain_Shop{payout_tool_id = PayoutToolID}, Contract) ->
    _PayoutTool = get_payout_tool(PayoutToolID, Contract),
    ok.

