%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/90a4eed/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/90a4eed/docs/domain/entities/merchant.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)
%% * Create accounts after shop claim confirmation

-module(hg_party).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-define(NS, <<"party">>).

%% Public

-export([get/2]).
-export([checkout/2]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%% Party support functions

-export([get_payments_service_terms/3]).

%%

-spec get(user_info(), party_id()) ->
    dmsl_domain_thrift:'Party'() | no_return().

get(UserInfo, PartyID) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    get_party(get_state(PartyID)).

-spec checkout(party_id(), revision()) ->
    dmsl_domain_thrift:'Party'().

%% DANGER! INSECURE METHOD! USE WITH SPECIAL CARE!
checkout(PartyID, Revision) ->
    {History, _LastID} = get_history(PartyID),
    case checkout_history(History, Revision) of
        {ok, St} ->
            get_party(St);
        {error, Reason} ->
            error(Reason)
    end.

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function('Create', [UserInfo, PartyID, PartyParams], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    start(PartyID, PartyParams);

handle_function('Get', [UserInfo, PartyID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    get_party(get_state(PartyID));

handle_function('CreateContract', [UserInfo, PartyID, ContractParams], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_contract, ContractParams});

handle_function('GetContract', [UserInfo, PartyID, ContractID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_contract(ContractID, get_party(St));

handle_function('BindContractLegalAgreemnet', [UserInfo, PartyID, ContractID, LegalAgreement], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {bind_contract_legal_agreemnet, ContractID, LegalAgreement});

handle_function('TerminateContract', [UserInfo, PartyID, ContractID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {terminate_contract, ContractID, Reason});

handle_function('CreateContractAdjustment', [UserInfo, PartyID, ContractID, Params], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_contract_adjustment, ContractID, Params});

handle_function('CreatePayoutTool', [UserInfo, PartyID, ContractID, Params], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_payout_tool, ContractID, Params});

handle_function('GetEvents', [UserInfo, PartyID, Range], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    get_public_history(PartyID, AfterID, Limit);

handle_function('Block', [UserInfo, PartyID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {block, Reason});

handle_function('Unblock', [UserInfo, PartyID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {unblock, Reason});

handle_function('Suspend', [UserInfo, PartyID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, suspend);

handle_function('Activate', [UserInfo, PartyID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, activate);

handle_function('CreateShop', [UserInfo, PartyID, Params], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_shop, Params});

handle_function('GetShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_shop(ID, get_party(St));

handle_function('UpdateShop', [UserInfo, PartyID, ID, Update], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {update_shop, ID, Update});

handle_function('BlockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {block_shop, ID, Reason});

handle_function('UnblockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {unblock_shop, ID, Reason});

handle_function('SuspendShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {suspend_shop, ID});

handle_function('ActivateShop', [UserInfo, PartyID, ID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {activate_shop, ID});

handle_function('GetClaim', [UserInfo, PartyID, ID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_claim(ID, St);

handle_function('GetPendingClaim', [UserInfo, PartyID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    ensure_claim(get_pending_claim(St));

handle_function('AcceptClaim', [UserInfo, PartyID, ID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {accept_claim, ID});

handle_function('DenyClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {deny_claim, ID, Reason});

handle_function('RevokeClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {revoke_claim, ID, Reason});

handle_function('GetAccountState', [UserInfo, PartyID, AccountID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_account_state(AccountID, St);

handle_function('GetShopAccount', [UserInfo, PartyID, ShopID], _Opts) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_shop_account(ShopID, St).

get_history(PartyID) ->
    map_history_error(hg_machine:get_history(?NS, PartyID)).

get_history(PartyID, AfterID, Limit) ->
    map_history_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit)).

get_state(PartyID) ->
    {History, _LastID} = get_history(PartyID),
    collapse_history(assert_nonempty_history(History)).

%% TODO remove this hack as soon as machinegun learns to tell the difference between
%%      nonexsitent machine and empty history
assert_nonempty_history([_ | _] = Result) ->
    Result;
assert_nonempty_history([]) ->
    throw(#payproc_PartyNotFound{}).

assert_party_accessible(UserInfo, PartyID) ->
    case hg_access_control:check_user_info(UserInfo, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

get_public_history(PartyID, AfterID, Limit) ->
    hg_history:get_public_history(
        fun (ID, Lim) -> get_history(PartyID, ID, Lim) end,
        fun (Event) -> publish_party_event({party, PartyID}, Event) end,
        AfterID, Limit
    ).

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, exists}) ->
    throw(#payproc_PartyExists{}).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{});
map_error({error, Reason}) ->
    error(Reason).

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
-type revision()              :: dmsl_base_thrift:'Timestamp'().
-type sequence()              :: pos_integer().
-type legal_agreement()       :: dmsl_domain_thrift:'LegalAgreement'().

-type ev() ::
    {sequence(), public_event() | private_event()}.

-type public_event() :: dmsl_payment_processing_thrift:'EventPayload'().
-type private_event() :: none().

-include("party_events.hrl").

publish_party_event(Source, {ID, Dt, {Seq, Payload = ?party_ev(_)}}) ->
    {true, #payproc_Event{id = ID, source = Source, created_at = Dt, sequence = Seq, payload = Payload}};
publish_party_event(_Source, {_ID, _Dt, _Event}) ->
    false.

-spec publish_event(party_id(), hg_machine:event(ev())) ->
    {true, hg_event_provider:public_event()} | false.

publish_event(PartyID, {Seq, Ev = ?party_ev(_)}) ->
    {true, {{party, PartyID}, Seq, Ev}};
publish_event(_InvoiceID, _) ->
    false.

%%

-record(st, {
    party          :: party(),
    revision       :: revision(),
    claims   = #{} :: #{claim_id() => claim()},
    sequence = 0   :: 0 | sequence()
}).

-type st() :: #st{}.

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(party_id(), dmsl_payment_processing_thrift:'PartyParams'()) ->
    hg_machine:result(ev()).

init(ID, PartyParams) ->
    {ok, StEvents} = create_party(ID, PartyParams, {#st{}, []}),
    Revision = hg_domain:head(),
    TestContractTemplpate = get_test_template(Revision),
    Changeset1 = create_contract(#payproc_ContractParams{template = TestContractTemplpate}, StEvents),
    [?contract_creation(TestContract)] = Changeset1,
    ShopParams = get_shop_prototype_params(Revision),
    Changeset2 = create_shop(
        ShopParams#payproc_ShopParams{
            contract_id = TestContract#domain_Contract.id
        },
        ?active(),
        Revision,
        StEvents
    ),
    {_ClaimID, StEvents1} = submit_accept_claim(Changeset1 ++ Changeset2, StEvents),
    ok(StEvents1).

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

process_signal(timeout, _History) ->
    ok();

process_signal({repair, _}, _History) ->
    ok().

-type call() ::
    {block, binary()}                                                |
    {unblock, binary()}                                              |
    suspend                                                          |
    activate                                                         |
    {create_contract, contract_params()}                             |
    {bind_contract_legal_agreemnet, contract_id(), legal_agreement()}|
    {terminate_contract, contract_id(), binary()}                    |
    {create_contract_adjustment, contract_id(), adjustment_params()} |
    {create_payout_tool, contract_id(), payout_tool_params()}        |
    {create_shop, shop_params()}                                     |
    {update_shop, shop_id(), shop_update()}                          |
    {block_shop, shop_id(), binary()}                                |
    {unblock_shop, shop_id(), binary()}                              |
    {suspend_shop, shop_id()}                                        |
    {activate_shop, shop_id()}                                       |
    {accept_claim, shop_id()}                                        |
    {deny_claim, shop_id(), binary()}                                |
    {revoke_claim, shop_id(), binary()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    St = collapse_history(History),
    try
        handle_call(Call, {St, []})
    catch
        {exception, Exception} ->
            respond_w_exception(Exception)
    end.

-spec raise(term()) -> no_return().

raise(What) ->
    throw({exception, What}).

handle_call({block, Reason}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?blocked(Reason)}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({unblock, Reason}, StEvents0) ->
    ok = assert_blocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?unblocked(Reason)}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call(suspend, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_active(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?suspended()}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call(activate, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_suspended(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?active()}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract, ContractParams}, StEvents0) ->
    ok = assert_operable(StEvents0),
    Changeset = create_contract(ContractParams, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({bind_contract_legal_agreemnet, ID, #domain_LegalAgreement{} = LegalAgreement}, StEvents0 = {St, _}) ->
    ok = assert_operable(StEvents0),
    Contract = get_contract(ID, get_party(get_pending_st(St))),
    ok = assert_contract_active(Contract),
    {ClaimID, StEvents1} = create_claim([?contract_legal_agreement_binding(ID, LegalAgreement)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({terminate_contract, ID, Reason}, StEvents0 = {St, _}) ->
    ok = assert_operable(StEvents0),
    Contract = get_contract(ID, get_party(St)),
    ok = assert_contract_active(Contract),
    TerminatedAt = hg_datetime:format_now(),
    {ClaimID, StEvents1} = create_claim([?contract_termination(ID, TerminatedAt, Reason)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract_adjustment, ID, Params}, StEvents0 = {St, _}) ->
    ok = assert_operable(StEvents0),
    Contract = get_contract(ID, get_party(St)),
    ok = assert_contract_active(Contract),
    Changeset = create_contract_adjustment(ID, Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_payout_tool, ContractID, Params}, StEvents0 = {St, _}) ->
    ok = assert_operable(StEvents0),
    ok = assert_contract_active(get_contract(ContractID, get_party(get_pending_st(St)))),
    Changeset = create_payout_tool(Params, ContractID, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_shop, Params}, StEvents0) ->
    ok = assert_operable(StEvents0),
    Revision = hg_domain:head(),
    Changeset = create_shop(Params, Revision, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({update_shop, ID, Update}, StEvents0) ->
    ok = assert_operable(StEvents0),
    ok = assert_shop_modification_allowed(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {update, Update})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({block_shop, ID, Reason}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?blocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({unblock_shop, ID, Reason}, StEvents0) ->
    ok = assert_shop_blocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?unblocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({suspend_shop, ID}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_active(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?suspended()})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({activate_shop, ID}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_suspended(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?active()})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({accept_claim, ID}, StEvents0) ->
    {ID, StEvents1} = accept_claim(ID, StEvents0),
    respond(ok, StEvents1);

handle_call({deny_claim, ID, Reason}, StEvents0) ->
    {ID, StEvents1} = finalize_claim(ID, ?denied(Reason), StEvents0),
    respond(ok, StEvents1);

handle_call({revoke_claim, ID, Reason}, StEvents0) ->
    ok = assert_operable(StEvents0),
    {ID, StEvents1} = finalize_claim(ID, ?revoked(Reason), StEvents0),
    respond(ok, StEvents1).

%%

create_party(PartyID, PartyParams, StEvents) ->
    Party = #domain_Party{
        id              = PartyID,
        contact_info    = PartyParams#payproc_PartyParams.contact_info,
        blocking        = ?unblocked(<<>>),
        suspension      = ?active(),
        contracts       = #{},
        shops           = #{}
    },
    Event = ?party_ev(?party_created(Party)),
    {ok, apply_state_event(Event, StEvents)}.

get_party(#st{party = Party}) ->
    Party.

%%

create_contract(
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef,
        payout_tool_params = PayoutToolParams
    },
    {St, _}
) ->
    ContractID = get_next_contract_id(get_pending_st(St)),
    PayoutTools = case PayoutToolParams of
        #payproc_PayoutToolParams{currency = Currency, tool_info = ToolInfo} ->
            [#domain_PayoutTool{id = 1, currency = Currency, payout_tool_info = ToolInfo}];
        undefined ->
            []
    end,
    Contract0 = instantiate_contract_template(TemplateRef),
    Contract = Contract0#domain_Contract{
        id = ContractID,
        contractor = Contractor,
        status = {active, #domain_ContractActive{}},
        adjustments = [],
        payout_tools = PayoutTools
    },
    [?contract_creation(Contract)].

get_next_contract_id(#st{party = #domain_Party{contracts = Contracts}}) ->
    get_next_id(maps:keys(Contracts)).

%%

create_contract_adjustment(ID, #payproc_ContractAdjustmentParams{template = TemplateRef}, {St, _}) ->
    Contract = get_contract(ID, get_party(get_pending_st(St))),
    AdjustmentID = get_next_contract_adjustment_id(Contract#domain_Contract.adjustments),
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = instantiate_contract_template(TemplateRef),
    Adjustment = #domain_ContractAdjustment{
        id = AdjustmentID,
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    },
    [?contract_adjustment_creation(ID, Adjustment)].

get_next_contract_adjustment_id(Adjustments) ->
    get_next_id([ID || #domain_ContractAdjustment{id = ID} <- Adjustments]).

%%

create_shop(ShopParams, Revision, StEvents) ->
    create_shop(ShopParams, ?suspended(), Revision, StEvents).

create_shop(ShopParams, Suspension, Revision, {St, _}) ->
    ShopID = get_next_shop_id(get_pending_st(St)),
    Shop = construct_shop(ShopID, ShopParams, Suspension),
    ShopAccount = create_shop_account(Revision),
    [
        ?shop_creation(Shop),
        ?shop_modification(ShopID, ?account_created(ShopAccount))
    ].

construct_shop(ShopID, ShopParams, Suspension) ->
    #domain_Shop{
        id         = ShopID,
        blocking   = ?unblocked(<<>>),
        suspension = Suspension,
        details    = ShopParams#payproc_ShopParams.details,
        category   = ShopParams#payproc_ShopParams.category,
        contract_id = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id = ShopParams#payproc_ShopParams.payout_tool_id
    }.

get_next_shop_id(#st{party = #domain_Party{shops = Shops}}) ->
    % TODO cache sequences on history collapse
    get_next_id(maps:keys(Shops)).

%%

create_payout_tool(#payproc_PayoutToolParams{currency = Currency, tool_info = PayoutToolInfo}, ContractID, {St, _}) ->
    ToolID = get_next_payout_tool_id(ContractID, get_pending_st(St)),
    PayoutTool = #domain_PayoutTool{
        id = ToolID,
        currency = Currency,
        payout_tool_info = PayoutToolInfo
    },
    [?contract_payout_tool_creation(ContractID, PayoutTool)].

get_next_payout_tool_id(ContractID, St) ->
    #domain_Contract{payout_tools = Tools} = get_contract(ContractID, get_party(St)),
    get_next_id([ID || #domain_PayoutTool{id = ID} <- Tools]).
%%

-spec get_payments_service_terms(shop_id(), party(), binary() | integer()) ->
    dmsl_domain_thrift:'PaymentsServiceTerms'().

get_payments_service_terms(ShopID, Party, Timestamp) ->
    Shop = get_shop(ShopID, Party),
    Contract = maps:get(Shop#domain_Shop.contract_id, Party#domain_Party.contracts),
    ok = assert_contract_active(Contract),
    #domain_TermSet{payments = PaymentTerms} = compute_terms(Contract, Timestamp),
    PaymentTerms.

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    raise(#payproc_InvalidContractStatus{status = Status}).

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

get_active_term_set([TimedTermSet | OtherTermSets], Timestamp) ->
    #domain_TimedTermSet{action_time = ActionTime, terms = Terms} = TimedTermSet,
    case hg_datetime:between(Timestamp, ActionTime) of
        true ->
            Terms;
        false ->
            get_active_term_set(OtherTermSets, Timestamp)
    end;

get_active_term_set([], _) ->
    #domain_TermSet{}.

merge_term_sets(TermSets) ->
    merge_term_sets(TermSets, #domain_TermSet{}).

merge_term_sets(
    [#domain_TermSet{payments = PaymentTerms1} | OtherTermSets],
    #domain_TermSet{payments = PaymentTerms0}
) ->
    NewTermSet = #domain_TermSet{
        payments = merge_payments_terms(PaymentTerms0, PaymentTerms1)
    },
    merge_term_sets(OtherTermSets, NewTermSet);

merge_term_sets([], TermSet) ->
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

%%

create_claim(Changeset, StEvents = {St, _}) ->
    ClaimPending = get_pending_claim(St),
    % Test if we can safely accept proposed changes.
    case does_changeset_need_acceptance(Changeset) of
        false when ClaimPending == undefined ->
            % We can and there is no pending claim, accept them right away.
            submit_accept_claim(Changeset, StEvents);
        false ->
            % We can but there is pending claim...
            try_submit_accept_claim(Changeset, ClaimPending, StEvents);
        true when ClaimPending == undefined ->
            % We can't and there is no pending claim, submit new pending claim with proposed changes.
            submit_claim(Changeset, StEvents);
        true ->
            % We can't and there is in fact pending claim, revoke it and submit new claim with
            % a combination of proposed changes and pending changes.
            resubmit_claim(Changeset, ClaimPending, StEvents)
    end.

try_submit_accept_claim(Changeset, ClaimPending, StEvents = {St, _}) ->
    % ...Test whether there's a conflict between pending changes and proposed changes.
    case has_changeset_conflict(Changeset, get_claim_changeset(ClaimPending), St) of
        false ->
            % If there's none then we can accept proposed changes safely.
            submit_accept_claim(Changeset, StEvents);
        true ->
            % If there is then we should revoke the pending claim and submit new claim with a
            % combination of proposed changes and pending changes.
            resubmit_claim(Changeset, ClaimPending, StEvents)
    end.

submit_claim(Changeset, StEvents = {St, _}) ->
    Claim = construct_claim(Changeset, St),
    submit_claim_event(Claim, StEvents).

submit_accept_claim(Changeset, StEvents = {St, _}) ->
    Claim = construct_claim(Changeset, St, ?accepted(hg_datetime:format_now())),
    submit_claim_event(Claim, StEvents).

submit_claim_event(Claim, StEvents) ->
    Event = ?party_ev(?claim_created(Claim)),
    {get_claim_id(Claim), apply_state_event(Event, StEvents)}.

resubmit_claim(Changeset, ClaimPending, StEvents0) ->
    ChangesetMerged = merge_changesets(Changeset, get_claim_changeset(ClaimPending)),
    {ID, StEvents1} = submit_claim(ChangesetMerged, StEvents0),
    Reason = <<"Superseded by ", (integer_to_binary(ID))/binary>>,
    {_ , StEvents2} = finalize_claim(get_claim_id(ClaimPending), ?revoked(Reason), StEvents1),
    {ID, StEvents2}.

does_changeset_need_acceptance(Changeset) ->
    lists:any(fun is_change_need_acceptance/1, Changeset).

%% TODO refine acceptance criteria
is_change_need_acceptance({blocking, _}) ->
    false;
is_change_need_acceptance({suspension, _}) ->
    false;
is_change_need_acceptance(?shop_modification(_, Modification)) ->
    is_shop_modification_need_acceptance(Modification);
is_change_need_acceptance(_) ->
    true.

is_shop_modification_need_acceptance({blocking, _}) ->
    false;
is_shop_modification_need_acceptance({suspension, _}) ->
    false;
is_shop_modification_need_acceptance({account_created, _}) ->
    false;
is_shop_modification_need_acceptance({update, ShopUpdate}) ->
    is_shop_update_need_acceptance(ShopUpdate);
is_shop_modification_need_acceptance(_) ->
    true.

is_shop_update_need_acceptance(ShopUpdate = #payproc_ShopUpdate{}) ->
    RecordInfo = record_info(fields, payproc_ShopUpdate),
    ShopUpdateUnits = hg_proto_utils:record_to_proplist(ShopUpdate, RecordInfo),
    lists:any(fun (E) -> is_shop_update_unit_need_acceptance(E) end, ShopUpdateUnits).

is_shop_update_unit_need_acceptance({proxy, _}) ->
    false;
is_shop_update_unit_need_acceptance(_) ->
    true.

has_changeset_conflict(Changeset, ChangesetPending, St) ->
    % NOTE We can safely assume that conflict is essentially the fact that two changesets are
    %      overlapping. Provided that any change is free of side effects (like computing unique
    %      identifiers), we can test if there's any overlapping by just applying changesets to the
    %      current state in different order and comparing produced states. If they're the same then
    %      there is no overlapping in changesets.
    apply_changeset(merge_changesets(ChangesetPending, Changeset), St) /=
        apply_changeset(merge_changesets(Changeset, ChangesetPending), St).

merge_changesets(Changeset, ChangesetBase) ->
    % TODO Evaluating a possibility to drop server-side claim merges completely, since it's the
    %      source of unwelcomed complexity. In the meantime this naïve implementation would suffice.
    ChangesetBase ++ Changeset.

accept_claim(ID, StEvents) ->
    finalize_claim(ID, ?accepted(hg_datetime:format_now()), StEvents).

finalize_claim(ID, Status, StEvents) ->
    ok = assert_claim_pending(ID, StEvents),
    Event = ?party_ev(?claim_status_changed(ID, Status)),
    {ID, apply_state_event(Event, StEvents)}.

assert_claim_pending(ID, {St, _}) ->
    case get_claim(ID, St) of
        #payproc_Claim{status = ?pending()} ->
            ok;
        #payproc_Claim{status = Status} ->
            raise(#payproc_InvalidClaimStatus{status = Status})
    end.

construct_claim(Changeset, St) ->
    construct_claim(Changeset, St, ?pending()).

construct_claim(Changeset, St, Status) ->
    #payproc_Claim{
        id        = get_next_claim_id(St),
        status    = Status,
        changeset = Changeset
    }.

get_next_claim_id(#st{claims = Claims}) ->
    % TODO cache sequences on history collapse
    get_next_id(maps:keys(Claims)).

get_claim_result(ID, {St, _}) ->
    #payproc_Claim{id = ID, status = Status} = get_claim(ID, St),
    #payproc_ClaimResult{id = ID, status = Status}.

get_claim_id(#payproc_Claim{id = ID}) ->
    ID.

get_claim_changeset(#payproc_Claim{changeset = Changeset}) ->
    Changeset.

%%

%% TODO there should be more concise way to express this assertions in terms of preconditions
assert_operable(StEvents) ->
    _ = assert_unblocked(StEvents),
    _ = assert_active(StEvents).

assert_unblocked({St, _}) ->
    assert_blocking(get_party(St), unblocked).

assert_blocked({St, _}) ->
    assert_blocking(get_party(St), blocked).

assert_active({St, _}) ->
    assert_suspension(get_party(St), active).

assert_suspended({St, _}) ->
    assert_suspension(get_party(St), suspended).

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    raise(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_suspension(#domain_Party{suspension = {Status, _}}, Status) ->
    ok;
assert_suspension(#domain_Party{suspension = Suspension}, _) ->
    raise(#payproc_InvalidPartyStatus{status = {suspension, Suspension}}).

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
    raise(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    raise(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

%%

-spec apply_state_event(public_event() | private_event(), StEvents) -> StEvents when
        StEvents :: {st(), [ev()]}.

apply_state_event(EventData, {St0, EventsAcc}) ->
    Event = construct_event(EventData, St0),
    {merge_history(Event, St0), EventsAcc ++ [Event]}.

construct_event(EventData = ?party_ev(_), #st{sequence = Seq}) ->
    {Seq + 1, EventData}.

%%

ok() ->
    {[], hg_machine_action:new()}.
ok(StEvents) ->
    ok(StEvents, hg_machine_action:new()).
ok({_St, Events}, Action) ->
    {Events, Action}.

respond(Response, StEvents) ->
    respond(Response, StEvents, hg_machine_action:new()).
respond(Response, {_St, Events}, Action) ->
    {{ok, Response}, {Events, Action}}.

respond_w_exception(Exception) ->
    respond_w_exception(Exception, hg_machine_action:new()).
respond_w_exception(Exception, Action) ->
    {{exception, Exception}, {[], Action}}.

%%

-spec collapse_history([ev()]) -> st().

collapse_history(History) ->
    lists:foldl(fun ({_ID, _, Ev}, St) -> merge_history(Ev, St) end, #st{}, History).

-spec checkout_history([ev()], revision()) -> {ok, st()} | {error, revision_not_found}.

checkout_history(History, Revision) ->
    checkout_history(History, Revision, #st{}).

checkout_history([{_ID, EventTimestamp, Ev} | Rest], Revision, St0 = #st{revision = Rev0}) ->
    case hg_datetime:compare(EventTimestamp, Revision) of
        later when Rev0 =/= undefined ->
            {ok, St0};
        later when Rev0 == undefined ->
            {error, revision_not_found};
        _ ->
            St1 = merge_history(Ev, St0),
            checkout_history(Rest, Revision, St1)
    end;
checkout_history([], _, St) ->
    {ok, St}.

merge_history({Seq, Event}, St) ->
    merge_event(Event, St#st{sequence = Seq}).

merge_event(?party_ev(Ev), St) ->
    merge_party_event(Ev, St).

merge_party_event(?party_created(Party), St) ->
    St#st{party = Party};
merge_party_event(?claim_created(Claim), St) ->
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_event(?claim_status_changed(ID, Status), St) ->
    Claim = get_claim(ID, St),
    Claim1 = Claim#payproc_Claim{status = Status},
    St1 = set_claim(Claim1, St),
    apply_accepted_claim(Claim1, St1).

get_pending_st(St) ->
    case get_pending_claim(St) of
        undefined ->
            St;
        Claim ->
            apply_claim(Claim, St)
    end.

get_contract(ID, #domain_Party{contracts = Contracts}) ->
    case Contract = maps:get(ID, Contracts, undefined) of
        #domain_Contract{} ->
            Contract;
        undefined ->
            throw(#payproc_ContractNotFound{})
    end.

set_contract(Contract = #domain_Contract{id = ID}, Party = #domain_Party{contracts = Contracts}) ->
    Party#domain_Party{contracts = Contracts#{ID => Contract}}.

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

get_claim(ID, #st{claims = Claims}) ->
    ensure_claim(maps:get(ID, Claims, undefined)).

get_pending_claim(#st{claims = Claims}) ->
    % TODO cache it during history collapse
    maps:fold(
        fun
            (_ID, #payproc_Claim{status = ?pending()} = Claim, undefined) -> Claim;
            (_ID, #payproc_Claim{status = _Another}, Claim)               -> Claim
        end,
        undefined,
        Claims
    ).

set_claim(Claim = #payproc_Claim{id = ID}, St = #st{claims = Claims}) ->
    St#st{claims = Claims#{ID => Claim}}.

ensure_claim(Claim = #payproc_Claim{}) ->
    Claim;
ensure_claim(undefined) ->
    throw(#payproc_ClaimNotFound{}).

apply_accepted_claim(Claim = #payproc_Claim{status = ?accepted(AcceptedAt)}, St) ->
    apply_claim(Claim, St#st{revision = AcceptedAt});
apply_accepted_claim(_Claim, St) ->
    St.

apply_claim(#payproc_Claim{changeset = Changeset}, St) ->
    apply_changeset(Changeset, St).

apply_changeset(Changeset, St) ->
    St#st{party = lists:foldl(fun apply_party_change/2, get_party(St), Changeset)}.

apply_party_change({blocking, Blocking}, Party) ->
    Party#domain_Party{blocking = Blocking};
apply_party_change({suspension, Suspension}, Party) ->
    Party#domain_Party{suspension = Suspension};
apply_party_change(?contract_creation(Contract), Party) ->
    set_contract(Contract, Party);
apply_party_change(?contract_termination(ID, TerminatedAt, _Reason), Party) ->
    Contract = get_contract(ID, Party),
    set_contract(
        Contract#domain_Contract{
            status = {terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}
        },
        Party
    );
apply_party_change(
    ?contract_legal_agreement_binding(ContractID, LegalAgreement),
    Party = #domain_Party{contracts = Contracts}
) ->
    % FIXME throw exception if already bound!
    Contract = maps:get(ContractID, Contracts),
    Party#domain_Party{
        contracts = Contracts#{
            ContractID => Contract#domain_Contract{legal_agreement = LegalAgreement}
        }
    };
apply_party_change(
    ?contract_payout_tool_creation(ContractID, PayoutTool),
    Party = #domain_Party{contracts = Contracts}
) ->
    Contract = #domain_Contract{payout_tools = PayoutTools} = maps:get(ContractID, Contracts),
    Party#domain_Party{
        contracts = Contracts#{
            ContractID => Contract#domain_Contract{payout_tools = PayoutTools ++ [PayoutTool]}
        }
    };
apply_party_change(?contract_adjustment_creation(ID, Adjustment), Party) ->
    Contract = get_contract(ID, Party),
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    set_contract(Contract#domain_Contract{adjustments = Adjustments}, Party);
apply_party_change({shop_creation, Shop}, Party) ->
    set_shop(Shop, Party);
apply_party_change(?shop_modification(ID, V), Party) ->
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

create_shop_account(Revision) ->
    ShopPrototype = get_shop_prototype(Revision),
    CurrencyRef = ShopPrototype#domain_ShopPrototype.currency,
    #domain_CurrencyRef{
        symbolic_code = SymbolicCode
    } = CurrencyRef,
    GuaranteeID = hg_accounting:create_account(SymbolicCode),
    SettlementID = hg_accounting:create_account(SymbolicCode),
    PayoutID = hg_accounting:create_account(SymbolicCode),
    #domain_ShopAccount{
        currency = CurrencyRef,
        settlement = SettlementID,
        guarantee = GuaranteeID,
        payout = PayoutID
    }.

get_shop_prototype_params(Revision) ->
    ShopPrototype = get_shop_prototype(Revision),
    #payproc_ShopParams{
        category = ShopPrototype#domain_ShopPrototype.category,
        details = ShopPrototype#domain_ShopPrototype.details
    }.

get_party_prototype(Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    hg_domain:get(Revision, {party_prototype, Globals#domain_Globals.party_prototype}).

get_shop_prototype(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.shop.

get_test_template(Revision) ->
    PartyPrototype = get_party_prototype(Revision),
    PartyPrototype#domain_PartyPrototype.test_contract_template.

instantiate_contract_template(TemplateRef) ->
    Revision = hg_domain:head(),
    Template = case TemplateRef of
        #domain_ContractTemplateRef{} ->
            get_template(TemplateRef, Revision);
        undefined ->
            get_default_template(Revision)
    end,
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = Template,
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

get_template(TemplateRef, Revision) ->
    hg_domain:get(Revision, {contract_template, TemplateRef}).

get_default_template(Revision) ->
    #domain_Globals{default_contract_template = Ref} = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    get_template(Ref, Revision).


add_interval(Timestamp, Interval) ->
    #domain_LifetimeInterval{
        years = YY,
        months = MM,
        days = DD
    } = Interval,
    hg_datetime:add_interval(Timestamp, {YY, MM, DD}).

