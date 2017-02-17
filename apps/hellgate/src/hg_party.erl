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

-export([get_payments_service_terms/2]).
-export([get_payments_service_terms/3]).

%%

-spec get(user_info(), party_id()) ->
    dmsl_domain_thrift:'Party'() | no_return().

get(UserInfo, PartyID) ->
    ok = assert_party_accessible(UserInfo, PartyID),
    get_party(get_state(PartyID)).

-spec checkout(party_id(), timestamp()) ->
    dmsl_domain_thrift:'Party'().

%% DANGER! INSECURE METHOD! USE WITH SPECIAL CARE!
checkout(PartyID, Timestamp) ->
    {History, _LastID} = get_history(PartyID),
    case checkout_history(History, Timestamp) of
        {ok, St} ->
            get_party(St);
        {error, Reason} ->
            error(Reason)
    end.

%%

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(partymgmt,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function_('Create', [UserInfo, PartyID, PartyParams], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    start(PartyID, PartyParams);

handle_function_('Get', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    get_party(get_state(PartyID));

handle_function_('CreateContract', [UserInfo, PartyID, ContractParams], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_contract, ContractParams});

handle_function_('GetContract', [UserInfo, PartyID, ContractID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_contract(ContractID, get_party(St));

handle_function_('BindContractLegalAgreemnet', [UserInfo, PartyID, ContractID, LegalAgreement], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {bind_contract_legal_agreemnet, ContractID, LegalAgreement});

handle_function_('TerminateContract', [UserInfo, PartyID, ContractID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {terminate_contract, ContractID, Reason});

handle_function_('CreateContractAdjustment', [UserInfo, PartyID, ContractID, Params], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_contract_adjustment, ContractID, Params});

handle_function_('CreatePayoutTool', [UserInfo, PartyID, ContractID, Params], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_payout_tool, ContractID, Params});

handle_function_('GetEvents', [UserInfo, PartyID, Range], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    get_public_history(PartyID, AfterID, Limit);

handle_function_('Block', [UserInfo, PartyID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {block, Reason});

handle_function_('Unblock', [UserInfo, PartyID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {unblock, Reason});

handle_function_('Suspend', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, suspend);

handle_function_('Activate', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, activate);

handle_function_('CreateShop', [UserInfo, PartyID, Params], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {create_shop, Params});

handle_function_('GetShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_shop(ID, get_party(St));

handle_function_('UpdateShop', [UserInfo, PartyID, ID, Update], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {update_shop, ID, Update});

handle_function_('BlockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {block_shop, ID, Reason});

handle_function_('UnblockShop', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {unblock_shop, ID, Reason});

handle_function_('SuspendShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {suspend_shop, ID});

handle_function_('ActivateShop', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {activate_shop, ID});

handle_function_('GetClaim', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_claim(ID, St);

handle_function_('GetPendingClaim', [UserInfo, PartyID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    ensure_claim(get_pending_claim(St));

handle_function_('AcceptClaim', [UserInfo, PartyID, ID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {accept_claim, ID});

handle_function_('DenyClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {deny_claim, ID, Reason});

handle_function_('RevokeClaim', [UserInfo, PartyID, ID, Reason], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    call(PartyID, {revoke_claim, ID, Reason});

handle_function_('GetAccountState', [UserInfo, PartyID, AccountID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
    ok = assert_party_accessible(UserInfo, PartyID),
    St = get_state(PartyID),
    get_account_state(AccountID, St);

handle_function_('GetShopAccount', [UserInfo, PartyID, ShopID], _Opts) ->
    _ = set_party_mgmt_meta(PartyID, UserInfo),
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

set_party_mgmt_meta(PartyID, #payproc_UserInfo{id = ID, type = {Type, _}}) ->
    hg_log_scope:set_meta(#{
        party_id => PartyID,
        user_info => #{id => ID, type => Type}
    }).

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
    timestamp      :: timestamp(),
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
    hg_log_scope:scope(
        party,
        fun() -> process_init(ID, PartyParams) end,
        #{
            id => ID,
            activity => init
        }
    ).

process_init(ID, PartyParams) ->
    Timestamp = hg_datetime:format_now(),
    {ok, StEvents} = create_party(ID, PartyParams, {#st{timestamp = Timestamp}, []}),
    Revision = hg_domain:head(),
    TestContractTemplate = get_test_template_ref(Revision),
    Changeset1 = create_contract(#payproc_ContractParams{template = TestContractTemplate}, StEvents),
    [?contract_creation(TestContract)] = Changeset1,

    ShopParams = get_shop_prototype_params(TestContract#domain_Contract.id, Revision),
    Changeset2 = create_shop(ShopParams, ?active(), StEvents),
    [?shop_creation(Shop)] = Changeset2,
    ok = assert_shop_contract_valid(Shop, TestContract, Timestamp, Revision),

    Currencies = get_contract_currencies(TestContract, Timestamp, Revision),
    CurrencyRef = erlang:hd(ordsets:to_list(Currencies)),
    Changeset3 = [?shop_modification(Shop#domain_Shop.id, ?account_created(create_shop_account(CurrencyRef)))],

    Changeset = Changeset1 ++ Changeset2 ++ Changeset3,
    {_ClaimID, StEvents1} = submit_accept_claim(Changeset, StEvents),
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
        hg_log_scope:scope(
            party,
            fun() -> handle_call(Call, {St, []}) end,
            #{
                id => get_party_id(get_party(St)),
                activity => get_call_name(Call)
            }
        )
    catch
        throw:Exception ->
            respond_w_exception(Exception)
    end.

get_call_name(Call) when is_tuple(Call) ->
    element(1, Call);
get_call_name(Call) when is_atom(Call) ->
    Call.

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

handle_call({create_contract, ContractParams0}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ContractParams = ensure_contract_creation_params(ContractParams0),
    Changeset = create_contract(ContractParams, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({bind_contract_legal_agreemnet, ID, #domain_LegalAgreement{} = LegalAgreement}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Contract = get_contract(ID, get_party(get_pending_st(St))),
    ok = assert_contract_active(Contract),
    {ClaimID, StEvents1} = create_claim([?contract_legal_agreement_binding(ID, LegalAgreement)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({terminate_contract, ID, Reason}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Contract = get_contract(ID, get_party(St)),
    ok = assert_contract_active(Contract),
    TerminatedAt = hg_datetime:format_now(),
    {ClaimID, StEvents1} = create_claim([?contract_termination(ID, TerminatedAt, Reason)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract_adjustment, ID, Params0}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_contract_active(get_contract(ID, get_party(St))),
    Params = ensure_adjustment_creation_params(Params0),
    Changeset = create_contract_adjustment(ID, Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_payout_tool, ContractID, Params}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Contract = get_contract(ContractID, get_party(get_pending_st(St))),
    ok = assert_contract_active(Contract),
    _ = not is_test_contract(Contract, get_timestamp(St), hg_domain:head()) orelse
        raise_invalid_request(<<"creating payout tool for test contract is forbidden">>),
    Changeset = create_payout_tool(Params, ContractID, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_shop, Params}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Timestamp = get_timestamp(St),
    Revision = hg_domain:head(),
    Changeset0 = [?shop_creation(Shop)] = create_shop(Params, StEvents0),

    Party = get_party(get_pending_st(St)),
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
    ok = assert_shop_payout_tool_valid(Shop, Contract),

    Currencies = get_contract_currencies(Contract, Timestamp, Revision),
    CurrencyRef = erlang:hd(ordsets:to_list(Currencies)),
    Changeset1 = [?shop_modification(Shop#domain_Shop.id, ?account_created(create_shop_account(CurrencyRef)))],

    {ClaimID, StEvents1} = create_claim(Changeset0 ++ Changeset1, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({update_shop, ID, Update}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_modification_allowed(ID, StEvents0),
    ok = assert_shop_update_valid(ID, Update, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {update, Update})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({block_shop, ID, Reason}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_unblocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?blocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({unblock_shop, ID, Reason}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_blocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?unblocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({suspend_shop, ID}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_active(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?suspended()})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({activate_shop, ID}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
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

get_timestamp(#st{timestamp = Timestamp}) ->
    Timestamp.

get_party(#st{party = Party}) ->
    Party.

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
        payout_tool_params = PayoutToolParams
    },
    {St, _}
) ->
    ContractID = get_next_contract_id(get_pending_st(St)),
    PayoutToolChangeset = case PayoutToolParams of
        #payproc_PayoutToolParams{currency = Currency, tool_info = ToolInfo} ->
            [?contract_payout_tool_creation(
                ContractID,
                #domain_PayoutTool{id = 1, currency = Currency, payout_tool_info = ToolInfo}
            )];
        undefined ->
            []
    end,
    Contract0 = instantiate_contract_template(TemplateRef),
    Contract = Contract0#domain_Contract{
        id = ContractID,
        contractor = Contractor,
        status = {active, #domain_ContractActive{}},
        adjustments = [],
        payout_tools = []
    },
    [?contract_creation(Contract)] ++ PayoutToolChangeset.

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

create_shop(ShopParams, StEvents) ->
    create_shop(ShopParams, ?suspended(), StEvents).

create_shop(ShopParams, Suspension, {St, _}) ->
    ShopID = get_next_shop_id(get_pending_st(St)),
    [?shop_creation(construct_shop(ShopID, ShopParams, Suspension))].

construct_shop(ShopID, ShopParams, Suspension) ->
    #domain_Shop{
        id         = ShopID,
        blocking   = ?unblocked(<<>>),
        suspension = Suspension,
        details    = ShopParams#payproc_ShopParams.details,
        category   = ShopParams#payproc_ShopParams.category,
        contract_id = ShopParams#payproc_ShopParams.contract_id,
        payout_tool_id = ShopParams#payproc_ShopParams.payout_tool_id,
        proxy      = ShopParams#payproc_ShopParams.proxy
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
            throw(#payproc_InvalidClaimStatus{status = Status})
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

assert_shop_update_valid(ShopID, Update, {St, _}) ->
    Party = get_party(get_pending_st(St)),
    Shop = apply_shop_change({update, Update}, get_shop(ShopID, Party)),
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    Timestamp = get_timestamp(St),
    Revision = hg_domain:head(),
    ok = assert_shop_contract_valid(Shop, Contract, Timestamp, Revision),
    case is_test_contract(Contract, Timestamp, Revision) of
        true when Shop#domain_Shop.payout_tool_id == undefined ->
            ok;
        true ->
            error({misconfiguration, {'Test and live categories in same TermSet', Contract#domain_Contract.terms}});
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

-spec raise_invalid_request(binary()) -> no_return().

raise_invalid_request(Error) ->
    throw(#'InvalidRequest'{errors = [Error]}).

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
    {ok, St} = checkout_history(History, hg_datetime:format_now()),
    St.

-spec checkout_history([ev()], timestamp()) -> {ok, st()} | {error, revision_not_found}.

checkout_history(History, Timestamp) ->
    checkout_history(History, undefined, #st{timestamp = Timestamp}).

checkout_history([{_ID, EventTimestamp, Ev} | Rest], PrevTimestamp, #st{timestamp = Timestamp} = St) ->
    case hg_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history(Rest, EventTimestamp, merge_history(Ev, St))
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

apply_accepted_claim(Claim = #payproc_Claim{status = ?accepted(_AcceptedAt)}, St) ->
    apply_claim(Claim, St);
apply_accepted_claim(_Claim, St) ->
    St.

apply_claim(#payproc_Claim{changeset = Changeset}, St) ->
    apply_changeset(Changeset, St).

apply_changeset(Changeset, St = #st{timestamp = Timestamp}) ->
    St#st{party = lists:foldl(
        fun(Change, Party) ->
            apply_party_change(Change, Party, Timestamp)
        end,
        get_party(St),
        Changeset
    )}.

apply_party_change({blocking, Blocking}, Party, _) ->
    Party#domain_Party{blocking = Blocking};
apply_party_change({suspension, Suspension}, Party, _) ->
    Party#domain_Party{suspension = Suspension};
apply_party_change(?contract_creation(Contract), Party, Timestamp) ->
    set_contract(update_contract_status(Contract, Timestamp), Party);
apply_party_change(?contract_termination(ID, TerminatedAt, _Reason), Party, _) ->
    Contract = get_contract(ID, Party),
    set_contract(
        Contract#domain_Contract{
            status = {terminated, #domain_ContractTerminated{terminated_at = TerminatedAt}}
        },
        Party
    );
apply_party_change(
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
apply_party_change(
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
apply_party_change(?contract_adjustment_creation(ID, Adjustment), Party, _) ->
    Contract = get_contract(ID, Party),
    Adjustments = Contract#domain_Contract.adjustments ++ [Adjustment],
    set_contract(Contract#domain_Contract{adjustments = Adjustments}, Party);
apply_party_change({shop_creation, Shop}, Party, _) ->
    set_shop(Shop, Party);
apply_party_change(?shop_modification(ID, V), Party, _) ->
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

create_shop_account(#domain_CurrencyRef{symbolic_code = SymbolicCode} = CurrencyRef) ->
    GuaranteeID = hg_accounting:create_account(SymbolicCode),
    SettlementID = hg_accounting:create_account(SymbolicCode),
    PayoutID = hg_accounting:create_account(SymbolicCode),
    #domain_ShopAccount{
        currency = CurrencyRef,
        settlement = SettlementID,
        guarantee = GuaranteeID,
        payout = PayoutID
    }.

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

