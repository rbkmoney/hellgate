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

%%

-spec get(user_info(), party_id()) ->
    dmsl_domain_thrift:'Party'() | no_return().

get(UserInfo, PartyID) ->
    St = get_state(UserInfo, PartyID),
    get_party(St).

-spec checkout(party_id(), revision()) ->
    dmsl_domain_thrift:'Party'().

checkout(PartyID, Revision) ->
    {History, _LastID} = get_history(PartyID),
    case checkout_history(History, Revision) of
        {ok, St} ->
            get_party(St);
        {error, Reason} ->
            error(Reason)
    end.

%%

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function('Create', {UserInfo, PartyID}, _Opts) ->
    start(PartyID, {UserInfo});

handle_function('Get', {UserInfo, PartyID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_party(St);

handle_function('CreateContract', {UserInfo, PartyID, ContractParams}, _Opts) ->
    call(PartyID, {create_contract, ContractParams, UserInfo});

handle_function('GetContract', {UserInfo, PartyID, ContractID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_contract(ContractID, get_party(St));

handle_function('TerminateContract', {UserInfo, PartyID, ContractID, Reason}, _Opts) ->
    call(PartyID, {terminate_contract, ContractID, Reason, UserInfo});

handle_function('CreateContractAdjustment', {UserInfo, PartyID, ContractID, Params}, _Opts) ->
    call(PartyID, {create_contract_adjustment, ContractID, Params, UserInfo});

handle_function('CreatePayoutAccount', {UserInfo, PartyID, Params}, _Opts) ->
    call(PartyID, {create_payout_account, Params, UserInfo});

handle_function('GetEvents', {UserInfo, PartyID, Range}, _Opts) ->
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    get_public_history(UserInfo, PartyID, AfterID, Limit);

handle_function('Block', {UserInfo, PartyID, Reason}, _Opts) ->
    call(PartyID, {block, Reason, UserInfo});

handle_function('Unblock', {UserInfo, PartyID, Reason}, _Opts) ->
    call(PartyID, {unblock, Reason, UserInfo});

handle_function('Suspend', {UserInfo, PartyID}, _Opts) ->
    call(PartyID, {suspend, UserInfo});

handle_function('Activate', {UserInfo, PartyID}, _Opts) ->
    call(PartyID, {activate, UserInfo});

handle_function('CreateShop', {UserInfo, PartyID, Params}, _Opts) ->
    call(PartyID, {create_shop, Params, UserInfo});

handle_function('GetShop', {UserInfo, PartyID, ID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_shop(ID, get_party(St));

handle_function('UpdateShop', {UserInfo, PartyID, ID, Update}, _Opts) ->
    call(PartyID, {update_shop, ID, Update, UserInfo});

handle_function('BlockShop', {UserInfo, PartyID, ID, Reason}, _Opts) ->
    call(PartyID, {block_shop, ID, Reason, UserInfo});

handle_function('UnblockShop', {UserInfo, PartyID, ID, Reason}, _Opts) ->
    call(PartyID, {unblock_shop, ID, Reason, UserInfo});

handle_function('SuspendShop', {UserInfo, PartyID, ID}, _Opts) ->
    call(PartyID, {suspend_shop, ID, UserInfo});

handle_function('ActivateShop', {UserInfo, PartyID, ID}, _Opts) ->
    call(PartyID, {activate_shop, ID, UserInfo});

handle_function('GetClaim', {UserInfo, PartyID, ID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_claim(ID, St);

handle_function('GetPendingClaim', {UserInfo, PartyID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    ensure_claim(get_pending_claim(St));

handle_function('AcceptClaim', {UserInfo, PartyID, ID}, _Opts) ->
    call(PartyID, {accept_claim, ID, UserInfo});

handle_function('DenyClaim', {UserInfo, PartyID, ID, Reason}, _Opts) ->
    call(PartyID, {deny_claim, ID, Reason, UserInfo});

handle_function('RevokeClaim', {UserInfo, PartyID, ID, Reason}, _Opts) ->
    call(PartyID, {revoke_claim, ID, Reason, UserInfo});

handle_function('GetShopAccountState', {UserInfo, PartyID, AccountID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_account_state(AccountID, St);

handle_function('GetShopAccountSet', {UserInfo, PartyID, ShopID}, _Opts) ->
    St = get_state(UserInfo, PartyID),
    get_account_set(ShopID, St).


get_history(PartyID) ->
    map_history_error(hg_machine:get_history(?NS, PartyID)).

get_history(PartyID, AfterID, Limit) ->
    map_history_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit)).

%% TODO remove this hack as soon as machinegun learns to tell the difference between
%%      nonexsitent machine and empty history
assert_nonempty_history({[], _LastID}) ->
    throw(#payproc_PartyNotFound{});
assert_nonempty_history({[_ | _], _LastID} = Result) ->
    Result.

get_state(_UserInfo, PartyID) ->
    {History, _LastID} = get_history(PartyID),
    collapse_history(History).

get_public_history(_UserInfo, PartyID, AfterID, Limit) ->
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
    assert_nonempty_history(Result);
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

-type party_id()   :: dmsl_domain_thrift:'PartyID'().
-type party()      :: dmsl_domain_thrift:'Party'().
-type claim_id()   :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()      :: dmsl_payment_processing_thrift:'Claim'().
-type user_info()  :: dmsl_payment_processing_thrift:'UserInfo'().
-type revision()   :: dmsl_base_thrift:'Timestamp'().
-type sequence()   :: pos_integer().

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

-spec init(party_id(), {user_info()}) ->
    hg_machine:result(ev()).

init(ID, {_UserInfo}) ->
    {ok, StEvents} = create_party(ID, {#st{}, []}),
    Revision = hg_domain:head(),
    TestContractTemplpate = get_test_template(Revision),
    Changeset1 = create_contract(#payproc_ContractParams{template = TestContractTemplpate}, StEvents),
    [?contract_creation(TestContract)] = Changeset1,
    ShopParams = get_shop_prototype_params(Revision),
    % FIXME payoutaccount should be optional at shop
    Changeset2 = create_payout_account(
        #payproc_PayoutAccountParams{
            currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
            method = {bank_account, #domain_BankAccount{
                account = <<"1234567890">>,
                bank_name = <<"TestBank">>,
                bank_post_account = <<"12345">>,
                bank_bik = <<"012345">>
            }}
        },
        StEvents
    ),
    [?payout_account_creation(TestPayoutAccount)] = Changeset2,
    Changeset3 = create_shop(
        ShopParams#payproc_ShopParams{
            contract_id = TestContract#domain_Contract.id,
            payout_account_id = TestPayoutAccount#domain_PayoutAccount.id
        },
        ?active(),
        Revision,
        StEvents
    ),
    {_ClaimID, StEvents1} = submit_accept_claim(Changeset1 ++ Changeset2 ++ Changeset3, StEvents),
    ok(StEvents1).

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

process_signal(timeout, _History) ->
    ok();

process_signal({repair, _}, _History) ->
    ok().

-type call() ::
    {suspend | activate, user_info()}.

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

handle_call({block, Reason, _UserInfo}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?blocked(Reason)}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({unblock, Reason, _UserInfo}, StEvents0) ->
    ok = assert_blocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?unblocked(Reason)}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({suspend, _UserInfo}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_active(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?suspended()}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({activate, _UserInfo}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_suspended(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?active()}], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract, ContractParams, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    Changeset = create_contract(ContractParams, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({terminate_contract, ID, Reason, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    ok = assert_contract_active(ID, StEvents0),
    TerminatedAt = hg_datetime:format_now(),
    {ClaimID, StEvents1} = create_claim([?contract_termination(ID, TerminatedAt, Reason)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract_adjustment, ID, Params, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    ok = assert_contract_active(ID, StEvents0),
    Changeset = create_contract_adjustment(ID, Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_payout_account, Params, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    Changeset = create_payout_account(Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_shop, Params, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    Revision = hg_domain:head(),
    Changeset = create_shop(Params, Revision, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({update_shop, ID, Update, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    ok = assert_shop_modification_allowed(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {update, Update})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({block_shop, ID, Reason, _UserInfo}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?blocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({unblock_shop, ID, Reason, _UserInfo}, StEvents0) ->
    ok = assert_shop_blocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?unblocked(Reason)})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({suspend_shop, ID, _UserInfo}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_active(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?suspended()})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({activate_shop, ID, _UserInfo}, StEvents0) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_suspended(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?active()})], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({accept_claim, ID, _UserInfo}, StEvents0) ->
    {ID, StEvents1} = accept_claim(ID, StEvents0),
    respond(ok, StEvents1);

handle_call({deny_claim, ID, Reason, _UserInfo}, StEvents0) ->
    {ID, StEvents1} = finalize_claim(ID, ?denied(Reason), StEvents0),
    respond(ok, StEvents1);

handle_call({revoke_claim, ID, Reason, _UserInfo}, StEvents0) ->
    ok = assert_operable(StEvents0),
    {ID, StEvents1} = finalize_claim(ID, ?revoked(Reason), StEvents0),
    respond(ok, StEvents1).

%%

create_party(PartyID, StEvents) ->
    Party = #domain_Party{
        id         = PartyID,
        blocking   = ?unblocked(<<>>),
        suspension = ?active(),
        contracts = #{},
        shops      = #{},
        payout_accounts = #{}
    },
    Event = ?party_ev(?party_created(Party)),
    {ok, apply_state_event(Event, StEvents)}.

get_party(#st{party = Party}) ->
    Party.

%%

create_contract(ContractParams, {St, _}) ->
    ContractID = get_next_contract_id(get_pending_st(St)),
    Template = case ContractParams#payproc_ContractParams.template of
        undefined ->
            get_default_template();
        Any ->
            Any
    end,
    Contract = #domain_Contract{
        id = ContractID,
        contractor = ContractParams#payproc_ContractParams.contractor,
        concluded_at = hg_datetime:format_now(),
        template = Template,
        adjustments = []
    },
    [?contract_creation(Contract)].

get_next_contract_id(#st{party = #domain_Party{contracts = Contracts}}) ->
    get_next_id(maps:keys(Contracts)).

%%

create_contract_adjustment(ID, #payproc_ContractAdjustmentParams{template = Template}, {St, _}) ->
    Contract = get_contract(ID, get_party(get_pending_st(St))),
    AdjustmentID = get_next_contract_adjustment_id(Contract#domain_Contract.adjustments),
    Adjustment = #domain_ContractAdjustment{
        id = AdjustmentID,
        template = Template
    },
    [?contract_adjustment_creation(ID, Adjustment)].

get_next_contract_adjustment_id(Adjustments) ->
    get_next_id([0 | [ID || #domain_ContractAdjustment{id = ID} <- Adjustments]]).

%%

create_shop(ShopParams, Revision, StEvents) ->
    create_shop(ShopParams, ?suspended(), Revision, StEvents).

create_shop(ShopParams, Suspension, Revision, {St, _}) ->
    ShopID = get_next_shop_id(get_pending_st(St)),
    Shop   = construct_shop(ShopID, ShopParams, Suspension),
    ShopAccountSet = create_shop_account_set(Revision),
    [
        ?shop_creation(Shop),
        ?shop_modification(ShopID, ?accounts_created(ShopAccountSet))
    ].

construct_shop(ShopID, ShopParams, Suspension) ->
    #domain_Shop{
        id         = ShopID,
        blocking   = ?unblocked(<<>>),
        suspension = Suspension,
        details    = ShopParams#payproc_ShopParams.details,
        category   = ShopParams#payproc_ShopParams.category,
        contract_id = ShopParams#payproc_ShopParams.contract_id,
        payout_account_id = ShopParams#payproc_ShopParams.payout_account_id
    }.

get_next_shop_id(#st{party = #domain_Party{shops = Shops}}) ->
    % TODO cache sequences on history collapse
    get_next_id(maps:keys(Shops)).

%%

create_payout_account(#payproc_PayoutAccountParams{currency = Currency, method = PayoutTool}, {St, _}) ->
    AccountID = get_next_payout_account_id(get_pending_st(St)),
    PayoutAccount = #domain_PayoutAccount{
        id = AccountID,
        currency = Currency,
        method = PayoutTool
    },
    [?payout_account_creation(PayoutAccount)].

get_next_payout_account_id(#st{party = #domain_Party{payout_accounts = Accounts}}) ->
    get_next_id(maps:keys(Accounts)).
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
    Reason = <<"Superseded by ", ID/binary>>,
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
    is_change_need_acceptance(Modification);
is_change_need_acceptance({accounts_created, _}) ->
    false;
is_change_need_acceptance(_) ->
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
    get_next_binary_id(maps:keys(Claims)).

get_next_binary_id(IDs) ->
    integer_to_binary(1 + lists:max([0 | lists:map(fun binary_to_integer/1, IDs)])).

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

assert_contract_active(ID, {St, _}) ->
    Contract = get_contract(ID, get_party(St)),
    case Contract#domain_Contract.terminated_at of
        undefined ->
            ok;
        Terminated ->
            Active = hg_datetime:to_integer(Terminated) > hg_datetime:now(),
            case Active of
                true ->
                    ok;
                false ->
                    raise(#payproc_ContractNotFound{})
            end
    end.

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
    checkout_history(History, hg_datetime:to_integer(Revision), #st{}).

checkout_history([{_ID, EventTimestamp, Ev} | Rest], Revision, St0 = #st{revision = Rev0}) ->
    Rev1 = hg_datetime:to_integer(EventTimestamp),
    case Rev1 > Revision of
        true when Rev0 =/= undefined ->
            {ok, St0};
        true when Rev0 == undefined ->
            {error, revision_not_found};
        false ->
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
            raise(#payproc_ContractNotFound{})
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
    raise(#payproc_ShopNotFound{}).

get_account_set(ShopID, St = #st{}) ->
    Shop = get_shop(ShopID, get_party(St)),
    get_account_set(Shop).

get_account_set(#domain_Shop{accounts = undefined}) ->
    raise(#payproc_AccountSetNotFound{});
get_account_set(#domain_Shop{accounts = Accounts}) ->
    Accounts.

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
    #payproc_ShopAccountState{
        account_id = AccountID,
        own_amount = OwnAmount,
        available_amount = MinAvailableAmount,
        currency = Currency
    }.

ensure_account(AccountID, #domain_Party{shops = Shops}) ->
    case find_shop_account_set(AccountID, maps:to_list(Shops)) of
        #domain_ShopAccountSet{} ->
            ok;
        undefined ->
            raise(#payproc_AccountNotFound{})
    end.

find_shop_account_set(_ID, []) ->
    undefined;
find_shop_account_set(ID, [{_, #domain_Shop{accounts = Accounts}} | Rest]) ->
    case Accounts of
        #domain_ShopAccountSet{general = ID} ->
            Accounts;
        #domain_ShopAccountSet{guarantee = ID} ->
            Accounts;
        _ -> find_shop_account_set(ID, Rest)
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
    raise(#payproc_ClaimNotFound{}).

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
apply_party_change(?payout_account_creation(PayoutAccount), Party = #domain_Party{payout_accounts = Accounts}) ->
    ID = PayoutAccount#domain_PayoutAccount.id,
    Party#domain_Party{payout_accounts = Accounts#{ID => PayoutAccount}};
apply_party_change(?contract_creation(Contract), Party) ->
    set_contract(Contract, Party);
apply_party_change(?contract_termination(ID, TerminatedAt, _Reason), Party) ->
    Contract = get_contract(ID, Party),
    set_contract(Contract#domain_Contract{terminated_at = TerminatedAt}, Party);
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
        {Update#payproc_ShopUpdate.category   , fun (V, S) -> S#domain_Shop{category = V}   end},
        {Update#payproc_ShopUpdate.details    , fun (V, S) -> S#domain_Shop{details = V}    end},
        {Update#payproc_ShopUpdate.contract_id , fun (V, S) -> S#domain_Shop{contract_id = V} end},
        {Update#payproc_ShopUpdate.payout_account_id, fun (V, S) -> S#domain_Shop{payout_account_id = V} end}
    ], Shop);
apply_shop_change(
    {
        accounts_created,
        #payproc_ShopAccountSetCreated{accounts = Accounts}
    },
    Shop
) ->
    Shop#domain_Shop{accounts = Accounts}.

fold_opt([], V) ->
    V;
fold_opt([{undefined, _} | Rest], V) ->
    fold_opt(Rest, V);
fold_opt([{E, Fun} | Rest], V) ->
    fold_opt(Rest, Fun(E, V)).

get_next_id(IDs) ->
    lists:max([0 | IDs]) + 1.

create_shop_account_set(Revision) ->
    ShopPrototype = get_shop_prototype(Revision),
    CurrencyRef = ShopPrototype#domain_ShopPrototype.currency,
    #domain_CurrencyRef{
        symbolic_code = SymbolicCode
    } = CurrencyRef,
    GuaranteeID = hg_accounting:create_account(SymbolicCode),
    GeneralID = hg_accounting:create_account(SymbolicCode),
    #domain_ShopAccountSet{
        currency = CurrencyRef,
        general = GeneralID,
        guarantee = GuaranteeID
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

get_default_template() ->
    Revision = hg_domain:head(),
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    hg_domain:get(Revision, {default_contract_template, Globals#domain_Globals.default_contract_template}).



