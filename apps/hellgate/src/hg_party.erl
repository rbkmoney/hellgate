%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/90a4eed/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/90a4eed/docs/domain/entities/merchant.md


%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)
%% * Create accounts after shop claim confirmation

-module(hg_party).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include_lib("hg_proto/include/hg_accounter_thrift.hrl").

-define(NS, <<"party">>).

%% Woody handler

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).

-export([init/3]).
-export([process_signal/3]).
-export([process_call/3]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%%

-define(try_w_context(Exp, Ctx),
    try {Exp, Ctx} catch {exception, E} -> throw({E, Ctx}) end
).

-spec handle_function(woody_t:func(), woody_server_thrift_handler:args(), woody_client:context(), []) ->
    {{ok, term()}, woody_client:context()} | no_return().

handle_function('Create', {UserInfo, PartyID}, Context0, _Opts) ->
    start(PartyID, {UserInfo}, Context0);

handle_function('Get', {UserInfo, PartyID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, PartyID, Context0),
    {{ok, get_party_state(St)}, Context};

handle_function('GetShop', {UserInfo, PartyID, ID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, PartyID, Context0),
    ?try_w_context({ok, get_shop_state(ID, St)}, Context);

handle_function('GetEvents', {UserInfo, PartyID, Range}, Context0, _Opts) ->
    #payproc_EventRange{'after' = AfterID, limit = Limit} = Range,
    {History, Context} = get_public_history(UserInfo, PartyID, AfterID, Limit, Context0),
    {{ok, History}, Context};

handle_function('Block', {UserInfo, PartyID, Reason}, Context0, _Opts) ->
    call(PartyID, {block, Reason, UserInfo}, Context0);

handle_function('Unblock', {UserInfo, PartyID, Reason}, Context0, _Opts) ->
    call(PartyID, {unblock, Reason, UserInfo}, Context0);

handle_function('Suspend', {UserInfo, PartyID}, Context0, _Opts) ->
    call(PartyID, {suspend, UserInfo}, Context0);

handle_function('Activate', {UserInfo, PartyID}, Context0, _Opts) ->
    call(PartyID, {activate, UserInfo}, Context0);

handle_function('CreateShop', {UserInfo, PartyID, Params}, Context0, _Opts) ->
    call(PartyID, {create_shop, Params, UserInfo}, Context0);

handle_function('UpdateShop', {UserInfo, PartyID, ID, Update}, Context0, _Opts) ->
    call(PartyID, {update_shop, ID, Update, UserInfo}, Context0);

handle_function('BlockShop', {UserInfo, PartyID, ID, Reason}, Context0, _Opts) ->
    call(PartyID, {block_shop, ID, Reason, UserInfo}, Context0);

handle_function('UnblockShop', {UserInfo, PartyID, ID, Reason}, Context0, _Opts) ->
    call(PartyID, {unblock_shop, ID, Reason, UserInfo}, Context0);

handle_function('SuspendShop', {UserInfo, PartyID, ID}, Context0, _Opts) ->
    call(PartyID, {suspend_shop, ID, UserInfo}, Context0);

handle_function('ActivateShop', {UserInfo, PartyID, ID}, Context0, _Opts) ->
    call(PartyID, {activate_shop, ID, UserInfo}, Context0);

handle_function('GetClaim', {UserInfo, PartyID, ID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, PartyID, Context0),
    ?try_w_context({ok, get_claim(ID, St)}, Context);

handle_function('GetPendingClaim', {UserInfo, PartyID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, PartyID, Context0),
    ?try_w_context({ok, ensure_claim(get_pending_claim(St))}, Context);

handle_function('AcceptClaim', {UserInfo, PartyID, ID}, Context, _Opts) ->
    call(PartyID, {accept_claim, ID, UserInfo}, Context);

handle_function('DenyClaim', {UserInfo, PartyID, ID, Reason}, Context, _Opts) ->
    call(PartyID, {deny_claim, ID, Reason, UserInfo}, Context);

handle_function('RevokeClaim', {UserInfo, PartyID, ID, Reason}, Context, _Opts) ->
    call(PartyID, {revoke_claim, ID, Reason, UserInfo}, Context);

handle_function('GetShopAccountState', {UserInfo, PartyID, AccountID}, Context0, _Opts) ->
    {St, Context1} = get_state(UserInfo, PartyID, Context0),
    {AccountState, Context} = ?try_w_context(
        get_account_state(AccountID, St, Context1),
        Context1
    ),
    {{ok, AccountState}, Context};

handle_function('GetShopAccountSet', {UserInfo, PartyID, ShopID}, Context0, _Opts) ->
    {St, Context} = get_state(UserInfo, PartyID, Context0),
    ?try_w_context({ok, get_account_set(ShopID, St)}, Context).


get_history(_UserInfo, PartyID, Context) ->
    assert_nonempty_history(
        map_error(hg_machine:get_history(?NS, PartyID, opts(Context)))
    ).

get_history(_UserInfo, PartyID, AfterID, Limit, Context) ->
    assert_nonempty_history(
        map_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit, opts(Context)))
    ).

%% TODO remove this hack as soon as machinegun learns to tell the difference between
%%      nonexsitent machine and empty history
assert_nonempty_history({{[], _LastID}, Context}) ->
    throw({#payproc_PartyNotFound{}, Context});
assert_nonempty_history({{[_ | _], _LastID}, _Context} = Result) ->
    Result.

get_state(UserInfo, PartyID, Context0) ->
    {{History, _LastID}, Context} = get_history(UserInfo, PartyID, Context0),
    {collapse_history(History), Context}.

get_public_history(UserInfo, PartyID, AfterID, Limit, Context) ->
    hg_history:get_public_history(
        fun (ID, Lim, Ctx) -> get_history(UserInfo, PartyID, ID, Lim, Ctx) end,
        fun (Event) -> publish_party_event({party, PartyID}, Event) end,
        AfterID, Limit,
        Context
    ).

start(ID, Args, Context) ->
    map_start_error(hg_machine:start(?NS, ID, Args, opts(Context))).

call(ID, Args, Context) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args, opts(Context))).

map_start_error({{error, exists}, Context}) ->
    throw({#payproc_PartyExists{}, Context});
map_start_error({Ok, Context}) ->
    {Ok, Context}.

map_error({{error, notfound}, Context}) ->
    throw({#payproc_PartyNotFound{}, Context});
map_error({{error, Reason}, _Context}) ->
    error(Reason);
map_error({Ok, Context}) ->
    {Ok, Context}.

opts(Context) ->
    #{client_context => Context}.

%%

-type party_id()   :: hg_domain_thrift:'PartyID'().
-type party()      :: hg_domain_thrift:'Party'().
-type claim_id()   :: hg_payment_processing_thrift:'ClaimID'().
-type claim()      :: hg_payment_processing_thrift:'Claim'().
-type user_info()  :: hg_payment_processing_thrift:'UserInfo'().
-type revision()   :: hg_domain_thrift:'DataRevision'().
-type sequence()   :: pos_integer().

-type ev() ::
    {sequence(), public_event() | private_event()}.

-type public_event() :: hg_payment_processing_thrift:'EventPayload'().
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

-spec init(party_id(), {user_info()}, hg_machine:context()) ->
    {hg_machine:result(ev()), hg_machine:context()}.

init(ID, {_UserInfo}, Context) ->
    {ok, StEvents} = create_party(ID, {#st{}, []}),
    {ok(StEvents), Context}.

-spec process_signal(hg_machine:signal(), hg_machine:history(ev()), hg_machine:context()) ->
    {hg_machine:result(ev()), hg_machine:context()}.

process_signal(timeout, _History, Context) ->
    {ok(), Context};

process_signal({repair, _}, _History, Context) ->
    {ok(), Context}.

-type call() ::
    {suspend | activate, user_info()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-spec process_call(call(), hg_machine:history(ev()), hg_machine:context()) ->
    {{response(), hg_machine:result(ev())}, hg_machine:context()}.

process_call(Call, History, Context) ->
    St = collapse_history(History),
    try handle_call(Call, {St, []}, Context) catch
        {exception, Exception} ->
            {respond_w_exception(Exception), Context}
    end.

-spec raise(term()) -> no_return().

raise(What) ->
    throw({exception, What}).

handle_call({block, Reason, _UserInfo}, StEvents0, Context) ->
    ok = assert_unblocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?blocked(Reason)}], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({unblock, Reason, _UserInfo}, StEvents0, Context) ->
    ok = assert_blocked(StEvents0),
    {ClaimID, StEvents1} = create_claim([{blocking, ?unblocked(Reason)}], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({suspend, _UserInfo}, StEvents0, Context) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_active(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?suspended()}], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({activate, _UserInfo}, StEvents0, Context) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_suspended(StEvents0),
    {ClaimID, StEvents1} = create_claim([{suspension, ?active()}], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({create_shop, Params, _UserInfo}, StEvents0 = {St0, _}, Context0) ->
    ok = assert_operable(StEvents0),
    {AccountShopSet, Context} = create_account_shop_set(StEvents0, Context0),
    ShopID = get_next_shop_id(get_pending_st(St0)),
    {ClaimID, StEvents1} = create_claim(
        [
            {shop_creation, construct_shop(ShopID, Params)},
            ?shop_modification(ShopID, ?accounts_created(AccountShopSet))
        ],
        StEvents0
    ),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({update_shop, ID, Update, _UserInfo}, StEvents0, Context) ->
    ok = assert_operable(StEvents0),
    ok = assert_shop_operable(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {update, Update})], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({block_shop, ID, Reason, _UserInfo}, StEvents0, Context) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?blocked(Reason)})], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({unblock_shop, ID, Reason, _UserInfo}, StEvents0, Context) ->
    ok = assert_shop_blocked(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {blocking, ?unblocked(Reason)})], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({suspend_shop, ID, _UserInfo}, StEvents0, Context) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_active(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?suspended()})], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({activate_shop, ID, _UserInfo}, StEvents0, Context) ->
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_suspended(ID, StEvents0),
    {ClaimID, StEvents1} = create_claim([?shop_modification(ID, {suspension, ?active()})], StEvents0),
    {respond(get_claim_result(ClaimID, StEvents1), StEvents1), Context};

handle_call({accept_claim, ID, _UserInfo}, StEvents0, Context) ->
    {ID, StEvents1} = accept_claim(ID, StEvents0),
    {respond(ok, StEvents1), Context};

handle_call({deny_claim, ID, Reason, _UserInfo}, StEvents0, Context) ->
    {ID, StEvents1} = finalize_claim(ID, ?denied(Reason), StEvents0),
    {respond(ok, StEvents1), Context};

handle_call({revoke_claim, ID, Reason, _UserInfo}, StEvents0, Context) ->
    ok = assert_operable(StEvents0),
    {ID, StEvents1} = finalize_claim(ID, ?revoked(Reason), StEvents0),
    {respond(ok, StEvents1), Context}.

%%

create_party(PartyID, StEvents) ->
    Party = #domain_Party{
        id         = PartyID,
        blocking   = ?unblocked(<<>>),
        suspension = ?active(),
        shops      = #{}
    },
    Event = ?party_ev(?party_created(Party, 1)),
    {ok, apply_state_event(Event, StEvents)}.

get_party(#st{party = Party}) ->
    Party.

get_party_state(#st{party = Party, revision = Revision}) ->
    #payproc_PartyState{party = Party, revision = Revision}.

get_shop_state(ID, #st{party = Party, revision = Revision}) ->
    #payproc_ShopState{shop = get_shop(ID, Party), revision = Revision}.

%%

construct_shop(ShopID, ShopParams) ->
    ShopServices = get_default_shop_services(),
    #domain_Shop{
        id         = ShopID,
        blocking   = ?unblocked(<<>>),
        suspension = ?suspended(),
        category   = ShopParams#payproc_ShopParams.category,
        details    = ShopParams#payproc_ShopParams.details,
        contractor = ShopParams#payproc_ShopParams.contractor,
        services   = ShopServices
    }.

get_next_shop_id(#st{party = #domain_Party{shops = Shops}}) ->
    % TODO cache sequences on history collapse
    get_next_id(maps:keys(Shops)).

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
    Claim = construct_claim(Changeset, St, ?accepted(St#st.revision + 1)),
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

accept_claim(ID, StEvents = {St, _}) ->
    finalize_claim(ID, ?accepted(St#st.revision + 1), StEvents).

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

assert_shop_operable(ID, StEvents) ->
    _ = assert_shop_unblocked(ID, StEvents),
    _ = assert_shop_active(ID, StEvents).

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

merge_history({Seq, Event}, St) ->
    merge_event(Event, St#st{sequence = Seq}).

merge_event(?party_ev(Ev), St) ->
    merge_party_event(Ev, St).

merge_party_event(?party_created(Party, Revision), St) ->
    St#st{party = Party, revision = Revision};
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

get_account_state(AccountID, St = #st{}, Context0) ->
    ok = ensure_account(AccountID, get_party(St)),
    {Account, Context0} = hg_account:get_account_by_id(AccountID, Context0),
    #{
        own_amount := OwnAmount,
        available_amount := AvailableAmount,
        currency_sym_code := CurrencySymCode
    } = Account,
    CurrencyRef = #domain_CurrencyRef{
        symbolic_code = CurrencySymCode
    },
    Currency = hg_domain:get(hg_domain:head(), CurrencyRef),
    #payproc_ShopAccountState{
        account_id = AccountID,
        own_amount = OwnAmount,
        available_amount = AvailableAmount,
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

apply_accepted_claim(Claim = #payproc_Claim{status = ?accepted(Revision)}, St) ->
    apply_claim(Claim, St#st{revision = Revision});
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
apply_party_change({shop_creation, Shop}, Party) ->
    set_shop(Shop, Party);
apply_party_change({shop_modification, #payproc_ShopModificationUnit{id = ID, modification = V}}, Party) ->
    set_shop(apply_shop_change(V, get_shop(ID, Party)), Party).

apply_shop_change({blocking, Blocking}, Shop) ->
    Shop#domain_Shop{blocking = Blocking};
apply_shop_change({suspension, Suspension}, Shop) ->
    Shop#domain_Shop{suspension = Suspension};
apply_shop_change({update, Update}, Shop) ->
    fold_opt([
        {Update#payproc_ShopUpdate.category   , fun (V, S) -> S#domain_Shop{category = V}   end},
        {Update#payproc_ShopUpdate.details    , fun (V, S) -> S#domain_Shop{details = V}    end},
        {Update#payproc_ShopUpdate.contractor , fun (V, S) -> S#domain_Shop{contractor = V} end}
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
    integer_to_binary(1 + lists:max([0 | lists:map(fun binary_to_integer/1, IDs)])).

create_account_shop_set(
    _Party,
    Context = #{
        client_context := ClientContext0
    }
) ->
    CurrencyRef = #domain_CurrencyRef{
        symbolic_code = <<"RUB">>
    }, %%% @FIXME use shop prototype instead
    #domain_CurrencyRef{
        symbolic_code = SymbolicCode
    } = CurrencyRef,
    {GuaranteeID, ClientContext1} = hg_account:create_account(SymbolicCode, ClientContext0),
    {GeneralID, ClientContext} = hg_account:create_account(SymbolicCode, ClientContext1),
    ShopAccountSet = #domain_ShopAccountSet {
        currency = CurrencyRef,
        general = GeneralID,
        guarantee = GuaranteeID
    },
    {ShopAccountSet, Context#{client_context => ClientContext}}.

get_default_shop_services() ->
    Head = hg_domain:head(),
    #domain_Globals{
        party_prototype = PartyPrototypeRef
    } = hg_domain:get(Head, #domain_GlobalsRef{}),
    #domain_PartyPrototype{
        default_services = ShopServices
    } = hg_domain:get(Head, PartyPrototypeRef),
    ShopServices.
