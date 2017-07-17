-module(hg_party_machine).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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

-export([start/2]).
-export([get_party/1]).
-export([checkout/2]).
-export([call/2]).
-export([get_claim/2]).
-export([get_claims/1]).
-export([get_public_history/3]).

%%

-define(NS, <<"party">>).

-record(st, {
    party                :: undefined | party(),
    timestamp            :: timestamp(),
    revision             :: hg_domain:revision(),
    claims   = #{}       :: #{claim_id() => claim()},
    migration_data = #{} :: #{any() => any()}
}).

-type st() :: #st{}.

-type call() ::
    {block, binary()}                                           |
    {unblock, binary()}                                         |
    suspend                                                     |
    activate                                                    |
    {block_shop, shop_id(), binary()}                           |
    {unblock_shop, shop_id(), binary()}                         |
    {suspend_shop, shop_id()}                                   |
    {activate_shop, shop_id()}                                  |
    {create_claim, changeset()}                                 |
    {update_claim, claim_id(), claim_revision(), changeset()}   |
    {accept_claim, claim_id(), claim_revision()}                |
    {deny_claim, claim_id(), claim_revision(), binary()}        |
    {revoke_claim, claim_id(), claim_revision(), binary()}.

-type response() ::
    ok | {ok, term()} | {exception, term()}.

-type ev() :: {party_changes, [dmsl_payment_processing_thrift:'PartyChange'()]}.

-type party()           :: dmsl_domain_thrift:'Party'().
-type party_id()        :: dmsl_domain_thrift:'PartyID'().
-type shop_id()         :: dmsl_domain_thrift:'ShopID'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().
-type timestamp()       :: dmsl_base_thrift:'Timestamp'().

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

process_init(PartyID, PartyParams) ->
    Timestamp = hg_datetime:format_now(),
    Revision = hg_domain:head(),
    Party = hg_party:create_party(PartyID, PartyParams, Timestamp),
    St = merge_party_change(?party_created(Party), #st{timestamp = Timestamp, revision = Revision}),
    Claim = hg_claim:create_party_initial_claim(get_next_claim_id(St), Party, Timestamp, Revision),
    Changes = [
        ?party_created(Party),
        ?claim_created(Claim)
    ] ++ finalize_claim(hg_claim:accept(Timestamp, Revision, Party, Claim), Timestamp),
    ok(Changes).

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

process_signal(timeout, _History) ->
    ok();

process_signal({repair, _}, _History) ->
    ok().

-spec process_call(call(), hg_machine:history(ev())) ->
    {response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    St = collapse_history(History),
    try
        Party = get_st_party(St),
        hg_log_scope:scope(
            party,
            fun() -> handle_call(Call, {St, []}) end,
            #{
                id => Party#domain_Party.id,
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

handle_call({block, Reason}, {St, _}) ->
    ok = assert_unblocked(St),
    respond(ok, [?party_blocking(?blocked(Reason, hg_datetime:format_now()))]);

handle_call({unblock, Reason}, {St, _}) ->
    ok = assert_blocked(St),
    respond(ok, [?party_blocking(?unblocked(Reason, hg_datetime:format_now()))]);

handle_call(suspend, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_active(St),
    respond(ok, [?party_suspension(?suspended(hg_datetime:format_now()))]);

handle_call(activate, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_suspended(St),
    respond(ok, [?party_suspension(?active(hg_datetime:format_now()))]);

handle_call({block_shop, ID, Reason}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    respond(ok, [?shop_blocking(ID, ?blocked(Reason, hg_datetime:format_now()))]);

handle_call({unblock_shop, ID, Reason}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_blocked(ID, St),
    respond(ok, [?shop_blocking(ID, ?unblocked(Reason, hg_datetime:format_now()))]);

handle_call({suspend_shop, ID}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    ok = assert_shop_active(ID, St),
    respond(ok, [?shop_suspension(ID, ?suspended(hg_datetime:format_now()))]);

handle_call({activate_shop, ID}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    ok = assert_shop_suspended(ID, St),
    respond(ok, [?shop_suspension(ID, ?active(hg_datetime:format_now()))]);

handle_call({create_claim, Changeset}, {St, _}) ->
    ok = assert_operable(St),
    {Claim, Changes} = create_claim(Changeset, St),
    respond(Claim, Changes);

handle_call({update_claim, ID, ClaimRevision, Changeset}, {St, _}) ->
    ok = assert_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    respond(ok, update_claim(ID, Changeset, St));

handle_call({accept_claim, ID, ClaimRevision}, {St, _}) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:accept(
        get_st_timestamp(St),
        get_st_revision(St),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    respond(ok, finalize_claim(Claim, get_st_timestamp(St)));

handle_call({deny_claim, ID, ClaimRevision, Reason}, {St, _}) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:deny(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(ok, finalize_claim(Claim, get_st_timestamp(St)));

handle_call({revoke_claim, ID, ClaimRevision, Reason}, {St, _}) ->
    ok = assert_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:revoke(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(ok, finalize_claim(Claim, get_st_timestamp(St))).

publish_party_event(Source, {ID, Dt, Ev = ?party_ev(_)}) ->
    #payproc_Event{id = ID, source = Source, created_at = Dt, payload = Ev}.

-spec publish_event(party_id(), ev()) ->
    hg_event_provider:public_event().

publish_event(PartyID, Ev = ?party_ev(_)) ->
    {{party_id, PartyID}, Ev}.

%%
-spec start(party_id(), Args :: term()) ->
    ok | no_return().

start(PartyID, Args) ->
    case hg_machine:start(?NS, PartyID, Args) of
        {ok, _} ->
            ok;
        {error, exists} ->
            throw(#payproc_PartyExists{})
    end.

-spec get_party(party_id()) ->
    dmsl_domain_thrift:'Party'() | no_return().

get_party(PartyID) ->
    get_st_party(get_state(PartyID)).

-spec checkout(party_id(), timestamp()) ->
    dmsl_domain_thrift:'Party'() | no_return().

checkout(PartyID, Timestamp) ->
    case checkout_history(get_history(PartyID), Timestamp) of
        {ok, St} ->
            get_st_party(St);
        {error, Reason} ->
            error(Reason)
    end.

-spec call(party_id(), call()) ->
    term() | no_return().

call(PartyID, Call) ->
    map_error(hg_machine:call(?NS, {id, PartyID}, Call)).

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

-spec get_claim(claim_id(), party_id()) ->
    claim() | no_return().

get_claim(ID, PartyID) ->
    get_st_claim(ID, get_state(PartyID)).

-spec get_claims(party_id()) ->
    [claim()] | no_return().

get_claims(PartyID) ->
    #st{claims = Claims} = get_state(PartyID),
    maps:values(Claims).

-spec get_public_history(party_id(), integer() | undefined, non_neg_integer()) ->
    [dmsl_payment_processing_thrift:'Event'()].

get_public_history(PartyID, AfterID, Limit) ->
    [publish_party_event({party_id, PartyID}, Ev) || Ev <- get_history(PartyID, AfterID, Limit)].

get_state(PartyID) ->
    collapse_history(assert_nonempty_history(get_history(PartyID))).

get_history(PartyID) ->
    map_history_error(hg_machine:get_history(?NS, PartyID)).

get_history(PartyID, AfterID, Limit) ->
    map_history_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit)).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

%%

get_st_party(#st{party = Party}) ->
    Party.

get_st_claim(ID, #st{claims = Claims}) ->
    assert_claim_exists(maps:get(ID, Claims, undefined)).

get_st_pending_claims(#st{claims = Claims})->
    % TODO cache it during history collapse
    % Looks like little overhead, compared to previous version (based on maps:fold),
    % but I hope for small amount of pending claims simultaniously.
    maps:values(maps:filter(
        fun(_ID, Claim) ->
            hg_claim:is_pending(Claim)
        end,
        Claims
    )).

-spec get_st_timestamp(st()) ->
    timestamp().

get_st_timestamp(#st{timestamp = Timestamp}) ->
    Timestamp.

-spec get_st_revision(st()) ->
    hg_domain:revision().

get_st_revision(#st{revision = Revision}) ->
    Revision.

%% TODO remove this hack as soon as machinegun learns to tell the difference between
%%      nonexsitent machine and empty history
assert_nonempty_history([_ | _] = Result) ->
    Result;
assert_nonempty_history([]) ->
    throw(#payproc_PartyNotFound{}).

set_claim(Claim = #payproc_Claim{id = ID}, St = #st{claims = Claims}) ->
    St#st{claims = Claims#{ID => Claim}}.

assert_claim_exists(Claim = #payproc_Claim{}) ->
    Claim;
assert_claim_exists(undefined) ->
    throw(#payproc_ClaimNotFound{}).

assert_claim_modification_allowed(ID, Revision, St) ->
    Claim = get_st_claim(ID, St),
    ok = hg_claim:assert_revision(Claim, Revision),
    ok = hg_claim:assert_pending(Claim).

assert_claims_not_conflict(Claim, ClaimsPending, St) ->
    Timestamp = get_st_timestamp(St),
    Revision = get_st_revision(St),
    Party = get_st_party(St),
    ConflictedClaims = lists:dropwhile(
        fun(PendingClaim) ->
            hg_claim:get_id(Claim) =:= hg_claim:get_id(PendingClaim) orelse
                not hg_claim:is_conflicting(Claim, PendingClaim, Timestamp, Revision, Party)
        end,
        ClaimsPending
    ),
    case ConflictedClaims of
        [] ->
            ok;
        [#payproc_Claim{id = ID} | _] ->
            throw(#payproc_ChangesetConflict{conflicted_id = ID})
    end.

%%

create_claim(Changeset, St) ->
    Timestamp = get_st_timestamp(St),
    Revision = get_st_revision(St),
    Party = get_st_party(St),
    Claim = hg_claim:create(get_next_claim_id(St), Changeset, Party, Timestamp, Revision),
    ClaimsPending = get_st_pending_claims(St),
    % Check for conflicts with other pending claims
    ok = assert_claims_not_conflict(Claim, ClaimsPending, St),
    % Test if we can safely accept proposed changes.
    case hg_claim:is_need_acceptance(Claim) of
        false ->
            % Submit new accepted claim
            AcceptedClaim = hg_claim:accept(Timestamp, Revision, Party, Claim),
            %% FIXME looks ugly
            {AcceptedClaim, [?claim_created(Claim)] ++ finalize_claim(AcceptedClaim, Timestamp)};
        true ->
            % Submit new pending claim
            {Claim, [?claim_created(Claim)]}
    end.

update_claim(ID, Changeset, St) ->
    Timestamp = get_st_timestamp(St),
    Claim = hg_claim:update(
        Changeset,
        get_st_claim(ID, St),
        get_st_party(St),
        Timestamp,
        get_st_revision(St)
    ),
    ClaimsPending = get_st_pending_claims(St),
    ok = assert_claims_not_conflict(Claim, ClaimsPending, St),
    [?claim_updated(ID, Changeset, hg_claim:get_revision(Claim), Timestamp)].

finalize_claim(Claim, Timestamp) ->
    [?claim_status_changed(
        hg_claim:get_id(Claim),
        hg_claim:get_status(Claim),
        hg_claim:get_revision(Claim),
        Timestamp
    )].

get_next_claim_id(#st{claims = Claims}) ->
    % TODO cache sequences on history collapse
    lists:max([0| maps:keys(Claims)]) + 1.

apply_accepted_claim(Claim, St) ->
    case hg_claim:is_accepted(Claim) of
        true ->
            Party = hg_claim:apply(Claim, get_st_timestamp(St), get_st_party(St)),
            St#st{party = Party};
        false ->
            St
    end.

ok() ->
    {[?party_ev([])], hg_machine_action:new()}.
ok(Changes) ->
    ok(Changes, hg_machine_action:new()).
ok(Changes, Action) when is_list(Changes) ->
    {[?party_ev(Changes)], Action}.

respond(Response, Changes) ->
    respond(Response, Changes, hg_machine_action:new()).
respond(Response, Changes, Action) when is_list(Changes) ->
        {{ok, Response}, {[?party_ev(Changes)], Action}}.

respond_w_exception(Exception) ->
    respond_w_exception(Exception, hg_machine_action:new()).
respond_w_exception(Exception, Action) ->
    {{exception, Exception}, {[], Action}}.

%%

-spec collapse_history(hg_machine:history(ev())) -> st().

collapse_history(History) ->
    {ok, St} = checkout_history(History, hg_datetime:format_now()),
    St.

-spec checkout_history(hg_machine:history(ev()), timestamp()) -> {ok, st()} | {error, revision_not_found}.

checkout_history(History, Timestamp) ->
    % FIXME hg_domain:head() looks strange here
    checkout_history(History, undefined, #st{timestamp = Timestamp, revision = hg_domain:head()}).

checkout_history([{_ID, EventTimestamp, Ev} | Rest], PrevTimestamp, #st{timestamp = Timestamp} = St) ->
    case hg_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history(Rest, EventTimestamp, merge_event(Ev, St))
    end;
checkout_history([], _, St) ->
    {ok, St}.

merge_event(?party_ev(PartyChanges), St) when is_list(PartyChanges) ->
     lists:foldl(fun merge_party_change/2, St, PartyChanges).

merge_party_change(?party_created(Party), St) ->
    St#st{party = Party};
merge_party_change(?party_blocking(Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:blocking(Blocking, Party)};
merge_party_change(?party_suspension(Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:suspension(Suspension, Party)};
merge_party_change(?shop_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_blocking(ID, Blocking, Party)};
merge_party_change(?shop_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_suspension(ID, Suspension, Party)};
merge_party_change(?claim_created(Claim), St) ->
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_change(?claim_updated(ID, Changeset, Revision, UpdatedAt), St) ->
    Claim = hg_claim:update_changeset(Changeset, Revision, UpdatedAt, get_st_claim(ID, St)),
    set_claim(Claim, St);
merge_party_change(?claim_status_changed(ID, Status, Revision, UpdatedAt), St) ->
    Claim = hg_claim:set_status(Status, Revision, UpdatedAt, get_st_claim(ID, St)),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1).

assert_operable(St) ->
    _ = assert_unblocked(St),
    _ = assert_active(St).

assert_unblocked(St) ->
    assert_blocking(get_st_party(St), unblocked).

assert_blocked(St) ->
    assert_blocking(get_st_party(St), blocked).

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_active(St) ->
    assert_suspension(get_st_party(St), active).

assert_suspended(St) ->
    assert_suspension(get_st_party(St), suspended).

assert_suspension(#domain_Party{suspension = {Status, _}}, Status) ->
    ok;
assert_suspension(#domain_Party{suspension = Suspension}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {suspension, Suspension}}).

assert_shop_found(#domain_Shop{} = Shop) ->
    Shop;
assert_shop_found(undefined) ->
    throw(#payproc_ShopNotFound{}).

assert_shop_unblocked(ID, St) ->
    Shop = assert_shop_found(hg_party:get_shop(ID, get_st_party(St))),
    assert_shop_blocking(Shop, unblocked).

assert_shop_blocked(ID, St) ->
    Shop = assert_shop_found(hg_party:get_shop(ID, get_st_party(St))),
    assert_shop_blocking(Shop, blocked).

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_active(ID, St) ->
    Shop = assert_shop_found(hg_party:get_shop(ID, get_st_party(St))),
    assert_shop_suspension(Shop, active).

assert_shop_suspended(ID, St) ->
    Shop = assert_shop_found(hg_party:get_shop(ID, get_st_party(St))),
    assert_shop_suspension(Shop, suspended).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).
