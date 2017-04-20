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
    party = undefined   :: undefined | party(),
    timestamp           :: timestamp(),
    revision            :: hg_domain:revision(),
    claims   = #{}      :: #{claim_id() => claim()},
    sequence = 0        :: 0 | sequence(),
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

-type public_event() :: dmsl_payment_processing_thrift:'EventPayload'().
-type private_event() :: none().

-type ev() ::
    {sequence(), public_event() | private_event()}.

-type party()           :: dmsl_domain_thrift:'Party'().
-type party_id()        :: dmsl_domain_thrift:'PartyID'().
-type shop_id()         :: dmsl_domain_thrift:'ShopID'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().
-type sequence()        :: pos_integer().
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

    {St, Events} = apply_state_event(
        ?party_ev(?party_created(Party)),
        {#st{timestamp = Timestamp, revision = Revision}, []}
    ),

    Claim = hg_claim:create_party_initial_claim(get_next_claim_id(St), Party, Timestamp, Revision),
    {_, StEvents1} = submit_claim(hg_claim:accept(Timestamp, Revision, Party, Claim), {St, Events}),
    ok(StEvents1).

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
        hg_log_scope:scope(
            party,
            fun() -> handle_call(Call, {St, []}) end,
            #{
                id => hg_party:get_party_id(get_st_party(St)),
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
    StEvents1 = apply_state_event(
        ?party_ev(?party_blocking(?blocked(Reason, hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({unblock, Reason}, StEvents0) ->
    ok = assert_blocked(StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?party_blocking(?unblocked(Reason, hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call(suspend, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_active(StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?party_suspension(?suspended(hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call(activate, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_suspended(StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?party_suspension(?active(hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({block_shop, ID, Reason}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_unblocked(ID, StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?shop_blocking(ID, ?blocked(Reason, hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({unblock_shop, ID, Reason}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_blocked(ID, StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?shop_blocking(ID, ?unblocked(Reason, hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({suspend_shop, ID}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_active(ID, StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?shop_suspension(ID, ?suspended(hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({activate_shop, ID}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    ok = assert_shop_unblocked(ID, StEvents0),
    ok = assert_shop_suspended(ID, StEvents0),
    StEvents1 = apply_state_event(
        ?party_ev(?shop_suspension(ID, ?active(hg_datetime:format_now()))),
        StEvents0
    ),
    respond(ok, StEvents1);

handle_call({create_claim, Changeset}, StEvents0) ->
    {Claim, StEvents1} = create_claim(Changeset, StEvents0),
    respond(Claim, StEvents1);

handle_call({update_claim, ID, ClaimRevision, Changeset}, StEvents0) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, StEvents0),
    StEvents1 = update_claim(ID, Changeset, StEvents0),
    respond(ok, StEvents1);

handle_call({accept_claim, ID, ClaimRevision}, StEvents0 = {St, _}) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, StEvents0),
    Claim = hg_claim:accept(
        get_st_timestamp(St),
        get_st_revision(St),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    StEvents1 = finalize_claim(Claim, StEvents0),
    respond(ok, StEvents1);

handle_call({deny_claim, ID, ClaimRevision, Reason}, StEvents0 = {St, _}) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, StEvents0),
    Claim = hg_claim:deny(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    StEvents1 = finalize_claim(Claim, StEvents0),
    respond(ok, StEvents1);

handle_call({revoke_claim, ID, ClaimRevision, Reason}, StEvents0 = {St, _}) ->
    ok = assert_operable(StEvents0),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, StEvents0),
    Claim = hg_claim:revoke(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    StEvents1 = finalize_claim(Claim, StEvents0),
    respond(ok, StEvents1).

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
    {History, _LastID} = get_history(PartyID),
    case checkout_history(History, Timestamp) of
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

-spec get_public_history(party_id(), integer(), non_neg_integer()) ->
    [ev()].

get_public_history(PartyID, AfterID, Limit) ->
    hg_history:get_public_history(
        fun (ID, Lim) -> get_history(PartyID, ID, Lim) end,
        fun (Event) -> publish_party_event({party, PartyID}, Event) end,
        AfterID, Limit
    ).

get_state(PartyID) ->
    {History, _LastID} = get_history(PartyID),
    collapse_history(assert_nonempty_history(History)).

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

assert_claim_modification_allowed(ID, Revision, {St, _}) ->
    Claim = get_st_claim(ID, St),
    ok = hg_claim:assert_revision(Claim, Revision),
    ok = hg_claim:assert_pending(Claim).

assert_claims_conflict(Claim, ClaimsPending, St) ->
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

create_claim(Changeset, StEvents = {St, _}) ->
    Timestamp = get_st_timestamp(St),
    Revision = get_st_revision(St),
    Claim = hg_claim:create(get_next_claim_id(St), Changeset, get_st_party(St), Timestamp, Revision),
    ClaimsPending = get_st_pending_claims(St),
    % Check for conflicts with other pending claims
    ok = assert_claims_conflict(Claim, ClaimsPending, St),
    % Test if we can safely accept proposed changes.
    case hg_claim:is_need_acceptance(Claim) of
        false ->
            % Submit new accepted claim
            AcceptedClaim = hg_claim:accept(get_st_timestamp(St), get_st_revision(St), get_st_party(St), Claim),
            submit_claim(AcceptedClaim, StEvents);
        true ->
            % Submit new pending claim
            submit_claim(Claim, StEvents)
    end.

submit_claim(Claim, StEvents) ->
    Event = ?party_ev(?claim_created(Claim)),
    {Claim, apply_state_event(Event, StEvents)}.

update_claim(ID, Changeset, StEvents = {St, _}) ->
    Timestamp = get_st_timestamp(St),
    Claim = hg_claim:update(
        Changeset,
        get_st_claim(ID, St),
        get_st_party(St),
        Timestamp,
        get_st_revision(St)
    ),
    ClaimsPending = get_st_pending_claims(St),
    % Check for conflicts with other pending claims
    ok = assert_claims_conflict(Claim, ClaimsPending, St),
    Event = ?party_ev(?claim_updated(ID, Changeset, hg_claim:get_revision(Claim), Timestamp)),
    apply_state_event(Event, StEvents).

% FIXME naming sucks
finalize_claim(Claim, StEvents = {St, _}) ->
    Event = ?party_ev(?claim_status_changed(
        hg_claim:get_id(Claim),
        hg_claim:get_status(Claim),
        hg_claim:get_revision(Claim),
        get_st_timestamp(St)
    )),
    apply_state_event(Event, StEvents).

apply_state_event(EventData, {St, EventsAcc}) ->
    Event = construct_event(EventData, St),
    {merge_history(Event, St), EventsAcc ++ [Event]}.

construct_event(EventData = ?party_ev(_), #st{sequence = Seq}) ->
    {Seq + 1, EventData}.

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
    % FIXME hg_domain:head() looks strange here
    checkout_history(History, undefined, #st{timestamp = Timestamp, revision = hg_domain:head()}).

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
merge_party_event(?party_blocking(Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:blocking(Blocking, Party)};
merge_party_event(?party_suspension(Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:suspension(Suspension, Party)};
merge_party_event(?shop_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_blocking(ID, Blocking, Party)};
merge_party_event(?shop_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_suspension(ID, Suspension, Party)};
merge_party_event(?claim_created(Claim), St) ->
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_event(?claim_updated(ID, Changeset, Revision, UpdatedAt), St) ->
    Claim = hg_claim:update_changeset(Changeset, Revision, UpdatedAt, get_st_claim(ID, St)),
    set_claim(Claim, St);
merge_party_event(?claim_status_changed(ID, Status, Revision, UpdatedAt), St) ->
    Claim = hg_claim:set_status(Status, Revision, UpdatedAt, get_st_claim(ID, St)),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1).

assert_operable(StEvents) ->
    _ = assert_unblocked(StEvents),
    _ = assert_active(StEvents).

assert_unblocked({St, _}) ->
    hg_party:assert_blocking(get_st_party(St), unblocked).

assert_blocked({St, _}) ->
    hg_party:assert_blocking(get_st_party(St), blocked).

assert_active({St, _}) ->
    hg_party:assert_suspension(get_st_party(St), active).

assert_suspended({St, _}) ->
    hg_party:assert_suspension(get_st_party(St), suspended).

assert_shop_unblocked(ID, {St, _}) ->
    Shop = hg_party:get_shop(ID, get_st_party(St)),
    hg_party:assert_shop_blocking(Shop, unblocked).

assert_shop_blocked(ID, {St, _}) ->
    Shop = hg_party:get_shop(ID, get_st_party(St)),
    hg_party:assert_shop_blocking(Shop, blocked).

assert_shop_active(ID, {St, _}) ->
    Shop = hg_party:get_shop(ID, get_st_party(St)),
    hg_party:assert_shop_suspension(Shop, active).

assert_shop_suspended(ID, {St, _}) ->
    Shop = hg_party:get_shop(ID, get_st_party(St)),
    hg_party:assert_shop_suspension(Shop, suspended).
