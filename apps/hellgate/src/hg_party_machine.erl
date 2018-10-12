-module(hg_party_machine).

-include("party_events.hrl").
-include("legacy_party_structures.hrl").
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
-export([get_meta/1]).
-export([get_metadata/2]).
-export([get_last_revision/1]).

%%

-define(NS, <<"party">>).
-define(STEP, 5).
-define(SNAPSHOT_STEP, 10).
-define(CT_ERLANG_BINARY, <<"application/x-erlang-binary">>).

-record(st, {
    party                :: undefined | party(),
    timestamp            :: undefined | timestamp(),
    revision             :: hg_domain:revision(),
    claims   = #{}       :: #{claim_id() => claim()},
    meta = #{}           :: meta(),
    migration_data = #{} :: #{any() => any()},
    last_event = 0       :: event_id()
}).

-type st() :: #st{}.

-type call() ::
    {block, call_target(), binary()}                            |
    {unblock, call_target(), binary()}                          |
    {suspend, call_target()}                                    |
    {activate, call_target()}                                   |
    {set_metadata, meta_ns(), meta_data()}                      |
    {remove_metadata, meta_ns()}                                |
    {create_claim, changeset()}                                 |
    {update_claim, claim_id(), claim_revision(), changeset()}   |
    {accept_claim, claim_id(), claim_revision()}                |
    {deny_claim, claim_id(), claim_revision(), binary()}        |
    {revoke_claim, claim_id(), claim_revision(), binary()}.

-type call_target()     :: party | {shop, shop_id()} | {wallet, wallet_id()}.

-type party()           :: hg_party:party().
-type party_id()        :: dmsl_domain_thrift:'PartyID'().
-type shop_id()         :: dmsl_domain_thrift:'ShopID'().
-type wallet_id()       :: dmsl_domain_thrift:'WalletID'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().
-type timestamp()       :: hg_datetime:timestamp().
-type meta()            :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns()         :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data()       :: dmsl_domain_thrift:'PartyMetaData'().
-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().
-type party_revision()  :: dmsl_domain_thrift:'PartyRevision'().
-type event_id()        :: non_neg_integer().

-type content_type()    :: binary().
-type party_aux_st()    :: #{
    snapshot_index := snapshot_index(),
    party_revision_index := party_revision_index()
}.
-type snapshot_index()  :: [event_id()].
-type party_revision_index() :: #{
    party_revision() => event_range()
}.
-type event_range()     :: {
    FromEventID :: event_id() | undefined,
    ToEventID   :: event_id() | undefined
}.

-export_type([party_revision/0]).

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(dmsl_payment_processing_thrift:'PartyParams'(), hg_machine:machine()) ->
    hg_machine:result().

init(PartyParams, #{id := ID}) ->
    scoper:scope(
        party,
        #{
            id => ID,
            activity => init
        },
        fun() -> process_init(ID, PartyParams) end
    ).

process_init(PartyID, #payproc_PartyParams{contact_info = ContactInfo}) ->
    Timestamp = hg_datetime:format_now(),
    Changes = [?party_created(PartyID, ContactInfo, Timestamp), ?revision_changed(Timestamp, 0)],
    #{
        events  => [wrap_event_payload(?party_ev(Changes))],
        auxst   => wrap_aux_state(#{
            snapshot_index => [],
            party_revision_index => #{}
        })
    }.

-spec process_signal(hg_machine:signal(), hg_machine:machine()) ->
    hg_machine:result().

process_signal(timeout, _Machine) ->
    #{};

process_signal({repair, _}, _Machine) ->
    #{}.

-spec process_call(call(), hg_machine:machine()) ->
    {hg_machine:response(), hg_machine:result()}.

process_call(Call, #{id := PartyID, history := History, aux_state := WrappedAuxSt}) ->
    try
        scoper:scope(
            party,
            #{
                id => PartyID,
                activity => get_call_name(Call)
            },
            fun() ->
                AuxSt = unwrap_aux_state(WrappedAuxSt),
                St = get_state(PartyID, History, AuxSt),
                handle_call(Call, AuxSt, St)
            end
        )
    catch
        throw:Exception ->
            respond_w_exception(Exception)
    end.

get_call_name(Call) when is_tuple(Call) ->
    element(1, Call);
get_call_name(Call) when is_atom(Call) ->
    Call.

handle_call({block, Target, Reason}, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [block(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );

handle_call({unblock, Target, Reason}, AuxSt, St) ->
    ok = assert_blocked(Target, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [unblock(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );

handle_call({suspend, Target}, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_active(Target, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [suspend(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );

handle_call({activate, Target}, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_suspended(Target, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [activate(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );

handle_call({set_metadata, NS, Data}, AuxSt, St) ->
    respond(
        ok,
        [?party_meta_set(NS, Data)],
        AuxSt,
        St
    );

handle_call({remove_metadata, NS}, AuxSt, St) ->
    _ = get_st_metadata(NS, St),
    respond(
        ok,
        [?party_meta_removed(NS)],
        AuxSt,
        St
    );

handle_call({create_claim, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    {Claim, Changes} = create_claim(Changeset, St),
    respond(
        Claim,
        Changes,
        AuxSt,
        St
    );

handle_call({update_claim, ID, ClaimRevision, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    respond(
        ok,
        update_claim(ID, Changeset, St),
        AuxSt,
        St
    );

handle_call({accept_claim, ID, ClaimRevision}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    Claim = hg_claim:accept(
        Timestamp,
        get_st_revision(St),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );

handle_call({deny_claim, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:deny(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, get_st_timestamp(St))],
        AuxSt,
        St
    );

handle_call({revoke_claim, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:revoke(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, get_st_timestamp(St))],
        AuxSt,
        St
    ).

publish_party_event(Source, {ID, Dt, Ev = ?party_ev(_)}) ->
    #payproc_Event{id = ID, source = Source, created_at = Dt, payload = Ev}.

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

-spec publish_event(party_id(), msgpack_value()) ->
    hg_event_provider:public_event().

publish_event(PartyID, Ev) ->
    {{party_id, PartyID}, unwrap_event_payload(Ev)}.

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

get_state(PartyID) ->
    #{history := ReversedHistoryPart, aux_state := AuxSt} = get_machine(PartyID),
    get_state(PartyID, ReversedHistoryPart, AuxSt).

get_state(PartyID, ReversedHistoryPart, AuxSt) ->
    SnapshotIndex = get_snapshot_index(AuxSt),
    get_state(PartyID, ReversedHistoryPart, [], SnapshotIndex).

get_state(PartyID, ReversedHistoryPart, EventsAcc, SnapshotIndex) ->
    case parse_history(ReversedHistoryPart) of
        {undefined, [{FirstID, _, _} | _] = Events} when FirstID > 1 ->
            Limit = get_limit(FirstID, SnapshotIndex),
            NewHistoryPart = get_history(PartyID, FirstID, Limit, backward),
            get_state(PartyID, NewHistoryPart, Events ++ EventsAcc, SnapshotIndex);
        {undefined, Events} ->
            merge_events(Events ++ EventsAcc, #st{revision = hg_domain:head()});
        {St, Events} ->
            merge_events(Events ++ EventsAcc, St)
    end.

parse_history(ReversedHistoryPart) ->
    parse_history(ReversedHistoryPart, []).

parse_history([WrappedEvent | Others], EventsAcc) ->
    Event = unwrap_event(WrappedEvent),
    case unwrap_state(WrappedEvent) of
        undefined ->
            parse_history(Others, [Event | EventsAcc]);
        #st{} = St ->
            {St, [Event | EventsAcc]}
    end;
parse_history([], EventsAcc) ->
    {undefined, EventsAcc}.

-spec checkout(party_id(), party_revision_param()) ->
    dmsl_domain_thrift:'Party'() | no_return().

checkout(PartyID, RevisionParam) ->
    get_st_party(
        hg_utils:unwrap_result(
            checkout_party(PartyID, RevisionParam)
        )
    ).

-spec get_last_revision(party_id()) ->
    party_revision() | no_return().

get_last_revision(PartyID) ->
    {History, Last, Step} = get_history_part(PartyID, undefined, ?STEP),
    get_revision_of_part(PartyID, History, Last, Step).

-spec call(party_id(), call()) ->
    term() | no_return().

call(PartyID, Call) ->
    map_error(hg_machine:call(
        ?NS,
        PartyID,
        Call,
        undefined,
        ?SNAPSHOT_STEP,
        backward
    )).

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

-spec get_meta(party_id()) ->
    meta() | no_return().

get_meta(PartyID) ->
    #st{meta = Meta} = get_state(PartyID),
    Meta.

-spec get_metadata(meta_ns(), party_id()) ->
    meta_data() | no_return().

get_metadata(NS, PartyID) ->
    get_st_metadata(NS, get_state(PartyID)).

-spec get_public_history(party_id(), integer() | undefined, non_neg_integer()) ->
    [dmsl_payment_processing_thrift:'Event'()].

get_public_history(PartyID, AfterID, Limit) ->
    Events = unwrap_events(get_history(PartyID, AfterID, Limit)),
    [publish_party_event({party_id, PartyID}, Ev) || Ev <- Events].

get_history(PartyID, AfterID, Limit) ->
    get_history(PartyID, AfterID, Limit, forward).

get_history(PartyID, AfterID, Limit, Direction) ->
    map_history_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit, Direction)).

get_machine(PartyID) ->
    map_history_error(hg_machine:get_machine(
        ?NS,
        PartyID,
        undefined,
        ?SNAPSHOT_STEP,
        backward
    )).

get_revision_of_part(PartyID, History, Last, Step) ->
    case find_revision_in_history(History) of
        revision_not_found when Last == 0 ->
            0;
        revision_not_found ->
            {History1, Last1, Step1} = get_history_part(PartyID, Last, Step*2),
            get_revision_of_part(PartyID, History1, Last1, Step1);
        Revision ->
            Revision
    end.

get_history_part(PartyID, Last, Step) ->
    case unwrap_events(get_history(PartyID, Last, Step, backward)) of
        [] ->
            {[], 0, 0};
        History ->
            {LastID, _, _} = lists:last(History),
            {History, LastID, Step}
    end.

find_revision_in_history([]) ->
    revision_not_found;
find_revision_in_history([{_, _, ?party_ev(PartyChanges)} | Rest]) when is_list(PartyChanges) ->
    case find_revision_in_changes(PartyChanges) of
        revision_not_found ->
            find_revision_in_history(Rest);
        Revision ->
            Revision
    end.

find_revision_in_changes([]) ->
    revision_not_found;
find_revision_in_changes([Event | Rest]) ->
    case Event of
        ?revision_changed(_, Revision) when Revision =/= undefined ->
            Revision;
        _ ->
            find_revision_in_changes(Rest)
    end.

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{}).

%%

get_st_party(#st{party = Party}) ->
    Party.

get_next_party_revision(#st{party = Party}) ->
    Party#domain_Party.revision + 1.

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

-spec get_st_metadata(meta_ns(), st()) ->
    meta_data().

get_st_metadata(NS, #st{meta = Meta}) ->
    case maps:get(NS, Meta, undefined) of
        MetaData when MetaData =/= undefined ->
            MetaData;
        undefined ->
            throw(#payproc_PartyMetaNamespaceNotFound{})
    end.

set_claim(
    #payproc_Claim{id = ID} = Claim,
    #st{claims = Claims} = St
) ->
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
    case hg_claim:is_need_acceptance(Claim, Party, Revision) of
        false ->
            % Try to submit new accepted claim
            try
                AcceptedClaim = hg_claim:accept(Timestamp, Revision, Party, Claim),
                PartyRevision = get_next_party_revision(St),
                {
                    AcceptedClaim,
                    [
                        ?claim_created(Claim),
                        finalize_claim(AcceptedClaim, Timestamp),
                        ?revision_changed(Timestamp, PartyRevision)
                    ]
                }
            catch
                throw:_AnyException ->
                    {Claim, [?claim_created(Claim)]}
            end;
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
    ?claim_status_changed(
        hg_claim:get_id(Claim),
        hg_claim:get_status(Claim),
        hg_claim:get_revision(Claim),
        Timestamp
    ).

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

respond(Response, Changes, AuxSt0, St) ->
    AuxSt1 = append_party_revision_index(St, AuxSt0),
    {Events, AuxSt2} = try_attach_snapshot(Changes, AuxSt1, St),
    {
        {ok, Response},
        #{
            events  => Events,
            auxst   => AuxSt2
        }
    }.

respond_w_exception(Exception) ->
    {{exception, Exception}, #{}}.

append_party_revision_index(St, AuxSt) ->
    #domain_Party{revision = PartyRevision} = get_st_party(St),
    EventID = St#st.last_event + 1,
    PartyRevisionIndex0 = get_party_revision_index(AuxSt),
    {FromEventID, _} = get_party_revision_range(PartyRevision, PartyRevisionIndex0),
    PartyRevisionIndex1 = PartyRevisionIndex0#{
        PartyRevision => {hg_utils:select_defined(FromEventID, EventID), EventID}
    },
    AuxSt#{party_revision_index => PartyRevisionIndex1}.

get_party_revision_index(AuxSt) ->
    maps:get(party_revision_index, AuxSt, #{}).

get_party_revision_range(PartyRevision, PartyRevisionIndex) ->
    maps:get(PartyRevision, PartyRevisionIndex, {undefined, undefined}).

append_snapshot_index(EventID, AuxSt) ->
    SnapshotIndex = get_snapshot_index(AuxSt),
    AuxSt#{snapshot_index => [EventID | SnapshotIndex]}.

get_snapshot_index(AuxSt) ->
    maps:get(snapshot_index, AuxSt, []).

get_last_revision_event_id(Revision, FirstID, AuxSt) ->
    {_, ToEventID0} = get_party_revision_range(
        Revision,
        get_party_revision_index(AuxSt)
    ),
    case ToEventID0 of
        undefined ->
            FirstID;
        _ ->
            min(ToEventID0, FirstID)
    end.

get_limit(ToEventID, [SnapshotEventID | _]) when SnapshotEventID < ToEventID ->
    ToEventID - SnapshotEventID;
get_limit(ToEventID, [_ | SnapshotIndex]) ->
    get_limit(ToEventID, SnapshotIndex);
get_limit(_ToEventID, []) ->
    undefined.

%%

-spec checkout_party(party_id(), party_revision_param()) -> {ok, st()} | {error, revision_not_found}.

checkout_party(PartyID, {timestamp, Timestamp}) ->
    Events = unwrap_events(get_history(PartyID, undefined, undefined)),
    checkout_history_by_timestamp(Events, Timestamp, #st{revision = hg_domain:head()});
checkout_party(PartyID, {revision, Revision}) ->
    checkout_party_by_revision(PartyID, Revision).

checkout_history_by_timestamp([Ev | Rest], Timestamp, #st{timestamp = PrevTimestamp} = St) ->
    St1 = merge_event(Ev, St),
    EventTimestamp = St1#st.timestamp,
    case hg_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St#st{timestamp = Timestamp}};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history_by_timestamp(Rest, Timestamp, St1)
    end;
checkout_history_by_timestamp([], Timestamp, St) ->
    {ok, St#st{timestamp = Timestamp}}.

checkout_party_by_revision(PartyID, Revision) ->
    #{history := ReversedHistoryPart, aux_state := AuxSt0} = get_machine(PartyID),
    AuxSt = unwrap_aux_state(AuxSt0),
    {FirstID, _, _} = lists:last(ReversedHistoryPart),
    FromEventID = get_last_revision_event_id(Revision, FirstID, AuxSt),
    SnapshotIndex = get_snapshot_index(AuxSt),
    ReversedHistory = case FromEventID < FirstID of
        true ->
            Limit = get_limit(FromEventID, SnapshotIndex),
            get_history(PartyID, FromEventID, Limit, backward);
        false ->
            Limit = get_limit(FirstID, SnapshotIndex),
            ReversedHistoryPart ++ get_history(PartyID, FirstID, Limit, backward)
    end,
    checkout_history_by_revision(ReversedHistory, Revision).

checkout_history_by_revision(ReversedHistory, Revision) ->
    case parse_history(ReversedHistory) of
        {undefined, Events} ->
            checkout_history_by_revision(Events, Revision, #st{revision = hg_domain:head()});
        {St, Events} ->
            checkout_history_by_revision(Events, Revision, St)
    end.

checkout_history_by_revision([Ev | Rest], Revision, St) ->
    St1 = merge_event(Ev, St),
    case get_st_party(St1) of
        #domain_Party{revision = Revision1} when Revision1 > Revision ->
            {ok, St};
        _ ->
            checkout_history_by_revision(Rest, Revision, St1)
    end;
checkout_history_by_revision([], Revision, St) ->
    case get_st_party(St) of
        #domain_Party{revision = Revision} ->
            {ok, St};
        _ ->
            {error, revision_not_found}
    end.

merge_events(Events, St) ->
    lists:foldl(fun merge_event/2, St, Events).

merge_event({ID, _Dt, ?party_ev(PartyChanges)}, #st{last_event = LastEventID} = St)
    when is_list(PartyChanges) andalso ID =:= LastEventID + 1
->
     lists:foldl(fun merge_party_change/2, St#st{last_event = ID}, PartyChanges).

merge_party_change(?party_created(PartyID, ContactInfo, Timestamp), St) ->
    St#st{
        timestamp = Timestamp,
        party = hg_party:create_party(PartyID, ContactInfo, Timestamp)
    };
merge_party_change(?party_blocking(Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:blocking(Blocking, Party)};
merge_party_change(?revision_changed(Timestamp, Revision), St) ->
    Party = get_st_party(St),
    St#st{
        timestamp = Timestamp,
        party = Party#domain_Party{revision = Revision}
    };
merge_party_change(?party_suspension(Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:suspension(Suspension, Party)};
merge_party_change(?party_meta_set(NS, Data), #st{meta = Meta} = St) ->
    NewMeta = Meta#{NS => Data},
    St#st{meta = NewMeta};
merge_party_change(?party_meta_removed(NS), #st{meta = Meta} = St) ->
    NewMeta = maps:remove(NS, Meta),
    St#st{meta = NewMeta};
merge_party_change(?shop_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_blocking(ID, Blocking, Party)};
merge_party_change(?shop_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:shop_suspension(ID, Suspension, Party)};
merge_party_change(?wallet_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:wallet_blocking(ID, Blocking, Party)};
merge_party_change(?wallet_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#st{party = hg_party:wallet_suspension(ID, Suspension, Party)};
merge_party_change(?claim_created(Claim0), St) ->
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_change(?claim_updated(ID, Changeset, Revision, UpdatedAt), St) ->
    Claim0 = hg_claim:update_changeset(Changeset, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    set_claim(Claim, St);
merge_party_change(?claim_status_changed(ID, Status, Revision, UpdatedAt), St) ->
    Claim0 = hg_claim:set_status(Status, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1).

block(party, Reason, Timestamp) ->
    ?party_blocking(?blocked(Reason, Timestamp));
block({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?blocked(Reason, Timestamp));
block({wallet, ID}, Reason, Timestamp) ->
    ?wallet_blocking(ID, ?blocked(Reason, Timestamp)).

unblock(party, Reason, Timestamp) ->
    ?party_blocking(?unblocked(Reason, Timestamp));
unblock({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?unblocked(Reason, Timestamp));
unblock({wallet, ID}, Reason, Timestamp) ->
    ?wallet_blocking(ID, ?unblocked(Reason, Timestamp)).

suspend(party, Timestamp) ->
    ?party_suspension(?suspended(Timestamp));
suspend({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?suspended(Timestamp));
suspend({wallet, ID}, Timestamp) ->
    ?wallet_suspension(ID, ?suspended(Timestamp)).

activate(party, Timestamp) ->
    ?party_suspension(?active(Timestamp));
activate({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?active(Timestamp));
activate({wallet, ID}, Timestamp) ->
    ?wallet_suspension(ID, ?active(Timestamp)).

assert_party_operable(St) ->
    _ = assert_unblocked(party, St),
    _ = assert_active(party, St).

assert_unblocked(party, St) ->
    assert_blocking(get_st_party(St), unblocked);
assert_unblocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(hg_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, unblocked);
assert_unblocked({wallet, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Wallet = assert_wallet_found(hg_party:get_wallet(ID, Party)),
    assert_wallet_blocking(Wallet, unblocked).

assert_blocked(party, St) ->
    assert_blocking(get_st_party(St), blocked);
assert_blocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(hg_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, blocked);
assert_blocked({wallet, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Wallet = assert_wallet_found(hg_party:get_wallet(ID, Party)),
    assert_wallet_blocking(Wallet, blocked).

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_active(party, St) ->
    assert_suspension(get_st_party(St), active);
assert_active({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(hg_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, active);
assert_active({wallet, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Wallet = assert_wallet_found(hg_party:get_wallet(ID, Party)),
    assert_wallet_suspension(Wallet, active).

assert_suspended(party, St) ->
    assert_suspension(get_st_party(St), suspended);
assert_suspended({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(hg_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, suspended);
assert_suspended({wallet, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Wallet = assert_wallet_found(hg_party:get_wallet(ID, Party)),
    assert_wallet_suspension(Wallet, suspended).

assert_suspension(#domain_Party{suspension = {Status, _}}, Status) ->
    ok;
assert_suspension(#domain_Party{suspension = Suspension}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {suspension, Suspension}}).

assert_shop_found(#domain_Shop{} = Shop) ->
    Shop;
assert_shop_found(undefined) ->
    throw(#payproc_ShopNotFound{}).

assert_shop_blocking(#domain_Shop{blocking = {Status, _}}, Status) ->
    ok;
assert_shop_blocking(#domain_Shop{blocking = Blocking}, _) ->
    throw(#payproc_InvalidShopStatus{status = {blocking, Blocking}}).

assert_shop_suspension(#domain_Shop{suspension = {Status, _}}, Status) ->
    ok;
assert_shop_suspension(#domain_Shop{suspension = Suspension}, _) ->
    throw(#payproc_InvalidShopStatus{status = {suspension, Suspension}}).

assert_wallet_found(#domain_Wallet{} = Wallet) ->
    Wallet;
assert_wallet_found(undefined) ->
    throw(#payproc_WalletNotFound{}).

assert_wallet_blocking(#domain_Wallet{blocking = {Status, _}}, Status) ->
    ok;
assert_wallet_blocking(#domain_Wallet{blocking = Blocking}, _) ->
    throw(#payproc_InvalidWalletStatus{status = {blocking, Blocking}}).

assert_wallet_suspension(#domain_Wallet{suspension = {Status, _}}, Status) ->
    ok;
assert_wallet_suspension(#domain_Wallet{suspension = Suspension}, _) ->
    throw(#payproc_InvalidWalletStatus{status = {suspension, Suspension}}).

%% backward compatibility stuff
%% TODO remove after migration

ensure_claim(
    #payproc_Claim{
        created_at = Timestamp,
        changeset = Changeset0,
        status = Status0
    } = Claim
) ->
    Changeset = ensure_claim_changeset(Changeset0, Timestamp),
    Status = ensure_claim_status(Status0, Timestamp),
    Claim#payproc_Claim{
        changeset = Changeset,
        status = Status
    }.

ensure_claim_changeset(Changeset, Timestamp) ->
    [ensure_contract_change(C, Timestamp) || C <- Changeset].

ensure_contract_change(?contract_modification(ID, {creation, ContractParams}), Timestamp) ->
    ?contract_modification(
        ID,
        {creation, ensure_payment_institution(ContractParams, Timestamp)}
    );
ensure_contract_change(C, _) ->
    C.

ensure_claim_status({accepted, #payproc_ClaimAccepted{effects = Effects} = S}, Timestamp) ->
    {accepted, S#payproc_ClaimAccepted{
        effects = [ensure_contract_effect(E, Timestamp) || E <- Effects]
    }};
ensure_claim_status(S, _) ->
    S.

ensure_contract_effect(?contract_effect(ID, {created, Contract}), Timestamp) ->
    ?contract_effect(ID, {created, ensure_payment_institution(Contract, Timestamp)});
ensure_contract_effect(E, _) ->
    E.

ensure_payment_institution(#domain_Contract{payment_institution = undefined} = Contract, Timestamp) ->
    Revision = hg_domain:head(),
    PaymentInstitutionRef = get_default_payment_institution(
        get_realm(Contract, Timestamp, Revision),
        Revision
    ),
    Contract#domain_Contract{payment_institution = PaymentInstitutionRef};
ensure_payment_institution(#domain_Contract{} = Contract, _) ->
    Contract;
ensure_payment_institution(
    #payproc_ContractParams{
        template = TemplateRef,
        payment_institution = undefined
    } = ContractParams,
    Timestamp
) ->
    Revision = hg_domain:head(),
    Realm = case TemplateRef of
        undefined ->
            % use default live payment institution
            live;
        _ ->
            Template = get_template(TemplateRef, Revision),
            get_realm(Template, Timestamp, Revision)
    end,
    ContractParams#payproc_ContractParams{
        payment_institution = get_default_payment_institution(Realm, Revision)
    };
ensure_payment_institution(#payproc_ContractParams{} = ContractParams, _) ->
    ContractParams.

get_realm(C, Timestamp, Revision) ->
    Categories = hg_contract:get_categories(C, Timestamp, Revision),
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
        true when Test =:= true ->
            test;
        true when Live =:= true ->
            live;
        false ->
            error({
                misconfiguration,
                {'Test and live category in same term set', C, Timestamp, Revision}
            })
    end.

get_default_payment_institution(Realm, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    Defaults = Globals#domain_Globals.contract_payment_institution_defaults,
    case Realm of
        test ->
            Defaults#domain_ContractPaymentInstitutionDefaults.test;
        live ->
            Defaults#domain_ContractPaymentInstitutionDefaults.live
    end.

get_template(TemplateRef, Revision) ->
    hg_domain:get(Revision, {contract_template, TemplateRef}).

%%

try_attach_snapshot(Changes, AuxSt0, #st{last_event = LastEventID} = St)
    when
        LastEventID > 0 andalso
        LastEventID rem ?SNAPSHOT_STEP =:= 0
->
    AuxSt1 = append_snapshot_index(LastEventID + 1, AuxSt0),
    {
        [wrap_event_payload_w_snapshot(?party_ev(Changes), St)],
        wrap_aux_state(AuxSt1)
    };
try_attach_snapshot(Changes, AuxSt, _) ->
    {
        [wrap_event_payload(?party_ev(Changes))],
        wrap_aux_state(AuxSt)
    }.

%% TODO add transmutations for new international legal entities and bank accounts

-define(TOP_VERSION, 6).

wrap_event_payload(Event) ->
    ContentType = ?CT_ERLANG_BINARY,
    Meta = #{
        <<"vsn">> => ?TOP_VERSION,
        <<"ct">>  => ContentType
    },
    Data = encode_event(ContentType, Event),
    [Meta, Data].

wrap_event_payload_w_snapshot(Event, St) ->
    ContentType = ?CT_ERLANG_BINARY,
    Meta = #{
        <<"vsn">> => ?TOP_VERSION,
        <<"ct">>  => ContentType,
        <<"state_snapshot">> => encode_state(ContentType, St)
    },
    Data = encode_event(ContentType, Event),
    [Meta, Data].

unwrap_events(History) ->
    [unwrap_event(E) || E <- History].

unwrap_event({ID, Dt, Event}) ->
    {ID, Dt, unwrap_event_payload(Event)}.

unwrap_event_payload([
    #{
        <<"vsn">> := Version,
        <<"ct">>  := ContentType
    },
    EncodedEvent
]) ->
    transmute([Version, decode_event(ContentType, EncodedEvent)]);
%% TODO legacy support, will be removed after migration
unwrap_event_payload(Event) when is_list(Event) ->
    transmute(hg_party_marshalling:unmarshal(Event));
unwrap_event_payload({bin, Bin}) when is_binary(Bin) ->
    transmute([1, binary_to_term(Bin)]).

unwrap_state({
    _ID,
    _Dt,
    [
        #{<<"ct">>  := ContentType, <<"state_snapshot">> := EncodedSt},
        _EncodedEvent
    ]
}) ->
    decode_state(ContentType, EncodedSt);
unwrap_state(_) ->
    undefined.

encode_state(?CT_ERLANG_BINARY, St) ->
    {bin, term_to_binary(St)}.

decode_state(?CT_ERLANG_BINARY, {bin, EncodedSt}) ->
    binary_to_term(EncodedSt).

encode_event(?CT_ERLANG_BINARY, Event) ->
    {bin, term_to_binary(Event)}.

decode_event(?CT_ERLANG_BINARY, {bin, EncodedEvent}) ->
    binary_to_term(EncodedEvent).

-spec wrap_aux_state(party_aux_st()) -> hg_msgpack_marshalling:msgpack_value().

wrap_aux_state(AuxSt) ->
    ContentType = ?CT_ERLANG_BINARY,
    #{<<"ct">> => ContentType, <<"aux_state">> => encode_aux_state(ContentType, AuxSt)}.

-spec unwrap_aux_state(hg_msgpack_marshalling:msgpack_value()) -> party_aux_st().

unwrap_aux_state(#{<<"ct">> := ContentType, <<"aux_state">> := AuxSt}) ->
    decode_aux_state(ContentType, AuxSt);
%% backward compatibility
unwrap_aux_state(undefined) ->
    #{}.

-spec encode_aux_state(content_type(), party_aux_st()) -> dmsl_msgpack_thrift:'Value'().

encode_aux_state(?CT_ERLANG_BINARY, AuxSt) ->
    {bin, term_to_binary(AuxSt)}.

-spec decode_aux_state(content_type(), dmsl_msgpack_thrift:'Value'()) -> party_aux_st().

decode_aux_state(?CT_ERLANG_BINARY, {bin, AuxSt}) ->
    binary_to_term(AuxSt).

transmute([Version, Event]) ->
    transmute_event(Version, ?TOP_VERSION, Event).

transmute_event(V1, V2, ?party_ev(Changes)) when V2 > V1->
    NewChanges = [transmute_change(V1, V1 + 1, C) || C <- Changes],
    transmute_event(V1 + 1, V2, ?party_ev(NewChanges));
transmute_event(V, V, Event) ->
    Event.

-spec transmute_change(pos_integer(), pos_integer(), term()) ->
    dmsl_payment_processing_thrift:'PartyChange'().

transmute_change(1, 2,
    ?legacy_party_created(?legacy_party(ID, ContactInfo, CreatedAt, _, _, _, _))
) ->
    ?party_created(ID, ContactInfo, CreatedAt);
transmute_change(V1, V2,
    ?claim_created(?legacy_claim(
        ID,
        Status,
        Changeset,
        Revision,
        CreatedAt,
        UpdatedAt
    ))
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    NewChangeset = [transmute_party_modification(V1, V2, M) || M <- Changeset],
    ?claim_created(#payproc_Claim{
        id = ID,
        status = Status,
        changeset = NewChangeset,
        revision = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    });
transmute_change(V1, V2,
    ?legacy_claim_updated(ID, Changeset, ClaimRevision, Timestamp)
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    NewChangeset = [transmute_party_modification(V1, V2, M) || M <- Changeset],
    ?claim_updated(ID, NewChangeset, ClaimRevision, Timestamp);
transmute_change(V1, V2,
    ?claim_status_changed(ID, ?accepted(Effects), ClaimRevision, Timestamp)
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    NewEffects = [transmute_claim_effect(V1, V2, E) || E <- Effects],
    ?claim_status_changed(ID, ?accepted(NewEffects), ClaimRevision, Timestamp);
transmute_change(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    C.
transmute_party_modification(1, 2,
    ?legacy_contract_modification(ID, {creation, ?legacy_contract_params_v1(Contractor, TemplateRef)})
) ->
    ?legacy_contract_modification(ID, {creation, ?legacy_contract_params_v2(
        transmute_contractor(1, 2, Contractor),
        TemplateRef,
        undefined
    )});
transmute_party_modification(2, 3,
    ?legacy_contract_modification(
        ID,
        {creation, ?legacy_contract_params_v2(
            Contractor,
            TemplateRef,
            PaymentInstitutionRef
        )}
    )
) ->
    ?legacy_contract_modification(
        ID,
        {creation, ?legacy_contract_params_v3_4(
            transmute_contractor(2, 3, Contractor),
            TemplateRef,
            PaymentInstitutionRef
        )}
    );
transmute_party_modification(4, 5,
    ?legacy_contract_modification(
        ID,
        {creation, ?legacy_contract_params_v3_4(
            Contractor,
            TemplateRef,
            PaymentInstitutionRef
        )}
    )
) ->
    ?contract_modification(ID, {creation, #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef,
        payment_institution = PaymentInstitutionRef
    }});
transmute_party_modification(V1, V2,
    ?legacy_contract_modification(ContractID, ?legacy_payout_tool_creation(
        ID,
        ?legacy_payout_tool_params(Currency, ToolInfo)
    ))
) when V1 =:= 1; V1 =:= 2 ; V1 =:= 5 ->
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = transmute_payout_tool_info(V1, V2, ToolInfo)
    },
    ?contract_modification(ContractID, ?payout_tool_creation(ID, PayoutToolParams));
transmute_party_modification(3, 4,
    ?legacy_contract_modification(
        ID,
        {legal_agreement_binding, LegalAgreement}
    )
) ->
    ?contract_modification(ID, {legal_agreement_binding, transmute_legal_agreement(3, 4, LegalAgreement)});
transmute_party_modification(3, 4,
    ?legacy_shop_modification(
        ID,
        {payout_schedule_modification, ?legacy_schedule_modification(PayoutScheduleRef)}
    )
) ->
    ?shop_modification(
        ID,
        {payout_schedule_modification, #payproc_ScheduleModification{
            schedule = transmute_payout_schedule_ref(3, 4, PayoutScheduleRef)
        }}
    );
transmute_party_modification(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    C.

transmute_claim_effect(1, 2, ?legacy_contract_effect(
    ID,
    {created, ?legacy_contract_v1(
        ID,
        Contractor,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        PayoutTools,
        LegalAgreement
    )}
)) ->
    Contract = ?legacy_contract_v2_3(
        ID,
        transmute_contractor(1, 2, Contractor),
        undefined,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        [transmute_payout_tool(1, 2, P) || P <- PayoutTools],
        LegalAgreement
    ),
    ?legacy_contract_effect(ID, {created, Contract});
transmute_claim_effect(2, 3, ?legacy_contract_effect(
    ID,
    {created, ?legacy_contract_v2_3(
        ID,
        Contractor,
        PaymentInstitutionRef,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        PayoutTools,
        LegalAgreement
    )}
)) ->
    Contract = ?legacy_contract_v2_3(
        ID,
        transmute_contractor(2, 3, Contractor),
        PaymentInstitutionRef,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        [transmute_payout_tool(2, 3, P) || P <- PayoutTools],
        LegalAgreement
    ),
    ?legacy_contract_effect(ID, {created, Contract});
transmute_claim_effect(3, 4, ?legacy_contract_effect(
    ID,
    {created, ?legacy_contract_v2_3(
        ID,
        Contractor,
        PaymentInstitutionRef,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        PayoutTools,
        LegalAgreement
    )}
)) ->
    Contract = ?legacy_contract_v4(
        ID,
        Contractor,
        PaymentInstitutionRef,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        PayoutTools,
        transmute_legal_agreement(3, 4, LegalAgreement),
        undefined
    ),
    ?legacy_contract_effect(ID, {created, Contract});
transmute_claim_effect(4, 5, ?legacy_contract_effect(
    ID,
    {created, ?legacy_contract_v4(
        ID,
        Contractor,
        PaymentInstitutionRef,
        CreatedAt,
        ValidSince,
        ValidUntil,
        Status,
        Terms,
        Adjustments,
        PayoutTools,
        LegalAgreement,
        ReportPreferences
    )}
)) ->
    Contract = #domain_Contract{
        id = ID,
        contractor = Contractor,
        payment_institution = PaymentInstitutionRef,
        created_at = CreatedAt,
        valid_since = ValidSince,
        valid_until = ValidUntil,
        status = Status,
        terms = Terms,
        adjustments = Adjustments,
        payout_tools = PayoutTools,
        legal_agreement = LegalAgreement,
        report_preferences = ReportPreferences
    },
    ?contract_effect(ID, {created, Contract});
transmute_claim_effect(5, 6, ?contract_effect(
    ID,
    {created, Contract = #domain_Contract{payout_tools = PayoutTools}})
) ->
    ?contract_effect(ID, {created, Contract#domain_Contract{
        payout_tools = [transmute_payout_tool(5, 6, P) || P <- PayoutTools]
    }});
transmute_claim_effect(V1, V2, ?legacy_contract_effect(
    ContractID,
    {payout_tool_created, PayoutTool}
)) when V1 =:= 1; V1 =:= 2 ; V1 =:= 5 ->
    ?contract_effect(
        ContractID,
        {payout_tool_created, transmute_payout_tool(V1, V2, PayoutTool)}
    );
transmute_claim_effect(3, 4, ?legacy_contract_effect(
    ContractID,
    {legal_agreement_bound, LegalAgreement}
)) ->
    ?contract_effect(ContractID, {legal_agreement_bound, transmute_legal_agreement(3, 4, LegalAgreement)});
transmute_claim_effect(2, 3, ?legacy_shop_effect(
    ID,
    {created, ?legacy_shop_v2(
        ID, CreatedAt, Blocking, Suspension, Details, Location, Category, Account, ContractID, PayoutToolID
    )}
)) ->
    Shop = #domain_Shop{
        id = ID,
        created_at = CreatedAt,
        blocking = Blocking,
        suspension = Suspension,
        details = Details,
        location = Location,
        category = Category,
        account = Account,
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    },
    ?shop_effect(ID, {created, Shop});
transmute_claim_effect(3, 4, ?legacy_shop_effect(
    ID,
    {created, ?legacy_shop_v3(
        ID,
        CreatedAt,
        Blocking,
        Suspension,
        Details,
        Location,
        Category,
        Account,
        ContractID,
        PayoutToolID,
        PayoutSchedule
    )}
)) ->
    Shop = #domain_Shop{
        id = ID,
        created_at = CreatedAt,
        blocking = Blocking,
        suspension = Suspension,
        details = Details,
        location = Location,
        category = Category,
        account = Account,
        contract_id = ContractID,
        payout_tool_id = PayoutToolID,
        payout_schedule = transmute_payout_schedule_ref(3, 4, PayoutSchedule)
    },
    ?shop_effect(ID, {created, Shop});
transmute_claim_effect(3, 4, ?legacy_shop_effect(
    ID,
    {payout_schedule_changed, ?legacy_schedule_changed(PayoutSchedule)}
)) ->
    ?shop_effect(ID, {payout_schedule_changed, #payproc_ScheduleChanged{
        schedule = transmute_payout_schedule_ref(3, 4, PayoutSchedule)
    }});
transmute_claim_effect(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4 ; V1 =:= 5 ->
    C.

transmute_contractor(1, 2,
    {legal_entity, {russian_legal_entity, ?legacy_russian_legal_entity(
        RegisteredName,
        RegisteredNumber,
        Inn,
        ActualAddress,
        PostAddress,
        RepresentativePosition,
        RepresentativeFullName,
        RepresentativeDocument,
        BankAccount
    )}}
) ->
    {legal_entity, {russian_legal_entity, #domain_RussianLegalEntity{
        registered_name = RegisteredName,
        registered_number = RegisteredNumber,
        inn = Inn,
        actual_address = ActualAddress,
        post_address = PostAddress,
        representative_position = RepresentativePosition,
        representative_full_name = RepresentativeFullName,
        representative_document = RepresentativeDocument,
        russian_bank_account = transmute_bank_account(1, 2, BankAccount)
    }}};
transmute_contractor(2, 3,
    {legal_entity, {international_legal_entity, ?legacy_international_legal_entity(
        LegalName,
        TradingName,
        RegisteredAddress,
        ActualAddress
    )}}
) ->
    {legal_entity, {international_legal_entity, #domain_InternationalLegalEntity{
        legal_name = LegalName,
        trading_name = TradingName,
        registered_address = RegisteredAddress,
        actual_address = ActualAddress
    }}};
transmute_contractor(V1, _, Contractor) when V1 =:= 1; V1 =:= 2 ->
    Contractor.

transmute_payout_tool(V1, V2, ?legacy_payout_tool(
    ID,
    CreatedAt,
    Currency,
    ToolInfo
)) when V1 =:= 1; V1 =:= 2 ->
    #domain_PayoutTool{
        id = ID,
        created_at = CreatedAt,
        currency = Currency,
        payout_tool_info = transmute_payout_tool_info(V1, V2, ToolInfo)
    };
transmute_payout_tool(V1, _, PayoutTool) when V1 =:= 1; V1 =:= 2 ->
    PayoutTool;
transmute_payout_tool(V1, V2, PayoutTool = #domain_PayoutTool{payout_tool_info = ToolInfo}) when V1 =:= 5 ->
    PayoutTool#domain_PayoutTool{payout_tool_info = transmute_payout_tool_info(V1, V2, ToolInfo)}.

transmute_payout_tool_info(1, 2, {bank_account, BankAccount}) ->
    {russian_bank_account, transmute_bank_account(1, 2, BankAccount)};
transmute_payout_tool_info(2, 3, {international_bank_account, ?legacy_international_bank_account(
    AccountHolder,
    BankName,
    BankAddress,
    Iban,
    Bic
)}) ->
    {international_bank_account, ?legacy_international_bank_account_v3_4_5(
        AccountHolder,
        BankName,
        BankAddress,
        Iban,
        Bic,
        undefined
    )};
transmute_payout_tool_info(5, 6, {international_bank_account, ?legacy_international_bank_account_v3_4_5(
    AccountHolder,
    BankName,
    BankAddress,
    Iban,
    Bic,
    _LocalBankCode
)}) ->
    {international_bank_account, #domain_InternationalBankAccount{
        bank = #domain_InternationalBankDetails{
            bic = Bic,
            name = BankName,
            address = BankAddress
        },
        iban = Iban,
        account_holder = AccountHolder
    }};
transmute_payout_tool_info(V1, _, ToolInfo) when V1 =:= 1; V1 =:= 2 ; V1 =:= 5 ->
    ToolInfo.

transmute_bank_account(1, 2, ?legacy_bank_account(Account, BankName, BankPostAccount, BankBik)) ->
    #domain_RussianBankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }.

transmute_legal_agreement(3, 4, ?legacy_legal_agreement(SignedAt, LegalAgreementID)) ->
    #domain_LegalAgreement{
        signed_at =  SignedAt,
        legal_agreement_id = LegalAgreementID
    };
transmute_legal_agreement(3, 4, undefined) ->
    undefined.

transmute_payout_schedule_ref(3, 4, ?legacy_payout_schedule_ref(ID)) ->
    #domain_BusinessScheduleRef{id = ID};
transmute_payout_schedule_ref(3, 4, undefined) ->
    undefined.
