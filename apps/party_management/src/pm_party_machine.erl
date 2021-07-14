-module(pm_party_machine).

-include("party_events.hrl").
-include("legacy_party_structures.hrl").

-include_lib("pm_proto/include/dmsl_party_state_thrift.hrl").
-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-include("claim_management.hrl").

%% Machine callbacks

-behaviour(pm_machine).

-export([namespace/0]).
-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%%

-export([start/2]).
-export([get_party/1]).
-export([checkout/2]).
-export([call/4]).
-export([get_claim/2]).
-export([get_claims/1]).
-export([get_public_history/3]).
-export([get_meta/1]).
-export([get_metadata/2]).
-export([get_last_revision/1]).
-export([get_status/1]).

%%

-define(NS, <<"party">>).
-define(STEP, 5).
-define(SNAPSHOT_STEP, 10).
-define(CT_ERLANG_BINARY, <<"application/x-erlang-binary">>).

-type st() :: #pm_State{}.

-type call() :: pm_machine:thrift_call().
-type service_name() :: atom().

-type call_target() :: party | {shop, shop_id()}.

-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type party_status() :: pm_party:party_status().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type claim_id() :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim() :: dmsl_payment_processing_thrift:'Claim'().
-type meta() :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns() :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data() :: dmsl_domain_thrift:'PartyMetaData'().
-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().
-type party_revision() :: dmsl_domain_thrift:'PartyRevision'().
-type event_id() :: non_neg_integer().

-type content_type() :: binary().
-type party_aux_st() :: #{
    snapshot_index := snapshot_index(),
    party_revision_index := party_revision_index(),
    last_event_id => event_id()
}.

-type snapshot_index() :: [event_id()].
-type party_revision_index() :: #{
    party_revision() => event_range()
}.

-type event_range() :: {
    FromEventID :: event_id() | undefined,
    ToEventID :: event_id() | undefined
}.

-export_type([party_revision/0]).
-export_type([st/0]).

-spec namespace() -> pm_machine:ns().
namespace() ->
    ?NS.

-spec init(binary(), pm_machine:machine()) -> pm_machine:result().
init(EncodedPartyParams, #{id := ID}) ->
    ParamsType = {struct, struct, {dmsl_payment_processing_thrift, 'PartyParams'}},
    PartyParams = pm_proto_utils:deserialize(ParamsType, EncodedPartyParams),
    scoper:scope(
        party,
        #{
            id => ID,
            activity => init
        },
        fun() -> process_init(ID, PartyParams) end
    ).

process_init(PartyID, #payproc_PartyParams{contact_info = ContactInfo}) ->
    Timestamp = pm_datetime:format_now(),
    Changes = [?party_created(PartyID, ContactInfo, Timestamp), ?revision_changed(Timestamp, 0)],
    #{
        events => [wrap_event_payload(Changes)],
        auxst => wrap_aux_state(#{
            snapshot_index => [],
            party_revision_index => #{}
        })
    }.

-spec process_signal(pm_machine:signal(), pm_machine:machine()) -> pm_machine:result().
process_signal(timeout, _Machine) ->
    #{};
process_signal({repair, _}, _Machine) ->
    #{}.

-spec process_call(call(), pm_machine:machine()) -> {pm_machine:response(), pm_machine:result()}.
process_call({{'PartyManagement', Fun}, Args}, Machine) ->
    PartyID = erlang:element(2, Args),
    process_call_(PartyID, Fun, Args, Machine);
process_call({{'ClaimCommitter', Fun}, Args}, Machine) ->
    PartyID = erlang:element(1, Args),
    process_call_(PartyID, Fun, Args, Machine).

process_call_(PartyID, Fun, Args, Machine) ->
    #{id := PartyID, history := History, aux_state := WrappedAuxSt} = Machine,
    try
        scoper:scope(
            party,
            #{
                id => PartyID,
                activity => Fun
            },
            fun() ->
                AuxSt0 = unwrap_aux_state(WrappedAuxSt),
                {St, AuxSt1} = get_state_for_call(PartyID, History, AuxSt0),
                handle_call(Fun, Args, AuxSt1, St)
            end
        )
    catch
        throw:Exception ->
            respond_w_exception(Exception)
    end.

%% Party

handle_call('Block', {_, _PartyID, Reason}, AuxSt, St) ->
    handle_block(party, Reason, AuxSt, St);
handle_call('Unblock', {_, _PartyID, Reason}, AuxSt, St) ->
    handle_unblock(party, Reason, AuxSt, St);
handle_call('Suspend', {_, _PartyID}, AuxSt, St) ->
    handle_suspend(party, AuxSt, St);
handle_call('Activate', {_, _PartyID}, AuxSt, St) ->
    handle_activate(party, AuxSt, St);
%% Shop

handle_call('BlockShop', {_, _PartyID, ID, Reason}, AuxSt, St) ->
    handle_block({shop, ID}, Reason, AuxSt, St);
handle_call('UnblockShop', {_, _PartyID, ID, Reason}, AuxSt, St) ->
    handle_unblock({shop, ID}, Reason, AuxSt, St);
handle_call('SuspendShop', {_, _PartyID, ID}, AuxSt, St) ->
    handle_suspend({shop, ID}, AuxSt, St);
handle_call('ActivateShop', {_, _PartyID, ID}, AuxSt, St) ->
    handle_activate({shop, ID}, AuxSt, St);
%% PartyMeta

handle_call('SetMetaData', {_, _PartyID, NS, Data}, AuxSt, St) ->
    respond(
        ok,
        [?party_meta_set(NS, Data)],
        AuxSt,
        St
    );
handle_call('RemoveMetaData', {_, _PartyID, NS}, AuxSt, St) ->
    _ = get_st_metadata(NS, St),
    respond(
        ok,
        [?party_meta_removed(NS)],
        AuxSt,
        St
    );
%% Claim

handle_call('CreateClaim', {_, _PartyID, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    {Claim, Changes} = create_claim(Changeset, St),
    respond(
        Claim,
        Changes,
        AuxSt,
        St
    );
handle_call('UpdateClaim', {_, _PartyID, ID, ClaimRevision, Changeset}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    respond(
        ok,
        update_claim(ID, Changeset, St),
        AuxSt,
        St
    );
handle_call('AcceptClaim', {_, _PartyID, ID, ClaimRevision}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    Claim = pm_claim:accept(
        Timestamp,
        pm_domain:head(),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    );
handle_call('DenyClaim', {_, _PartyID, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Claim = pm_claim:deny(Reason, Timestamp, get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp)],
        AuxSt,
        St
    );
handle_call('RevokeClaim', {_, _PartyID, ID, ClaimRevision, Reason}, AuxSt, St) ->
    ok = assert_party_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Timestamp = pm_datetime:format_now(),
    Claim = pm_claim:revoke(Reason, Timestamp, get_st_claim(ID, St)),
    respond(
        ok,
        [finalize_claim(Claim, Timestamp)],
        AuxSt,
        St
    );
%% ClaimCommitter

handle_call('Accept', {_PartyID, Claim}, AuxSt, St) ->
    #claim_management_Claim{
        changeset = Changeset
    } = Claim,
    try
        Party = get_st_party(St),
        ok = pm_claim_committer:assert_cash_regisrter_modifications_applicable(Changeset, Party),
        case pm_claim_committer:from_claim_mgmt(Claim) of
            undefined ->
                ok;
            PayprocClaim ->
                Timestamp = pm_datetime:format_now(),
                Revision = pm_domain:head(),

                ok = pm_claim:assert_applicable(PayprocClaim, Timestamp, Revision, Party),
                ok = pm_claim:assert_acceptable(PayprocClaim, Timestamp, Revision, Party)
        end,
        respond(
            ok,
            [],
            AuxSt,
            St
        )
    catch
        throw:#payproc_InvalidChangeset{reason = Reason0} ->
            Reason1 = io_lib:format("~0tp", [Reason0]),
            Reason2 = unicode:characters_to_binary(Reason1),
            InvalidModificationChangeset = [
                Modification
                || #claim_management_ModificationUnit{
                       modification = Modification
                   } <- Changeset
            ],
            erlang:throw(#claim_management_InvalidChangeset{
                reason = Reason2,
                invalid_changeset = InvalidModificationChangeset
            })
    end;
handle_call('Commit', {_PartyID, CmClaim}, AuxSt, St) ->
    PayprocClaim = pm_claim_committer:from_claim_mgmt(CmClaim),
    Changes = get_changes(PayprocClaim, St),
    respond(
        ok,
        Changes,
        AuxSt,
        St
    ).

get_changes(undefined, _St) ->
    [];
get_changes(PayprocClaim, St) ->
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Party = get_st_party(St),
    AcceptedClaim = pm_claim:accept(Timestamp, Revision, Party, PayprocClaim),
    PartyRevision = get_next_party_revision(St),
    [
        ?claim_created(PayprocClaim),
        finalize_claim(AcceptedClaim, Timestamp),
        ?revision_changed(Timestamp, PartyRevision)
    ].

%% Generic handlers

-spec handle_block(call_target(), binary(), party_aux_st(), st()) -> {pm_machine:response(), pm_machine:result()}.
handle_block(Target, Reason, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [block(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_unblock(call_target(), binary(), party_aux_st(), st()) -> {pm_machine:response(), pm_machine:result()}.
handle_unblock(Target, Reason, AuxSt, St) ->
    ok = assert_blocked(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [unblock(Target, Reason, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_suspend(call_target(), party_aux_st(), st()) -> {pm_machine:response(), pm_machine:result()}.
handle_suspend(Target, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_active(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [suspend(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

-spec handle_activate(call_target(), party_aux_st(), st()) -> {pm_machine:response(), pm_machine:result()}.
handle_activate(Target, AuxSt, St) ->
    ok = assert_unblocked(Target, St),
    ok = assert_suspended(Target, St),
    Timestamp = pm_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(
        ok,
        [activate(Target, Timestamp), ?revision_changed(Timestamp, Revision)],
        AuxSt,
        St
    ).

publish_party_event(Source, {ID, Dt, {Changes, _}}) ->
    #payproc_Event{id = ID, source = Source, created_at = Dt, payload = ?party_ev(Changes)}.

%%
-spec start(party_id(), Args :: term()) -> ok | no_return().
start(PartyID, PartyParams) ->
    ParamsType = {struct, struct, {dmsl_payment_processing_thrift, 'PartyParams'}},
    EncodedPartyParams = pm_proto_utils:serialize(ParamsType, PartyParams),
    case pm_machine:start(?NS, PartyID, EncodedPartyParams) of
        {ok, _} ->
            ok;
        {error, exists} ->
            throw(#payproc_PartyExists{})
    end.

-spec get_party(party_id()) -> dmsl_domain_thrift:'Party'() | no_return().
get_party(PartyID) ->
    get_st_party(get_state(PartyID)).

get_state(PartyID) ->
    AuxSt = get_aux_state(PartyID),
    get_state(PartyID, get_snapshot_index(AuxSt)).

get_state(PartyID, []) ->
    %% No snapshots, so we need entire history
    Events = unwrap_events(get_history(PartyID, undefined, undefined, forward)),
    merge_events(Events, #pm_State{});
get_state(PartyID, [FirstID | _]) ->
    History = get_history(PartyID, FirstID - 1, undefined, forward),
    Events = [FirstEvent | _] = unwrap_events(History),
    St = unwrap_state(FirstEvent),
    merge_events(Events, St).

get_state_for_call(PartyID, ReversedHistoryPart, AuxSt) ->
    {St, History} = parse_history(ReversedHistoryPart),
    get_state_for_call(PartyID, {St, History}, [], AuxSt).

get_state_for_call(PartyID, {undefined, [{FirstID, _, _} | _] = Events}, EventsAcc, AuxSt) when FirstID > 1 ->
    Limit = get_limit(FirstID, get_snapshot_index(AuxSt)),
    NewHistoryPart = parse_history(get_history(PartyID, FirstID, Limit, backward)),
    get_state_for_call(PartyID, NewHistoryPart, Events ++ EventsAcc, AuxSt);
get_state_for_call(_, {St0, Events}, EventsAcc, AuxSt0) ->
    %% here we can get entire history.
    %% we can use it to create revision index for AuxSt
    PartyRevisionIndex0 = get_party_revision_index(AuxSt0),
    {St1, PartyRevisionIndex1} = build_revision_index(
        Events ++ EventsAcc,
        PartyRevisionIndex0,
        pm_utils:select_defined(St0, #pm_State{})
    ),
    AuxSt1 = set_party_revision_index(PartyRevisionIndex1, AuxSt0),
    {St1, AuxSt1}.

parse_history(ReversedHistoryPart) ->
    parse_history(ReversedHistoryPart, []).

parse_history([WrappedEvent | Others], EventsAcc) ->
    Event = unwrap_event(WrappedEvent),
    case unwrap_state(Event) of
        undefined ->
            parse_history(Others, [Event | EventsAcc]);
        #pm_State{} = St ->
            {St, [Event | EventsAcc]}
    end;
parse_history([], EventsAcc) ->
    {undefined, EventsAcc}.

-spec checkout(party_id(), party_revision_param()) -> dmsl_domain_thrift:'Party'() | no_return().
checkout(PartyID, RevisionParam) ->
    get_st_party(
        pm_utils:unwrap_result(
            checkout_party(PartyID, RevisionParam)
        )
    ).

-spec get_last_revision(party_id()) -> party_revision() | no_return().
get_last_revision(PartyID) ->
    AuxState = get_aux_state(PartyID),
    LastEventID = maps:get(last_event_id, AuxState),
    case get_party_revision_index(AuxState) of
        RevisionIndex when map_size(RevisionIndex) > 0 ->
            MaxRevision = lists:max(maps:keys(RevisionIndex)),
            % we should check if this is the last revision for real
            {_, ToEventID} = get_party_revision_range(MaxRevision, RevisionIndex),
            case ToEventID < LastEventID of
                true ->
                    % there are events after MaxRevision, so it can be a bug
                    _ = logger:warning(
                        "Max revision EventID (~p) and LastEventID (~p) missmatch",
                        [ToEventID, LastEventID]
                    ),
                    get_last_revision_old_way(PartyID);
                false ->
                    MaxRevision
            end;
        _ ->
            get_last_revision_old_way(PartyID)
    end.

-spec get_last_revision_old_way(party_id()) -> party_revision() | no_return().
get_last_revision_old_way(PartyID) ->
    get_revision_of_part(PartyID, undefined, ?STEP).

-spec get_status(party_id()) -> party_status() | no_return().
get_status(PartyID) ->
    pm_party:get_status(
        get_party(PartyID)
    ).

-spec call(party_id(), service_name(), pm_proto_utils:thrift_fun_ref(), woody:args()) -> term() | no_return().
call(PartyID, ServiceName, FucntionRef, Args) ->
    map_error(
        pm_machine:thrift_call(
            ?NS,
            PartyID,
            ServiceName,
            FucntionRef,
            Args,
            undefined,
            ?SNAPSHOT_STEP,
            backward
        )
    ).

map_error(ok) ->
    ok;
map_error({ok, CallResult}) ->
    CallResult;
map_error({exception, Reason}) ->
    throw(Reason);
map_error({error, notfound}) ->
    throw(#payproc_PartyNotFound{});
map_error({error, Reason}) ->
    error(Reason).

-spec get_claim(claim_id(), party_id()) -> claim() | no_return().
get_claim(ID, PartyID) ->
    get_st_claim(ID, get_state(PartyID)).

-spec get_claims(party_id()) -> [claim()] | no_return().
get_claims(PartyID) ->
    #pm_State{claims = Claims} = get_state(PartyID),
    maps:values(Claims).

-spec get_meta(party_id()) -> meta() | no_return().
get_meta(PartyID) ->
    #pm_State{meta = Meta} = get_state(PartyID),
    Meta.

-spec get_metadata(meta_ns(), party_id()) -> meta_data() | no_return().
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
    map_history_error(pm_machine:get_history(?NS, PartyID, AfterID, Limit, Direction)).

-spec get_aux_state(party_id()) -> party_aux_st().
get_aux_state(PartyID) ->
    #{aux_state := AuxSt, history := History} = map_history_error(
        pm_machine:get_machine(
            ?NS,
            PartyID,
            undefined,
            1,
            backward
        )
    ),
    AuxState = unwrap_aux_state(AuxSt),
    case History of
        [] ->
            AuxState#{last_event_id => 0};
        [{EventID, _, _}] ->
            AuxState#{last_event_id => EventID}
    end.

get_revision_of_part(PartyID, Last, Step) ->
    {History, LastNext, StepNext} = get_history_part(PartyID, Last, Step),
    case find_revision_in_history(History) of
        revision_not_found when Last == 0 ->
            0;
        revision_not_found ->
            get_revision_of_part(PartyID, LastNext, StepNext);
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
find_revision_in_history([{_, _, {PartyChanges, _}} | Rest]) when is_list(PartyChanges) ->
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

get_st_party(#pm_State{party = Party}) ->
    Party.

get_next_party_revision(#pm_State{party = Party}) ->
    Party#domain_Party.revision + 1.

get_st_claim(ID, #pm_State{claims = Claims}) ->
    assert_claim_exists(maps:get(ID, Claims, undefined)).

get_st_pending_claims(#pm_State{claims = Claims}) ->
    % TODO cache it during history collapse
    % Looks like little overhead, compared to previous version (based on maps:fold),
    % but I hope for small amount of pending claims simultaniously.
    maps:values(
        maps:filter(
            fun(_ID, Claim) ->
                pm_claim:is_pending(Claim)
            end,
            Claims
        )
    ).

-spec get_st_metadata(meta_ns(), st()) -> meta_data().
get_st_metadata(NS, #pm_State{meta = Meta}) ->
    case maps:get(NS, Meta, undefined) of
        MetaData when MetaData =/= undefined ->
            MetaData;
        undefined ->
            throw(#payproc_PartyMetaNamespaceNotFound{})
    end.

set_claim(
    #payproc_Claim{id = ID} = Claim,
    #pm_State{claims = Claims} = St
) ->
    St#pm_State{claims = Claims#{ID => Claim}}.

assert_claim_exists(Claim = #payproc_Claim{}) ->
    Claim;
assert_claim_exists(undefined) ->
    throw(#payproc_ClaimNotFound{}).

assert_claim_modification_allowed(ID, Revision, St) ->
    Claim = get_st_claim(ID, St),
    ok = pm_claim:assert_revision(Claim, Revision),
    ok = pm_claim:assert_pending(Claim).

assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party) ->
    ConflictedClaims = lists:dropwhile(
        fun(PendingClaim) ->
            pm_claim:get_id(Claim) =:= pm_claim:get_id(PendingClaim) orelse
                not pm_claim:is_conflicting(Claim, PendingClaim, Timestamp, Revision, Party)
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
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Party = get_st_party(St),
    Claim = pm_claim:create(get_next_claim_id(St), Changeset, Party, Timestamp, Revision),
    ClaimsPending = get_st_pending_claims(St),
    % Check for conflicts with other pending claims
    ok = assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party),
    % Test if we can safely accept proposed changes.
    case pm_claim:is_need_acceptance(Claim, Party, Revision) of
        false ->
            % Try to submit new accepted claim
            try
                AcceptedClaim = pm_claim:accept(Timestamp, Revision, Party, Claim),
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
    Timestamp = pm_datetime:format_now(),
    Revision = pm_domain:head(),
    Party = get_st_party(St),
    Claim = pm_claim:update(
        Changeset,
        get_st_claim(ID, St),
        Party,
        Timestamp,
        Revision
    ),
    ClaimsPending = get_st_pending_claims(St),
    ok = assert_claims_not_conflict(Claim, ClaimsPending, Timestamp, Revision, Party),
    [?claim_updated(ID, Changeset, pm_claim:get_revision(Claim), Timestamp)].

finalize_claim(Claim, Timestamp) ->
    ?claim_status_changed(
        pm_claim:get_id(Claim),
        pm_claim:get_status(Claim),
        pm_claim:get_revision(Claim),
        Timestamp
    ).

get_next_claim_id(#pm_State{claims = Claims}) ->
    % TODO cache sequences on history collapse
    lists:max([0 | maps:keys(Claims)]) + 1.

apply_accepted_claim(Claim, St) ->
    case pm_claim:is_accepted(Claim) of
        true ->
            Party = pm_claim:apply(Claim, pm_datetime:format_now(), get_st_party(St)),
            St#pm_State{party = Party};
        false ->
            St
    end.

respond(ok, Changes, AuxSt, St) ->
    do_respond(ok, Changes, AuxSt, St);
respond(Response, Changes, AuxSt, St) ->
    do_respond({ok, Response}, Changes, AuxSt, St).

do_respond(Response, Changes, AuxSt0, St) ->
    AuxSt1 = append_party_revision_index(Changes, St, AuxSt0),
    {Events, AuxSt2} = try_attach_snapshot(Changes, AuxSt1, St),
    {
        Response,
        #{
            events => Events,
            auxst => AuxSt2
        }
    }.

respond_w_exception(Exception) ->
    {{exception, Exception}, #{}}.

append_party_revision_index(Changes, St0, AuxSt) ->
    PartyRevisionIndex0 = get_party_revision_index(AuxSt),
    LastEventID = St0#pm_State.last_event,
    % Brave prediction of next EventID ))
    St1 = merge_party_changes(Changes, St0#pm_State{last_event = LastEventID + 1}),
    PartyRevisionIndex1 = update_party_revision_index(St1, PartyRevisionIndex0),
    set_party_revision_index(PartyRevisionIndex1, AuxSt).

update_party_revision_index(St, PartyRevisionIndex) ->
    #domain_Party{revision = PartyRevision} = get_st_party(St),
    EventID = St#pm_State.last_event,
    {FromEventID, ToEventID} = get_party_revision_range(PartyRevision, PartyRevisionIndex),
    PartyRevisionIndex#{
        PartyRevision => {
            pm_utils:select_defined(FromEventID, EventID),
            max(pm_utils:select_defined(ToEventID, EventID), EventID)
        }
    }.

get_party_revision_index(AuxSt) ->
    maps:get(party_revision_index, AuxSt, #{}).

set_party_revision_index(PartyRevisionIndex, AuxSt) ->
    AuxSt#{party_revision_index => PartyRevisionIndex}.

get_party_revision_range(PartyRevision, PartyRevisionIndex) ->
    maps:get(PartyRevision, PartyRevisionIndex, {undefined, undefined}).

%% TODO crunch func, will be removed after a short (or not so short) time
build_revision_index([Event | History], PartyRevisionIndex0, St0) ->
    St1 = merge_event(Event, St0),
    PartyRevisionIndex1 = update_party_revision_index(St1, PartyRevisionIndex0),
    build_revision_index(History, PartyRevisionIndex1, St1);
build_revision_index([], PartyRevisionIndex, St) ->
    {St, PartyRevisionIndex}.

append_snapshot_index(EventID, AuxSt) ->
    SnapshotIndex = get_snapshot_index(AuxSt),
    set_snapshot_index([EventID | SnapshotIndex], AuxSt).

get_snapshot_index(AuxSt) ->
    maps:get(snapshot_index, AuxSt, []).

set_snapshot_index(SnapshotIndex, AuxSt) ->
    AuxSt#{snapshot_index => SnapshotIndex}.

get_limit(undefined, _) ->
    %% we can't get any reasonable limit in this case
    undefined;
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
    checkout_history_by_timestamp(Events, Timestamp, #pm_State{});
checkout_party(PartyID, {revision, Revision}) ->
    checkout_cached_party_by_revision(PartyID, Revision).

checkout_history_by_timestamp([Ev | Rest], Timestamp, #pm_State{timestamp = PrevTimestamp} = St) ->
    St1 = merge_event(Ev, St),
    EventTimestamp = St1#pm_State.timestamp,
    case pm_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St#pm_State{timestamp = Timestamp}};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history_by_timestamp(Rest, Timestamp, St1)
    end;
checkout_history_by_timestamp([], Timestamp, St) ->
    {ok, St#pm_State{timestamp = Timestamp}}.

checkout_cached_party_by_revision(PartyID, Revision) ->
    case pm_party_cache:get_party(PartyID, Revision) of
        {ok, Party} ->
            _ = logger:info("PartyID: ~p Revision: ~p cache hit", [PartyID, Revision]),
            {ok, Party};
        not_found ->
            case checkout_party_by_revision(PartyID, Revision) of
                {ok, Party} = Res ->
                    _ = logger:info("PartyID: ~p Revision: ~p cache miss", [PartyID, Revision]),
                    ok = pm_party_cache:update_party(PartyID, Revision, Party),
                    Res;
                OtherRes ->
                    OtherRes
            end
    end.

checkout_party_by_revision(PartyID, Revision) ->
    AuxSt = get_aux_state(PartyID),
    FromEventID =
        case get_party_revision_range(Revision, get_party_revision_index(AuxSt)) of
            {_, undefined} ->
                undefined;
            {_, EventID} ->
                EventID + 1
        end,
    Limit = get_limit(FromEventID, get_snapshot_index(AuxSt)),
    ReversedHistory = get_history(PartyID, FromEventID, Limit, backward),
    case parse_history(ReversedHistory) of
        {undefined, Events} ->
            checkout_history_by_revision(Events, Revision, #pm_State{});
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

merge_event({ID, _Dt, {PartyChanges, _}}, #pm_State{last_event = LastEventID} = St) when
    is_list(PartyChanges) andalso ID =:= LastEventID + 1
->
    merge_party_changes(PartyChanges, St#pm_State{last_event = ID}).

merge_party_changes(Changes, St) ->
    lists:foldl(fun merge_party_change/2, St, Changes).

merge_party_change(?party_created(PartyID, ContactInfo, Timestamp), St) ->
    St#pm_State{
        timestamp = Timestamp,
        party = pm_party:create_party(PartyID, ContactInfo, Timestamp)
    };
merge_party_change(?party_blocking(Blocking), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:blocking(Blocking, Party)};
merge_party_change(?revision_changed(Timestamp, Revision), St) ->
    Party = get_st_party(St),
    St#pm_State{
        timestamp = Timestamp,
        party = Party#domain_Party{revision = Revision}
    };
merge_party_change(?party_suspension(Suspension), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:suspension(Suspension, Party)};
merge_party_change(?party_meta_set(NS, Data), #pm_State{meta = Meta} = St) ->
    NewMeta = Meta#{NS => Data},
    St#pm_State{meta = NewMeta};
merge_party_change(?party_meta_removed(NS), #pm_State{meta = Meta} = St) ->
    NewMeta = maps:remove(NS, Meta),
    St#pm_State{meta = NewMeta};
merge_party_change(?shop_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:shop_blocking(ID, Blocking, Party)};
merge_party_change(?shop_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:shop_suspension(ID, Suspension, Party)};
merge_party_change(?wallet_blocking(ID, Blocking), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:wallet_blocking(ID, Blocking, Party)};
merge_party_change(?wallet_suspension(ID, Suspension), St) ->
    Party = get_st_party(St),
    St#pm_State{party = pm_party:wallet_suspension(ID, Suspension, Party)};
merge_party_change(?claim_created(Claim0), St) ->
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_change(?claim_updated(ID, Changeset, Revision, UpdatedAt), St) ->
    Claim0 = pm_claim:update_changeset(Changeset, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    set_claim(Claim, St);
merge_party_change(?claim_status_changed(ID, Status, Revision, UpdatedAt), St) ->
    Claim0 = pm_claim:set_status(Status, Revision, UpdatedAt, get_st_claim(ID, St)),
    Claim = ensure_claim(Claim0),
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1).

block(party, Reason, Timestamp) ->
    ?party_blocking(?blocked(Reason, Timestamp));
block({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?blocked(Reason, Timestamp)).

unblock(party, Reason, Timestamp) ->
    ?party_blocking(?unblocked(Reason, Timestamp));
unblock({shop, ID}, Reason, Timestamp) ->
    ?shop_blocking(ID, ?unblocked(Reason, Timestamp)).

suspend(party, Timestamp) ->
    ?party_suspension(?suspended(Timestamp));
suspend({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?suspended(Timestamp)).

activate(party, Timestamp) ->
    ?party_suspension(?active(Timestamp));
activate({shop, ID}, Timestamp) ->
    ?shop_suspension(ID, ?active(Timestamp)).

assert_party_operable(St) ->
    _ = assert_unblocked(party, St),
    _ = assert_active(party, St).

assert_unblocked(party, St) ->
    assert_blocking(get_st_party(St), unblocked);
assert_unblocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, unblocked).

assert_blocked(party, St) ->
    assert_blocking(get_st_party(St), blocked);
assert_blocked({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_blocking(Party, unblocked),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_blocking(Shop, blocked).

assert_blocking(#domain_Party{blocking = {Status, _}}, Status) ->
    ok;
assert_blocking(#domain_Party{blocking = Blocking}, _) ->
    throw(#payproc_InvalidPartyStatus{status = {blocking, Blocking}}).

assert_active(party, St) ->
    assert_suspension(get_st_party(St), active);
assert_active({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, active).

assert_suspended(party, St) ->
    assert_suspension(get_st_party(St), suspended);
assert_suspended({shop, ID}, St) ->
    Party = get_st_party(St),
    ok = assert_suspension(Party, active),
    Shop = assert_shop_found(pm_party:get_shop(ID, Party)),
    assert_shop_suspension(Shop, suspended).

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
    Revision = pm_domain:head(),
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
    Revision = pm_domain:head(),
    Realm =
        case TemplateRef of
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
    Categories = pm_contract:get_categories(C, Timestamp, Revision),
    {Test, Live} = lists:foldl(
        fun(CategoryRef, {TestFound, LiveFound}) ->
            case pm_domain:get(Revision, {category, CategoryRef}) of
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
    Globals = pm_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    Defaults = Globals#domain_Globals.contract_payment_institution_defaults,
    case Realm of
        test ->
            Defaults#domain_ContractPaymentInstitutionDefaults.test;
        live ->
            Defaults#domain_ContractPaymentInstitutionDefaults.live
    end.

get_template(TemplateRef, Revision) ->
    pm_domain:get(Revision, {contract_template, TemplateRef}).

%%

try_attach_snapshot(Changes, AuxSt0, #pm_State{last_event = LastEventID} = St) when
    LastEventID > 0 andalso
        LastEventID rem ?SNAPSHOT_STEP =:= 0
->
    AuxSt1 = append_snapshot_index(LastEventID + 1, AuxSt0),
    {
        [wrap_event_payload_w_snapshot(Changes, St)],
        wrap_aux_state(AuxSt1)
    };
try_attach_snapshot(Changes, AuxSt, _) ->
    {
        [wrap_event_payload(Changes)],
        wrap_aux_state(AuxSt)
    }.

%% TODO add transmutations for new international legal entities and bank accounts

-define(TOP_VERSION, 7).

% NOTE
% Version of any legacy encoded party state from the point of view of transmutation
% facilities.
-define(PARTY_STATE_ERLBIN_VERSION, 6).

% NOTE
% These pertain to the format of state snapshots in events.
% Event payloads themselves are always thrift-serialized in such events.
-define(FORMAT_VERSION_THRIFT, 2).
-define(FORMAT_VERSION_ERLBIN, 1).

wrap_event_payload(Changes) ->
    marshal_event_payload(?FORMAT_VERSION_THRIFT, Changes, undefined).

wrap_event_payload_w_snapshot(Changes, St) ->
    {FormatVsn, StateSnapshot} = encode_state(St),
    marshal_event_payload(FormatVsn, Changes, StateSnapshot).

marshal_event_payload(FormatVsn, Changes, StateSnapshot) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'PartyEventData'}},
    Bin = pm_proto_utils:serialize(Type, #payproc_PartyEventData{changes = Changes, state_snapshot = StateSnapshot}),
    #{
        format_version => FormatVsn,
        data => {bin, Bin}
    }.

unwrap_events(History) ->
    [unwrap_event(E) || E <- History].

unwrap_event({ID, Dt, Event}) ->
    {ID, Dt, unwrap_event_payload(Event)}.

unwrap_event_payload(#{format_version := Format, data := Data}) ->
    unwrap_event_payload(Format, Data).

unwrap_event_payload(
    FormatVsn,
    {bin, ThriftEncodedBin}
) when is_integer(FormatVsn) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'PartyEventData'}},
    ?party_event_data(Changes, Snapshot) = pm_proto_utils:deserialize(Type, ThriftEncodedBin),
    {Changes, pm_maybe:apply(fun(S) -> {FormatVsn, S} end, Snapshot)};
%% TODO legacy support, will be removed after migration
unwrap_event_payload(
    undefined,
    [Header = #{<<"vsn">> := Version, <<"ct">> := ContentType}, EncodedEvent]
) ->
    Snapshot =
        case maps:get(<<"state_snapshot">>, Header, undefined) of
            undefined -> undefined;
            EncodedSt -> {ctype_to_format_version(ContentType), EncodedSt}
        end,
    {transmute([Version, decode_event(ContentType, EncodedEvent)]), Snapshot};
unwrap_event_payload(undefined, Event) when is_list(Event) ->
    {transmute(pm_party_marshalling:unmarshal(Event)), undefined};
unwrap_event_payload(undefined, {bin, Bin}) when is_binary(Bin) ->
    {transmute([1, binary_to_term(Bin)]), undefined}.

unwrap_state({_ID, _Dt, {_Changes, {FormatVsn, EncodedSt}}}) ->
    decode_state_format(FormatVsn, EncodedSt);
unwrap_state({_ID, _Dt, {_Changes, undefined}}) ->
    undefined.

-define(STATE_THRIFT_TYPE, {struct, struct, {dmsl_party_state_thrift, 'State'}}).

encode_state(St) ->
    {?FORMAT_VERSION_THRIFT, {bin, pm_proto_utils:serialize(?STATE_THRIFT_TYPE, St)}}.

decode_state_format(?FORMAT_VERSION_THRIFT, {bin, EncodedSt}) ->
    pm_proto_utils:deserialize(?STATE_THRIFT_TYPE, EncodedSt);
decode_state_format(?FORMAT_VERSION_ERLBIN, {bin, EncodedSt}) ->
    transmute_state(validate_state(binary_to_term(EncodedSt))).

decode_event(?CT_ERLANG_BINARY, {bin, EncodedEvent}) ->
    binary_to_term(EncodedEvent).

%% NOTE
%% Just to be sure this field was never used.
validate_state(St = ?legacy_st(_, _, _, _, MigrationData, _)) when map_size(MigrationData) == 0 ->
    St.

ctype_to_format_version(?CT_ERLANG_BINARY) ->
    ?FORMAT_VERSION_ERLBIN.

-spec wrap_aux_state(party_aux_st()) -> pm_msgpack_marshalling:msgpack_value().
wrap_aux_state(AuxSt) ->
    ContentType = ?CT_ERLANG_BINARY,
    #{<<"ct">> => ContentType, <<"aux_state">> => encode_aux_state(ContentType, AuxSt)}.

-spec unwrap_aux_state(pm_msgpack_marshalling:msgpack_value()) -> party_aux_st().
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
    ?party_ev(Changes) = transmute_event(Version, ?TOP_VERSION, Event),
    Changes.

transmute_event(V1, V2, ?party_ev(Changes)) when V2 > V1 ->
    NewChanges = [transmute_change(V1, V1 + 1, C) || C <- Changes],
    transmute_event(V1 + 1, V2, ?party_ev(NewChanges));
transmute_event(V, V, Event) ->
    Event.

transmute_state(St) ->
    transmute_state(?PARTY_STATE_ERLBIN_VERSION, ?TOP_VERSION, St).

-spec transmute_change(pos_integer(), pos_integer(), term()) -> dmsl_payment_processing_thrift:'PartyChange'().
transmute_change(
    1,
    2,
    ?legacy_party_created(?legacy_party(ID, ContactInfo, CreatedAt, _, _, _, _))
) ->
    ?party_created(ID, ContactInfo, CreatedAt);
transmute_change(
    V1,
    V2,
    ?claim_created(
        ?legacy_claim(
            ID,
            Status,
            Changeset,
            Revision,
            CreatedAt,
            UpdatedAt
        )
    )
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    NewChangeset = [transmute_party_modification(V1, V2, M) || M <- Changeset],
    ?claim_created(#payproc_Claim{
        id = ID,
        status = Status,
        changeset = NewChangeset,
        revision = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    });
transmute_change(
    V1,
    V2,
    ?legacy_claim_updated(ID, Changeset, ClaimRevision, Timestamp)
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    NewChangeset = [transmute_party_modification(V1, V2, M) || M <- Changeset],
    ?claim_updated(ID, NewChangeset, ClaimRevision, Timestamp);
transmute_change(
    V1,
    V2,
    ?claim_status_changed(ID, ?accepted(Effects), ClaimRevision, Timestamp)
) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    NewEffects = [transmute_claim_effect(V1, V2, E) || E <- Effects],
    ?claim_status_changed(ID, ?accepted(NewEffects), ClaimRevision, Timestamp);
transmute_change(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    C.

-spec transmute_state(pos_integer(), pos_integer(), _LegacyState) -> st().
transmute_state(V1, V2, ?legacy_st(Party, Timestamp, Claims, Meta, _, LastEventID)) ->
    #pm_State{
        party = transmute_party(V1, V2, Party),
        timestamp = Timestamp,
        claims = maps:map(fun(_, C) -> transmute_claim(V1, V2, C) end, Claims),
        meta = Meta,
        last_event = LastEventID
    }.

transmute_claim(V1, V2, Claim = #payproc_Claim{changeset = Changeset}) ->
    transmute_claim_status(V1, V2, Claim#payproc_Claim{
        changeset = [transmute_party_modification(V1, V2, M) || M <- Changeset]
    }).

transmute_claim_status(V1, V2, Claim = #payproc_Claim{status = ?accepted(Effects = [_ | _])}) ->
    Claim#payproc_Claim{
        status = ?accepted([transmute_claim_effect(V1, V2, E) || E <- Effects])
    };
transmute_claim_status(_V1, _V2, Claim) ->
    Claim.

transmute_party(
    V1,
    V2,
    Party = #domain_Party{
        contractors = Contractors,
        contracts = Contracts
    }
) ->
    Party#domain_Party{
        contractors = maps:map(fun(_, C) -> transmute_party_contractor(V1, V2, C) end, Contractors),
        contracts = maps:map(fun(_, C) -> transmute_contract(V1, V2, C) end, Contracts)
    };
transmute_party(_, _, undefined) ->
    undefined.

transmute_party_contractor(V1, V2, PartyContractor = #domain_PartyContractor{contractor = Contractor}) ->
    PartyContractor#domain_PartyContractor{contractor = transmute_contractor(V1, V2, Contractor)}.

transmute_contract(V1, V2, Contract = #domain_Contract{contractor = Contractor}) ->
    Contract#domain_Contract{contractor = transmute_contractor(V1, V2, Contractor)}.

transmute_party_modification(
    1,
    2,
    ?legacy_contract_modification(ID, {creation, ?legacy_contract_params_v1(Contractor, TemplateRef)})
) ->
    ?legacy_contract_modification(
        ID,
        {creation,
            ?legacy_contract_params_v2(
                transmute_contractor(1, 2, Contractor),
                TemplateRef,
                undefined
            )}
    );
transmute_party_modification(
    2,
    3,
    ?legacy_contract_modification(
        ID,
        {creation,
            ?legacy_contract_params_v2(
                Contractor,
                TemplateRef,
                PaymentInstitutionRef
            )}
    )
) ->
    ?legacy_contract_modification(
        ID,
        {creation,
            ?legacy_contract_params_v3_4(
                transmute_contractor(2, 3, Contractor),
                TemplateRef,
                PaymentInstitutionRef
            )}
    );
transmute_party_modification(
    4,
    5,
    ?legacy_contract_modification(
        ID,
        {creation,
            ?legacy_contract_params_v3_4(
                Contractor,
                TemplateRef,
                PaymentInstitutionRef
            )}
    )
) ->
    ?contract_modification(
        ID,
        {creation, #payproc_ContractParams{
            contractor = Contractor,
            template = TemplateRef,
            payment_institution = PaymentInstitutionRef
        }}
    );
transmute_party_modification(
    6 = V1,
    7 = V2,
    ?legacy_contract_modification(
        ID,
        {creation,
            ContractParams = #payproc_ContractParams{
                contractor = Contractor
            }}
    )
) ->
    ?contract_modification(
        ID,
        {creation, ContractParams#payproc_ContractParams{
            contractor = transmute_contractor(V1, V2, Contractor)
        }}
    );
transmute_party_modification(
    6 = V1,
    7 = V2,
    ?contractor_modification(
        ID,
        {creation, Contractor}
    )
) ->
    ?contractor_modification(
        ID,
        {creation, transmute_contractor(V1, V2, Contractor)}
    );
transmute_party_modification(
    V1,
    V2,
    ?legacy_contract_modification(
        ContractID,
        ?legacy_payout_tool_creation(
            ID,
            ?legacy_payout_tool_params(Currency, ToolInfo)
        )
    )
) when V1 =:= 1; V1 =:= 2; V1 =:= 5 ->
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = transmute_payout_tool_info(V1, V2, ToolInfo)
    },
    ?contract_modification(ContractID, ?payout_tool_creation(ID, PayoutToolParams));
transmute_party_modification(
    3,
    4,
    ?legacy_contract_modification(
        ID,
        {legal_agreement_binding, LegalAgreement}
    )
) ->
    ?contract_modification(ID, {legal_agreement_binding, transmute_legal_agreement(3, 4, LegalAgreement)});
transmute_party_modification(
    3,
    4,
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
transmute_party_modification(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    C.

transmute_claim_effect(
    1,
    2,
    ?legacy_contract_effect(
        ID,
        {created,
            ?legacy_contract_v1(
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
    )
) ->
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
transmute_claim_effect(
    2,
    3,
    ?legacy_contract_effect(
        ID,
        {created,
            ?legacy_contract_v2_3(
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
    )
) ->
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
transmute_claim_effect(
    3,
    4,
    ?legacy_contract_effect(
        ID,
        {created,
            ?legacy_contract_v2_3(
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
    )
) ->
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
transmute_claim_effect(
    4,
    5,
    ?legacy_contract_effect(
        ID,
        {created,
            ?legacy_contract_v4(
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
    )
) ->
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
transmute_claim_effect(
    5,
    6,
    ?contract_effect(
        ID,
        {created, Contract = #domain_Contract{payout_tools = PayoutTools}}
    )
) ->
    ?contract_effect(
        ID,
        {created, Contract#domain_Contract{
            payout_tools = [transmute_payout_tool(5, 6, P) || P <- PayoutTools]
        }}
    );
transmute_claim_effect(
    6 = V1,
    7 = V2,
    ?contract_effect(
        ID,
        {created, Contract = #domain_Contract{contractor = Contractor}}
    )
) ->
    ?contract_effect(
        ID,
        {created, Contract#domain_Contract{
            contractor = transmute_contractor(V1, V2, Contractor)
        }}
    );
transmute_claim_effect(
    6 = V1,
    7 = V2,
    ?contractor_effect(
        ID,
        {created, PartyContractor}
    )
) ->
    ?contractor_effect(
        ID,
        {created, transmute_party_contractor(V1, V2, PartyContractor)}
    );
transmute_claim_effect(
    V1,
    V2,
    ?legacy_contract_effect(
        ContractID,
        {payout_tool_created, PayoutTool}
    )
) when V1 =:= 1; V1 =:= 2; V1 =:= 5 ->
    ?contract_effect(
        ContractID,
        {payout_tool_created, transmute_payout_tool(V1, V2, PayoutTool)}
    );
transmute_claim_effect(
    3,
    4,
    ?legacy_contract_effect(
        ContractID,
        {legal_agreement_bound, LegalAgreement}
    )
) ->
    ?contract_effect(ContractID, {legal_agreement_bound, transmute_legal_agreement(3, 4, LegalAgreement)});
transmute_claim_effect(
    2,
    3,
    ?legacy_shop_effect(
        ID,
        {created,
            ?legacy_shop_v2(
                ID,
                CreatedAt,
                Blocking,
                Suspension,
                Details,
                Location,
                Category,
                Account,
                ContractID,
                PayoutToolID
            )}
    )
) ->
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
transmute_claim_effect(
    3,
    4,
    ?legacy_shop_effect(
        ID,
        {created,
            ?legacy_shop_v3(
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
    )
) ->
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
transmute_claim_effect(
    3,
    4,
    ?legacy_shop_effect(
        ID,
        {payout_schedule_changed, ?legacy_schedule_changed(PayoutSchedule)}
    )
) ->
    ?shop_effect(
        ID,
        {payout_schedule_changed, #payproc_ScheduleChanged{
            schedule = transmute_payout_schedule_ref(3, 4, PayoutSchedule)
        }}
    );
transmute_claim_effect(V1, _, C) when V1 =:= 1; V1 =:= 2; V1 =:= 3; V1 =:= 4; V1 =:= 5; V1 =:= 6 ->
    C.

transmute_contractor(
    1,
    2,
    {legal_entity,
        {russian_legal_entity,
            ?legacy_russian_legal_entity(
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
    {legal_entity,
        {russian_legal_entity, #domain_RussianLegalEntity{
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
transmute_contractor(
    2,
    3,
    {legal_entity,
        {international_legal_entity,
            ?legacy_international_legal_entity(
                LegalName,
                TradingName,
                RegisteredAddress,
                ActualAddress
            )}}
) ->
    {legal_entity,
        {international_legal_entity,
            ?legacy_international_legal_entity_v2(
                LegalName,
                TradingName,
                RegisteredAddress,
                ActualAddress,
                undefined
            )}};
transmute_contractor(
    6,
    7,
    {legal_entity,
        {international_legal_entity,
            ?legacy_international_legal_entity_v2(
                LegalName,
                TradingName,
                RegisteredAddress,
                ActualAddress,
                RegisteredNumber
            )}}
) ->
    {legal_entity,
        {international_legal_entity, #domain_InternationalLegalEntity{
            legal_name = LegalName,
            trading_name = TradingName,
            registered_address = RegisteredAddress,
            actual_address = ActualAddress,
            registered_number = RegisteredNumber
        }}};
transmute_contractor(V1, _, Contractor) when V1 =:= 1; V1 =:= 2; V1 =:= 6 ->
    Contractor.

transmute_payout_tool(
    V1,
    V2,
    ?legacy_payout_tool(
        ID,
        CreatedAt,
        Currency,
        ToolInfo
    )
) when V1 =:= 1; V1 =:= 2 ->
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
transmute_payout_tool_info(
    2,
    3,
    {international_bank_account,
        ?legacy_international_bank_account(
            AccountHolder,
            BankName,
            BankAddress,
            Iban,
            Bic
        )}
) ->
    {international_bank_account,
        ?legacy_international_bank_account_v3_4_5(
            AccountHolder,
            BankName,
            BankAddress,
            Iban,
            Bic,
            undefined
        )};
transmute_payout_tool_info(
    5,
    6,
    {international_bank_account,
        ?legacy_international_bank_account_v3_4_5(
            AccountHolder,
            BankName,
            BankAddress,
            Iban,
            Bic,
            _LocalBankCode
        )}
) ->
    {international_bank_account, #domain_InternationalBankAccount{
        bank = #domain_InternationalBankDetails{
            bic = Bic,
            name = BankName,
            address = BankAddress
        },
        iban = Iban,
        account_holder = AccountHolder
    }};
transmute_payout_tool_info(V1, _, ToolInfo) when V1 =:= 1; V1 =:= 2; V1 =:= 5 ->
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
        signed_at = SignedAt,
        legal_agreement_id = LegalAgreementID
    };
transmute_legal_agreement(3, 4, undefined) ->
    undefined.

transmute_payout_schedule_ref(3, 4, ?legacy_payout_schedule_ref(ID)) ->
    #domain_BusinessScheduleRef{id = ID};
transmute_payout_schedule_ref(3, 4, undefined) ->
    undefined.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% NOTE
%% Adapted from:
%% ```
%% -record(st, {
%%     party :: undefined | party(),
%%     timestamp :: undefined | timestamp(),
%%     claims = #{} :: #{claim_id() => claim()},
%%     meta = #{} :: meta(),
%%     migration_data = #{} :: #{},
%%     last_event = 0 :: event_id()
%% }).
%% ```
-define(INITIAL_LEGACY_ST, ?legacy_st(undefined, undefined, #{}, #{}, #{}, 0)).

-spec test() -> _.

-spec encode_decode_success_test_() -> _.
encode_decode_success_test_() ->
    ?_assertEqual(
        #pm_State{},
        begin
            decode_state_format(?FORMAT_VERSION_ERLBIN, {bin, term_to_binary(?INITIAL_LEGACY_ST)})
        end
    ).

-endif.
