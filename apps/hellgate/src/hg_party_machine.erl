-module(hg_party_machine).

-include("party_events.hrl").
-include("legacy_party_structures.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).
-export([init/2]).
-export([process_signal/3]).
-export([process_call/3]).

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

%%

-define(NS, <<"party">>).

-record(st, {
    party                :: undefined | party(),
    timestamp            :: undefined | timestamp(),
    revision             :: hg_domain:revision(),
    claims   = #{}       :: #{claim_id() => claim()},
    meta = #{}           :: meta(),
    migration_data = #{} :: #{any() => any()}
}).

-type st() :: #st{}.

-type call() ::
    {block, binary()}                                           |
    {unblock, binary()}                                         |
    suspend                                                     |
    activate                                                    |
    {set_metadata, meta_ns(), meta_data()}                      |
    {remove_metadata, meta_ns()}                                |
    {block_shop, shop_id(), binary()}                           |
    {unblock_shop, shop_id(), binary()}                         |
    {suspend_shop, shop_id()}                                   |
    {activate_shop, shop_id()}                                  |
    {create_claim, changeset()}                                 |
    {update_claim, claim_id(), claim_revision(), changeset()}   |
    {accept_claim, claim_id(), claim_revision()}                |
    {deny_claim, claim_id(), claim_revision(), binary()}        |
    {revoke_claim, claim_id(), claim_revision(), binary()}.

-type party()           :: dmsl_domain_thrift:'Party'().
-type party_id()        :: dmsl_domain_thrift:'PartyID'().
-type shop_id()         :: dmsl_domain_thrift:'ShopID'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().
-type timestamp()       :: dmsl_base_thrift:'Timestamp'().
-type meta()            :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns()         :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data()       :: dmsl_domain_thrift:'PartyMetaData'().
-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(party_id(), dmsl_payment_processing_thrift:'PartyParams'()) ->
    hg_machine:result().

init(ID, PartyParams) ->
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
    ok([?party_created(PartyID, ContactInfo, Timestamp), ?revision_changed(Timestamp, 0)]).

-spec process_signal(hg_machine:signal(), hg_machine:history(), hg_machine:auxst()) ->
    hg_machine:result().

process_signal(timeout, _History, _AuxSt) ->
    #{};

process_signal({repair, _}, _History, _AuxSt) ->
    #{}.

-spec process_call(call(), hg_machine:history(), hg_machine:auxst()) ->
    {hg_machine:response(), hg_machine:result()}.

process_call(Call, History, _AuxSt) ->
    St = collapse_history(unwrap_events(History)),
    try
        Party = get_st_party(St),
        scoper:scope(
            party,
            #{
                id => Party#domain_Party.id,
                activity => get_call_name(Call)
            },
            fun() -> handle_call(Call, {St, []}) end
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
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?party_blocking(?blocked(Reason, Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call({unblock, Reason}, {St, _}) ->
    ok = assert_blocked(St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?party_blocking(?unblocked(Reason, Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call(suspend, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_active(St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?party_suspension(?suspended(Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call(activate, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_suspended(St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?party_suspension(?active(Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call({set_metadata, NS, Data}, _) ->
    respond(ok, [?party_meta_set(NS, Data)]);

handle_call({remove_metadata, NS}, {St, _}) ->
    _ = get_st_metadata(NS, St),
    respond(ok, [?party_meta_removed(NS)]);

handle_call({block_shop, ID, Reason}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?shop_blocking(ID, ?blocked(Reason, Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call({unblock_shop, ID, Reason}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_blocked(ID, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?shop_blocking(ID, ?unblocked(Reason, Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call({suspend_shop, ID}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    ok = assert_shop_active(ID, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?shop_suspension(ID, ?suspended(Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

handle_call({activate_shop, ID}, {St, _}) ->
    ok = assert_unblocked(St),
    ok = assert_shop_unblocked(ID, St),
    ok = assert_shop_suspended(ID, St),
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    respond(ok, [
        ?shop_suspension(ID, ?active(Timestamp)),
        ?revision_changed(Timestamp, Revision)
    ]);

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
    Timestamp = hg_datetime:format_now(),
    Revision = get_next_party_revision(St),
    Claim = hg_claim:accept(
        Timestamp,
        get_st_revision(St),
        get_st_party(St),
        get_st_claim(ID, St)
    ),
    respond(ok, [finalize_claim(Claim, Timestamp), ?revision_changed(Timestamp, Revision)]);

handle_call({deny_claim, ID, ClaimRevision, Reason}, {St, _}) ->
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:deny(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(ok, [finalize_claim(Claim, get_st_timestamp(St))]);

handle_call({revoke_claim, ID, ClaimRevision, Reason}, {St, _}) ->
    ok = assert_operable(St),
    ok = assert_claim_modification_allowed(ID, ClaimRevision, St),
    Claim = hg_claim:revoke(Reason, get_st_timestamp(St), get_st_claim(ID, St)),
    respond(ok, [finalize_claim(Claim, get_st_timestamp(St))]).

publish_party_event(Source, {ID, Dt, Ev = ?party_ev(_)}) ->
    #payproc_Event{id = ID, source = Source, created_at = Dt, payload = Ev}.

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

-spec publish_event(party_id(), msgpack_value()) ->
    hg_event_provider:public_event().

publish_event(PartyID, Ev) ->
    {{party_id, PartyID}, unwrap_event(Ev)}.

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

-spec checkout(party_id(), party_revision_param()) ->
    dmsl_domain_thrift:'Party'() | no_return().

checkout(PartyID, RevisionParam) ->
    case checkout_history(get_history(PartyID), RevisionParam) of
        {ok, St} ->
            get_st_party(St);
        {error, Reason} ->
            error(Reason)
    end.

-spec call(party_id(), call()) ->
    term() | no_return().

call(PartyID, Call) ->
    map_error(hg_machine:call(?NS, PartyID, Call)).

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
    [publish_party_event({party_id, PartyID}, Ev) || Ev <- get_history(PartyID, AfterID, Limit)].

get_state(PartyID) ->
    collapse_history(assert_nonempty_history(get_history(PartyID))).

get_history(PartyID) ->
    map_history_error(hg_machine:get_history(?NS, PartyID)).

get_history(PartyID, AfterID, Limit) ->
    map_history_error(hg_machine:get_history(?NS, PartyID, AfterID, Limit)).

map_history_error({ok, Result}) ->
    unwrap_events(Result);
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

%% TODO remove this hack as soon as machinegun learns to tell the difference between
%%      nonexsitent machine and empty history
assert_nonempty_history([_ | _] = Result) ->
    Result;
assert_nonempty_history([]) ->
    throw(#payproc_PartyNotFound{}).

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

ok(Changes) ->
    #{events => wrap_events([?party_ev(Changes)])}.

respond(Response, Changes) ->
    {{ok, Response}, #{events => wrap_events([?party_ev(Changes)])}}.

respond_w_exception(Exception) ->
    {{exception, Exception}, #{}}.

%%

-spec collapse_history(hg_machine:history()) -> st().

collapse_history(History) ->
    {ok, St} = checkout_history(History, {timestamp, hg_datetime:format_now()}),
    St.

-spec checkout_history(hg_machine:history(), party_revision_param()) -> {ok, st()} | {error, revision_not_found}.

checkout_history(History, {timestamp, Timestamp}) ->
    % FIXME hg_domain:head() looks strange here
    checkout_history_by_timestamp(History, Timestamp, #st{revision = hg_domain:head()});
checkout_history(History, {revision, Revision}) ->
    checkout_history_by_revision(History, Revision, #st{revision = hg_domain:head()}).

checkout_history_by_timestamp([{_, _, Ev} | Rest], Timestamp, #st{timestamp = PrevTimestamp} = St) ->
    St1 = merge_event(Ev, St),
    EventTimestamp = St1#st.timestamp,
    case hg_datetime:compare(EventTimestamp, Timestamp) of
        later when PrevTimestamp =/= undefined ->
            {ok, St};
        later when PrevTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            checkout_history_by_timestamp(Rest, Timestamp, St1)
    end;
checkout_history_by_timestamp([], _, St) ->
    {ok, St}.

checkout_history_by_revision([{_, _, Ev} | Rest], Revision, St) ->
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

merge_event(?party_ev(PartyChanges), St) when is_list(PartyChanges) ->
     lists:foldl(fun merge_party_change/2, St, PartyChanges).

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
    Categories = hg_party:get_categories(C, Timestamp, Revision),
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

-define(TOP_VERSION, 2).

wrap_events(Events) ->
    [hg_party_marshalling:marshal([?TOP_VERSION, E]) || E <- Events].

unwrap_events(History) ->
    lists:map(
        fun({ID, Dt, Event}) ->
            {ID, Dt, unwrap_event(Event)}
        end,
        History
    ).

unwrap_event(Event) when is_list(Event) ->
    transmute(hg_party_marshalling:unmarshal(Event));
unwrap_event({bin, Bin}) when is_binary(Bin) ->
    transmute([1, binary_to_term(Bin)]).

transmute([Version, Event]) ->
    transmute_event(Version, ?TOP_VERSION, Event).

transmute_event(V1, V2, ?party_ev(Changes)) when V2 > V1->
    NewChanges = [transmute_change(V1, V1 + 1, C) || C <- Changes],
    transmute_event(V1 + 1, V2, ?party_ev(NewChanges));
transmute_event(V, V, Event) ->
    Event.

transmute_change(1, 2,
    ?legacy_party_created(?legacy_party(ID, ContactInfo, CreatedAt, _, _, _, _))
) ->
    ?party_created(ID, ContactInfo, CreatedAt);
transmute_change(1, 2,
    ?claim_created(?legacy_claim(
        ID,
        Status,
        Changeset,
        Revision,
        CreatedAt,
        UpdatedAt
    ))
) ->
    NewChangeset = [transmute_party_modification(1, 2, M) || M <- Changeset],
    ?claim_created(#payproc_Claim{
        id = ID,
        status = Status,
        changeset = NewChangeset,
        revision = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    });
transmute_change(1, 2,
    ?legacy_claim_updated(ID, Changeset, ClaimRevision, Timestamp)
) ->
    NewChangeset = [transmute_party_modification(1, 2, M) || M <- Changeset],
    ?claim_updated(ID, NewChangeset, ClaimRevision, Timestamp);
transmute_change(1, 2,
    ?claim_status_changed(ID, ?accepted(Effects), ClaimRevision, Timestamp)
) ->
    NewEffects = [transmute_claim_effect(1, 2, E) || E <- Effects],
    ?claim_status_changed(ID, ?accepted(NewEffects), ClaimRevision, Timestamp);
transmute_change(1, 2, C) ->
    C.

transmute_party_modification(1, 2,
    ?legacy_contract_modification(ID, {creation, ?legacy_contract_params(Contractor, TemplateRef)})
) ->
    ?contract_modification(ID, {creation, #payproc_ContractParams{
        contractor = transmute_contractor(1, 2, Contractor),
        template = TemplateRef
    }});
transmute_party_modification(1, 2,
    ?legacy_contract_modification(ContractID, ?legacy_payout_tool_creation(
        ID,
        ?legacy_payout_tool_params(Currency, {bank_account, BankAccount})
    ))
) ->
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = Currency,
        tool_info = {russian_bank_account, transmute_bank_account(1, 2, BankAccount)}
    },
    ?contract_modification(ContractID, ?payout_tool_creation(ID, PayoutToolParams));
transmute_party_modification(1, 2, C) ->
    C.

transmute_claim_effect(1, 2, ?legacy_contract_effect(
    ID,
    {created, ?legacy_contract(
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
    Contract = #domain_Contract{
        id = ID,
        contractor = transmute_contractor(1, 2, Contractor),
        created_at = CreatedAt,
        valid_since = ValidSince,
        valid_until = ValidUntil,
        status = Status,
        terms = Terms,
        adjustments = Adjustments,
        payout_tools = [transmute_payout_tool(1, 2, P) || P <- PayoutTools],
        legal_agreement = LegalAgreement
    },
    ?contract_effect(ID, {created, Contract});
transmute_claim_effect(1, 2, ?legacy_contract_effect(
    ContractID,
    {payout_tool_created, PayoutTool}
)) ->
    ?contract_effect(
        ContractID,
        {payout_tool_created, transmute_payout_tool(1, 2, PayoutTool)}
    );
transmute_claim_effect(1, 2, C) ->
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
transmute_contractor(1, 2, Contractor) ->
    Contractor.

transmute_payout_tool(1, 2, ?legacy_payout_tool(
    ID,
    CreatedAt,
    Currency,
    {bank_account, BankAccount}
)) ->
    #domain_PayoutTool{
        id = ID,
        created_at = CreatedAt,
        currency = Currency,
        payout_tool_info = {russian_bank_account, transmute_bank_account(1, 2, BankAccount)}
    };
transmute_payout_tool(1, 2, PayoutTool) ->
    PayoutTool.

transmute_bank_account(1, 2, ?legacy_bank_account(Account, BankName, BankPostAccount, BankBik)) ->
    #domain_RussianBankAccount{
        account = Account,
        bank_name = BankName,
        bank_post_account = BankPostAccount,
        bank_bik = BankBik
    }.
