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

-export([get_party/1]).
-export([get_claim/2]).
-export([get_pending_claim/1]).
-export([get_public_history/3]).

%%

-define(NS, <<"party">>).

-record(st, {
    party          :: party(),
    timestamp      :: timestamp(),
    claims   = #{} :: #{claim_id() => claim()},
    sequence = 0   :: 0 | sequence()
}).

-type st() :: #st{}.

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

-type public_event() :: dmsl_payment_processing_thrift:'EventPayload'().
-type private_event() :: none().

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
    {ok, {St, Events}} = create_party(ID, PartyParams, {#st{}, []}),
    Revision = hg_domain:head(),
    TestContractTemplate = get_test_template_ref(Revision),
    TestContract = hg_party:create_contract(#payproc_ContractParams{template = TestContractTemplate}, get_st_party(St)),
    Changeset1 = [?contract_creation(TestContract)],

    ShopParams = get_shop_prototype_params(TestContract#domain_Contract.id, Revision),
    Shop = hg_party:create_shop(ShopParams, ?active(), get_st_party(St)),
    Changeset2 = [?shop_creation(Shop)],

    Currencies = hg_party:get_contract_currencies(TestContract, hg_datetime:format_now(), Revision),
    CurrencyRef = erlang:hd(ordsets:to_list(Currencies)),
    Changeset3 = [?shop_modification(Shop#domain_Shop.id, ?account_created(create_shop_account(CurrencyRef)))],

    Changeset = Changeset1 ++ Changeset2 ++ Changeset3,
    {_ClaimID, StEvents1} = submit_accept_claim(Changeset, {St, Events}),
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
    ok = assert_unblocked(StEvents0),
    Changeset = create_contract(ContractParams, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({bind_contract_legal_agreemnet, ID, #domain_LegalAgreement{} = LegalAgreement}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Contract = hg_party:get_contract(ID, get_st_pending_party(St)),
    ok = hg_party:assert_contract_active(Contract),
    {ClaimID, StEvents1} = create_claim([?contract_legal_agreement_binding(ID, LegalAgreement)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({terminate_contract, ContractID, Reason}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Contract = hg_party:get_contract(ContractID, get_st_party(St)),
    ok = hg_party:assert_contract_active(Contract),
    TerminatedAt = hg_datetime:format_now(),
    {ClaimID, StEvents1} = create_claim([?contract_termination(ContractID, TerminatedAt, Reason)], StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_contract_adjustment, ContractID, Params}, StEvents0) ->
    ok = assert_unblocked(StEvents0),
    Changeset = create_contract_adjustment(ContractID, Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_payout_tool, ContractID, Params}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Changeset = create_payout_tool(ContractID, Params, StEvents0),
    {ClaimID, StEvents1} = create_claim(Changeset, StEvents0),
    respond(get_claim_result(ClaimID, StEvents1), StEvents1);

handle_call({create_shop, Params}, StEvents0 = {St, _}) ->
    ok = assert_unblocked(StEvents0),
    Revision = hg_domain:head(),
    Timestamp = get_st_timestamp(St),
    Party = get_st_pending_party(St),

    Shop = hg_party:create_shop(Params, Party),
    Changeset0 = [?shop_creation(Shop)],
    Contract = get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_shop_contract_valid(Shop, Contract, Revision),
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

publish_party_event(Source, {ID, Dt, {Seq, Payload = get_st_party_ev(_)}}) ->
    {true, #payproc_Event{id = ID, source = Source, created_at = Dt, sequence = Seq, payload = Payload}};
publish_party_event(_Source, {_ID, _Dt, _Event}) ->
    false.

-spec publish_event(party_id(), hg_machine:event(ev())) ->
    {true, hg_event_provider:public_event()} | false.

publish_event(PartyID, {Seq, Ev = get_st_party_ev(_)}) ->
    {true, {{party, PartyID}, Seq, Ev}};
publish_event(_InvoiceID, _) ->
    false.

%%

get_party(PartyID) ->
    get_st_party(get_state(PartyID)).

-spec checkout(party_id(), timestamp()) ->
    dmsl_domain_thrift:'Party'().

checkout(PartyID, Timestamp) ->
    {History, _LastID} = get_history(PartyID),
    case checkout_history(History, Timestamp) of
        {ok, St} ->
            get_st_party(St);
        {error, Reason} ->
            error(Reason)
    end.

get_claim(ID, PartyID) ->
    get_st_claim(ID, get_state(PartyID)).

get_pending_claim(PartyID) ->
    get_st_pending_claim(get_state(PartyID)).

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

get_st_pending_party(St0) ->
    St = case get_st_pending_claim(St0) of
        undefined ->
            St0;
        Claim ->
            apply_claim(Claim, St0)
    end,
    get_st_party(St).

get_st_claim(ID, #st{claims = Claims}) ->
    assert_claim_exists(maps:get(ID, Claims, undefined)).

get_st_pending_claim(#st{claims = Claims})->
    % TODO cache it during history collapse
    maps:fold(
        fun
            (_ID, #payproc_Claim{status = ?pending()} = Claim, undefined) -> Claim;
            (_ID, #payproc_Claim{status = _Another}, Claim)               -> Claim
        end,
        undefined,
        Claims
    ).

get_st_timestamp(#st{timestamp = Timestamp}) ->
    Timestamp.

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

apply_accepted_claim(Claim = #payproc_Claim{status = ?accepted(_AcceptedAt)}, St) ->
    apply_claim(Claim, St);
apply_accepted_claim(_Claim, St) ->
    St.

apply_claim(#payproc_Claim{changeset = Changeset}, St) ->
    apply_changeset(Changeset, St).

apply_changeset(Changeset, St) ->
    St#st{party = lists:foldl(fun hg_party:apply_change/2, get_st_party(St), Changeset)}.

%%

create_claim(Changeset, StEvents = {St, _}) ->
    ClaimPending = get_st_pending_claim(St),
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
    Event = get_st_party_ev(?claim_created(Claim)),
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
    Event = get_st_party_ev(?claim_status_changed(ID, Status)),
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

create_party(PartyID, PartyParams, StEvents) ->
    Party = hg_party:new(PartyID, PartyParams),
    Event = get_st_party_ev(get_st_party_created(Party)),
    {ok, apply_state_event(Event, StEvents)}.

create_contract(ContractParams0, {St, _}) ->
    Party = get_st_pending_party(St),
    ContractParams = hg_party:ensure_contract_creation_params(ContractParams0),
    Contract = hg_party:create_contract(ContractParams),
    Changeset1 = [?contract_creation(Contract)],
    #payproc_ContractParams{payout_tool_params = PayoutToolParams} = ContractParams,
    PayoutTool = hg_party:create_payout_tool(PayoutToolParams, Contract),
    Changeset2 = [?contract_payout_tool_creation(hg_party:get_contract_id(Contract), PayoutTool)]
    Changeset1 ++ Changeset2.

create_contract_adjustment(ContractID, Params0, {St, _}) ->
    Contract = hg_party:get_contract(ContractID, get_st_pending_party(St)),
    ok = hg_party:assert_contract_active(Contract),
    Params = hg_party:ensure_adjustment_creation_params(Params0),
    Adjustment = hg_party:create_contract_adjustment(Params, Contract),
    [?contract_adjustment_creation(ContractID, Adjustment)].

create_payout_tool(ContractID, Params, {St, _}) ->
    Contract = hg_party:get_contract(ContractID, get_st_pending_party(St)),
    ok = hg_party:assert_contract_active(Contract),
    _ = not hg_party:is_test_contract(Contract, get_st_timestamp(St), hg_domain:head()) orelse
        raise_invalid_request(<<"creating payout tool for test contract unavailable">>),
    PayoutTool = hg_party:create_payout_tool(Params, Contract),
    [?contract_payout_tool_creation(ContractID, PayoutTool)].

%%

apply_state_event(EventData, {St0, EventsAcc}) ->
    Event = construct_event(EventData, St0),
    {merge_history(Event, St0), EventsAcc ++ [Event]}.

construct_event(EventData = get_st_party_ev(_), #st{sequence = Seq}) ->
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
    case checkout_history(History, Timestamp, #st{}) of
        {ok, St} ->
            apply_checkout_history_hook(St, Timestamp);
        {error, _} = Error ->
            Error
    end.

checkout_history([{_ID, EventTimestamp, Ev} | Rest], Timestamp, St0 = #st{timestamp = StTimestamp}) ->
    case hg_datetime:compare(EventTimestamp, Timestamp) of
        later when StTimestamp =/= undefined ->
            {ok, St0};
        later when StTimestamp == undefined ->
            {error, revision_not_found};
        _ ->
            St1 = merge_history(Ev, St0#st{timestamp = EventTimestamp}),
            checkout_history(Rest, Timestamp, St1)
    end;
checkout_history([], _, St) ->
    {ok, St}.

merge_history({Seq, Event}, St) ->
    merge_event(Event, St#st{sequence = Seq}).

merge_event(get_st_party_ev(Ev), St) ->
    merge_party_event(Ev, St).

merge_party_event(get_st_party_created(Party), St) ->
    St#st{party = Party};
merge_party_event(?claim_created(Claim), St) ->
    St1 = set_claim(Claim, St),
    apply_accepted_claim(Claim, St1);
merge_party_event(?claim_status_changed(ID, Status), St) ->
    Claim = get_claim(ID, St),
    Claim1 = Claim#payproc_Claim{status = Status},
    St1 = set_claim(Claim1, St),
    apply_accepted_claim(Claim1, St1).

apply_checkout_history_hook(St, Timestamp) ->
    Party0 = get_st_party(St),
    Contracts = maps:map(
        fun(_, Contract) ->
            update_contract_status(Contract, Timestamp)
        end,
        Party0#domain_Party.contracts
    ),
    Party = Party0#domain_Party{contracts = Contracts},
    {ok, St#st{party = Party}}.

update_contract_status(
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        status = {active, _}} = Contract,
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
