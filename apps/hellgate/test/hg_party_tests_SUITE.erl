-module(hg_party_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([party_creation/1]).
-export([party_not_found_on_retrieval/1]).
-export([party_already_exists/1]).
-export([party_retrieval/1]).
-export([party_claim_acceptance/1]).
-export([party_claim_denial/1]).
-export([party_claim_revocation/1]).
-export([party_claim_not_found_on_retrieval/1]).
-export([party_revisioning/1]).
-export([party_blocking/1]).
-export([party_unblocking/1]).
-export([party_already_blocked/1]).
-export([party_already_unblocked/1]).
-export([party_blocked_on_suspend/1]).
-export([party_suspension/1]).
-export([party_activation/1]).
-export([party_already_suspended/1]).
-export([party_already_active/1]).

-export([consistent_history/1]).

%%

-define(config(Key), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-spec all() -> [{group, group_name()}].

all() ->
    [
        {group, party_creation},
        {group, party_revisioning},
        {group, party_blocking_suspension},
        {group, claim_management},

        {group, consistent_history}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {party_creation, [sequence], [
            party_not_found_on_retrieval,
            party_creation,
            party_already_exists,
            party_retrieval
        ]},
        {party_revisioning, [sequence], [
            party_creation,
            party_revisioning
        ]},
        {party_blocking_suspension, [sequence], [
            party_creation,
            party_blocking,
            party_already_blocked,
            party_blocked_on_suspend,
            party_unblocking,
            party_already_unblocked,
            party_suspension,
            party_already_suspended,
            party_blocking,
            party_unblocking,
            party_activation,
            party_already_active
        ]},
        {claim_management, [sequence], [
            party_creation,
            party_claim_not_found_on_retrieval,
            party_claim_revocation,
            party_claim_acceptance,
            party_claim_denial
        ]},
        {consistent_history, [], [
            consistent_history
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("party_events.hrl").

-define(config(Key, C), begin element(2, lists:keyfind(Key, 1, C)) end).

-spec init_per_group(group_name(), config()) -> config().

init_per_group(Group, C) ->
    Client = hg_client:start(?config(root_url), make_userinfo()),
    PartyID = list_to_binary(lists:concat([Group, ".", erlang:system_time()])),
    [{party_id, PartyID}, {client, Client} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    Client = ?config(client, C),
    hg_client:stop(Client).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-define(party_w_status(ID, Blocking, Suspension),
    #domain_Party{id = ID, blocking = Blocking, suspension = Suspension}).

-define(party_state(Party),
    #payproc_PartyState{party = Party}).
-define(party_state(Party, Revision),
    #payproc_PartyState{party = Party, revision = Revision}).

-define(party_blocked(Reason),
    #payproc_InvalidPartyStatus{status = {blocking, ?blocked(Reason)}}).
-define(party_unblocked(Reason),
    #payproc_InvalidPartyStatus{status = {blocking, ?unblocked(Reason)}}).
-define(party_suspended(),
    #payproc_InvalidPartyStatus{status = {suspension, ?suspended()}}).
-define(party_active(),
    #payproc_InvalidPartyStatus{status = {suspension, ?active()}}).

-define(claim_result(ID, Status),
    #payproc_ClaimResult{id = ID, status = Status}).
-define(invalid_claim_status(Status),
    #payproc_InvalidClaimStatus{status = Status}).

-spec party_creation(config()) -> _ | no_return().
-spec party_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_already_exists(config()) -> _ | no_return().
-spec party_retrieval(config()) -> _ | no_return().
-spec party_revisioning(config()) -> _ | no_return().
-spec party_claim_revocation(config()) -> _ | no_return().
-spec party_claim_acceptance(config()) -> _ | no_return().
-spec party_claim_denial(config()) -> _ | no_return().
-spec party_claim_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_blocking(config()) -> _ | no_return().
-spec party_unblocking(config()) -> _ | no_return().
-spec party_already_blocked(config()) -> _ | no_return().
-spec party_already_unblocked(config()) -> _ | no_return().
-spec party_blocked_on_suspend(config()) -> _ | no_return().
-spec party_suspension(config()) -> _ | no_return().
-spec party_activation(config()) -> _ | no_return().
-spec party_already_suspended(config()) -> _ | no_return().
-spec party_already_active(config()) -> _ | no_return().

party_creation(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    ok = hg_client:create_party(PartyID, Client),
    ?party_created(?party_w_status(PartyID, ?unblocked(_), ?active()), 1) = next_event(PartyID, Client).

party_already_exists(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, #payproc_PartyExists{}} = hg_client:create_party(PartyID, Client).

party_not_found_on_retrieval(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, #payproc_PartyNotFound{}} = hg_client:get_party(PartyID, Client).

party_retrieval(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {ok, ?party_state(#domain_Party{id = PartyID})} = hg_client:get_party(PartyID, Client).

party_revisioning(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {ok, ?party_state(_Party1, Rev1)} = hg_client:get_party(PartyID, Client),
    {ok, _} = party_suspension(C),
    {ok, ?party_state(_Party2, Rev2)} = hg_client:get_party(PartyID, Client),
    Rev2 = Rev1 + 1,
    {ok, _} = party_activation(C),
    {ok, ?party_state(_Party3, Rev3)} = hg_client:get_party(PartyID, Client),
    Rev3 = Rev2 + 1.

party_claim_revocation(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    Reason = <<"The End is near">>,
    ID1 = ensure_block_party(PartyID, Reason, Client),
    {exception, ?party_blocked(Reason)} = hg_client:revoke_claim(PartyID, ID1, <<>>, Client),
    ID2 = ensure_unblock_party(PartyID, <<>>, Client),
    {exception, ?invalid_claim_status(?accepted(_))} = hg_client:revoke_claim(PartyID, ID2, <<>>, Client).

party_claim_acceptance(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    Reason = <<"And behold">>,
    ID = ensure_block_party(PartyID, Reason, Client),
    {exception, ?invalid_claim_status(?accepted(_))} = hg_client:accept_claim(PartyID, ID, Client),
    _ = ensure_unblock_party(PartyID, <<>>, Client).

party_claim_denial(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    Reason = <<"I am about to destroy them">>,
    ID = ensure_block_party(PartyID, Reason, Client),
    {exception, ?invalid_claim_status(?accepted(_))} = hg_client:deny_claim(PartyID, ID, <<>>, Client),
    _ = ensure_unblock_party(PartyID, <<>>, Client).

party_claim_not_found_on_retrieval(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, #payproc_ClaimNotFound{}} = hg_client:get_claim(PartyID, <<"no such id">>, Client).

party_blocking(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    _ = assert_claim_accepted(hg_client:block_party(PartyID, <<"i said so">>, Client), PartyID, Client),
    {ok, ?party_state(?party_w_status(PartyID, ?blocked(_), _))} = hg_client:get_party(PartyID, Client).

party_unblocking(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    _ = assert_claim_accepted(hg_client:unblock_party(PartyID, <<"enough">>, Client), PartyID, Client),
    {ok, ?party_state(?party_w_status(PartyID, ?unblocked(_), _))} = hg_client:get_party(PartyID, Client).

party_already_blocked(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, ?party_blocked(_)} = hg_client:block_party(PartyID, <<"too much">>, Client).

party_already_unblocked(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, ?party_unblocked(_)} = hg_client:unblock_party(PartyID, <<"too free">>, Client).

party_blocked_on_suspend(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, ?party_blocked(_)} = hg_client:suspend_party(PartyID, Client).

party_suspension(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    _ = assert_claim_accepted(hg_client:suspend_party(PartyID, Client), PartyID, Client),
    {ok, ?party_state(?party_w_status(PartyID, _, ?suspended()))} = hg_client:get_party(PartyID, Client).

party_activation(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    _ = assert_claim_accepted(hg_client:activate_party(PartyID, Client), PartyID, Client),
    {ok, ?party_state(?party_w_status(PartyID, _, ?active()))} = hg_client:get_party(PartyID, Client).

party_already_suspended(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, ?party_suspended()} = hg_client:suspend_party(PartyID, Client).

party_already_active(C) ->
    Client = ?config(client, C),
    PartyID = ?config(party_id, C),
    {exception, ?party_active()} = hg_client:activate_party(PartyID, Client).

ensure_block_party(PartyID, Reason, Client) ->
    assert_claim_accepted(hg_client:block_party(PartyID, Reason, Client), PartyID, Client).

ensure_unblock_party(PartyID, Reason, Client) ->
    assert_claim_accepted(hg_client:unblock_party(PartyID, Reason, Client), PartyID, Client).

assert_claim_accepted(Result, PartyID, Client) ->
    {ok, ?claim_result(ID, Status = ?accepted(_))} = Result,
    {ok, #payproc_Claim{status = Status}} = hg_client:get_claim(PartyID, ID, Client),
    ?claim_created(#payproc_Claim{id = ID}) = next_event(PartyID, Client),
    ?claim_status_changed(ID, Status) = next_event(PartyID, Client),
    ID.

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = ?config(client, C),
    {ok, Events} = hg_client:pull_events(_N = 5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

%%

next_event(PartyID, Client) ->
    next_event(PartyID, 3000, Client).

next_event(PartyID, Timeout, Client) ->
    case hg_client:pull_party_event(PartyID, Timeout, Client) of
        {ok, Event} ->
            unwrap_event(Event);
        Result ->
            Result
    end.

unwrap_event(?party_ev(E)) ->
    unwrap_event(E);
unwrap_event(E) ->
    E.

%%

make_userinfo() ->
    #payproc_UserInfo{id = <<?MODULE_STRING>>}.
