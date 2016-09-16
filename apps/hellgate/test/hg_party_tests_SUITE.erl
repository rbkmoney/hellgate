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

-export([claim_acceptance/1]).
-export([claim_denial/1]).
-export([claim_revocation/1]).
-export([claim_not_found_on_retrieval/1]).
-export([no_pending_claim/1]).

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

-export([shop_not_found_on_retrieval/1]).
-export([shop_creation/1]).
-export([shop_update/1]).
-export([shop_blocking/1]).
-export([shop_unblocking/1]).
-export([shop_already_blocked/1]).
-export([shop_already_unblocked/1]).
-export([shop_blocked_on_suspend/1]).
-export([shop_suspension/1]).
-export([shop_activation/1]).
-export([shop_already_suspended/1]).
-export([shop_already_active/1]).

-export([consistent_history/1]).

%%

-define(c(Key, C), begin element(2, lists:keyfind(Key, 1, C)) end).

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
        {group, shop_management},
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
        {shop_management, [sequence], [
            party_creation,
            shop_not_found_on_retrieval,
            shop_creation,
            shop_update,
            {group, shop_blocking_suspension}
        ]},
        {shop_blocking_suspension, [sequence], [
            shop_blocking,
            shop_already_blocked,
            shop_blocked_on_suspend,
            shop_unblocking,
            shop_already_unblocked,
            shop_suspension,
            shop_already_suspended,
            shop_activation,
            shop_already_active
        ]},
        {claim_management, [sequence], [
            party_creation,
            claim_not_found_on_retrieval,
            claim_revocation,
            claim_acceptance,
            claim_denial,
            no_pending_claim
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
    [application:stop(App) || App <- ?c(apps, C)].

%% tests

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("party_events.hrl").

-spec init_per_group(group_name(), config()) -> config().

init_per_group(shop_blocking_suspension, C) ->
    Client = ?c(client, C),
    _ = shop_creation(C),
    #domain_Shop{id = ShopID} = get_some_shop(Client),
    [{shop_id, ShopID} | C];

init_per_group(Group, C) ->
    PartyID = list_to_binary(lists:concat([Group, ".", erlang:system_time()])),
    Client = hg_client_party:start(make_userinfo(), PartyID, hg_client_api:new(?c(root_url, C))),
    [{party_id, PartyID}, {client, Client} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    Client = ?c(client, C),
    hg_client_party:stop(Client).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-define(party_w_status(ID, Blocking, Suspension),
    #domain_Party{id = ID, blocking = Blocking, suspension = Suspension}).
-define(shop_w_status(ID, Blocking, Suspension),
    #domain_Shop{id = ID, blocking = Blocking, suspension = Suspension}).

-define(party_state(Party),
    #payproc_PartyState{party = Party}).
-define(party_state(Party, Revision),
    #payproc_PartyState{party = Party, revision = Revision}).
-define(shop_state(Shop),
    #payproc_ShopState{shop = Shop}).

-define(party_not_found(),
    {exception, #payproc_PartyNotFound{}}).
-define(party_exists(),
    {exception, #payproc_PartyExists{}}).
-define(shop_not_found(),
    {exception, #payproc_ShopNotFound{}}).
-define(claim_not_found(),
    {exception, #payproc_ClaimNotFound{}}).
-define(invalid_claim_status(Status),
    {exception, #payproc_InvalidClaimStatus{status = Status}}).

-define(party_blocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?blocked(Reason)}}}).
-define(party_unblocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?unblocked(Reason)}}}).
-define(party_suspended(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?suspended()}}}).
-define(party_active(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?active()}}}).

-define(shop_blocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?blocked(Reason)}}}).
-define(shop_unblocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?unblocked(Reason)}}}).
-define(shop_suspended(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?suspended()}}}).
-define(shop_active(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?active()}}}).

-define(claim(ID),
    #payproc_Claim{id = ID}).
-define(claim(ID, Status),
    #payproc_Claim{id = ID, status = Status}).
-define(claim(ID, Status, Changeset),
    #payproc_Claim{id = ID, status = Status, changeset = Changeset}).
-define(claim_result(ID, Status),
    #payproc_ClaimResult{id = ID, status = Status}).

-spec party_creation(config()) -> _ | no_return().
-spec party_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_already_exists(config()) -> _ | no_return().
-spec party_retrieval(config()) -> _ | no_return().
-spec shop_not_found_on_retrieval(config()) -> _ | no_return().
-spec shop_creation(config()) -> _ | no_return().
-spec shop_update(config()) -> _ | no_return().
-spec party_revisioning(config()) -> _ | no_return().
-spec claim_revocation(config()) -> _ | no_return().
-spec claim_acceptance(config()) -> _ | no_return().
-spec claim_denial(config()) -> _ | no_return().
-spec claim_not_found_on_retrieval(config()) -> _ | no_return().
-spec no_pending_claim(config()) -> _ | no_return().
-spec party_blocking(config()) -> _ | no_return().
-spec party_unblocking(config()) -> _ | no_return().
-spec party_already_blocked(config()) -> _ | no_return().
-spec party_already_unblocked(config()) -> _ | no_return().
-spec party_blocked_on_suspend(config()) -> _ | no_return().
-spec party_suspension(config()) -> _ | no_return().
-spec party_activation(config()) -> _ | no_return().
-spec party_already_suspended(config()) -> _ | no_return().
-spec party_already_active(config()) -> _ | no_return().
-spec shop_blocking(config()) -> _ | no_return().
-spec shop_unblocking(config()) -> _ | no_return().
-spec shop_already_blocked(config()) -> _ | no_return().
-spec shop_already_unblocked(config()) -> _ | no_return().
-spec shop_blocked_on_suspend(config()) -> _ | no_return().
-spec shop_suspension(config()) -> _ | no_return().
-spec shop_activation(config()) -> _ | no_return().
-spec shop_already_suspended(config()) -> _ | no_return().
-spec shop_already_active(config()) -> _ | no_return().

party_creation(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    ok = hg_client_party:create(Client),
    ?party_created(?party_w_status(PartyID, ?unblocked(_), ?active()), 1) = next_event(Client).

party_already_exists(C) ->
    Client = ?c(client, C),
    ?party_exists() = hg_client_party:create(Client).

party_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?party_not_found() = hg_client_party:get(Client).

party_retrieval(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    {ok, ?party_state(#domain_Party{id = PartyID})} = hg_client_party:get(Client).

party_revisioning(C) ->
    Client = ?c(client, C),
    {ok, ?party_state(_Party1, Rev1)} = hg_client_party:get(Client),
    {ok, _} = party_suspension(C),
    {ok, ?party_state(_Party2, Rev2)} = hg_client_party:get(Client),
    Rev2 = Rev1 + 1,
    {ok, _} = party_activation(C),
    {ok, ?party_state(_Party3, Rev3)} = hg_client_party:get(Client),
    Rev3 = Rev2 + 1.

shop_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?shop_not_found() = hg_client_party:get_shop(<<"no such shop">>, Client).

shop_creation(C) ->
    Client = ?c(client, C),
    Params = #payproc_ShopParams{
        category = #domain_CategoryObject{
            ref  = #domain_CategoryRef{id = 42},
            data = #domain_Category{name = <<"TEST">>}
        },
        details  = Details = #domain_ShopDetails{
            name        = <<"THRIFT SHOP">>,
            description = <<"Hot. Fancy. Almost free.">>
        }
    },
    Result = hg_client_party:create_shop(Params, Client),
    Claim = assert_claim_created(Result, Client),
    ?claim(_, ?pending(), [{shop_creation, #domain_Shop{id = ShopID}}]) = Claim,
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client),
    ok = accept_claim(Claim, Client),
    {ok, ?shop_state(#domain_Shop{
        id = ShopID,
        suspension = ?active(),
        details = Details
    })} = hg_client_party:get_shop(ShopID, Client).

shop_update(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_some_shop(Client),
    Update = #payproc_ShopUpdate{
        details = Details = #domain_ShopDetails{
            name        = <<"BARBER SHOP">>,
            description = <<"Nice. Short. Clean.">>
        }
    },
    Result = hg_client_party:update_shop(ShopID, Update, Client),
    Claim = assert_claim_created(Result, Client),
    ok = accept_claim(Claim, Client),
    {ok, ?shop_state(#domain_Shop{details = Details})} = hg_client_party:get_shop(ShopID, Client).

claim_revocation(C) ->
    Client = ?c(client, C),
    Reason = <<"The End is near">>,
    ?claim(ID1) = ensure_block_party(Reason, Client),
    ?party_blocked(Reason) = hg_client_party:revoke_claim(ID1, <<>>, Client),
    ?claim(ID2) = ensure_unblock_party(<<>>, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:revoke_claim(ID2, <<>>, Client).

claim_acceptance(C) ->
    Client = ?c(client, C),
    Reason = <<"And behold">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:accept_claim(ID, Client),
    _ = ensure_unblock_party(<<>>, Client).

claim_denial(C) ->
    Client = ?c(client, C),
    Reason = <<"I am about to destroy them">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:deny_claim(ID, <<>>, Client),
    _ = ensure_unblock_party(<<>>, Client).

claim_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?claim_not_found() = hg_client_party:get_claim(<<"no such id">>, Client).

no_pending_claim(C) ->
    Client = ?c(client, C),
    ?claim_not_found() = hg_client_party:get_pending_claim(Client).

party_blocking(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:block(<<"i said so">>, Client), Client),
    {ok, ?party_state(?party_w_status(PartyID, ?blocked(_), _))} = hg_client_party:get(Client).

party_unblocking(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:unblock(<<"enough">>, Client), Client),
    {ok, ?party_state(?party_w_status(PartyID, ?unblocked(_), _))} = hg_client_party:get(Client).

party_already_blocked(C) ->
    Client = ?c(client, C),
    ?party_blocked(_) = hg_client_party:block(<<"too much">>, Client).

party_already_unblocked(C) ->
    Client = ?c(client, C),
    ?party_unblocked(_) = hg_client_party:unblock(<<"too free">>, Client).

party_blocked_on_suspend(C) ->
    Client = ?c(client, C),
    ?party_blocked(_) = hg_client_party:suspend(Client).

party_suspension(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:suspend(Client), Client),
    {ok, ?party_state(?party_w_status(PartyID, _, ?suspended()))} = hg_client_party:get(Client).

party_activation(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:activate(Client), Client),
    {ok, ?party_state(?party_w_status(PartyID, _, ?active()))} = hg_client_party:get(Client).

party_already_suspended(C) ->
    Client = ?c(client, C),
    ?party_suspended() = hg_client_party:suspend(Client).

party_already_active(C) ->
    Client = ?c(client, C),
    ?party_active() = hg_client_party:activate(Client).

shop_blocking(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    _ = assert_claim_accepted(hg_client_party:block_shop(ShopID, <<"i said so">>, Client), Client),
    {ok, ?shop_state(?shop_w_status(ShopID, ?blocked(_), _))} = hg_client_party:get_shop(ShopID, Client).

shop_unblocking(C) ->
    Client  = ?c(client, C),
    ShopID  = ?c(shop_id, C),
    _ = assert_claim_accepted(hg_client_party:unblock_shop(ShopID, <<"enough">>, Client), Client),
    {ok, ?shop_state(?shop_w_status(ShopID, ?unblocked(_), _))} = hg_client_party:get_shop(ShopID, Client).

shop_already_blocked(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    ?shop_blocked(_) = hg_client_party:block_shop(ShopID, <<"too much">>, Client).

shop_already_unblocked(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    ?shop_unblocked(_) = hg_client_party:unblock_shop(ShopID, <<"too free">>, Client).

shop_blocked_on_suspend(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    ?shop_blocked(_) = hg_client_party:suspend_shop(ShopID, Client).

shop_suspension(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    _ = assert_claim_accepted(hg_client_party:suspend_shop(ShopID, Client), Client),
    {ok, ?shop_state(?shop_w_status(ShopID, _, ?suspended()))} = hg_client_party:get_shop(ShopID, Client).

shop_activation(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    _ = assert_claim_accepted(hg_client_party:activate_shop(ShopID, Client), Client),
    {ok, ?shop_state(?shop_w_status(ShopID, _, ?active()))} = hg_client_party:get_shop(ShopID, Client).

shop_already_suspended(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    ?shop_suspended() = hg_client_party:suspend_shop(ShopID, Client).

shop_already_active(C) ->
    Client = ?c(client, C),
    ShopID = ?c(shop_id, C),
    ?shop_active() = hg_client_party:activate_shop(ShopID, Client).

get_some_shop(Client) ->
    {ok, ?party_state(#domain_Party{shops = Shops})} = hg_client_party:get(Client),
    [ShopID | _] = maps:keys(Shops),
    maps:get(ShopID, Shops).

ensure_block_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:block(Reason, Client), Client).

ensure_unblock_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:unblock(Reason, Client), Client).

accept_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:accept_claim(ClaimID, Client),
    ?claim_status_changed(ClaimID, ?accepted(_)) = next_event(Client),
    ok.

assert_claim_created({ok, ?claim_result(ClaimID, Status = ?pending())}, Client) ->
    {ok, Claim = ?claim(ClaimID, Status)} = hg_client_party:get_claim(ClaimID, Client),
    ?claim_created(?claim(ClaimID)) = next_event(Client),
    Claim.

assert_claim_accepted({ok, ?claim_result(ClaimID, Status = ?accepted(_))}, Client) ->
    {ok, Claim = ?claim(ClaimID, Status)} = hg_client_party:get_claim(ClaimID, Client),
    ?claim_created(?claim(ClaimID)) = next_event(Client),
    ?claim_status_changed(ClaimID, Status) = next_event(Client),
    Claim.

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(?c(root_url, C))),
    {ok, Events} = hg_client_eventsink:pull_events(_N = 5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

%%

next_event(Client) ->
    case hg_client_party:pull_event(Client) of
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
