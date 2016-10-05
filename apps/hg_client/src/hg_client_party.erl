-module(hg_client_party).
-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-export([start/3]).
-export([start_link/3]).
-export([stop/1]).

-export([create/1]).
-export([get/1]).
-export([block/2]).
-export([unblock/2]).
-export([suspend/1]).
-export([activate/1]).
-export([get_shop/2]).
-export([create_shop/2]).
-export([update_shop/3]).
-export([block_shop/3]).
-export([unblock_shop/3]).
-export([suspend_shop/2]).
-export([activate_shop/2]).
-export([get_claim/2]).
-export([get_pending_claim/1]).
-export([accept_claim/2]).
-export([deny_claim/3]).
-export([revoke_claim/3]).
-export([get_shop_account/2]).
-export([get_shop_account_set/2]).
-export([pull_event/1]).
-export([pull_event/2]).

%% GenServer

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%

-type user_info() :: hg_payment_processing_thrift:'UserInfo'().
-type party_id() :: hg_domain_thrift:'PartyID'().
-type shop_id() :: hg_domain_thrift:'ShopID'().
-type claim_id() :: hg_payment_processing_thrift:'ClaimID'().
-type shop_params() :: hg_payment_processing_thrift:'ShopParams'().
-type shop_account_id() :: hg_domain_thrift:'ShopAccountID'().


-spec start(user_info(), party_id(), hg_client_api:t()) -> pid().

start(UserInfo, PartyID, ApiClient) ->
    start(start, UserInfo, PartyID, ApiClient).

-spec start_link(user_info(), party_id(), hg_client_api:t()) -> pid().

start_link(UserInfo, PartyID, ApiClient) ->
    start(start_link, UserInfo, PartyID, ApiClient).

start(Mode, UserInfo, PartyID, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, {UserInfo, PartyID, ApiClient}, []),
    Pid.

-spec stop(pid()) -> ok.

stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(pid()) ->
    ok | woody_client:result_error().

create(Client) ->
    gen_server:call(Client, {call, 'Create', []}).

-spec get(pid()) ->
    {ok, hg_payment_processing_thrift:'PartyState'()} | woody_client:result_error().

get(Client) ->
    gen_server:call(Client, {call, 'Get', []}).

-spec block(binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

block(Reason, Client) ->
    gen_server:call(Client, {call, 'Block', [Reason]}).

-spec unblock(binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

unblock(Reason, Client) ->
    gen_server:call(Client, {call, 'Unblock', [Reason]}).

-spec suspend(pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

suspend(Client) ->
    gen_server:call(Client, {call, 'Suspend', []}).

-spec activate(pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

activate(Client) ->
    gen_server:call(Client, {call, 'Activate', []}).

-spec get_shop(shop_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

get_shop(ID, Client) ->
    gen_server:call(Client, {call, 'GetShop', [ID]}).

-spec create_shop(shop_params(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

create_shop(Params, Client) ->
    gen_server:call(Client, {call, 'CreateShop', [Params]}).

-spec update_shop(shop_id(), hg_payment_processing_thrift:'ShopUpdate'(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

update_shop(ID, Update, Client) ->
    gen_server:call(Client, {call, 'UpdateShop', [ID, Update]}).

-spec block_shop(shop_id(), binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

block_shop(ID, Reason, Client) ->
    gen_server:call(Client, {call, 'BlockShop', [ID, Reason]}).

-spec unblock_shop(shop_id(), binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

unblock_shop(ID, Reason, Client) ->
    gen_server:call(Client, {call, 'UnblockShop', [ID, Reason]}).

-spec suspend_shop(shop_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

suspend_shop(ID, Client) ->
    gen_server:call(Client, {call, 'SuspendShop', [ID]}).

-spec activate_shop(shop_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

activate_shop(ID, Client) ->
    gen_server:call(Client, {call, 'ActivateShop', [ID]}).

-spec get_claim(claim_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

get_claim(ID, Client) ->
    gen_server:call(Client, {call, 'GetClaim', [ID]}).

-spec get_pending_claim(pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

get_pending_claim(Client) ->
    gen_server:call(Client, {call, 'GetPendingClaim', []}).

-spec accept_claim(claim_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

accept_claim(ID, Client) ->
    gen_server:call(Client, {call, 'AcceptClaim', [ID]}).

-spec deny_claim(claim_id(), binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

deny_claim(ID, Reason, Client) ->
    gen_server:call(Client, {call, 'DenyClaim', [ID, Reason]}).

-spec revoke_claim(claim_id(), binary(), pid()) ->
    {ok, hg_payment_processing_thrift:'ClaimResult'()} | woody_client:result_error().

revoke_claim(ID, Reason, Client) ->
    gen_server:call(Client, {call, 'RevokeClaim', [ID, Reason]}).

-spec get_shop_account(shop_account_id(), pid()) ->
    {ok, hg_payment_processing_thrift:'ShopAccountState'()} | woody_client:result_error().

get_shop_account(AccountID, Client) ->
    gen_server:call(Client, {call, 'GetShopAccountState', [AccountID]}).

-spec get_shop_account_set(shop_id(), pid()) ->
    {ok, hg_domain_thrift:'ShopAccountSet'()} | woody_client:result_error().

get_shop_account_set(ShopID, Client) ->
    gen_server:call(Client, {call, 'GetShopAccountSet', [ShopID]}).

-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-spec pull_event(pid()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_event(Client) ->
    pull_event(?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_event(timeout(), pid()) ->
    {ok, tuple()} | timeout | woody_client:result_error().

pull_event(Timeout, Client) ->
    gen_server:call(Client, {pull_event, Timeout}, infinity).

%%

-record(st, {
    user_info :: user_info(),
    party_id  :: party_id(),
    poller    :: hg_client_event_poller:t(),
    client    :: hg_client_api:t()
}).

-type st() :: #st{}.
-type callref() :: {pid(), Tag :: reference()}.

-spec init({user_info(), party_id(), hg_client_api:t()}) ->
    {ok, st()}.

init({UserInfo, PartyID, ApiClient}) ->
    {ok, #st{
        user_info = UserInfo,
        party_id = PartyID,
        client = ApiClient,
        poller = hg_client_event_poller:new(party_management, 'GetEvents', [UserInfo, PartyID])
    }}.

-spec handle_call(term(), callref(), st()) ->
    {reply, term(), st()} | {noreply, st()}.

handle_call({call, Function, Args0}, _From, St = #st{client = Client}) ->
    Args = [St#st.user_info, St#st.party_id | Args0],
    {Result, ClientNext} = hg_client_api:call(party_management, Function, Args, Client),
    {reply, Result, St#st{client = ClientNext}};

handle_call({pull_event, Timeout}, _From, St = #st{poller = Poller, client = Client}) ->
    {Result, ClientNext, PollerNext} = hg_client_event_poller:poll(1, Timeout, Client, Poller),
    StNext = St#st{poller = PollerNext, client = ClientNext},
    case Result of
        {ok, []} ->
            {reply, timeout, StNext};
        {ok, [#payproc_Event{payload = Payload}]} ->
            {reply, {ok, Payload}, StNext};
        Error ->
            {reply, Error, StNext}
    end;

handle_call(Call, _From, State) ->
    _ = lager:warning("unexpected call received: ~tp", [Call]),
    {noreply, State}.

-spec handle_cast(_, st()) ->
    {noreply, st()}.

handle_cast(Cast, State) ->
    _ = lager:warning("unexpected cast received: ~tp", [Cast]),
    {noreply, State}.

-spec handle_info(_, st()) ->
    {noreply, st()}.

handle_info(Info, State) ->
    _ = lager:warning("unexpected info received: ~tp", [Info]),
    {noreply, State}.

-spec terminate(Reason, st()) ->
    ok when
        Reason :: normal | shutdown | {shutdown, term()} | term().

terminate(_Reason, _State) ->
    ok.

-spec code_change(Vsn | {down, Vsn}, st(), term()) ->
    {error, noimpl} when
        Vsn :: term().

code_change(_OldVsn, _State, _Extra) ->
    {error, noimpl}.
