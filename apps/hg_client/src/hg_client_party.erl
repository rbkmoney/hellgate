-module(hg_client_party).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([start/2]).
-export([start/3]).
-export([start_link/2]).
-export([stop/1]).

-export([create/2]).
-export([get/1]).
-export([checkout/2]).
-export([block/2]).
-export([unblock/2]).
-export([suspend/1]).
-export([activate/1]).

-export([get_meta/1]).
-export([get_metadata/2]).
-export([set_metadata/3]).
-export([remove_metadata/2]).

-export([get_contract/2]).
-export([compute_contract_terms/3]).
-export([get_shop/2]).
-export([compute_shop_terms/3]).
-export([compute_payment_institution_terms/2]).

-export([block_shop/3]).
-export([unblock_shop/3]).
-export([suspend_shop/2]).
-export([activate_shop/2]).

-export([get_claim/2]).
-export([get_claims/1]).
-export([create_claim/2]).
-export([update_claim/4]).
-export([accept_claim/3]).
-export([deny_claim/4]).
-export([revoke_claim/4]).

-export([get_account_state/2]).
-export([get_shop_account/2]).
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

-type user_info()       :: dmsl_payment_processing_thrift:'UserInfo'().
-type party_id()        :: dmsl_domain_thrift:'PartyID'().
-type party_params()    :: dmsl_payment_processing_thrift:'PartyParams'().
-type contract_id()     :: dmsl_domain_thrift:'ContractID'().
-type shop_id()         :: dmsl_domain_thrift:'ShopID'().
-type claim_id()        :: dmsl_payment_processing_thrift:'ClaimID'().
-type claim()           :: dmsl_payment_processing_thrift:'Claim'().
-type claim_revision()  :: dmsl_payment_processing_thrift:'ClaimRevision'().
-type changeset()       :: dmsl_payment_processing_thrift:'PartyChangeset'().
-type shop_account_id() :: dmsl_domain_thrift:'AccountID'().
-type meta()            :: dmsl_domain_thrift:'PartyMeta'().
-type meta_ns()         :: dmsl_domain_thrift:'PartyMetaNamespace'().
-type meta_data()       :: dmsl_domain_thrift:'PartyMetaData'().
-type timestamp()       :: dmsl_base_thrift:'Timestamp'().

-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().
-type payment_intitution_ref() :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-spec start(party_id(), hg_client_api:t()) -> pid().

start(PartyID, ApiClient) ->
    start(start, undefined, PartyID, ApiClient).

-spec start(user_info(), party_id(), hg_client_api:t()) -> pid().

start(UserInfo, PartyID, ApiClient) ->
    start(start, UserInfo, PartyID, ApiClient).

-spec start_link(party_id(), hg_client_api:t()) -> pid().

start_link(PartyID, ApiClient) ->
    start(start_link, undefined, PartyID, ApiClient).

start(Mode, UserInfo, PartyID, ApiClient) ->
    {ok, Pid} = gen_server:Mode(?MODULE, {UserInfo, PartyID, ApiClient}, []),
    Pid.

-spec stop(pid()) -> ok.

stop(Client) ->
    _ = exit(Client, shutdown),
    ok.

%%

-spec create(party_params(), pid()) ->
    ok | woody_error:business_error().

create(PartyParams, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Create', [PartyParams]})).

-spec get(pid()) ->
    dmsl_domain_thrift:'Party'() | woody_error:business_error().

get(Client) ->
    map_result_error(gen_server:call(Client, {call, 'Get', []})).

-spec checkout(party_revision_param(), pid()) ->
    dmsl_domain_thrift:'Party'() | woody_error:business_error().

checkout(PartyRevisionParam, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Checkout', [PartyRevisionParam]})).

-spec block(binary(), pid()) ->
    ok | woody_error:business_error().

block(Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Block', [Reason]})).

-spec unblock(binary(), pid()) ->
    ok | woody_error:business_error().

unblock(Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'Unblock', [Reason]})).

-spec suspend(pid()) ->
    ok | woody_error:business_error().

suspend(Client) ->
    map_result_error(gen_server:call(Client, {call, 'Suspend', []})).

-spec activate(pid()) ->
    ok | woody_error:business_error().

activate(Client) ->
    map_result_error(gen_server:call(Client, {call, 'Activate', []})).

-spec get_meta(pid()) ->
    meta() | woody_error:business_error().

get_meta(Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetMeta', []})).

-spec get_metadata(meta_ns(), pid()) ->
    meta_data() | woody_error:business_error().

get_metadata(NS, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetMetaData', [NS]})).

-spec set_metadata(meta_ns(), meta_data(), pid()) ->
    ok | woody_error:business_error().

set_metadata(NS, Data, Client) ->
    map_result_error(gen_server:call(Client, {call, 'SetMetaData', [NS, Data]})).

-spec remove_metadata(meta_ns(), pid()) ->
    ok | woody_error:business_error().

remove_metadata(NS, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RemoveMetaData', [NS]})).

-spec get_contract(contract_id(), pid()) ->
    dmsl_domain_thrift:'Contract'() | woody_error:business_error().

get_contract(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetContract', [ID]})).

-spec compute_contract_terms(contract_id(), timestamp(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().

compute_contract_terms(ID, Timestamp, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ComputeContractTerms', [ID, Timestamp]})).

-spec compute_payment_institution_terms(payment_intitution_ref(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().

compute_payment_institution_terms(Ref, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ComputePaymentInstitutionTerms', [Ref]})).

-spec get_shop(shop_id(), pid()) ->
    dmsl_domain_thrift:'Shop'() | woody_error:business_error().

get_shop(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetShop', [ID]})).

-spec block_shop(shop_id(), binary(), pid()) ->
    ok | woody_error:business_error().

block_shop(ID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'BlockShop', [ID, Reason]})).

-spec unblock_shop(shop_id(), binary(), pid()) ->
    ok | woody_error:business_error().

unblock_shop(ID, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'UnblockShop', [ID, Reason]})).

-spec suspend_shop(shop_id(), pid()) ->
    ok | woody_error:business_error().

suspend_shop(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'SuspendShop', [ID]})).

-spec activate_shop(shop_id(), pid()) ->
    ok | woody_error:business_error().

activate_shop(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ActivateShop', [ID]})).

-spec compute_shop_terms(shop_id(), timestamp(), pid()) ->
    dmsl_domain_thrift:'TermSet'() | woody_error:business_error().

compute_shop_terms(ID, Timestamp, Client) ->
    map_result_error(gen_server:call(Client, {call, 'ComputeShopTerms', [ID, Timestamp]})).

-spec get_claim(claim_id(), pid()) ->
    claim() | woody_error:business_error().

get_claim(ID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetClaim', [ID]})).

-spec get_claims(pid()) ->
    [claim()] | woody_error:business_error().

get_claims(Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetClaims', []})).

-spec create_claim(changeset(), pid()) ->
    claim() | woody_error:business_error().

create_claim(Changeset, Client) ->
    map_result_error(gen_server:call(Client, {call, 'CreateClaim', [Changeset]})).

-spec update_claim(claim_id(), claim_revision(), changeset(), pid()) ->
    ok | woody_error:business_error().

update_claim(ID, Revision, Changeset, Client) ->
    map_result_error(gen_server:call(Client, {call, 'UpdateClaim', [ID, Revision, Changeset]})).

-spec accept_claim(claim_id(), claim_revision(), pid()) ->
    ok | woody_error:business_error().

accept_claim(ID, Revision, Client) ->
    map_result_error(gen_server:call(Client, {call, 'AcceptClaim', [ID, Revision]})).

-spec deny_claim(claim_id(), claim_revision(), binary() | undefined, pid()) ->
    ok | woody_error:business_error().

deny_claim(ID, Revision, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'DenyClaim', [ID, Revision, Reason]})).

-spec revoke_claim(claim_id(), claim_revision(), binary() | undefined, pid()) ->
    ok | woody_error:business_error().

revoke_claim(ID, Revision, Reason, Client) ->
    map_result_error(gen_server:call(Client, {call, 'RevokeClaim', [ID, Revision, Reason]})).

-spec get_account_state(shop_account_id(), pid()) ->
    dmsl_payment_processing_thrift:'AccountState'() | woody_error:business_error().

get_account_state(AccountID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetAccountState', [AccountID]})).

-spec get_shop_account(shop_id(), pid()) ->
    dmsl_domain_thrift:'ShopAccount'() | woody_error:business_error().

get_shop_account(ShopID, Client) ->
    map_result_error(gen_server:call(Client, {call, 'GetShopAccount', [ShopID]})).

-define(DEFAULT_NEXT_EVENT_TIMEOUT, 5000).

-spec pull_event(pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(Client) ->
    pull_event(?DEFAULT_NEXT_EVENT_TIMEOUT, Client).

-spec pull_event(timeout(), pid()) ->
    tuple() | timeout | woody_error:business_error().

pull_event(Timeout, Client) ->
    gen_server:call(Client, {pull_event, Timeout}, infinity).

map_result_error({ok, Result}) ->
    Result;
map_result_error({exception, _} = Exception) ->
    Exception;
map_result_error({error, Error}) ->
    error(Error).

%%

-type event() :: dmsl_payment_processing_thrift:'Event'().

-record(st, {
    user_info :: user_info(),
    party_id  :: party_id(),
    poller    :: hg_client_event_poller:st(event()),
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
        poller = hg_client_event_poller:new(
            {party_management, 'GetEvents', [UserInfo, PartyID]},
            fun (Event) -> Event#payproc_Event.id end
        )
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
        [] ->
            {reply, timeout, StNext};
        [#payproc_Event{payload = Payload}] ->
            {reply, Payload, StNext};
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
