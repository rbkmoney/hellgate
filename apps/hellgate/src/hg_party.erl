%% References:
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/party.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/merchant.md
%%  * https://github.com/rbkmoney/coredocs/blob/529bc03/docs/domain/entities/contract.md

%% @TODO
%% * Deal with default shop services (will need to change thrift-protocol as well)
%% * Access check before shop creation is weird (think about adding context)

-module(hg_party).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

%% Party support functions

-export([get_party/1]).
-export([get_party_revision/1]).
-export([checkout/2]).

-export([get_contract/2]).

-export([get_shop/2]).

-export_type([party/0]).
-export_type([party_revision/0]).
-export_type([party_status/0]).

%%

-type party() :: dmsl_domain_thrift:'Party'().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type party_revision() :: dmsl_domain_thrift:'PartyRevision'().
-type party_status() :: dmsl_domain_thrift:'PartyStatus'().
-type contract() :: dmsl_domain_thrift:'Contract'().
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type shop() :: dmsl_domain_thrift:'Shop'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().

%% Interface

-spec get_party(party_id()) -> party() | no_return().
get_party(PartyID) ->
    Revision = get_party_revision(PartyID),
    checkout(PartyID, {revision, Revision}).

-spec get_party_revision(party_id()) -> party_revision() | no_return().
get_party_revision(PartyID) ->
    {Client, Context} = get_party_client(),
    unwrap_party_result(party_client_thrift:get_revision(PartyID, Client, Context)).

-spec checkout(party_id(), party_client_thrift:party_revision_param()) -> party() | no_return().
checkout(PartyID, RevisionParam) ->
    {Client, Context} = get_party_client(),
    unwrap_party_result(party_client_thrift:checkout(PartyID, RevisionParam, Client, Context)).

-spec get_contract(contract_id(), party()) -> contract() | undefined.
get_contract(ID, #domain_Party{contracts = Contracts}) ->
    maps:get(ID, Contracts, undefined).

-spec get_shop(shop_id(), party()) -> shop() | undefined.
get_shop(ID, #domain_Party{shops = Shops}) ->
    maps:get(ID, Shops, undefined).

%% Internals

get_party_client() ->
    HgContext = hg_context:load(),
    Client = hg_context:get_party_client(HgContext),
    Context = hg_context:get_party_client_context(HgContext),
    {Client, Context}.

unwrap_party_result({ok, Result}) ->
    Result;
unwrap_party_result({error, Error}) ->
    erlang:throw(Error).
