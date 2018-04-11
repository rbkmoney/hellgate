-module(hg_party_contractor).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%%

-export([create/2]).

%% Interface

-type id() :: dmsl_domain_thrift:'ContractorID'().
-type contractor() :: dmsl_domain_thrift:'Contractor'().
-type party_contractor() :: dmsl_domain_thrift:'PartyContractor'().

-spec create(id(), contractor()) -> party_contractor().

create(ID, Contractor) ->
    #domain_PartyContractor{
        id = ID,
        contractor = Contractor,
        status = none,
        identity_documents = []
    }.

%% Implementation

