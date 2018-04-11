-module(hg_claim_effect).

-include("party_events.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([make/3]).
-export([make_safe/3]).

-export_type([effect/0]).

%% Interface

-type change()      :: dmsl_payment_processing_thrift:'PartyModification'().
-type effect()      :: dmsl_payment_processing_thrift:'ClaimEffect'().
-type timestamp()   :: hg_datetime:timestamp().
-type revision()    :: hg_domain:revision().

-spec make(change(), timestamp(), revision()) -> effect() | no_return().

make(?contractor_modification(ID, Modification), Timestamp, Revision) ->
    ?contractor_effect(ID, make_contractor_effect(ID, Modification, Timestamp, Revision));

make(?contract_modification(ID, Modification), Timestamp, Revision) ->
    try
        ?contract_effect(ID, make_contract_effect(ID, Modification, Timestamp, Revision))
    catch
        throw:{payment_institution_invalid, Ref} ->
            raise_invalid_changeset(?invalid_contract(
                ID,
                {invalid_object_reference, #payproc_InvalidObjectReference{
                    ref = make_optional_domain_ref(payment_institution, Ref)
                }}
            ));
        throw:{template_invalid, Ref} ->
            raise_invalid_changeset(?invalid_contract(
                ID,
                {invalid_object_reference, #payproc_InvalidObjectReference{
                    ref = make_optional_domain_ref(contract_template, Ref)
                }}
            ))
    end;

make(?shop_modification(ID, Modification), Timestamp, _Revision) ->
    ?shop_effect(ID, make_shop_effect(ID, Modification, Timestamp));

make(?wallet_modification(ID, Modification), Timestamp, _Revision) ->
    ?wallet_effect(ID, make_wallet_effect(ID, Modification, Timestamp)).

-spec make_safe(change(), timestamp(), revision()) -> effect() | no_return().

make_safe(
    ?shop_modification(ID, {shop_account_creation, #payproc_ShopAccountParams{currency = Currency}}),
    _Timestamp,
    _Revision
) ->
    ?shop_effect(ID,
        {account_created, #domain_ShopAccount{
            currency = Currency,
            settlement = 0,
            guarantee = 0,
            payout = 0
        }}
    );
make_safe(?wallet_modification(ID, {account_creation, Params}), _, _) ->
    ?wallet_effect(ID, {account_created, hg_wallet:create_fake_account(Params)});
make_safe(Change, Timestamp, Revision) ->
    make(Change, Timestamp, Revision).

%% Implementation

make_contractor_effect(ID, {creation, Contractor}, _, _) ->
    {created, hg_party_contractor:create(ID, Contractor)};
make_contractor_effect(_, {identification_level_modification, Level}, _, _) ->
    {identification_level_changed, Level};
make_contractor_effect(_, ?identity_documents_modification(Docs), _, _) ->
    {identity_documents_changed, #payproc_ContractorIdentityDocumentsChanged{
        identity_documents = Docs
    }}.

make_contract_effect(ID, {creation, ContractParams}, Timestamp, Revision) ->
    {created, hg_contract:create(ID, ContractParams, Timestamp, Revision)};
make_contract_effect(_, ?contract_termination(_), Timestamp, _) ->
    {status_changed, {terminated, #domain_ContractTerminated{terminated_at = Timestamp}}};
make_contract_effect(_, ?adjustment_creation(AdjustmentID, Params), Timestamp, Revision) ->
    {adjustment_created, hg_contract:create_adjustment(AdjustmentID, Params, Timestamp, Revision)};
make_contract_effect(_, ?payout_tool_creation(PayoutToolID, Params), Timestamp, _) ->
    {payout_tool_created, hg_payout_tool:create(PayoutToolID, Params, Timestamp)};
make_contract_effect(_, {legal_agreement_binding, LegalAgreement}, _, _) ->
    {legal_agreement_bound, LegalAgreement}.

make_shop_effect(ID, {creation, ShopParams}, Timestamp) ->
    {created, hg_party:create_shop(ID, ShopParams, Timestamp)};
make_shop_effect(_, {category_modification, Category}, _) ->
    {category_changed, Category};
make_shop_effect(_, {details_modification, Details}, _) ->
    {details_changed, Details};
make_shop_effect(_, ?shop_contract_modification(ContractID, PayoutToolID), _) ->
    {contract_changed, #payproc_ShopContractChanged{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }};
make_shop_effect(_, {payout_tool_modification, PayoutToolID}, _) ->
    {payout_tool_changed, PayoutToolID};
make_shop_effect(_, ?proxy_modification(Proxy), _) ->
    {proxy_changed, #payproc_ShopProxyChanged{proxy = Proxy}};
make_shop_effect(_, {location_modification, Location}, _) ->
    {location_changed, Location};
make_shop_effect(_, {shop_account_creation, Params}, _) ->
    {account_created, create_shop_account(Params)};
make_shop_effect(_, ?payout_schedule_modification(PayoutScheduleRef), _) ->
    ?payout_schedule_changed(PayoutScheduleRef).

make_wallet_effect(ID, {creation, Params}, Timestamp) ->
    {created, hg_wallet:create(ID, Params, Timestamp)};
make_wallet_effect(_, {account_creation, Params}, _) ->
    {account_created, hg_wallet:create_account(Params)}.

create_shop_account(#payproc_ShopAccountParams{currency = Currency}) ->
    create_shop_account(Currency);
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

