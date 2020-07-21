-module(pm_claim_effect).

-include("party_events.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([make/3]).
-export([make_safe/3]).

-export_type([effect/0]).

%% Interface

-type change()      :: dmsl_payment_processing_thrift:'PartyModification'().
-type effect()      :: dmsl_payment_processing_thrift:'ClaimEffect'().
-type timestamp()   :: pm_datetime:timestamp().
-type revision()    :: pm_domain:revision().

-spec make(change(), timestamp(), revision()) -> effect() | no_return().

make(?contractor_modification(ID, Modification), Timestamp, Revision) ->
    ?contractor_effect(ID, make_contractor_effect(ID, Modification, Timestamp, Revision));

make(?contract_modification(ID, Modification), Timestamp, Revision) ->
    try
        ?contract_effect(ID, make_contract_effect(ID, Modification, Timestamp, Revision))
    catch
        throw:{payment_institution_invalid, Ref} ->
            raise_invalid_object_ref({contract, ID}, make_optional_domain_ref(payment_institution, Ref));
        throw:{template_invalid, Ref} ->
            raise_invalid_object_ref({contract, ID}, make_optional_domain_ref(contract_template, Ref))
    end;

make(?shop_modification(ID, Modification), Timestamp, Revision) ->
    ?shop_effect(ID, make_shop_effect(ID, Modification, Timestamp, Revision));

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
    ?wallet_effect(ID, {account_created, pm_wallet:create_fake_account(Params)});
make_safe(Change, Timestamp, Revision) ->
    make(Change, Timestamp, Revision).

%% Implementation

make_contractor_effect(ID, {creation, Contractor}, _, _) ->
    {created, pm_party_contractor:create(ID, Contractor)};
make_contractor_effect(_, {identification_level_modification, Level}, _, _) ->
    {identification_level_changed, Level};
make_contractor_effect(_, ?identity_documents_modification(Docs), _, _) ->
    {identity_documents_changed, #payproc_ContractorIdentityDocumentsChanged{
        identity_documents = Docs
    }}.

make_contract_effect(ID, {creation, ContractParams}, Timestamp, Revision) ->
    {created, pm_contract:create(ID, ContractParams, Timestamp, Revision)};
make_contract_effect(_, ?contract_termination(_), Timestamp, _) ->
    {status_changed, {terminated, #domain_ContractTerminated{terminated_at = Timestamp}}};
make_contract_effect(_, ?adjustment_creation(AdjustmentID, Params), Timestamp, Revision) ->
    {adjustment_created, pm_contract:create_adjustment(AdjustmentID, Params, Timestamp, Revision)};
make_contract_effect(_, ?payout_tool_creation(PayoutToolID, Params), Timestamp, _) ->
    {payout_tool_created, pm_payout_tool:create(PayoutToolID, Params, Timestamp)};
make_contract_effect(_, ?payout_tool_info_modification(PayoutToolID, Info), _, _) ->
    {payout_tool_info_changed, #payproc_PayoutToolInfoChanged{
        payout_tool_id = PayoutToolID,
        info = Info
    }};
make_contract_effect(_, {legal_agreement_binding, LegalAgreement}, _, _) ->
    {legal_agreement_bound, LegalAgreement};
make_contract_effect(ID, {report_preferences_modification, ReportPreferences}, _, Revision) ->
    _ = assert_report_schedule_valid(ID, ReportPreferences, Revision),
    {report_preferences_changed, ReportPreferences};
make_contract_effect(_, {contractor_modification, ContractorID}, _, _) ->
    {contractor_changed, ContractorID}.

make_shop_effect(ID, {creation, ShopParams}, Timestamp, _) ->
    {created, pm_party:create_shop(ID, ShopParams, Timestamp)};
make_shop_effect(_, {category_modification, Category}, _, _) ->
    {category_changed, Category};
make_shop_effect(_, {details_modification, Details}, _, _) ->
    {details_changed, Details};
make_shop_effect(_, ?shop_contract_modification(ContractID, PayoutToolID), _, _) ->
    {contract_changed, #payproc_ShopContractChanged{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }};
make_shop_effect(_, {payout_tool_modification, PayoutToolID}, _, _) ->
    {payout_tool_changed, PayoutToolID};
make_shop_effect(_, ?proxy_modification(Proxy), _, _) ->
    {proxy_changed, #payproc_ShopProxyChanged{proxy = Proxy}};
make_shop_effect(_, {location_modification, Location}, _, _) ->
    {location_changed, Location};
make_shop_effect(_, {shop_account_creation, Params}, _, _) ->
    {account_created, create_shop_account(Params)};
make_shop_effect(ID, ?payout_schedule_modification(PayoutScheduleRef), _, Revision) ->
    _ = assert_payout_schedule_valid(ID, PayoutScheduleRef, Revision),
    ?payout_schedule_changed(PayoutScheduleRef).

make_wallet_effect(ID, {creation, Params}, Timestamp) ->
    {created, pm_wallet:create(ID, Params, Timestamp)};
make_wallet_effect(_, {account_creation, Params}, _) ->
    {account_created, pm_wallet:create_account(Params)}.

assert_report_schedule_valid(_, #domain_ReportPreferences{service_acceptance_act_preferences = undefined}, _) ->
    ok;
assert_report_schedule_valid(
    ID,
    #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = BusinessScheduleRef
        }
    },
    Revision
) ->
    assert_valid_object_ref({contract, ID}, {business_schedule, BusinessScheduleRef}, Revision).

assert_payout_schedule_valid(ID, #domain_BusinessScheduleRef{} = BusinessScheduleRef, Revision) ->
    assert_valid_object_ref({shop, ID}, {business_schedule, BusinessScheduleRef}, Revision);
assert_payout_schedule_valid(_, undefined, _) ->
    ok.

assert_valid_object_ref(Prefix, Ref, Revision) ->
    case pm_domain:exists(Revision, Ref) of
        true ->
            ok;
        false ->
            raise_invalid_object_ref(Prefix, Ref)
    end.

-spec raise_invalid_object_ref(
    {shop, dmsl_domain_thrift:'ShopID'()} | {contract, dmsl_domain_thrift:'ContractID'()},
    pm_domain:ref()
) ->
    no_return().

raise_invalid_object_ref(Prefix, Ref) ->
    Ex = {invalid_object_reference, #payproc_InvalidObjectReference{ref = Ref}},
    raise_invalid_object_ref_(Prefix, Ex).

-spec raise_invalid_object_ref_(term(), term()) -> no_return().

raise_invalid_object_ref_({shop, ID}, Ex) ->
    pm_claim:raise_invalid_changeset(?invalid_shop(ID, Ex));
raise_invalid_object_ref_({contract, ID}, Ex) ->
    pm_claim:raise_invalid_changeset(?invalid_contract(ID, Ex)).

create_shop_account(#payproc_ShopAccountParams{currency = Currency}) ->
    create_shop_account(Currency);
create_shop_account(#domain_CurrencyRef{symbolic_code = _SymbolicCode} = CurrencyRef) ->
    % WARNING ADHOC FOR TESTING ONLY
    GuaranteeID = rand:uniform(1000), %pm_accounting:create_account(SymbolicCode),
    SettlementID = rand:uniform(1000), %pm_accounting:create_account(SymbolicCode),
    PayoutID = rand:uniform(1000), %pm_accounting:create_account(SymbolicCode),
    #domain_ShopAccount{
        currency = CurrencyRef,
        settlement = SettlementID,
        guarantee = GuaranteeID,
        payout = PayoutID
    }.

make_optional_domain_ref(_, undefined) ->
    undefined;
make_optional_domain_ref(Type, Ref) ->
    {Type, Ref}.
