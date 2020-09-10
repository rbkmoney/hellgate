-module(pm_claim_committer).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").
-include("claim_management.hrl").
-include("party_events.hrl").

-export([from_claim_mgmt/1]).

-spec from_claim_mgmt(dmsl_claim_management_thrift:'Claim'()) ->
    dmsl_payment_processing_thrift:'Claim'().

from_claim_mgmt(#claim_management_Claim{
    id         = ID,
    changeset  = Changeset,
    revision   = Revision,
    created_at = CreatedAt,
    updated_at = UpdatedAt
}) ->
    #payproc_Claim{
        id         = ID,
        status     = ?pending(),
        changeset  = from_cm_changeset(Changeset),
        revision   = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    }.

%%% Internal functions

from_cm_changeset(Changeset) ->
    lists:foldr(
        fun (#claim_management_ModificationUnit{
                modification = {party_modification, PartyMod}
            }, Acc) ->
            maybe_apply_change(PartyMod, Acc)
        end,
        [],
        Changeset
    ).

maybe_apply_change(?cm_shop_modification(_ShopID, {cash_register_modification_unit, _}), Acc) ->
    %% Filter out unsupported modification
    Acc;
maybe_apply_change(PartyMod, Acc) ->
    [from_cm_party_mod(PartyMod) | Acc].

from_cm_party_mod(?cm_contractor_modification(ContractorID, ContractorModification)) ->
    ?contractor_modification(ContractorID, ContractorModification);
from_cm_party_mod(?cm_contract_modification(ContractID, ContractModification)) ->
    ?contract_modification(
        ContractID,
        from_cm_contract_modification(ContractModification)
    );
from_cm_party_mod(?cm_shop_modification(ShopID, ShopModification)) ->
    ?shop_modification(
        ShopID,
        from_cm_shop_modification(ShopModification)
    ).

from_cm_contract_modification(
    {creation, #claim_management_ContractParams{
        contractor_id       = ContractorID,
        template            = ContractTemplateRef,
        payment_institution = PaymentInstitutionRef
    }}
) ->
    {creation, #payproc_ContractParams{
        contractor_id       = ContractorID,
        template            = ContractTemplateRef,
        payment_institution = PaymentInstitutionRef
    }};
from_cm_contract_modification(?cm_contract_termination(Reason)) ->
    ?contract_termination(Reason);
from_cm_contract_modification(?cm_adjustment_creation(ContractAdjustmentID, ContractTemplateRef)
) ->
    ?adjustment_creation(
        ContractAdjustmentID,
        #payproc_ContractAdjustmentParams{template = ContractTemplateRef}
    );
from_cm_contract_modification(
    ?cm_payout_tool_creation(PayoutToolID, #claim_management_PayoutToolParams{
        currency  = CurrencyRef,
        tool_info = PayoutToolInfo
    })
) ->
    ?payout_tool_creation(PayoutToolID, #payproc_PayoutToolParams{
        currency  = CurrencyRef,
        tool_info = PayoutToolInfo
    });
from_cm_contract_modification(
    ?cm_payout_tool_info_modification(PayoutToolID, PayoutToolModification)
) ->
    ?payout_tool_info_modification(PayoutToolID, PayoutToolModification);
from_cm_contract_modification({legal_agreement_binding, _LegalAgreement} = LegalAgreementBinding) ->
    LegalAgreementBinding;
from_cm_contract_modification({report_preferences_modification, _ReportPreferences} = ReportPreferencesModification) ->
    ReportPreferencesModification;
from_cm_contract_modification({contractor_modification, _ContractorID} = ContractorModification) ->
    ContractorModification.

from_cm_shop_modification({creation, ShopParams}) ->
    #claim_management_ShopParams{
        category       = CategoryRef,
        location       = ShopLocation,
        details        = ShopDetails,
        contract_id    = ContractID,
        payout_tool_id = PayoutToolID
    } = ShopParams,
    {creation, #payproc_ShopParams{
        category       = CategoryRef,
        location       = ShopLocation,
        details        = ShopDetails,
        contract_id    = ContractID,
        payout_tool_id = PayoutToolID
    }};
from_cm_shop_modification({category_modification, _CategoryRef} = CategoryModification) ->
    CategoryModification;
from_cm_shop_modification({details_modification, _ShopDetails} = DetailsModification) ->
    DetailsModification;
from_cm_shop_modification(?cm_shop_contract_modification(ContractID, PayoutToolID)) ->
    ?shop_contract_modification(ContractID, PayoutToolID);
from_cm_shop_modification({payout_tool_modification, _PayoutToolID} = PayoutToolModification) ->
    PayoutToolModification;
from_cm_shop_modification({location_modification, _ShopLocation} = LocationModification) ->
    LocationModification;
from_cm_shop_modification(?cm_shop_account_creation_params(CurrencyRef)) ->
    ?shop_account_creation_params(CurrencyRef);
from_cm_shop_modification(?cm_payout_schedule_modification(BusinessScheduleRef)) ->
    ?payout_schedule_modification(BusinessScheduleRef).
