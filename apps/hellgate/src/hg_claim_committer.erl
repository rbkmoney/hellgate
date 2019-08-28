-module(hg_claim_committer).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-export([from_claim_mgmt/1]).

-spec from_claim_mgmt(dmsl_claim_management_thrift:'Claim'()) ->
    dmsl_payment_processing_thrift:'Claim'().

from_claim_mgmt(#claim_management_Claim{
    id         = ID,
    status     = Status,
    changeset  = Changeset,
    revision   = Revision,
    created_at = CreatedAt,
    updated_at = UpdatedAt
}) ->
    #payproc_Claim{
        id         = ID,
        status     = from_cm_status(Status),
        changeset  = from_cm_changeset(Changeset),
        revision   = Revision,
        created_at = CreatedAt,
        updated_at = UpdatedAt
    }.

%%% Internal functions

from_cm_status({pending, #claim_management_ClaimPending{}}) ->
    {pending, #payproc_ClaimPending{}};
from_cm_status({review, #claim_management_ClaimReview{}}) ->
    {pending, #payproc_ClaimPending{}};
from_cm_status({pending_acceptance, #claim_management_ClaimPendingAcceptance{}}) ->
    {pending, #payproc_ClaimPending{}};
from_cm_status({accepted, #claim_management_ClaimAccepted{}}) ->
    {pending, #payproc_ClaimAccepted{}};
from_cm_status({denied, #claim_management_ClaimDenied{reason = Reason}}) ->
    {pending, #payproc_ClaimDenied{reason = Reason}};
from_cm_status({revoked, #claim_management_ClaimRevoked{reason = Reason}}) ->
    {pending, #payproc_ClaimRevoked{reason = Reason}}.

from_cm_changeset(Changeset) ->
    [from_cm_party_mod(PartyMod) ||
     #claim_management_ModificationUnit{
        modification = {party_modification, PartyMod}
     } <- Changeset].

from_cm_party_mod({contractor_modification, Mod}) ->
    #claim_management_ContractorModificationUnit{
        id           = ContractorID,
        modification = ContractorModification
    } = Mod,
    NewContractorModification =
        case ContractorModification of
            {identity_documents_modification,
            #claim_management_ContractorIdentityDocumentsModification{
                identity_documents = Documents
            }} ->
                {identity_documents_modification,
                #payproc_ContractorIdentityDocumentsModification{
                    identity_documents = Documents
                }};
            Other ->
                Other
        end,
    {contractor_modification, #payproc_ContractorModificationUnit{
         id           = ContractorID,
         modification = NewContractorModification
    }};
from_cm_party_mod({contract_modification, Mod}) ->
    #claim_management_ContractModificationUnit{
        id           = ContractID,
        modification = ContractModification
    } = Mod,
    {contract_modification, #payproc_ContractModificationUnit{
        id           = ContractID,
        modification = from_cm_contract_modification(ContractModification)
    }};
from_cm_party_mod({shop_modification, Mod}) ->
    #claim_management_ShopModificationUnit{
        id           = ShopID,
        modification = ShopModification
    } = Mod,
    {shop_modification, #payproc_ShopModificationUnit{
        id           = ShopID,
        modification = from_cm_shop_modification(ShopModification)
    }}.

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
from_cm_contract_modification(
    {termination, #claim_management_ContractTermination{
        reason = Reason
    }}
) ->
    {termination, #payproc_ContractTermination{
        reason = Reason
    }};
from_cm_contract_modification(
    {adjustment_modification, #claim_management_ContractAdjustmentModificationUnit{
        adjustment_id = ContractAdjustmentID,
        modification = {creation, #claim_management_ContractAdjustmentParams{
            template = ContractTemplateRef
        }}
    }}
) ->
    {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
        adjustment_id = ContractAdjustmentID,
        modification = {creation, #payproc_ContractAdjustmentParams{
            template = ContractTemplateRef
        }}
    }};
from_cm_contract_modification(
    {payout_tool_modification, #claim_management_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification   = PayoutToolModification
    }}
) ->
    NewPayoutToolModification =
        case PayoutToolModification of
            {creation, #claim_management_PayoutToolParams{
                currency  = CurrencyRef,
                tool_info = PayoutToolInfo
            }} ->
                {creation, #payproc_PayoutToolParams{
                    currency  = CurrencyRef,
                    tool_info = PayoutToolInfo
                }};
            {info_modification, _PayoutToolInfo} = InfoModification ->
                InfoModification
        end,
    {payout_tool_modification, #payproc_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification = NewPayoutToolModification
    }};
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
from_cm_shop_modification({contract_modification, ShopContractModification}) ->
    #claim_management_ShopContractModification{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    } = ShopContractModification,
    {contract_modification, #payproc_ShopContractModification{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }};
from_cm_shop_modification({payout_tool_modification, _PayoutToolID} = PayoutToolModification) ->
    PayoutToolModification;
from_cm_shop_modification({location_modification, _ShopLocation} = LocationModification) ->
    LocationModification;
from_cm_shop_modification({shop_account_creation, ShopAccountParams}) ->
    #claim_management_ShopAccountParams{
        currency = CurrencyRef
    } = ShopAccountParams,
    {shop_account_creation, #payproc_ShopAccountParams{
        currency = CurrencyRef
    }};
from_cm_shop_modification({payout_schedule_modification, ScheduleModification}) ->
    #claim_management_ScheduleModification{
        schedule = BusinessScheduleRef
    } = ScheduleModification,
    {payout_schedule_modification, #payproc_ScheduleModification{
        schedule = BusinessScheduleRef
    }}.
