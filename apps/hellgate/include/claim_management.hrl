-ifndef(__hellgate_claim_management__).
-define(__hellgate_claim_management__, included).

-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-define(cm_modification_unit(ModID, Timestamp, Mod), #claim_management_ModificationUnit{
    modification_id = ModID,
    created_at      = Timestamp,
    modification    = Mod
}).

-define(
    cm_party_modification(ModID, Timestamp, Mod),
    ?cm_modification_unit(ModID, Timestamp, {party_modification, Mod})
).

%%% Contractor

-define(
    cm_contractor_modification(ContractorID, Mod),
    {contractor_modification, #claim_management_ContractorModificationUnit{
        id           = ContractorID,
        modification = Mod
    }}
).

-define(
    cm_contractor_creation(ContractorID, Contractor),
    ?cm_contractor_modification(ContractorID, {creation, Contractor})
).

-define(
    cm_identity_documents_modification(Documents),
    {
        identity_documents_modification,
        #claim_management_ContractorIdentityDocumentsModification{
            identity_documents = Documents
        }
    }
).

-define(
    cm_contractor_identity_documents_modification(ContractorID, Documents),
    ?cm_contractor_modification(ContractorID, ?cm_identity_documents_modification(Documents))
).

-define(
    cm_contractor_identification_level_modification(ContractorID, Level),
    ?cm_contractor_modification(ContractorID, {identification_level_modification, Level})
).

%%% Contract

-define(
    cm_contract_modification(ContractID, Mod),
    {contract_modification, #claim_management_ContractModificationUnit{
        id           = ContractID,
        modification = Mod
    }}
).

-define(
    cm_contract_creation(ContractID, ContractParams),
    ?cm_contract_modification(ContractID, {creation, ContractParams})
).

-define(cm_contract_termination(Reason),
    {termination, #claim_management_ContractTermination{reason = Reason}}).

-define(
    cm_payout_tool_modification(PayoutToolID, Mod),
    {payout_tool_modification, #claim_management_PayoutToolModificationUnit{
        payout_tool_id = PayoutToolID,
        modification   = Mod
    }}
).

-define(
    cm_payout_tool_creation(PayoutToolID, PayoutToolParams),
    ?cm_payout_tool_modification(PayoutToolID, {creation, PayoutToolParams})
).

-define(
    cm_payout_tool_info_modification(PayoutToolID, Info),
    ?cm_payout_tool_modification(PayoutToolID, {info_modification, Info})
).

-define(
    cm_adjustment_modification(ContractAdjustmentID, Mod),
    {adjustment_modification, #claim_management_ContractAdjustmentModificationUnit{
        adjustment_id = ContractAdjustmentID,
        modification  = Mod
    }}
).

-define(
    cm_adjustment_creation(ContractAdjustmentID, ContractAdjustmentParams),
    ?cm_adjustment_modification(ContractAdjustmentID, {creation, ContractAdjustmentParams})
).

%%% Shop

-define(
    cm_shop_modification(ShopID, Mod),
    {shop_modification, #claim_management_ShopModificationUnit{
        id           = ShopID,
        modification = Mod
    }}
).

-define(
    cm_shop_contract_modification(ContractID, PayoutToolID),
    {contract_modification, #claim_management_ShopContractModification{
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    }}
).

-define(
    cm_shop_creation(ShopID, ShopParams),
    ?cm_shop_modification(ShopID, {creation, ShopParams})
).


-define(
    cm_shop_account_creation(ShopID, ShopAccountParams),
    ?cm_shop_modification(ShopID, {shop_account_creation, ShopAccountParams})
).

-endif.