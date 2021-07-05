-ifndef(__pm_legacy_party_structures_hrl__).
-define(__pm_legacy_party_structures_hrl__, included).

-define(legacy_party_created(Party),
    {party_created, Party}
).

-define(legacy_party(ID, ContactInfo, CreatedAt, Blocking, Suspension, Contracts, Shops),
    {domain_Party, ID, ContactInfo, CreatedAt, Blocking, Suspension, Contracts, Shops}
).

-define(legacy_claim(
    ID,
    Status,
    Changeset,
    Revision,
    CreatedAt,
    UpdatedAt
),
    {payproc_Claim, ID, Status, Changeset, Revision, CreatedAt, UpdatedAt}
).

-define(legacy_claim_updated(ID, Changeset, ClaimRevision, Timestamp),
    {claim_updated, {payproc_ClaimUpdated, ID, Changeset, ClaimRevision, Timestamp}}
).

-define(legacy_contract_modification(ID, Modification),
    {contract_modification, {payproc_ContractModificationUnit, ID, Modification}}
).

-define(legacy_contract_params_v1(Contractor, TemplateRef),
    {payproc_ContractParams, Contractor, TemplateRef}
).

-define(legacy_contract_params_v2(Contractor, TemplateRef, PaymentInstitutionRef),
    {payproc_ContractParams, Contractor, TemplateRef, PaymentInstitutionRef}
).

-define(legacy_contract_params_v3_4(Contractor, TemplateRef, PaymentInstitutionRef),
    {payproc_ContractParams, Contractor, TemplateRef, PaymentInstitutionRef}
).

-define(legacy_payout_tool_creation(ID, Params),
    {payout_tool_modification, {payproc_PayoutToolModificationUnit, ID, {creation, Params}}}
).

-define(legacy_payout_tool_params(Currency, PayoutToolInfo),
    {payproc_PayoutToolParams, Currency, PayoutToolInfo}
).

-define(legacy_russian_legal_entity(
    RegisteredName,
    RegisteredNumber,
    Inn,
    ActualAddress,
    PostAddress,
    RepresentativePosition,
    RepresentativeFullName,
    RepresentativeDocument,
    BankAccount
),
    {domain_RussianLegalEntity, RegisteredName, RegisteredNumber, Inn, ActualAddress, PostAddress,
        RepresentativePosition, RepresentativeFullName, RepresentativeDocument, BankAccount}
).

-define(legacy_international_legal_entity(LegalName, TradingName, RegisteredAddress, ActualAddress),
    {domain_InternationalLegalEntity, LegalName, TradingName, RegisteredAddress, ActualAddress}
).

-define(legacy_international_legal_entity_v2(
    LegalName,
    TradingName,
    RegisteredAddress,
    ActualAddress,
    RegisteredNumber
),
    {domain_InternationalLegalEntity, LegalName, TradingName, RegisteredAddress, ActualAddress, RegisteredNumber}
).

-define(legacy_bank_account(Account, BankName, BankPostAccount, BankBik),
    {domain_BankAccount, Account, BankName, BankPostAccount, BankBik}
).

-define(legacy_international_bank_account(AccountHolder, BankName, BankAddress, Iban, Bic),
    {domain_InternationalBankAccount, AccountHolder, BankName, BankAddress, Iban, Bic}
).

-define(legacy_international_bank_account_v3_4_5(AccountHolder, BankName, BankAddress, Iban, Bic, LocalBankCode),
    {domain_InternationalBankAccount, AccountHolder, BankName, BankAddress, Iban, Bic, LocalBankCode}
).

-define(legacy_shop_modification(ID, Modification),
    {shop_modification, {payproc_ShopModificationUnit, ID, Modification}}
).

-define(legacy_schedule_modification(PayoutScheduleRef),
    {payproc_ScheduleModification, PayoutScheduleRef}
).

-define(legacy_shop_effect(ID, Effect),
    {shop_effect, {payproc_ShopEffectUnit, ID, Effect}}
).

-define(legacy_shop_v2(
    ID,
    CreatedAt,
    Blocking,
    Suspension,
    Details,
    Location,
    Category,
    Account,
    ContractID,
    PayoutToolID
),
    {domain_Shop, ID, CreatedAt, Blocking, Suspension, Details, Location, Category, Account, ContractID, PayoutToolID}
).

-define(legacy_shop_v3(
    ID,
    CreatedAt,
    Blocking,
    Suspension,
    Details,
    Location,
    Category,
    Account,
    ContractID,
    PayoutToolID,
    PayoutScheduleRef
),
    {domain_Shop, ID, CreatedAt, Blocking, Suspension, Details, Location, Category, Account, ContractID, PayoutToolID,
        PayoutScheduleRef}
).

-define(legacy_payout_schedule_ref(ID),
    {domain_PayoutScheduleRef, ID}
).

-define(legacy_schedule_changed(PayoutScheduleRef),
    {payproc_ScheduleChanged, PayoutScheduleRef}
).

-define(legacy_contract_effect(ID, Effect),
    {contract_effect, {payproc_ContractEffectUnit, ID, Effect}}
).

-define(legacy_contract_v1(
    ID,
    Contractor,
    CreatedAt,
    ValidSince,
    ValidUntil,
    Status,
    Terms,
    Adjustments,
    PayoutTools,
    LegalAgreement
),
    {domain_Contract, ID, Contractor, CreatedAt, ValidSince, ValidUntil, Status, Terms, Adjustments, PayoutTools,
        LegalAgreement}
).

-define(legacy_contract_v2_3(
    ID,
    Contractor,
    PaymentInstitutionRef,
    CreatedAt,
    ValidSince,
    ValidUntil,
    Status,
    Terms,
    Adjustments,
    PayoutTools,
    LegalAgreement
),
    {domain_Contract, ID, Contractor, PaymentInstitutionRef, CreatedAt, ValidSince, ValidUntil, Status, Terms,
        Adjustments, PayoutTools, LegalAgreement}
).

-define(legacy_contract_v4(
    ID,
    Contractor,
    PaymentInstitutionRef,
    CreatedAt,
    ValidSince,
    ValidUntil,
    Status,
    Terms,
    Adjustments,
    PayoutTools,
    LegalAgreement,
    ReportPreferences
),
    {domain_Contract, ID, Contractor, PaymentInstitutionRef, CreatedAt, ValidSince, ValidUntil, Status, Terms,
        Adjustments, PayoutTools, LegalAgreement, ReportPreferences}
).

-define(legacy_payout_tool(
    ID,
    CreatedAt,
    Currency,
    PayoutToolInfo
),
    {domain_PayoutTool, ID, CreatedAt, Currency, PayoutToolInfo}
).

-define(legacy_legal_agreement(
    SignedAt,
    LegalAgreementID
),
    {domain_LegalAgreement, SignedAt, LegalAgreementID}
).

-define(legacy_st(Party, Timestamp, Claims, Meta, MigrationData, LastEvent),
    {st,
        % undefined | party()
        Party,
        % undefined | timestamp()
        Timestamp,
        % #{claim_id() => claim()}
        Claims,
        % meta()
        Meta,
        % NOTE
        % This is a part of persisted state of almost every party machine out there.
        % Good news is this field was never really used which means it is just `#{}`
        % all the time.
        MigrationData,
        % event_id()
        LastEvent}
).

-endif.
