-module(hg_claim_committer_SUITE).

-include("claim_management.hrl").
-include("hg_ct_domain.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([party_creation/1]).
-export([contractor_one_creation/1]).
-export([contractor_two_creation/1]).
-export([contractor_modification/1]).
-export([contract_one_creation/1]).
-export([contract_two_creation/1]).
-export([contract_contractor_modification/1]).
-export([contract_adjustment_creation/1]).
-export([contract_legal_agreement_binding/1]).
-export([contract_report_preferences_modification/1]).
-export([shop_creation/1]).
-export([shop_complex_modification/1]).
-export([shop_contract_modification/1]).
-export([contract_termination/1]).
-export([contractor_already_exists/1]).
-export([contract_already_exists/1]).
-export([contract_already_terminated/1]).
-export([shop_already_exists/1]).

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-define(REAL_CONTRACTOR_ID1,  <<"CONTRACTOR2">>).
-define(REAL_CONTRACTOR_ID2,  <<"CONTRACTOR3">>).
-define(REAL_CONTRACT_ID1,    <<"CONTRACT2">>).
-define(REAL_CONTRACT_ID2,    <<"CONTRACT3">>).
-define(REAL_PAYOUT_TOOL_ID1, <<"PAYOUTTOOL2">>).
-define(REAL_PAYOUT_TOOL_ID2, <<"PAYOUTTOOL3">>).
-define(REAL_SHOP_ID,        <<"SHOP2">>).

%%% CT

-spec all() -> [test_case_name()].

all() ->
    [
        party_creation,
        contractor_one_creation,
        contractor_two_creation,
        contractor_modification,
        contract_one_creation,
        contract_two_creation,
        contract_contractor_modification,
        contract_adjustment_creation,
        contract_legal_agreement_binding,
        contract_report_preferences_modification,
        shop_creation,
        shop_complex_modification,
        shop_contract_modification,
        contract_termination,
        contractor_already_exists,
        contract_already_exists,
        contract_already_terminated,
        shop_already_exists
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([woody, scoper, dmt_client, party_client, hellgate]),
    RootUrl     = maps:get(hellgate_root_url, Ret),
    ok          = hg_domain:insert(construct_domain_fixture()),
    PartyID     = erlang:list_to_binary([?MODULE_STRING, ".", erlang:integer_to_list(erlang:system_time())]),
    ApiClient   = hg_ct_helper:create_client(RootUrl, PartyID),
    [{root_url, RootUrl}, {apps, Apps}, {party_id, PartyID}, {api_client, ApiClient} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

%%% Tests

-spec party_creation(config()) -> _.

party_creation(C) ->
    PartyID = cfg(party_id, C),
    ContactInfo = #domain_PartyContactInfo{email = <<?MODULE_STRING>>},
    ok = create_party(PartyID, ContactInfo, C),
    {ok, Party} = get_party(PartyID, C),
    #domain_Party{
        id           = PartyID,
        contact_info = ContactInfo,
        blocking     = {unblocked, #domain_Unblocked{}},
        suspension   = {active, #domain_Active{}},
        shops        = Shops,
        contracts    = Contracts
    } = Party,
    0 = maps:size(Shops),
    0 = maps:size(Contracts).

-spec contractor_one_creation(config()) -> _.

contractor_one_creation(C) ->
    ContractorParams = hg_ct_helper:make_battle_ready_contractor(),
    ContractorID = ?REAL_CONTRACTOR_ID1,
    Modifications = [
        ?cm_contractor_creation(ContractorID, ContractorParams)
    ],
    PartyID = cfg(party_id, C),
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, Party} = get_party(PartyID, C),
    #domain_PartyContractor{} = pm_party:get_contractor(ContractorID, Party).

-spec contractor_two_creation(config()) -> _.

contractor_two_creation(C) ->
    ContractorParams = hg_ct_helper:make_battle_ready_contractor(),
    ContractorID = ?REAL_CONTRACTOR_ID2,
    Modifications = [
        ?cm_contractor_creation(ContractorID, ContractorParams)
    ],
    PartyID = cfg(party_id, C),
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, Party} = get_party(PartyID, C),
    #domain_PartyContractor{} = pm_party:get_contractor(ContractorID, Party).

-spec contractor_modification(config()) -> _.

contractor_modification(C) ->
    ContractorID = ?REAL_CONTRACTOR_ID1,
    PartyID = cfg(party_id, C),
    {ok, Party1} = get_party(PartyID, C),
    #domain_PartyContractor{} = C1 = pm_party:get_contractor(ContractorID, Party1),
    Modifications = [
        ?cm_contractor_identification_level_modification(ContractorID, full)
    ],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, Party2} = get_party(PartyID, C),
    #domain_PartyContractor{} = C2 = pm_party:get_contractor(ContractorID, Party2),
    C1 /= C2 orelse error(same_contractor).

-spec contract_one_creation(config()) -> _.

contract_one_creation(C) ->
    ContractParams = make_contract_params(?REAL_CONTRACTOR_ID1),
    PayoutToolParams = make_payout_tool_params(),
    ContractID = ?REAL_CONTRACT_ID1,
    PayoutToolID1 = ?REAL_PAYOUT_TOOL_ID1,
    PayoutToolID2 = ?REAL_PAYOUT_TOOL_ID2,
    Modifications = [
        ?cm_contract_creation(ContractID, ContractParams),
        ?cm_contract_modification(ContractID, ?cm_payout_tool_creation(PayoutToolID1, PayoutToolParams)),
        ?cm_contract_modification(ContractID, ?cm_payout_tool_creation(PayoutToolID2, PayoutToolParams))
    ],
    PartyID = cfg(party_id, C),
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id = ContractID,
        payout_tools = PayoutTools
    }} = get_contract(PartyID, ContractID, C),
    true = lists:keymember(PayoutToolID1, #domain_PayoutTool.id, PayoutTools),
    true = lists:keymember(PayoutToolID2, #domain_PayoutTool.id, PayoutTools).

-spec contract_two_creation(config()) -> _.

contract_two_creation(C) ->
    ContractParams = make_contract_params(?REAL_CONTRACTOR_ID1),
    PayoutToolParams = make_payout_tool_params(),
    ContractID = ?REAL_CONTRACT_ID2,
    PayoutToolID1 = ?REAL_PAYOUT_TOOL_ID1,
    Modifications = [
        ?cm_contract_creation(ContractID, ContractParams),
        ?cm_contract_modification(ContractID, ?cm_payout_tool_creation(PayoutToolID1, PayoutToolParams))
    ],
    PartyID = cfg(party_id, C),
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id = ContractID,
        payout_tools = PayoutTools
    }} = get_contract(PartyID, ContractID, C),
    true = lists:keymember(PayoutToolID1, #domain_PayoutTool.id, PayoutTools).

-spec contract_contractor_modification(config()) -> _.

contract_contractor_modification(C) ->
    PartyID       = cfg(party_id, C),
    ContractID    = ?REAL_CONTRACT_ID2,
    NewContractor = ?REAL_CONTRACTOR_ID2,
    Modifications = [
        ?cm_contract_modification(ContractID, {contractor_modification, NewContractor})
    ],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id            = ContractID,
        contractor_id = NewContractor
    }} = get_contract(PartyID, ContractID, C).

-spec contract_adjustment_creation(config()) -> _.

contract_adjustment_creation(C) ->
    PartyID = cfg(party_id, C),
    ContractID = ?REAL_CONTRACT_ID1,
    ID = <<"ADJ1">>,
    AdjustmentTemplate = #domain_ContractTemplateRef{id = 2},
    Modifications = [?cm_contract_modification(ContractID, ?cm_adjustment_creation(ID, AdjustmentTemplate))],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    }} = get_contract(PartyID, ContractID, C),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments).

-spec contract_legal_agreement_binding(config()) -> _.

contract_legal_agreement_binding(C) ->
    PartyID = cfg(party_id, C),
    ContractID = ?REAL_CONTRACT_ID1,
    LA = #domain_LegalAgreement{
        signed_at = hg_datetime:format_now(),
        legal_agreement_id = <<"20160123-0031235-OGM/GDM">>
    },
    Changeset = [?cm_contract_modification(ContractID, {legal_agreement_binding, LA})],
    Claim = claim(Changeset, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id = ContractID,
        legal_agreement = LA
    }} = get_contract(PartyID, ContractID, C).

-spec contract_report_preferences_modification(config()) -> _.

contract_report_preferences_modification(C) ->
    PartyID = cfg(party_id, C),
    ContractID = ?REAL_CONTRACT_ID1,
    Pref1 = #domain_ReportPreferences{},
    Pref2 = #domain_ReportPreferences{
        service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
            schedule = ?bussched(1),
            signer = #domain_Representative{
                position  = <<"69">>,
                full_name = <<"Generic Name">>,
                document  = {articles_of_association, #domain_ArticlesOfAssociation{}}
            }
        }
    },
    Modifications = [
        ?cm_contract_modification(ContractID, {report_preferences_modification, Pref1}),
        ?cm_contract_modification(ContractID, {report_preferences_modification, Pref2})
    ],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id = ContractID,
        report_preferences = Pref2
    }} = get_contract(PartyID, ContractID, C).

-spec shop_creation(config()) -> _.

shop_creation(C) ->
    PartyID = cfg(party_id, C),
    Details = #domain_ShopDetails{
        name        = <<"SOME SHOP NAME">>,
        description = <<"Very meaningfull description of the shop.">>
    },
    Category = ?cat(2),
    Location = {url, <<"https://example.com">>},
    ContractID = ?REAL_CONTRACT_ID1,
    ShopID = ?REAL_SHOP_ID,
    PayoutToolID1 = ?REAL_PAYOUT_TOOL_ID1,
    ShopParams = #claim_management_ShopParams{
        category       = Category,
        location       = Location,
        details        = Details,
        contract_id    = ContractID,
        payout_tool_id = PayoutToolID1
    },
    Schedule = ?bussched(1),
    ScheduleParams = #claim_management_ScheduleModification{schedule = Schedule},
    Modifications = [
        ?cm_shop_creation(ShopID, ShopParams),
        ?cm_shop_account_creation(ShopID, ?cur(<<"RUB">>)),
        ?cm_shop_modification(ShopID, {payout_schedule_modification, ScheduleParams})
    ],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Shop{
        id = ShopID,
        details         = Details,
        location        = Location,
        category        = Category,
        account         = #domain_ShopAccount{currency = ?cur(<<"RUB">>)},
        contract_id     = ContractID,
        payout_tool_id  = PayoutToolID1,
        payout_schedule = Schedule
    }} = get_shop(PartyID, ShopID, C).

-spec shop_complex_modification(config()) -> _.

shop_complex_modification(C) ->
    PartyID = cfg(party_id, C),
    ShopID = ?REAL_SHOP_ID,
    NewCategory = ?cat(3),
    NewDetails = #domain_ShopDetails{
        name        = <<"UPDATED SHOP NAME">>,
        description = <<"Updated shop description.">>
    },
    NewLocation = {url, <<"http://localhost">>},
    PayoutToolID2 = ?REAL_PAYOUT_TOOL_ID2,
    Schedule = ?bussched(2),
    ScheduleParams = #claim_management_ScheduleModification{schedule = Schedule},
    Modifications = [
        ?cm_shop_modification(ShopID, {category_modification, NewCategory}),
        ?cm_shop_modification(ShopID, {details_modification, NewDetails}),
        ?cm_shop_modification(ShopID, {location_modification, NewLocation}),
        ?cm_shop_modification(ShopID, {payout_tool_modification, PayoutToolID2}),
        ?cm_shop_modification(ShopID, {payout_schedule_modification, ScheduleParams})
    ],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Shop{
        category        = NewCategory,
        details         = NewDetails,
        location        = NewLocation,
        payout_tool_id  = PayoutToolID2,
        payout_schedule = Schedule
    }} = get_shop(PartyID, ShopID, C).

-spec shop_contract_modification(config()) -> _.

shop_contract_modification(C) ->
    PartyID      = cfg(party_id, C),
    ShopID       = ?REAL_SHOP_ID,
    ContractID   = ?REAL_CONTRACT_ID2,
    PayoutToolID = ?REAL_PAYOUT_TOOL_ID1,
    ShopContractParams = #claim_management_ShopContractModification{
        contract_id    = ContractID,
        payout_tool_id = PayoutToolID
    },
    Modifications = [?cm_shop_modification(ShopID, {contract_modification, ShopContractParams})],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Shop{
        contract_id    = ContractID,
        payout_tool_id = PayoutToolID
    }} = get_shop(PartyID, ShopID, C).

-spec contract_termination(config()) -> _.

contract_termination(C) ->
    PartyID       = cfg(party_id, C),
    ContractID    = ?REAL_CONTRACT_ID1,
    Reason        = #claim_management_ContractTermination{reason = <<"Because!">>},
    Modifications = [?cm_contract_modification(ContractID, {termination, Reason})],
    Claim = claim(Modifications, PartyID),
    ok = accept_claim(Claim, C),
    ok = commit_claim(Claim, C),
    {ok, #domain_Contract{
        id     = ContractID,
        status = {terminated, _}
    }} = get_contract(PartyID, ContractID, C).

-spec contractor_already_exists(config()) -> _.

contractor_already_exists(C) ->
    ContractorParams = hg_ct_helper:make_battle_ready_contractor(),
    PartyID = cfg(party_id, C),
    ContractorID = ?REAL_CONTRACTOR_ID1,
    Modifications = [?cm_contractor_creation(ContractorID, ContractorParams)],
    Claim = claim(Modifications, PartyID),
    Reason = <<"{invalid_contractor,{payproc_InvalidContractor,<<\"", ContractorID/binary,
               "\">>,{already_exists,<<\"", ContractorID/binary, "\">>}}}">>,
    {exception, #claim_management_InvalidChangeset{
        reason = Reason
    }} = accept_claim(Claim, C).

-spec contract_already_exists(config()) -> _.

contract_already_exists(C) ->
    PartyID = cfg(party_id, C),
    ContractParams = make_contract_params(?REAL_CONTRACTOR_ID1),
    ContractID = ?REAL_CONTRACT_ID1,
    Modifications = [?cm_contract_creation(ContractID, ContractParams)],
    Claim = claim(Modifications, PartyID),
    Reason = <<"{invalid_contract,{payproc_InvalidContract,<<\"", ContractID/binary,
               "\">>,{already_exists,<<\"", ContractID/binary, "\">>}}}">>,
    {exception, #claim_management_InvalidChangeset{
        reason = Reason
    }} = accept_claim(Claim, C).

-spec contract_already_terminated(config()) -> _.

contract_already_terminated(C) ->
    ContractID    = ?REAL_CONTRACT_ID1,
    PartyID = cfg(party_id, C),
    Reason        = #claim_management_ContractTermination{reason = <<"Because!">>},
    Modifications = [?cm_contract_modification(ContractID, {termination, Reason})],
    Claim         = claim(Modifications, PartyID),
    ErrorReason   = <<"{invalid_contract,{payproc_InvalidContract,<<\"", ContractID/binary,
                      "\">>,{invalid_status,{terminated,{domain_ContractTerminated">>,
    ErrorReasonSize = erlang:byte_size(ErrorReason),
    {exception, #claim_management_InvalidChangeset{
        reason = <<ErrorReason:ErrorReasonSize/binary, _/binary>>
    }} = accept_claim(Claim, C).

-spec shop_already_exists(config()) -> _.

shop_already_exists(C) ->
    Details = #domain_ShopDetails{
        name        = <<"SOME SHOP NAME">>,
        description = <<"Very meaningfull description of the shop.">>
    },
    ShopID = ?REAL_SHOP_ID,
    PartyID = cfg(party_id, C),
    ShopParams = #claim_management_ShopParams{
        category       = ?cat(2),
        location       = {url, <<"https://example.com">>},
        details        = Details,
        contract_id    = ?REAL_CONTRACT_ID1,
        payout_tool_id = ?REAL_PAYOUT_TOOL_ID1
    },
    ScheduleParams = #claim_management_ScheduleModification{schedule = ?bussched(1)},
    Modifications = [
        ?cm_shop_creation(ShopID, ShopParams),
        ?cm_shop_account_creation(ShopID, ?cur(<<"RUB">>)),
        ?cm_shop_modification(ShopID, {payout_schedule_modification, ScheduleParams})
    ],
    Claim = claim(Modifications, PartyID),
    Reason = <<"{invalid_shop,{payproc_InvalidShop,<<\"", ShopID/binary,
               "\">>,{already_exists,<<\"", ShopID/binary, "\">>}}}">>,
    {exception, #claim_management_InvalidChangeset{
        reason = Reason
    }} = accept_claim(Claim, C).

%%% Internal functions

claim(PartyModifications, PartyID) ->
    UserInfo = #claim_management_UserInfo{
        id = <<"test">>,
        email = <<"test@localhost">>,
        username = <<"test">>,
        type = {internal_user, #claim_management_InternalUser{}}
    },
    #claim_management_Claim{
        id         = id(),
        party_id   = PartyID,
        status     = {pending, #claim_management_ClaimPending{}},
        changeset  = [?cm_party_modification(id(), ts(), Mod, UserInfo) || Mod <- PartyModifications],
        revision   = 1,
        created_at = ts()
    }.

id() ->
    erlang:unique_integer([positive, monotonic]).

ts() ->
    hg_datetime:format_now().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

call(Function, Args, C) ->
    ApiClient   = cfg(api_client, C),
    PartyID     = cfg(party_id, C),
    {Result, _} = pm_client_api:call(claim_committer, Function, [PartyID | Args], ApiClient),
    map_call_result(Result).

accept_claim(Claim, C) ->
    call('Accept', [Claim], C).

commit_claim(Claim, C) ->
    call('Commit', [Claim], C).

map_call_result({ok, ok}) ->
    ok;
map_call_result(Other) ->
    Other.

call_pm(Fun, Args, C) ->
    ApiClient   = cfg(api_client, C),
    {Result, _} = pm_client_api:call(party_management, Fun, [undefined | Args], ApiClient),
    map_call_result(Result).

create_party(PartyID, ContactInfo, C) ->
    Params = #payproc_PartyParams{contact_info = ContactInfo},
    call_pm('Create', [PartyID, Params], C).

get_party(PartyID, C) ->
    call_pm('Get', [PartyID], C).

get_contract(PartyID, ContractID, C) ->
    call_pm('GetContract', [PartyID, ContractID], C).

get_shop(PartyID, ShopID, C) ->
    call_pm('GetShop', [PartyID, ShopID], C).

make_contract_params(ContractorID) ->
    make_contract_params(ContractorID, undefined).

make_contract_params(ContractorID, TemplateRef) ->
    make_contract_params(ContractorID, TemplateRef, ?pinst(2)).

make_contract_params(ContractorID, TemplateRef, PaymentInstitutionRef) ->
    #claim_management_ContractParams{
        contractor_id       = ContractorID,
        template            = TemplateRef,
        payment_institution = PaymentInstitutionRef
    }.

make_payout_tool_params() ->
    #claim_management_PayoutToolParams{
        currency = ?cur(<<"RUB">>),
        tool_info = {russian_bank_account, #domain_RussianBankAccount{
            account = <<"4276300010908312893">>,
            bank_name = <<"SomeBank">>,
            bank_post_account = <<"123129876">>,
            bank_bik = <<"66642666">>
        }}
    }.

-spec construct_domain_fixture() -> [pm_domain:object()].

construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
            categories = {value, ordsets:from_list([?cat(1)])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(2),
                ?cat(3)
            ])},
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa)
            ])}
        }
    },
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            cash_limit = {value, #domain_CashRange{
                lower = {inclusive, #domain_Cash{amount = 1000, currency = ?cur(<<"RUB">>)}},
                upper = {exclusive, #domain_Cash{amount = 4200000, currency = ?cur(<<"RUB">>)}}
            }},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(45, 1000, operation_amount)
                )
            ]}
        },
        payouts = #domain_PayoutsServiceTerms{
            payout_methods = {decisions, [
                #domain_PayoutMethodDecision{
                    if_   = {condition, {payment_tool,
                        {bank_card, #domain_BankCardCondition{
                            definition = {issuer_bank_is, ?bank(1)}
                        }}
                    }},
                    then_ = {value, ordsets:from_list([?pomt(russian_bank_account), ?pomt(international_bank_account)])}
                },
                #domain_PayoutMethodDecision{
                    if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                        definition = {empty_cvv_is, true}
                    }}}},
                    then_ = {value, ordsets:from_list([])}
                },
                #domain_PayoutMethodDecision{
                    if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{}}}},
                    then_ = {value, ordsets:from_list([?pomt(russian_bank_account)])}
                },
                #domain_PayoutMethodDecision{
                    if_   = {condition, {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{}}}},
                    then_ = {value, ordsets:from_list([?pomt(international_bank_account)])}
                },
                #domain_PayoutMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([])}
                }
            ]},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {merchant, payout},
                    ?share(750, 1000, operation_amount)
                ),
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(250, 1000, operation_amount)
                )
            ]}
        },
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])}
        }
    },
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        hg_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, maestro)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(empty_cvv_bank_card, visa)),

        hg_ct_fixture:construct_payout_method(?pomt(russian_bank_account)),
        hg_ct_fixture:construct_payout_method(?pomt(international_bank_account)),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_system_account_set(?sas(2)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        hg_ct_fixture:construct_business_schedule(?bussched(1)),
        hg_ct_fixture:construct_business_schedule(?bussched(2)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = test
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(3),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, ?eas(1)},
                payment_institutions = ?ordset([?pinst(1), ?pinst(2)])
            }
        }},
        hg_ct_fixture:construct_contract_template(
            ?tmpl(1),
            ?trms(1)
        ),
        hg_ct_fixture:construct_contract_template(
            ?tmpl(2),
            ?trms(3)
        ),
        hg_ct_fixture:construct_contract_template(
            ?tmpl(3),
            ?trms(2),
            {interval, #domain_LifetimeInterval{years = -1}},
            {interval, #domain_LifetimeInterval{days = -1}}
        ),
        hg_ct_fixture:construct_contract_template(
            ?tmpl(4),
            ?trms(1),
            undefined,
            {interval, #domain_LifetimeInterval{months = 1}}
        ),
        hg_ct_fixture:construct_contract_template(
            ?tmpl(5),
            ?trms(4)
        ),
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TestTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = DefaultTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(2),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(4),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(3),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            currencies = {value, ordsets:from_list([
                                ?cur(<<"RUB">>)
                            ])},
                            categories = {value, ordsets:from_list([
                                ?cat(2)
                            ])},
                            payment_methods = {value, ordsets:from_list([
                                ?pmt(bank_card, visa)
                            ])}
                        }
                    }
                }]
            }
        }},
        {bank, #domain_BankObject{
            ref = ?bank(1),
            data = #domain_Bank {
                name = <<"Test BIN range">>,
                description = <<"Test BIN range">>,
                bins = ordsets:from_list([<<"1234">>, <<"5678">>])
            }
        }}
    ].
