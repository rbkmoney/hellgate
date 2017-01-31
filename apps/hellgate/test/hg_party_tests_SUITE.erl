-module(hg_party_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([party_creation/1]).
-export([party_not_found_on_retrieval/1]).
-export([party_already_exists/1]).
-export([party_retrieval/1]).

-export([claim_already_accepted_on_accept/1]).
-export([claim_already_accepted_on_deny/1]).
-export([claim_already_accepted_on_revoke/1]).
-export([claim_acceptance/1]).
-export([claim_denial/1]).
-export([claim_revocation/1]).
-export([claim_not_found_on_retrieval/1]).
-export([no_pending_claim/1]).
-export([complex_claim_acceptance/1]).

-export([party_revisioning/1]).
-export([party_blocking/1]).
-export([party_unblocking/1]).
-export([party_already_blocked/1]).
-export([party_already_unblocked/1]).
-export([party_blocked_on_suspend/1]).
-export([party_suspension/1]).
-export([party_activation/1]).
-export([party_already_suspended/1]).
-export([party_already_active/1]).

-export([shop_not_found_on_retrieval/1]).
-export([shop_creation/1]).
-export([shop_update/1]).
-export([shop_update_before_confirm/1]).
-export([shop_update_with_bad_params/1]).
-export([shop_blocking/1]).
-export([shop_unblocking/1]).
-export([shop_already_blocked/1]).
-export([shop_already_unblocked/1]).
-export([shop_blocked_on_suspend/1]).
-export([shop_suspension/1]).
-export([shop_activation/1]).
-export([shop_already_suspended/1]).
-export([shop_already_active/1]).

-export([shop_account_set_retrieval/1]).
-export([shop_account_retrieval/1]).

-export([consistent_history/1]).

-export([party_access_control/1]).

-export([contract_not_found/1]).
-export([contract_creation/1]).
-export([contract_termination/1]).
-export([contract_expiration/1]).
-export([contract_legal_agreement_binding/1]).
-export([contract_payout_tool_creation/1]).
-export([contract_adjustment_creation/1]).
-export([contract_adjustment_expiration/1]).

%%

-define(c(Key, C), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-spec all() -> [{group, group_name()}].

all() ->
    [
        {group, party_access_control},
        {group, party_creation},
        {group, party_revisioning},
        {group, party_blocking_suspension},
        {group, contract_management},
        {group, shop_management},
        {group, shop_account_lazy_creation},

        {group, claim_management},

        {group, consistent_history}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {party_creation, [sequence], [
            party_not_found_on_retrieval,
            party_creation,
            party_already_exists,
            party_retrieval
        ]},
        {party_access_control, [sequence], [
            party_creation,
            party_access_control
        ]},
        {party_revisioning, [sequence], [
            party_creation,
            party_revisioning
        ]},
        {party_blocking_suspension, [sequence], [
            party_creation,
            party_blocking,
            party_already_blocked,
            party_blocked_on_suspend,
            party_unblocking,
            party_already_unblocked,
            party_suspension,
            party_already_suspended,
            party_blocking,
            party_unblocking,
            party_activation,
            party_already_active
        ]},
        {contract_management, [sequence], [
            party_creation,
            contract_not_found,
            contract_creation,
            contract_termination,
            contract_expiration,
            contract_legal_agreement_binding,
            contract_payout_tool_creation,
            contract_adjustment_creation,
            contract_adjustment_expiration
        ]},
        {shop_management, [sequence], [
            party_creation,
            contract_creation,
            shop_not_found_on_retrieval,
            shop_update_before_confirm,
            shop_update_with_bad_params,
            shop_creation,
            shop_activation,
            shop_update,
            {group, shop_blocking_suspension}
        ]},
        {shop_blocking_suspension, [sequence], [
            shop_blocking,
            shop_already_blocked,
            shop_blocked_on_suspend,
            shop_unblocking,
            shop_already_unblocked,
            shop_suspension,
            shop_already_suspended,
            shop_activation,
            shop_already_active
        ]},
        {shop_account_lazy_creation, [sequence], [
            party_creation,
            contract_creation,
            shop_creation,
            shop_account_set_retrieval,
            shop_account_retrieval
        ]},
        {claim_management, [sequence], [
            party_creation,
            contract_creation,
            claim_not_found_on_retrieval,
            claim_already_accepted_on_revoke,
            claim_already_accepted_on_accept,
            claim_already_accepted_on_deny,
            shop_creation,
            shop_activation,
            claim_acceptance,
            claim_denial,
            claim_revocation,
            no_pending_claim,
            complex_claim_acceptance,
            no_pending_claim
        ]},
        {consistent_history, [], [
            consistent_history
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
    ok = hg_domain:insert(hg_ct_helper:construct_domain_fixture()),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- ?c(apps, C)].

%% tests

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include("party_events.hrl").

-spec init_per_group(group_name(), config()) -> config().

init_per_group(shop_blocking_suspension, C) ->
    C;
init_per_group(Group, C) ->
    PartyID = list_to_binary(lists:concat([Group, ".", erlang:system_time()])),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(?c(root_url, C))),
    [{party_id, PartyID}, {client, Client} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    Client = ?c(client, C),
    hg_client_party:stop(Client).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%%

-define(party_w_status(ID, Blocking, Suspension),
    #domain_Party{id = ID, blocking = Blocking, suspension = Suspension}).
-define(shop_w_status(ID, Blocking, Suspension),
    #domain_Shop{id = ID, blocking = Blocking, suspension = Suspension}).

-define(invalid_user(),
    {exception, #payproc_InvalidUser{}}).
-define(invalid_request(Errors),
    {exception, #'InvalidRequest'{errors = Errors}}).

-define(party_not_found(),
    {exception, #payproc_PartyNotFound{}}).
-define(party_exists(),
    {exception, #payproc_PartyExists{}}).
-define(party_blocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?blocked(Reason)}}}).
-define(party_unblocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?unblocked(Reason)}}}).
-define(party_suspended(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?suspended()}}}).
-define(party_active(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?active()}}}).

-define(contract_not_found(),
    {exception, #payproc_ContractNotFound{}}).
-define(invalid_contract_status(Status),
    {exception, #payproc_InvalidContractStatus{status = Status}}).
-define(payout_tool_not_found(),
    {exception, #payproc_PayoutToolNotFound{}}).

-define(shop_not_found(),
    {exception, #payproc_ShopNotFound{}}).
-define(shop_blocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?blocked(Reason)}}}).
-define(shop_unblocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?unblocked(Reason)}}}).
-define(shop_suspended(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?suspended()}}}).
-define(shop_active(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?active()}}}).

-define(claim(ID),
    #payproc_Claim{id = ID}).
-define(claim(ID, Status),
    #payproc_Claim{id = ID, status = Status}).
-define(claim(ID, Status, Changeset),
    #payproc_Claim{id = ID, status = Status, changeset = Changeset}).
-define(claim_result(ID, Status),
    #payproc_ClaimResult{id = ID, status = Status}).

-define(claim_not_found(),
    {exception, #payproc_ClaimNotFound{}}).
-define(invalid_claim_status(Status),
    {exception, #payproc_InvalidClaimStatus{status = Status}}).

-spec party_creation(config()) -> _ | no_return().
-spec party_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_already_exists(config()) -> _ | no_return().
-spec party_retrieval(config()) -> _ | no_return().
-spec shop_not_found_on_retrieval(config()) -> _ | no_return().
-spec shop_creation(config()) -> _ | no_return().
-spec shop_update(config()) -> _ | no_return().
-spec shop_update_before_confirm(config()) -> _ | no_return().
-spec shop_update_with_bad_params(config()) -> _ | no_return().
-spec party_revisioning(config()) -> _ | no_return().
-spec claim_already_accepted_on_revoke(config()) -> _ | no_return().
-spec claim_already_accepted_on_accept(config()) -> _ | no_return().
-spec claim_already_accepted_on_deny(config()) -> _ | no_return().
-spec claim_acceptance(config()) -> _ | no_return().
-spec claim_denial(config()) -> _ | no_return().
-spec claim_revocation(config()) -> _ | no_return().
-spec claim_not_found_on_retrieval(config()) -> _ | no_return().
-spec no_pending_claim(config()) -> _ | no_return().
-spec complex_claim_acceptance(config()) -> _ | no_return().
-spec party_blocking(config()) -> _ | no_return().
-spec party_unblocking(config()) -> _ | no_return().
-spec party_already_blocked(config()) -> _ | no_return().
-spec party_already_unblocked(config()) -> _ | no_return().
-spec party_blocked_on_suspend(config()) -> _ | no_return().
-spec party_suspension(config()) -> _ | no_return().
-spec party_activation(config()) -> _ | no_return().
-spec party_already_suspended(config()) -> _ | no_return().
-spec party_already_active(config()) -> _ | no_return().
-spec shop_blocking(config()) -> _ | no_return().
-spec shop_unblocking(config()) -> _ | no_return().
-spec shop_already_blocked(config()) -> _ | no_return().
-spec shop_already_unblocked(config()) -> _ | no_return().
-spec shop_blocked_on_suspend(config()) -> _ | no_return().
-spec shop_suspension(config()) -> _ | no_return().
-spec shop_activation(config()) -> _ | no_return().
-spec shop_already_suspended(config()) -> _ | no_return().
-spec shop_already_active(config()) -> _ | no_return().
-spec shop_account_set_retrieval(config()) -> _ | no_return().
-spec shop_account_retrieval(config()) -> _ | no_return().
-spec party_access_control(config()) -> _ | no_return().
-spec contract_not_found(config()) -> _ | no_return().
-spec contract_creation(config()) -> _ | no_return().
-spec contract_termination(config()) -> _ | no_return().
-spec contract_expiration(config()) -> _ | no_return().
-spec contract_legal_agreement_binding(config()) -> _ | no_return().
-spec contract_payout_tool_creation(config()) -> _ | no_return().
-spec contract_adjustment_creation(config()) -> _ | no_return().
-spec contract_adjustment_expiration(config()) -> _ | no_return().

party_creation(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    ContactInfo = #domain_PartyContactInfo{email = <<?MODULE_STRING>>},
    ok = hg_client_party:create(make_party_params(ContactInfo), Client),
    ?party_created(?party_w_status(PartyID, ?unblocked(_), ?active())) = next_event(Client),
    ?claim_created(?claim(_, ?accepted(_), [_ | _])) = next_event(Client),
    #domain_Party{contact_info = ContactInfo, shops = Shops} = hg_client_party:get(Client),
    [{_ShopID, #domain_Shop{suspension = ?active()}}] = maps:to_list(Shops).

party_already_exists(C) ->
    Client = ?c(client, C),
    ?party_exists() = hg_client_party:create(make_party_params(), Client).

party_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?party_not_found() = hg_client_party:get(Client).

party_retrieval(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    #domain_Party{id = PartyID} = hg_client_party:get(Client).

party_revisioning(C) ->
    Client = ?c(client, C),
    hg_context:set(woody_context:new()),
    Party1 = hg_client_party:get(Client),
    T1 = hg_datetime:format_now(),
    Party2 = party_suspension(C),
    Party1 = hg_party:checkout(Party1#domain_Party.id, T1),
    T2 = hg_datetime:format_now(),
    _ = party_activation(C),
    Party2 = hg_party:checkout(Party2#domain_Party.id, T2),
    hg_context:cleanup().

contract_not_found(C) ->
    Client = ?c(client, C),
    ?contract_not_found() = hg_client_party:get_contract(666, Client),
    Params = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(42),
        details  = hg_ct_helper:make_shop_details(<<"SOME SHOP">>, <<"WITH WRONG PARAMS">>),
        contract_id = 666,
        payout_tool_id = 42
    },
    ?contract_not_found() = hg_client_party:create_shop(Params, Client).

contract_creation(C) ->
    Client = ?c(client, C),
    Params = hg_ct_helper:make_battle_ready_contract_params(),
    Claim = assert_claim_pending(hg_client_party:create_contract(Params, Client), Client),
    ?claim(
        _,
        ?pending(),
        [
            ?contract_creation(#domain_Contract{id = ID}),
            ?contract_payout_tool_creation(ID, #domain_PayoutTool{id = PayoutToolID})
        ]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{id = ID, payout_tools = PayoutTools} = hg_client_party:get_contract(ID, Client),
    true = lists:keymember(PayoutToolID, #domain_PayoutTool.id, PayoutTools).

contract_termination(C) ->
    Client = ?c(client, C),
    ContractID = hg_ct_helper:get_first_contract_id(Client),
    Reason = <<"WHY NOT?!">>,
    Claim = assert_claim_pending(hg_client_party:terminate_contract(ContractID, Reason, Client), Client),
    ?claim(
        _,
        ?pending(),
        [?contract_termination(ContractID, _, Reason)]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {terminated, _}
    } = hg_client_party:get_contract(ContractID, Client),
    ?invalid_contract_status({terminated, _}) = hg_client_party:terminate_contract(
        ContractID,
        <<"JUST TO BE SURE">>,
        Client
    ).

contract_expiration(C) ->
    Client = ?c(client, C),
    Params0 = hg_ct_helper:make_battle_ready_contract_params(),
    Params = Params0#payproc_ContractParams{template = #domain_ContractTemplateRef{id = 3}},
    Claim = assert_claim_pending(hg_client_party:create_contract(Params, Client), Client),
    ?claim(
        _,
        ?pending(),
        [{contract_creation, #domain_Contract{id = ContractID}}]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {terminated, _}
    } = hg_client_party:get_contract(ContractID, Client).

contract_legal_agreement_binding(C) ->
    Client = ?c(client, C),
    LA = #domain_LegalAgreement{
        signed_at = hg_datetime:format_now(),
        legal_agreement_id = <<"20160123-0031235-OGM/GDM">>
    },
    TerminatedContractID = hg_ct_helper:get_first_contract_id(Client),
    ?invalid_contract_status({terminated, _}) = hg_client_party:bind_legal_agreement(
        TerminatedContractID,
        LA,
        Client
    ),

    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Claim = assert_claim_pending(hg_client_party:bind_legal_agreement(ContractID, LA, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        legal_agreement = LA
    } = hg_client_party:get_contract(ContractID, Client).

contract_payout_tool_creation(C) ->
    Client = ?c(client, C),
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = #domain_CurrencyRef{symbolic_code = <<"RUB">>},
        tool_info  = {bank_account, #domain_BankAccount{
            account = <<"4276300010908312893">>,
            bank_name = <<"SomeBank">>,
            bank_post_account = <<"123129876">>,
            bank_bik = <<"66642666">>
        }}
    },
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Claim = assert_claim_pending(
        hg_client_party:create_payout_tool(ContractID, PayoutToolParams, Client),
        Client
    ),
    ?claim(
        _,
        ?pending(),
        [?contract_payout_tool_creation(ContractID, #domain_PayoutTool{id = ID})]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        payout_tools = PayoutTools
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_PayoutTool.id, PayoutTools).

contract_adjustment_creation(C) ->
    Client = ?c(client, C),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 2}
    },
    Claim = assert_claim_pending(
        hg_client_party:create_contract_adjustment(ContractID, AdjustmentParams, Client),
        Client
    ),
    ?claim(
        _,
        ?pending(),
        [?contract_adjustment_creation(ContractID, #domain_ContractAdjustment{id = ID})]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments).

contract_adjustment_expiration(C) ->
    Client = ?c(client, C),
    hg_context:set(woody_context:new()),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Terms = hg_party:get_payments_service_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now()
    ),
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 4}
    },
    Claim = assert_claim_pending(
        hg_client_party:create_contract_adjustment(ContractID, AdjustmentParams, Client),
        Client
    ),
    ?claim(
        _,
        ?pending(),
        [?contract_adjustment_creation(ContractID, #domain_ContractAdjustment{id = ID})]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments),
    Terms = hg_party:get_payments_service_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now()
    ),
    hg_context:cleanup().

shop_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?shop_not_found() = hg_client_party:get_shop(666, Client).

shop_creation(C) ->
    Client = ?c(client, C),
    Details = hg_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Params = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(2),
        details  = Details,
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Result = hg_client_party:create_shop(Params, Client),
    Claim = assert_claim_pending(Result, Client),
    ?claim(
        _,
        ?pending(),
        [
            {shop_creation, #domain_Shop{id = ShopID}},
            {shop_modification, #payproc_ShopModificationUnit{
                id = ShopID,
                modification = ?account_created(_)
            }}
        ]
    ) = Claim,
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{
        id = ShopID,
        suspension = ?suspended(),
        details = Details
    } = hg_client_party:get_shop(ShopID, Client).

shop_update(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    Details = hg_ct_helper:make_shop_details(<<"BARBER SHOP">>, <<"Nice. Short. Clean.">>),
    Update = #payproc_ShopUpdate{details = Details},
    Result = hg_client_party:update_shop(ShopID, Update, Client),
    Claim = assert_claim_pending(Result, Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{details = Details} = hg_client_party:get_shop(ShopID, Client).

shop_update_before_confirm(C) ->
    Client = ?c(client, C),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Params = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(2),
        details  = hg_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Result = hg_client_party:create_shop(Params, Client),
    Claim1 = assert_claim_pending(Result, Client),
    ?claim(
        ClaimID1,
        ?pending(),
        [
            {shop_creation, #domain_Shop{id = ShopID}},
            {shop_modification, #payproc_ShopModificationUnit{
                id = ShopID,
                modification = ?account_created(_)
            }}
        ]
    ) = Claim1,
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client),
    NewCategory = hg_ct_helper:make_category_ref(3),
    NewDetails = hg_ct_helper:make_shop_details(<<"BARBIES SHOP">>, <<"Hot. Short. Clean.">>),
    Update = #payproc_ShopUpdate{category = NewCategory, details = NewDetails},
    UpdateResult = hg_client_party:update_shop(ShopID, Update, Client),
    Claim2 = assert_claim_pending(UpdateResult, Client),
    ?claim_status_changed(ClaimID1, ?revoked(_)) = next_event(Client),
    ok = accept_claim(Claim2, Client),
    #domain_Shop{category = NewCategory, details = NewDetails} = hg_client_party:get_shop(ShopID, Client).

shop_update_with_bad_params(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    Params0 = hg_ct_helper:make_battle_ready_contract_params(),
    Params = Params0#payproc_ContractParams{template = #domain_ContractTemplateRef{id = 5}},
    Claim = assert_claim_pending(hg_client_party:create_contract(Params, Client), Client),
    ?claim(
        _,
        ?pending(),
        [{contract_creation, #domain_Contract{id = ContractID}}]
    ) = Claim,
    ok = accept_claim(Claim, Client),

    ?invalid_request(CategoryError) = hg_client_party:update_shop(
        ShopID,
        #payproc_ShopUpdate{category = hg_ct_helper:make_category_ref(1)},
        Client
    ),
    ?contract_not_found() = hg_client_party:update_shop(
        ShopID,
        #payproc_ShopUpdate{contract_id = 666},
        Client
    ),
    ?invalid_request(CategoryError) = hg_client_party:update_shop(
        ShopID,
        #payproc_ShopUpdate{contract_id = ContractID},
        Client
    ),
    ?payout_tool_not_found() = hg_client_party:update_shop(
        ShopID,
        #payproc_ShopUpdate{payout_tool_id = 42},
        Client
    ).

claim_acceptance(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    Update = #payproc_ShopUpdate{details = Details = hg_ct_helper:make_shop_details(<<"McDolan">>)},
    Result = hg_client_party:update_shop(ShopID, Update, Client),
    Claim = assert_claim_pending(Result, Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{details = Details} = hg_client_party:get_shop(ShopID, Client).

claim_denial(C) ->
    Client = ?c(client, C),
    Shop = #domain_Shop{id = ShopID} = get_last_shop(Client),
    Update = #payproc_ShopUpdate{details = #domain_ShopDetails{name = <<"Pr0nHub">>}},
    Result = hg_client_party:update_shop(ShopID, Update, Client),
    Claim = assert_claim_pending(Result, Client),
    ok = deny_claim(Claim, Client),
    Shop = hg_client_party:get_shop(ShopID, Client).

claim_revocation(C) ->
    Client = ?c(client, C),
    Party = hg_client_party:get(Client),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    Params = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(2),
        details  = hg_ct_helper:make_shop_details(<<"OOPS">>),
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Result = hg_client_party:create_shop(Params, Client),
    Claim = assert_claim_pending(Result, Client),
    ?claim(
        _,
        _,
        [
            {shop_creation, #domain_Shop{id = ShopID}},
            {shop_modification, #payproc_ShopModificationUnit{
                id = ShopID,
                modification = ?account_created(_)
            }}
        ]
    ) = Claim,
    ok = revoke_claim(Claim, Client),
    Party = hg_client_party:get(Client),
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client).

complex_claim_acceptance(C) ->
    Client = ?c(client, C),
    ContractID = hg_ct_helper:get_first_battle_ready_contract_id(Client),
    PayoutToolID = hg_ct_helper:get_first_payout_tool_id(ContractID, Client),
    Params1 = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(2),
        details  = Details1 = hg_ct_helper:make_shop_details(<<"SHOP 1">>),
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    },
    Params2 = #payproc_ShopParams{
        category = hg_ct_helper:make_category_ref(3),
        details  = Details2 = hg_ct_helper:make_shop_details(<<"SHOP 2">>),
        contract_id = ContractID,
        payout_tool_id = PayoutToolID
    },
    Claim1 = assert_claim_pending(hg_client_party:create_shop(Params1, Client), Client),
    ?claim(ClaimID1, _, [
        {shop_creation, #domain_Shop{id = ShopID1, details = Details1}}, _
    ]) = Claim1,
    _ = assert_claim_accepted(hg_client_party:suspend(Client), Client),
    Claim1 = ?claim(_) = hg_client_party:get_pending_claim(Client),
    _ = assert_claim_accepted(hg_client_party:activate(Client), Client),
    Claim1 = ?claim(_) = hg_client_party:get_pending_claim(Client),
    Claim2 = assert_claim_pending(hg_client_party:create_shop(Params2, Client), Client),
    ?claim_status_changed(ClaimID1, ?revoked(_)) = next_event(Client),
    Claim2 = ?claim(_) = hg_client_party:get_pending_claim(Client),
    ?claim(_, _, [
        {shop_creation, #domain_Shop{id = ShopID1, details = Details1}},
        {shop_modification, #payproc_ShopModificationUnit{
            id = ShopID1,
            modification = ?account_created(_)
        }},
        {shop_creation, #domain_Shop{id = ShopID2, details = Details2}},
        {shop_modification, #payproc_ShopModificationUnit{
            id = ShopID2,
            modification = ?account_created(_)
        }}
    ]) = Claim2,
    ok = accept_claim(Claim2, Client),
    #domain_Shop{details = Details1} = hg_client_party:get_shop(ShopID1, Client),
    #domain_Shop{details = Details2} = hg_client_party:get_shop(ShopID2, Client).

claim_already_accepted_on_revoke(C) ->
    Client = ?c(client, C),
    Reason = <<"The End is near">>,
    ?claim(ID1) = ensure_block_party(Reason, Client),
    ?party_blocked(Reason) = hg_client_party:revoke_claim(ID1, <<>>, Client),
    ?claim(ID2) = ensure_unblock_party(<<>>, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:revoke_claim(ID2, <<>>, Client).

claim_already_accepted_on_accept(C) ->
    Client = ?c(client, C),
    Reason = <<"And behold">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:accept_claim(ID, Client),
    _ = ensure_unblock_party(<<>>, Client).

claim_already_accepted_on_deny(C) ->
    Client = ?c(client, C),
    Reason = <<"I am about to destroy them">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:deny_claim(ID, <<>>, Client),
    _ = ensure_unblock_party(<<>>, Client).

claim_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?claim_not_found() = hg_client_party:get_claim(-666, Client).

no_pending_claim(C) ->
    Client = ?c(client, C),
    ?claim_not_found() = hg_client_party:get_pending_claim(Client).

party_blocking(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:block(<<"i said so">>, Client), Client),
    ?party_w_status(PartyID, ?blocked(_), _) = hg_client_party:get(Client).

party_unblocking(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:unblock(<<"enough">>, Client), Client),
    ?party_w_status(PartyID, ?unblocked(_), _) = hg_client_party:get(Client).

party_already_blocked(C) ->
    Client = ?c(client, C),
    ?party_blocked(_) = hg_client_party:block(<<"too much">>, Client).

party_already_unblocked(C) ->
    Client = ?c(client, C),
    ?party_unblocked(_) = hg_client_party:unblock(<<"too free">>, Client).

party_blocked_on_suspend(C) ->
    Client = ?c(client, C),
    ?party_blocked(_) = hg_client_party:suspend(Client).

party_suspension(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:suspend(Client), Client),
    ?party_w_status(PartyID, _, ?suspended()) = hg_client_party:get(Client).

party_activation(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    _ = assert_claim_accepted(hg_client_party:activate(Client), Client),
    ?party_w_status(PartyID, _, ?active()) = hg_client_party:get(Client).

party_already_suspended(C) ->
    Client = ?c(client, C),
    ?party_suspended() = hg_client_party:suspend(Client).

party_already_active(C) ->
    Client = ?c(client, C),
    ?party_active() = hg_client_party:activate(Client).

shop_blocking(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    _ = assert_claim_accepted(hg_client_party:block_shop(ShopID, <<"i said so">>, Client), Client),
    ?shop_w_status(ShopID, ?blocked(_), _) = hg_client_party:get_shop(ShopID, Client).

shop_unblocking(C) ->
    Client  = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    _ = assert_claim_accepted(hg_client_party:unblock_shop(ShopID, <<"enough">>, Client), Client),
    ?shop_w_status(ShopID, ?unblocked(_), _) = hg_client_party:get_shop(ShopID, Client).

shop_already_blocked(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    ?shop_blocked(_) = hg_client_party:block_shop(ShopID, <<"too much">>, Client).

shop_already_unblocked(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    ?shop_unblocked(_) = hg_client_party:unblock_shop(ShopID, <<"too free">>, Client).

shop_blocked_on_suspend(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    ?shop_blocked(_) = hg_client_party:suspend_shop(ShopID, Client).

shop_suspension(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    _ = assert_claim_accepted(hg_client_party:suspend_shop(ShopID, Client), Client),
    ?shop_w_status(ShopID, _, ?suspended()) = hg_client_party:get_shop(ShopID, Client).

shop_activation(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    _ = assert_claim_accepted(hg_client_party:activate_shop(ShopID, Client), Client),
    ?shop_w_status(ShopID, _, ?active()) = hg_client_party:get_shop(ShopID, Client).

shop_already_suspended(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    ?shop_suspended() = hg_client_party:suspend_shop(ShopID, Client).

shop_already_active(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    ?shop_active() = hg_client_party:activate_shop(ShopID, Client).

shop_account_set_retrieval(C) ->
    Client = ?c(client, C),
    #domain_Shop{id = ShopID} = get_last_shop(Client),
    S = #domain_ShopAccount{} = hg_client_party:get_shop_account(ShopID, Client),
    {save_config, S}.

shop_account_retrieval(C) ->
    Client = ?c(client, C),
    {shop_account_set_retrieval, #domain_ShopAccount{guarantee = AccountID}} = ?config(saved_config, C),
    #payproc_AccountState{account_id = AccountID} = hg_client_party:get_account_state(AccountID, Client).

%% Access control tests

party_access_control(C) ->
    PartyID = ?c(party_id, C),
    % External Success
    GoodExternalClient = ?c(client, C),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodExternalClient),

    % External Reject
    BadExternalClient = hg_client_party:start(
        #payproc_UserInfo{id = <<"FakE1D">>, type = {external_user, #payproc_ExternalUser{}}},
        PartyID,
        hg_client_api:new(?c(root_url, C))
    ),
    ?invalid_user() = hg_client_party:get(BadExternalClient),
    hg_client_party:stop(BadExternalClient),

    % Internal Success
    GoodInternalClient = hg_client_party:start(
        #payproc_UserInfo{id = <<"F4KE1D">>, type = {internal_user, #payproc_InternalUser{}}},
        PartyID,
        hg_client_api:new(?c(root_url, C))
    ),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodInternalClient),
    hg_client_party:stop(GoodInternalClient),

    % Service Success
    GoodServiceClient = hg_client_party:start(
        #payproc_UserInfo{id = <<"fAkE1D">>, type = {service_user, #payproc_ServiceUser{}}},
        PartyID,
        hg_client_api:new(?c(root_url, C))
    ),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodServiceClient),
    hg_client_party:stop(GoodServiceClient),
    ok.

get_last_shop(Client) ->
    #domain_Party{shops = Shops} = hg_client_party:get(Client),
    ShopID = lists:last(lists:sort(maps:keys(Shops))),
    maps:get(ShopID, Shops).

ensure_block_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:block(Reason, Client), Client).

ensure_unblock_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:unblock(Reason, Client), Client).

accept_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:accept_claim(ClaimID, Client),
    ?claim_status_changed(ClaimID, ?accepted(_)) = next_event(Client),
    ok.

deny_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:deny_claim(ClaimID, Reason = <<"The Reason">>, Client),
    ?claim_status_changed(ClaimID, ?denied(Reason)) = next_event(Client),
    ok.

revoke_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:revoke_claim(ClaimID, <<>>, Client),
    ?claim_status_changed(ClaimID, ?revoked(<<>>)) = next_event(Client),
    ok.

assert_claim_pending(?claim_result(ClaimID, Status = ?pending()), Client) ->
    Claim = ?claim(ClaimID, Status) = hg_client_party:get_claim(ClaimID, Client),
    ?claim_created(?claim(ClaimID)) = next_event(Client),
    Claim.

assert_claim_accepted(?claim_result(ClaimID, Status = ?accepted(_)), Client) ->
    Claim = ?claim(ClaimID, Status) = hg_client_party:get_claim(ClaimID, Client),
    ?claim_created(?claim(ClaimID, ?accepted(_))) = next_event(Client),
    Claim.

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(?c(root_url, C))),
    Events = hg_client_eventsink:pull_events(_N = 5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

%%

next_event(Client) ->
    case hg_client_party:pull_event(Client) of
        ?party_ev(Event) ->
            Event;
        Result ->
            Result
    end.

%%

make_userinfo(PartyID) ->
    #payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}}.

make_party_params() ->
    make_party_params(#domain_PartyContactInfo{email = <<?MODULE_STRING>>}).
make_party_params(ContactInfo) ->
    #payproc_PartyParams{contact_info = ContactInfo}.



