-module(hg_party_tests_SUITE).

-include("hg_ct_domain.hrl").
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
    ok = hg_domain:insert(construct_domain_fixture()),
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
    ?claim_created(?claim(_, ?accepted(_, _), [_ | _])) = next_event(Client),
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
    Party1 = hg_party_machine:checkout(Party1#domain_Party.id, T1),
    T2 = hg_datetime:format_now(),
    _ = party_activation(C),
    Party2 = hg_party_machine:checkout(Party2#domain_Party.id, T2),
    hg_context:cleanup().

contract_not_found(C) ->
    Client = ?c(client, C),
    ?contract_not_found() = hg_client_party:get_contract(<<"666">>, Client).
    % FIXME
    % Params = #payproc_ShopParams{
    %     category = ?cat(42),
    %     details  = hg_ct_helper:make_shop_details(<<"SOME SHOP">>, <<"WITH WRONG PARAMS">>),
    %     contract_id = 666,
    %     payout_tool_id = 42
    % },
    % ?contract_not_found() = hg_client_party:create_shop(Params, Client).

contract_creation(C) ->
    % FIXME check for creation with already existed ID, maybe in separate test
    Client = ?c(client, C),
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(),
    PayoutToolParams = hg_ct_helper:make_battle_ready_payout_tool_params(),
    ContractID = <<"CONTRACT1">>,
    PayoutToolID = <<"1">>,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams}),
        ?contract_modification(ContractID, ?payout_tool_creation(PayoutToolID, PayoutToolParams))
    ],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{id = ContractID, payout_tools = PayoutTools} = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(PayoutToolID, #domain_PayoutTool.id, PayoutTools).

contract_termination(C) ->
    Client = ?c(client, C),
    % FIXME
    ContractID = <<"TESTCONTRACT">>,
    Changeset = [?contract_modification(ContractID, ?contract_termination(hg_datetime:format_now(), <<"WHY NOT?!">>))],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {terminated, _}
    } = hg_client_party:get_contract(ContractID, Client).
    % FIXME
    % ?invalid_contract_status({terminated, _}) = hg_client_party:terminate_contract(
    %     ContractID,
    %     <<"JUST TO BE SURE">>,
    %     Client
    % ).

contract_expiration(C) ->
    Client = ?c(client, C),
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(?tmpl(3)),
    PayoutToolParams = hg_ct_helper:make_battle_ready_payout_tool_params(),
    ContractID = <<"CONTRACT2">>,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams}),
        ?contract_modification(ContractID, ?payout_tool_creation(<<"1">>, PayoutToolParams))
    ],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {expired, _}
    } = hg_client_party:get_contract(ContractID, Client).

contract_legal_agreement_binding(C) ->
    Client = ?c(client, C),
    ContractID = <<"CONTRACT1">>,
    LA = #domain_LegalAgreement{
        signed_at = hg_datetime:format_now(),
        legal_agreement_id = <<"20160123-0031235-OGM/GDM">>
    },
    % FIXME
    % TerminatedContractID = hg_ct_helper:get_first_contract_id(Client),
    % ?invalid_contract_status({terminated, _}) = hg_client_party:bind_legal_agreement(
    %     TerminatedContractID,
    %     LA,
    %     Client
    % ),
    Changeset = [?contract_modification(ContractID, {legal_agreement_binding, LA})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        legal_agreement = LA
    } = hg_client_party:get_contract(ContractID, Client).

contract_payout_tool_creation(C) ->
    Client = ?c(client, C),
    ContractID = <<"CONTRACT1">>,
    PayoutToolID = <<"2">>,
    PayoutToolParams = #payproc_PayoutToolParams{
        currency = ?cur(<<"RUB">>),
        tool_info  = {bank_account, #domain_BankAccount{
            account = <<"4276300010908312893">>,
            bank_name = <<"SomeBank">>,
            bank_post_account = <<"123129876">>,
            bank_bik = <<"66642666">>
        }}
    },
    Changeset = [?contract_modification(ContractID, ?payout_tool_creation(PayoutToolID, PayoutToolParams))],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        payout_tools = PayoutTools
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(PayoutToolID, #domain_PayoutTool.id, PayoutTools).

contract_adjustment_creation(C) ->
    Client = ?c(client, C),
    ContractID = <<"CONTRACT1">>,
    ID = <<"ADJ1">>,
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 2}
    },
    Changeset = [?contract_modification(ContractID, ?adjustment_creation(ID, AdjustmentParams))],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments).

contract_adjustment_expiration(C) ->
    Client = ?c(client, C),
    hg_context:set(woody_context:new()),
    ContractID = <<"CONTRACT1">>,
    ID = <<"ADJ2">>,
    Terms = hg_party:get_payments_service_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now()
    ),
    AdjustmentParams = #payproc_ContractAdjustmentParams{
        template = #domain_ContractTemplateRef{id = 4}
    },
    Changeset = [?contract_modification(ContractID, ?adjustment_creation(ID, AdjustmentParams))],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        adjustments = Adjustments
    } = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(ID, #domain_ContractAdjustment.id, Adjustments),
    true = Terms /= hg_party:get_payments_service_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now()
    ),
    AfterExpiration = hg_datetime:add_interval(hg_datetime:format_now(), {0, 1, 1}),
    Terms = hg_party:get_payments_service_terms(hg_client_party:get_contract(ContractID, Client), AfterExpiration),
    hg_context:cleanup().

shop_not_found_on_retrieval(C) ->
    Client = ?c(client, C),
    ?shop_not_found() = hg_client_party:get_shop(<<"666">>, Client).

shop_creation(C) ->
    Client = ?c(client, C),
    Details = hg_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
    ContractID = <<"CONTRACT1">>,
    ShopID = <<"SHOP1">>,
    Params = #payproc_ShopParams{
        category = ?cat(2),
        location = {url, <<"https://somename.somedomain/p/123?redirect=1">>},
        details  = Details,
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Changeset = [?shop_modification(ShopID, {creation, Params})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ?claim(
        _,
        _,
        [
            ?shop_modification(ShopID, {creation, Params}),
            ?shop_modification(ShopID, {shop_account_creation, _})
        ]
    ) = Claim,
    ok = accept_claim(Claim, Client),
    #domain_Shop{
        id = ShopID,
        % FIXME
        % suspension = ?suspended(),
        details = Details
    } = hg_client_party:get_shop(ShopID, Client).

shop_update(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    Details = hg_ct_helper:make_shop_details(<<"BARBER SHOP">>, <<"Nice. Short. Clean.">>),
    Changeset = [?shop_modification(ShopID, {details_modification, Details})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{details = Details} = hg_client_party:get_shop(ShopID, Client).

shop_update_before_confirm(C) ->
    Client = ?c(client, C),
    ContractID = <<"CONTRACT1">>,
    ShopID = <<"SHOP2">>,
    Params = #payproc_ShopParams{
        location = {url, <<"">>},
        details  = hg_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Changeset1 = [?shop_modification(ShopID, {creation, Params})],
    Claim0 = assert_claim_pending(hg_client_party:create_claim(Changeset1, Client), Client),
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client),
    NewCategory = ?cat(3),
    NewDetails = hg_ct_helper:make_shop_details(<<"BARBIES SHOP">>, <<"Hot. Short. Clean.">>),

    Changeset2 = [
        ?shop_modification(ShopID, {category_modification, NewCategory}),
        ?shop_modification(ShopID, {details_modification, NewDetails})
    ],
    ok = update_claim(Claim0, Changeset2, Client),
    Claim1 = hg_client_party:get_claim(hg_claim:get_id(Claim0), Client),
    ok = accept_claim(Claim1, Client),
    #domain_Shop{category = NewCategory, details = NewDetails} = hg_client_party:get_shop(ShopID, Client).

shop_update_with_bad_params(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP2">>,
    ContractID = <<"CONTRACT3">>,
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(#domain_ContractTemplateRef{id = 5}),
    PayoutToolParams = hg_ct_helper:make_battle_ready_payout_tool_params(),
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams}),
        ?contract_modification(ContractID, ?payout_tool_creation(<<"1">>, PayoutToolParams))
    ],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),

    #payproc_Claim{id = ID1, revision = Rev1} = assert_claim_pending(
        hg_client_party:create_claim(
            [?shop_modification(ShopID, {category_modification, ?cat(1)})],
            Client
        ),
        Client
    ),
    ?invalid_request(_CategoryError) = hg_client_party:accept_claim(ID1, Rev1, Client).

    % FIXME
    % ?contract_not_found() = hg_client_party:update_shop(
    %     ShopID,
    %     #payproc_ShopUpdate{contract_id = 666},
    %     Client
    % ),
    % ?invalid_request(CategoryError) = hg_client_party:update_shop(
    %     ShopID,
    %     #payproc_ShopUpdate{contract_id = ContractID},
    %     Client
    % ),
    % ?payout_tool_not_found() = hg_client_party:update_shop(
    %     ShopID,
    %     #payproc_ShopUpdate{payout_tool_id = 42},
    %     Client
    % ).

claim_acceptance(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    Details = hg_ct_helper:make_shop_details(<<"McDolan">>),
    Changeset = [?shop_modification(ShopID, {details_modification, Details})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{details = Details} = hg_client_party:get_shop(ShopID, Client).

claim_denial(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    Shop = hg_client_party:get_shop(ShopID, Client),
    Details = #domain_ShopDetails{name = <<"Pr0nHub">>},
    Changeset = [?shop_modification(ShopID, {details_modification, Details})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = deny_claim(Claim, Client),
    Shop = hg_client_party:get_shop(ShopID, Client).

claim_revocation(C) ->
    Client = ?c(client, C),
    Party = hg_client_party:get(Client),
    ShopID = <<"SHOP3">>,
    ContractID = <<"CONTRACT1">>,
    Params = #payproc_ShopParams{
        details  = hg_ct_helper:make_shop_details(<<"OOPS">>),
        contract_id = ContractID,
        payout_tool_id = <<"1">>
    },
    Changeset = [?shop_modification(ShopID, {creation, Params})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = revoke_claim(Claim, Client),
    Party = hg_client_party:get(Client),
    ?shop_not_found() = hg_client_party:get_shop(ShopID, Client).

complex_claim_acceptance(C) ->
    Client = ?c(client, C),
    ContractID = <<"CONTRACT1">>,
    ShopID1 = <<"SHOP4">>,
    Params1 = #payproc_ShopParams{
        location = {url, <<"https://url4">>},
        category = ?cat(2),
        details  = Details1 = hg_ct_helper:make_shop_details(<<"SHOP4">>),
        contract_id = ContractID,
        payout_tool_id = <<"1">>
    },
    ShopID2 = <<"SHOP5">>,
    Params2 = #payproc_ShopParams{
        location = {url, <<"http://url5">>},
        category = ?cat(3),
        details  = Details2 = hg_ct_helper:make_shop_details(<<"SHOP5">>),
        contract_id = ContractID,
        payout_tool_id = <<"1">>
    },
    Claim1 = assert_claim_pending(
        hg_client_party:create_claim([?shop_modification(ShopID1, {creation, Params1})], Client),
        Client
    ),
    ok = hg_client_party:suspend(Client),
    ok = hg_client_party:activate(Client),
    Claim1 = hg_client_party:get_claim(hg_claim:get_id(Claim1), Client),

    Claim2 = assert_claim_pending(
        hg_client_party:create_claim([?shop_modification(ShopID2, {creation, Params2})], Client),
        Client
    ),
    ok = update_claim(Claim1, [?shop_modification(ShopID1, {category_modification, ?cat(3)})], Client),
    Claim1_1 = hg_client_party:get_claim(hg_claim:get_id(Claim1), Client),
    true = Claim1#payproc_Claim.changeset =/= Claim1_1#payproc_Claim.changeset,
    true = Claim1#payproc_Claim.revision =/= Claim1_1#payproc_Claim.revision,
    ok = accept_claim(Claim2, Client),
    ok = accept_claim(Claim1, Client),
    ok = accept_claim(Claim1_1, Client),
    #domain_Shop{details = Details1, category = ?cat(3)} = hg_client_party:get_shop(ShopID1, Client),
    #domain_Shop{details = Details2} = hg_client_party:get_shop(ShopID2, Client).

claim_already_accepted_on_revoke(C) ->
    Client = ?c(client, C),
    Reason = <<"The End is near">>,
    ?claim(ID1) = ensure_block_party(Reason, Client),
    ?party_blocked(Reason) = hg_client_party:revoke_claim(ID1, <<>>, Client),
    ?claim(ID2) = ensure_unblock_party(<<>>, Client),
    ?invalid_claim_status(?accepted(_, _)) = hg_client_party:revoke_claim(ID2, <<>>, Client).

claim_already_accepted_on_accept(C) ->
    Client = ?c(client, C),
    Reason = <<"And behold">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_, _)) = hg_client_party:accept_claim(ID, Client),
    _ = ensure_unblock_party(<<>>, Client).

claim_already_accepted_on_deny(C) ->
    Client = ?c(client, C),
    Reason = <<"I am about to destroy them">>,
    ?claim(ID) = ensure_block_party(Reason, Client),
    ?invalid_claim_status(?accepted(_, _)) = hg_client_party:deny_claim(ID, <<>>, Client),
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
    ok = hg_client_party:block(<<"i said so">>, Client),
    ?party_w_status(PartyID, ?blocked(_), _) = hg_client_party:get(Client).

party_unblocking(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    ok = hg_client_party:unblock(<<"enough">>, Client),
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
    ok = hg_client_party:suspend(Client),
    ?party_w_status(PartyID, _, ?suspended()) = hg_client_party:get(Client).

party_activation(C) ->
    Client = ?c(client, C),
    PartyID = ?c(party_id, C),
    ok = hg_client_party:activate(Client),
    ?party_w_status(PartyID, _, ?active()) = hg_client_party:get(Client).

party_already_suspended(C) ->
    Client = ?c(client, C),
    ?party_suspended() = hg_client_party:suspend(Client).

party_already_active(C) ->
    Client = ?c(client, C),
    ?party_active() = hg_client_party:activate(Client).

shop_blocking(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ok = hg_client_party:block_shop(ShopID, <<"i said so">>, Client),
    ?shop_w_status(ShopID, ?blocked(_), _) = hg_client_party:get_shop(ShopID, Client).

shop_unblocking(C) ->
    Client  = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ok = hg_client_party:unblock_shop(ShopID, <<"enough">>, Client),
    ?shop_w_status(ShopID, ?unblocked(_), _) = hg_client_party:get_shop(ShopID, Client).

shop_already_blocked(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ?shop_blocked(_) = hg_client_party:block_shop(ShopID, <<"too much">>, Client).

shop_already_unblocked(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ?shop_unblocked(_) = hg_client_party:unblock_shop(ShopID, <<"too free">>, Client).

shop_blocked_on_suspend(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ?shop_blocked(_) = hg_client_party:suspend_shop(ShopID, Client).

shop_suspension(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ok = hg_client_party:suspend_shop(ShopID, Client),
    ?shop_w_status(ShopID, _, ?suspended()) = hg_client_party:get_shop(ShopID, Client).

shop_activation(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ok = hg_client_party:activate_shop(ShopID, Client),
    ?shop_w_status(ShopID, _, ?active()) = hg_client_party:get_shop(ShopID, Client).

shop_already_suspended(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ?shop_suspended() = hg_client_party:suspend_shop(ShopID, Client).

shop_already_active(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
    ?shop_active() = hg_client_party:activate_shop(ShopID, Client).

shop_account_set_retrieval(C) ->
    Client = ?c(client, C),
    ShopID = <<"SHOP1">>,
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

ensure_block_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:block(Reason, Client), Client).

ensure_unblock_party(Reason, Client) ->
    assert_claim_accepted(hg_client_party:unblock(Reason, Client), Client).

update_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Changeset, Client) ->
    ok = hg_client_party:update_claim(ClaimID, Revision, Changeset, Client),
    ?claim_updated(ClaimID, Changeset) = next_event(Client),
    ok.

accept_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = hg_client_party:accept_claim(ClaimID, Revision, Client),
    ?claim_status_changed(ClaimID, ?accepted(_, _)) = next_event(Client),
    ok.

deny_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:deny_claim(ClaimID, Reason = <<"The Reason">>, Client),
    ?claim_status_changed(ClaimID, ?denied(Reason)) = next_event(Client),
    ok.

revoke_claim(#payproc_Claim{id = ClaimID}, Client) ->
    ok = hg_client_party:revoke_claim(ClaimID, <<>>, Client),
    ?claim_status_changed(ClaimID, ?revoked(<<>>)) = next_event(Client),
    ok.

assert_claim_pending(?claim(ClaimID, ?pending()) = Claim, Client) ->
    ?claim_created(?claim(ClaimID)) = next_event(Client),
    Claim.

assert_claim_accepted(?claim(ClaimID, ?accepted(_, _)) = Claim, Client) ->
    ?claim_created(?claim(ClaimID, ?accepted(_, _))) = next_event(Client),
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

-spec construct_domain_fixture() -> [hg_domain:object()].

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
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
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
                    ?share(45, 1000, payment_amount)
                )
            ]}
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

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ordsets:new()},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(2),
                common_merchant_proxy = ?prx(1),
                inspector = {value, ?insp(1)}
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = #domain_PartyPrototypeRef{id = 42},
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    shop_id = <<"TESTSHOP">>,
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    },
                    location = {url, <<"">>}
                },
                contract = #domain_ContractPrototype{
                    contract_id = <<"TESTCONTRACT">>,
                    test_contract_template = ?tmpl(1),
                    payout_tool = #domain_PayoutToolPrototype{
                        payout_tool_id = <<"TESTPAYOUTTOOL">>,
                        payout_tool_info = {bank_account, #domain_BankAccount{
                            account = <<"">>,
                            bank_name = <<"">>,
                            bank_post_account = <<"">>,
                            bank_bik = <<"">>
                        }},
                        payout_tool_currency = ?cur(<<"RUB">>)
                    }
                }
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
        }}
    ].


