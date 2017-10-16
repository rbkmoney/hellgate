-module(hg_party_tests_SUITE).

-include("hg_ct_domain.hrl").
-include("party_events.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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
-export([no_pending_claims/1]).
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

-export([party_meta_retrieval/1]).
-export([party_metadata_setting/1]).
-export([party_metadata_retrieval/1]).
-export([party_metadata_removing/1]).

-export([shop_not_found_on_retrieval/1]).
-export([shop_creation/1]).
-export([shop_terms_retrieval/1]).
-export([shop_already_exists/1]).
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
-export([contract_terms_retrieval/1]).
-export([contract_already_exists/1]).
-export([contract_termination/1]).
-export([contract_already_terminated/1]).
-export([contract_expiration/1]).
-export([contract_legal_agreement_binding/1]).
-export([contract_payout_tool_creation/1]).
-export([contract_adjustment_creation/1]).
-export([contract_adjustment_expiration/1]).

%% tests descriptions

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name() :: hg_ct_helper:group_name().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [{group, group_name()}].

all() ->
    [
        {group, party_access_control},
        {group, party_creation},
        {group, party_revisioning},
        {group, party_blocking_suspension},
        {group, party_meta},
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
        {party_meta, [sequence], [
            party_creation,
            party_metadata_setting,
            party_metadata_retrieval,
            party_metadata_removing,
            party_meta_retrieval
        ]},
        {contract_management, [sequence], [
            party_creation,
            contract_not_found,
            contract_creation,
            contract_terms_retrieval,
            contract_already_exists,
            contract_termination,
            contract_already_terminated,
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
            shop_terms_retrieval,
            shop_already_exists,
            % shop_activation,
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
            % shop_activation,
            claim_acceptance,
            claim_denial,
            claim_revocation,
            no_pending_claims,
            complex_claim_acceptance,
            no_pending_claims
        ]},
        {consistent_history, [], [
            consistent_history
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, dmt_client, hellgate]),
    ok = hg_domain:insert(construct_domain_fixture()),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

%% tests

-spec init_per_group(group_name(), config()) -> config().

init_per_group(shop_blocking_suspension, C) ->
    C;
init_per_group(Group, C) ->
    PartyID = list_to_binary(lists:concat([Group, ".", erlang:system_time()])),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(cfg(root_url, C))),
    [{party_id, PartyID}, {client, Client} | C].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    Client = cfg(client, C),
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
-define(party_not_exists_yet(),
    {exception, #payproc_PartyNotExistsYet{}}).
-define(party_blocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?blocked(Reason, _)}}}).
-define(party_unblocked(Reason),
    {exception, #payproc_InvalidPartyStatus{status = {blocking, ?unblocked(Reason, _)}}}).
-define(party_suspended(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?suspended(_)}}}).
-define(party_active(),
    {exception, #payproc_InvalidPartyStatus{status = {suspension, ?active(_)}}}).

-define(namespace_not_found(),
    {exception, #payproc_PartyMetaNamespaceNotFound{}}).

-define(contract_not_found(),
    {exception, #payproc_ContractNotFound{}}).
-define(invalid_contract_status(Status),
    {exception, #payproc_InvalidContractStatus{status = Status}}).
-define(payout_tool_not_found(),
    {exception, #payproc_PayoutToolNotFound{}}).

-define(shop_not_found(),
    {exception, #payproc_ShopNotFound{}}).
-define(shop_blocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?blocked(Reason, _)}}}).
-define(shop_unblocked(Reason),
    {exception, #payproc_InvalidShopStatus{status = {blocking, ?unblocked(Reason, _)}}}).
-define(shop_suspended(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?suspended(_)}}}).
-define(shop_active(),
    {exception, #payproc_InvalidShopStatus{status = {suspension, ?active(_)}}}).

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
-define(invalid_changeset(Reason),
    {exception, #payproc_InvalidChangeset{reason = Reason}}).

-define(REAL_SHOP_ID, <<"SHOP1">>).
-define(REAL_CONTRACT_ID, <<"CONTRACT1">>).
-define(REAL_PARTY_PAYMENT_METHODS,
    [?pmt(bank_card, maestro), ?pmt(bank_card, mastercard), ?pmt(bank_card, visa)]).

-spec party_creation(config()) -> _ | no_return().
-spec party_not_found_on_retrieval(config()) -> _ | no_return().
-spec party_already_exists(config()) -> _ | no_return().
-spec party_retrieval(config()) -> _ | no_return().

-spec shop_not_found_on_retrieval(config()) -> _ | no_return().
-spec shop_creation(config()) -> _ | no_return().
-spec shop_terms_retrieval(config()) -> _ | no_return().
-spec shop_already_exists(config()) -> _ | no_return().
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
-spec no_pending_claims(config()) -> _ | no_return().
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

-spec party_meta_retrieval(config()) -> _ | no_return().
-spec party_metadata_setting(config()) -> _ | no_return().
-spec party_metadata_retrieval(config()) -> _ | no_return().
-spec party_metadata_removing(config()) -> _ | no_return().

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
-spec contract_terms_retrieval(config()) -> _ | no_return().
-spec contract_already_exists(config()) -> _ | no_return().
-spec contract_termination(config()) -> _ | no_return().
-spec contract_already_terminated(config()) -> _ | no_return().
-spec contract_expiration(config()) -> _ | no_return().
-spec contract_legal_agreement_binding(config()) -> _ | no_return().
-spec contract_payout_tool_creation(config()) -> _ | no_return().
-spec contract_adjustment_creation(config()) -> _ | no_return().
-spec contract_adjustment_expiration(config()) -> _ | no_return().

party_creation(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ContactInfo = #domain_PartyContactInfo{email = <<?MODULE_STRING>>},
    ok = hg_client_party:create(make_party_params(ContactInfo), Client),
    [
        ?party_created(?party_w_status(PartyID, ?unblocked(_, _), ?active(_))),
        ?claim_created(?claim(_, ?pending(), [_ | _])),
        ?claim_status_changed(_, ?accepted(_), _, _)
    ] = next_event(Client),
    #domain_Party{contact_info = ContactInfo, shops = Shops} = hg_client_party:get(Client),
    [{_ShopID, #domain_Shop{suspension = ?active(_)}}] = maps:to_list(Shops).

party_already_exists(C) ->
    Client = cfg(client, C),
    ?party_exists() = hg_client_party:create(make_party_params(), Client).

party_not_found_on_retrieval(C) ->
    Client = cfg(client, C),
    ?party_not_found() = hg_client_party:get(Client).

party_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    #domain_Party{id = PartyID} = hg_client_party:get(Client).

party_revisioning(C) ->
    Client = cfg(client, C),
    T0 = hg_datetime:add_interval(hg_datetime:format_now(), {undefined, undefined, -1}), % yesterday
    ?party_not_exists_yet() = hg_client_party:checkout(T0, Client),
    Party1 = hg_client_party:get(Client),
    T1 = hg_datetime:format_now(),
    Party2 = party_suspension(C),
    Party1 = hg_client_party:checkout(T1, Client),
    T2 = hg_datetime:format_now(),
    _ = party_activation(C),
    Party2 = hg_client_party:checkout(T2, Client),
    Party3 = hg_client_party:get(Client),
    T3 = hg_datetime:add_interval(T2, {undefined, undefined, 1}), % tomorrow
    Party3 = hg_client_party:checkout(T3, Client).

contract_not_found(C) ->
    Client = cfg(client, C),
    ?contract_not_found() = hg_client_party:get_contract(<<"666">>, Client).

contract_creation(C) ->
    Client = cfg(client, C),
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(),
    PayoutToolParams = hg_ct_helper:make_battle_ready_payout_tool_params(),
    ContractID = ?REAL_CONTRACT_ID,
    PayoutToolID = <<"1">>,
    Changeset = [
        ?contract_modification(ContractID, {creation, ContractParams}),
        ?contract_modification(ContractID, ?payout_tool_creation(PayoutToolID, PayoutToolParams))
    ],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{id = ContractID, payout_tools = PayoutTools} = hg_client_party:get_contract(ContractID, Client),
    true = lists:keymember(PayoutToolID, #domain_PayoutTool.id, PayoutTools).

contract_terms_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ContractID = ?REAL_CONTRACT_ID,
    TermSet1 = hg_client_party:compute_contract_terms(ContractID, hg_datetime:format_now(), Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, [?pmt(bank_card, visa)]}
    }} = TermSet1,
    ok = hg_domain:update(construct_term_set_for_party(PartyID, undefined)),
    TermSet2 = hg_client_party:compute_contract_terms(ContractID, hg_datetime:format_now(), Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, ?REAL_PARTY_PAYMENT_METHODS}
    }} = TermSet2.

contract_already_exists(C) ->
    Client = cfg(client, C),
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(),
    ContractID = ?REAL_CONTRACT_ID,
    Changeset = [?contract_modification(ContractID, {creation, ContractParams})],
    ?invalid_changeset({contract_already_exists, ContractID}) = hg_client_party:create_claim(Changeset, Client).

contract_termination(C) ->
    Client = cfg(client, C),
    ContractID = <<"TESTCONTRACT">>,
    Changeset = [?contract_modification(ContractID, ?contract_termination(<<"WHY NOT?!">>))],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        status = {terminated, _}
    } = hg_client_party:get_contract(ContractID, Client).

contract_already_terminated(C) ->
    Client = cfg(client, C),
    ContractID = <<"TESTCONTRACT">>,
    Changeset = [
        ?contract_modification(ContractID, ?contract_termination(<<"JUST TO BE SURE.">>))
    ],
    ?invalid_changeset({contract_status_invalid, _}) = hg_client_party:create_claim(Changeset, Client).

contract_expiration(C) ->
    Client = cfg(client, C),
    ContractParams = hg_ct_helper:make_battle_ready_contract_params(?tmpl(3)),
    PayoutToolParams = hg_ct_helper:make_battle_ready_payout_tool_params(),
    ContractID = <<"CONTRACT_EXPIRED">>,
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
    % FIXME how about already terminated contract?
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
    LA = #domain_LegalAgreement{
        signed_at = hg_datetime:format_now(),
        legal_agreement_id = <<"20160123-0031235-OGM/GDM">>
    },
    Changeset = [?contract_modification(ContractID, {legal_agreement_binding, LA})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Contract{
        id = ContractID,
        legal_agreement = LA
    } = hg_client_party:get_contract(ContractID, Client).

contract_payout_tool_creation(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
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
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
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
    Client = cfg(client, C),
    hg_context:set(woody_context:new()),
    ContractID = ?REAL_CONTRACT_ID,
    ID = <<"ADJ2">>,
    Revision = hg_domain:head(),
    Terms = hg_party:get_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now(),
        Revision
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
    true = Terms /= hg_party:get_terms(
        hg_client_party:get_contract(ContractID, Client),
        hg_datetime:format_now(),
        Revision
    ),
    AfterExpiration = hg_datetime:add_interval(hg_datetime:format_now(), {0, 1, 1}),
    Terms = hg_party:get_terms(hg_client_party:get_contract(ContractID, Client), AfterExpiration, Revision),
    hg_context:cleanup().

shop_not_found_on_retrieval(C) ->
    Client = cfg(client, C),
    ?shop_not_found() = hg_client_party:get_shop(<<"666">>, Client).

shop_creation(C) ->
    Client = cfg(client, C),
    Details = hg_ct_helper:make_shop_details(<<"THRIFT SHOP">>, <<"Hot. Fancy. Almost free.">>),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID = ?REAL_SHOP_ID,
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
        % FIXME is this a bug?
        % suspension = ?suspended(),
        details = Details
    } = hg_client_party:get_shop(ShopID, Client).

shop_terms_retrieval(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = ?REAL_SHOP_ID,
    TermSet1 = hg_client_party:compute_shop_terms(ShopID, hg_datetime:format_now(), Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, [?pmt(bank_card, visa)]}
    }} = TermSet1,
    ok = hg_domain:update(construct_term_set_for_party(PartyID, {shop_is, ShopID})),
    TermSet2 = hg_client_party:compute_shop_terms(ShopID, hg_datetime:format_now(), Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, ?REAL_PARTY_PAYMENT_METHODS}
    }} = TermSet2.

shop_already_exists(C) ->
    Client = cfg(client, C),
    Details = hg_ct_helper:make_shop_details(<<"THRlFT SHOP">>, <<"Hot. Fancy. Almost like thrift.">>),
    ContractID = ?REAL_CONTRACT_ID,
    ShopID = ?REAL_SHOP_ID,
    Params = #payproc_ShopParams{
        category = ?cat(2),
        location = {url, <<"https://s0mename.s0med0main">>},
        details  = Details,
        contract_id = ContractID,
        payout_tool_id = hg_ct_helper:get_first_payout_tool_id(ContractID, Client)
    },
    Changeset = [?shop_modification(ShopID, {creation, Params})],
    ?invalid_changeset({shop_already_exists, ShopID}) = hg_client_party:create_claim(Changeset, Client).

shop_update(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Details = hg_ct_helper:make_shop_details(<<"BARBER SHOP">>, <<"Nice. Short. Clean.">>),
    Changeset1 = [?shop_modification(ShopID, {details_modification, Details})],
    _Claim1 = assert_claim_accepted(hg_client_party:create_claim(Changeset1, Client), Client),
    #domain_Shop{details = Details} = hg_client_party:get_shop(ShopID, Client),

    Location = {url, <<"suspicious_url">>},
    Changeset2 = [?shop_modification(ShopID, {location_modification, Location})],
    Claim2 = assert_claim_pending(hg_client_party:create_claim(Changeset2, Client), Client),
    ok = accept_claim(Claim2, Client),
    #domain_Shop{location = Location, details = Details} = hg_client_party:get_shop(ShopID, Client).

shop_update_before_confirm(C) ->
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
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
    % FIXME add more invalid params checks
    Client = cfg(client, C),
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

    Claim1 = #payproc_Claim{id = ID1, revision = Rev1} = assert_claim_pending(
        hg_client_party:create_claim(
            [?shop_modification(ShopID, {category_modification, ?cat(1)})],
            Client
        ),
        Client
    ),
    ?invalid_changeset(_CategoryError) = hg_client_party:accept_claim(ID1, Rev1, Client),
    ok = revoke_claim(Claim1, Client).

claim_acceptance(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Details = hg_ct_helper:make_shop_details(<<"McDolan">>),
    Location = {url, <<"very_suspicious_url">>},
    Changeset = [
        ?shop_modification(ShopID, {details_modification, Details}),
        ?shop_modification(ShopID, {location_modification, Location})
    ],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = accept_claim(Claim, Client),
    #domain_Shop{location = Location, details = Details} = hg_client_party:get_shop(ShopID, Client).

claim_denial(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Shop = hg_client_party:get_shop(ShopID, Client),
    Location = {url, <<"Pr0nHub">>},
    Changeset = [?shop_modification(ShopID, {location_modification, Location})],
    Claim = assert_claim_pending(hg_client_party:create_claim(Changeset, Client), Client),
    ok = deny_claim(Claim, Client),
    Shop = hg_client_party:get_shop(ShopID, Client).

claim_revocation(C) ->
    Client = cfg(client, C),
    Party = hg_client_party:get(Client),
    ShopID = <<"SHOP3">>,
    ContractID = ?REAL_CONTRACT_ID,
    Params = #payproc_ShopParams{
        location = {url, <<"https://url3">>},
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
    Client = cfg(client, C),
    ContractID = ?REAL_CONTRACT_ID,
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
    [?party_suspension(?suspended(_))] = next_event(Client),
    ok = hg_client_party:activate(Client),
    [?party_suspension(?active(_))] = next_event(Client),
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
    ok = accept_claim(Claim1_1, Client),
    #domain_Shop{details = Details1, category = ?cat(3)} = hg_client_party:get_shop(ShopID1, Client),
    #domain_Shop{details = Details2} = hg_client_party:get_shop(ShopID2, Client).

claim_already_accepted_on_revoke(C) ->
    Client = cfg(client, C),
    Reason = <<"The End is near">>,
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:revoke_claim(
        hg_claim:get_id(Claim),
        hg_claim:get_revision(Claim),
        Reason,
        Client
    ).

claim_already_accepted_on_accept(C) ->
    Client = cfg(client, C),
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:accept_claim(
        hg_claim:get_id(Claim),
        hg_claim:get_revision(Claim),
        Client
    ).

claim_already_accepted_on_deny(C) ->
    Client = cfg(client, C),
    Reason = <<"I am about to destroy them">>,
    Claim = get_first_accepted_claim(Client),
    ?invalid_claim_status(?accepted(_)) = hg_client_party:deny_claim(
        hg_claim:get_id(Claim),
        hg_claim:get_revision(Claim),
        Reason,
        Client
    ).

get_first_accepted_claim(Client) ->
    Claims = lists:filter(
        fun(?claim(_, Status)) ->
            case Status of
                ?accepted(_) ->
                    true;
                _ ->
                    false
            end
        end,
        hg_client_party:get_claims(Client)
    ),
    case Claims of
        [Claim | _] ->
            Claim;
        [] ->
            error(accepted_claim_not_found)
    end.

claim_not_found_on_retrieval(C) ->
    Client = cfg(client, C),
    ?claim_not_found() = hg_client_party:get_claim(-666, Client).

no_pending_claims(C) ->
    Client = cfg(client, C),
    Claims = hg_client_party:get_claims(Client),
    [] = lists:filter(
        fun (?claim(_, ?pending())) ->
                true;
            (_) ->
                false
        end,
        Claims
    ),
    ok.

party_blocking(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    Reason = <<"i said so">>,
    ok = hg_client_party:block(Reason, Client),
    [?party_blocking(?blocked(Reason, _))] = next_event(Client),
    ?party_w_status(PartyID, ?blocked(Reason, _), _) = hg_client_party:get(Client).

party_unblocking(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    Reason = <<"enough">>,
    ok = hg_client_party:unblock(Reason, Client),
    [?party_blocking(?unblocked(Reason, _))] = next_event(Client),
    ?party_w_status(PartyID, ?unblocked(Reason, _), _) = hg_client_party:get(Client).

party_already_blocked(C) ->
    Client = cfg(client, C),
    ?party_blocked(_) = hg_client_party:block(<<"too much">>, Client).

party_already_unblocked(C) ->
    Client = cfg(client, C),
    ?party_unblocked(_) = hg_client_party:unblock(<<"too free">>, Client).

party_blocked_on_suspend(C) ->
    Client = cfg(client, C),
    ?party_blocked(_) = hg_client_party:suspend(Client).

party_suspension(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ok = hg_client_party:suspend(Client),
    [?party_suspension(?suspended(_))] = next_event(Client),
    ?party_w_status(PartyID, _, ?suspended(_)) = hg_client_party:get(Client).

party_activation(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ok = hg_client_party:activate(Client),
    [?party_suspension(?active(_))] = next_event(Client),
    ?party_w_status(PartyID, _, ?active(_)) = hg_client_party:get(Client).

party_already_suspended(C) ->
    Client = cfg(client, C),
    ?party_suspended() = hg_client_party:suspend(Client).

party_already_active(C) ->
    Client = cfg(client, C),
    ?party_active() = hg_client_party:activate(Client).

party_metadata_setting(C) ->
    Client = cfg(client, C),
    NS = hg_ct_helper:make_meta_ns(),
    Data = hg_ct_helper:make_meta_data(NS),
    ok = hg_client_party:set_metadata(NS, Data, Client),
    % lets check for idempotency
    ok = hg_client_party:set_metadata(NS, Data, Client).

party_metadata_retrieval(C) ->
    Client = cfg(client, C),
    ?namespace_not_found() = hg_client_party:get_metadata(<<"NoSuchNamespace">>, Client),
    NS = hg_ct_helper:make_meta_ns(),
    Data0 = hg_ct_helper:make_meta_data(),
    ok = hg_client_party:set_metadata(NS, Data0, Client),
    Data0 = hg_client_party:get_metadata(NS, Client),
    % lets change it and check again
    Data1 = hg_ct_helper:make_meta_data(NS),
    ok = hg_client_party:set_metadata(NS, Data1, Client),
    Data1 = hg_client_party:get_metadata(NS, Client).

party_metadata_removing(C) ->
    Client = cfg(client, C),
    ?namespace_not_found() = hg_client_party:remove_metadata(<<"NoSuchNamespace">>, Client),
    NS = hg_ct_helper:make_meta_ns(),
    ok = hg_client_party:set_metadata(NS, hg_ct_helper:make_meta_data(), Client),
    ok = hg_client_party:remove_metadata(NS, Client),
    ?namespace_not_found() = hg_client_party:remove_metadata(NS, Client).

party_meta_retrieval(C) ->
    Client = cfg(client, C),
    Meta0 = hg_client_party:get_meta(Client),
    NS = hg_ct_helper:make_meta_ns(),
    ok = hg_client_party:set_metadata(NS, hg_ct_helper:make_meta_data(), Client),
    Meta1 = hg_client_party:get_meta(Client),
    Meta0 =/= Meta1.

shop_blocking(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Reason = <<"i said so">>,
    ok = hg_client_party:block_shop(ShopID, Reason, Client),
    [?shop_blocking(ShopID, ?blocked(Reason, _))] = next_event(Client),
    ?shop_w_status(ShopID, ?blocked(Reason, _), _) = hg_client_party:get_shop(ShopID, Client).

shop_unblocking(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    Reason = <<"enough">>,
    ok = hg_client_party:unblock_shop(ShopID, Reason, Client),
    [?shop_blocking(ShopID, ?unblocked(Reason, _))] = next_event(Client),
    ?shop_w_status(ShopID, ?unblocked(Reason, _), _) = hg_client_party:get_shop(ShopID, Client).

shop_already_blocked(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_blocked(_) = hg_client_party:block_shop(ShopID, <<"too much">>, Client).

shop_already_unblocked(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_unblocked(_) = hg_client_party:unblock_shop(ShopID, <<"too free">>, Client).

shop_blocked_on_suspend(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_blocked(_) = hg_client_party:suspend_shop(ShopID, Client).

shop_suspension(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ok = hg_client_party:suspend_shop(ShopID, Client),
    [?shop_suspension(ShopID, ?suspended(_))] = next_event(Client),
    ?shop_w_status(ShopID, _, ?suspended(_)) = hg_client_party:get_shop(ShopID, Client).

shop_activation(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ok = hg_client_party:activate_shop(ShopID, Client),
    [?shop_suspension(ShopID, ?active(_))] = next_event(Client),
    ?shop_w_status(ShopID, _, ?active(_)) = hg_client_party:get_shop(ShopID, Client).

shop_already_suspended(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_suspended() = hg_client_party:suspend_shop(ShopID, Client).

shop_already_active(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    ?shop_active() = hg_client_party:activate_shop(ShopID, Client).

shop_account_set_retrieval(C) ->
    Client = cfg(client, C),
    ShopID = ?REAL_SHOP_ID,
    S = #domain_ShopAccount{} = hg_client_party:get_shop_account(ShopID, Client),
    {save_config, S}.

shop_account_retrieval(C) ->
    Client = cfg(client, C),
    {shop_account_set_retrieval, #domain_ShopAccount{guarantee = AccountID}} = ?config(saved_config, C),
    #payproc_AccountState{account_id = AccountID} = hg_client_party:get_account_state(AccountID, Client).

%% Access control tests

party_access_control(C) ->
    PartyID = cfg(party_id, C),
    % External Success
    GoodExternalClient = cfg(client, C),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodExternalClient),

    % External Reject
    BadExternalClient0 = hg_client_party:start(
        #payproc_UserInfo{id = <<"FakE1D">>, type = {external_user, #payproc_ExternalUser{}}},
        PartyID,
        hg_client_api:new(cfg(root_url, C))
    ),
    ?invalid_user() = hg_client_party:get(BadExternalClient0),
    hg_client_party:stop(BadExternalClient0),

    % UserIdentity has priority
    UserIdentity = #{
        id => PartyID,
        realm => <<"internal">>
    },
    Context = woody_user_identity:put(UserIdentity, woody_context:new()),
    UserIdentityClient1 = hg_client_party:start(
        #payproc_UserInfo{id = <<"FakE1D">>, type = {external_user, #payproc_ExternalUser{}}},
        PartyID,
        hg_client_api:new(cfg(root_url, C), Context)
    ),
    #domain_Party{id = PartyID} = hg_client_party:get(UserIdentityClient1),
    hg_client_party:stop(UserIdentityClient1),

    % Internal Success
    GoodInternalClient = hg_client_party:start(
        #payproc_UserInfo{id = <<"F4KE1D">>, type = {internal_user, #payproc_InternalUser{}}},
        PartyID,
        hg_client_api:new(cfg(root_url, C))
    ),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodInternalClient),
    hg_client_party:stop(GoodInternalClient),

    % Service Success
    GoodServiceClient = hg_client_party:start(
        #payproc_UserInfo{id = <<"fAkE1D">>, type = {service_user, #payproc_ServiceUser{}}},
        PartyID,
        hg_client_api:new(cfg(root_url, C))
    ),
    #domain_Party{id = PartyID} = hg_client_party:get(GoodServiceClient),
    hg_client_party:stop(GoodServiceClient),
    ok.

update_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Changeset, Client) ->
    ok = hg_client_party:update_claim(ClaimID, Revision, Changeset, Client),
    NextRevision = Revision + 1,
    [?claim_updated(ClaimID, Changeset, NextRevision, _)] = next_event(Client),
    ok.

accept_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = hg_client_party:accept_claim(ClaimID, Revision, Client),
    NextRevision = Revision + 1,
    [?claim_status_changed(ClaimID, ?accepted(_), NextRevision, _)] = next_event(Client),
    ok.

deny_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = hg_client_party:deny_claim(ClaimID, Revision, Reason = <<"The Reason">>, Client),
    NextRevision = Revision + 1,
    [?claim_status_changed(ClaimID, ?denied(Reason), NextRevision, _)] = next_event(Client),
    ok.

revoke_claim(#payproc_Claim{id = ClaimID, revision = Revision}, Client) ->
    ok = hg_client_party:revoke_claim(ClaimID, Revision, undefined, Client),
    NextRevision = Revision + 1,
    % FIXME Remove as soon as offline services switch to rbkmoney/damsel@2223cc6
    [?claim_status_changed(ClaimID, ?revoked(<<>>), NextRevision, _)] = next_event(Client),
    ok.

assert_claim_pending(?claim(ClaimID, ?pending()) = Claim, Client) ->
    [?claim_created(?claim(ClaimID))] = next_event(Client),
    Claim.

assert_claim_accepted(?claim(ClaimID, ?accepted(_)) = Claim, Client) ->
    [
        ?claim_created(?claim(ClaimID, ?pending())),
        ?claim_status_changed(ClaimID, ?accepted(_), _, _)
    ] = next_event(Client),
    Claim.

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(cfg(root_url, C))),
    Events = hg_client_eventsink:pull_events(_N = 5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events).

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
    hg_ct_helper:make_userinfo(PartyID).

make_party_params() ->
    make_party_params(#domain_PartyContactInfo{email = <<?MODULE_STRING>>}).
make_party_params(ContactInfo) ->
    #payproc_PartyParams{contact_info = ContactInfo}.

construct_term_set_for_party(PartyID, Def) ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(2),
                ?cat(3)
            ])},
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(PartyID, Def),
                    then_ = {value, ordsets:from_list(?REAL_PARTY_PAYMENT_METHODS)}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([
                        ?pmt(bank_card, visa)
                    ])}
                }
            ]}
        }
    },
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = ?trms(2),
        data = #domain_TermSetHierarchy{
            parent_terms = undefined,
            term_sets = [#domain_TimedTermSet{
                action_time = #'TimestampInterval'{},
                terms = TermSet
            }]
        }
    }}.

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
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, maestro)),

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
                common_merchant_proxy = ?prx(1), % FIXME
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


