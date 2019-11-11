%%% TODO
%%%  - Do not share state between test cases
%%%  - Run cases in parallel

-module(hg_invoice_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invoice_creation_idempotency/1]).
-export([invalid_invoice_shop/1]).
-export([invalid_invoice_amount/1]).
-export([invalid_invoice_currency/1]).
-export([invalid_party_status/1]).
-export([invalid_shop_status/1]).
-export([invalid_invoice_template_cost/1]).
-export([invalid_invoice_template_id/1]).
-export([invoive_w_template_idempotency/1]).
-export([invoice_w_template/1]).
-export([invoice_cancellation/1]).
-export([overdue_invoice_cancellation/1]).
-export([invoice_cancellation_after_payment_timeout/1]).
-export([invalid_payment_amount/1]).

-export([payment_start_idempotency/1]).
-export([payment_success/1]).
-export([processing_deadline_reached_test/1]).
-export([payment_success_empty_cvv/1]).
-export([payment_success_additional_info/1]).
-export([payment_w_terminal_success/1]).
-export([payment_w_crypto_currency_success/1]).
-export([payment_w_wallet_success/1]).
-export([payment_w_customer_success/1]).
-export([payment_w_another_shop_customer/1]).
-export([payment_w_another_party_customer/1]).
-export([payment_w_deleted_customer/1]).
-export([payment_w_mobile_commerce/1]).
-export([payment_suspend_timeout_failure/1]).
-export([payments_w_bank_card_issuer_conditions/1]).
-export([payments_w_bank_conditions/1]).
-export([payment_success_on_second_try/1]).
-export([payment_fail_after_silent_callback/1]).
-export([invoice_success_on_third_payment/1]).
-export([party_revision_check/1]).
-export([payment_risk_score_check/1]).
-export([payment_risk_score_check_fail/1]).
-export([payment_risk_score_check_timeout/1]).
-export([invalid_payment_adjustment/1]).
-export([payment_adjustment_success/1]).
-export([invalid_payment_w_deprived_party/1]).
-export([external_account_posting/1]).
-export([terminal_cashflow_overrides_provider/1]).
-export([payment_hold_cancellation/1]).
-export([payment_hold_double_cancellation/1]).
-export([payment_hold_cancellation_captured/1]).
-export([payment_hold_auto_cancellation/1]).
-export([payment_hold_capturing/1]).
-export([payment_hold_double_capturing/1]).
-export([payment_hold_capturing_cancelled/1]).
-export([deadline_doesnt_affect_payment_capturing/1]).
-export([payment_hold_partial_capturing/1]).
-export([payment_hold_partial_capturing_with_cart/1]).
-export([payment_hold_partial_capturing_with_cart_missing_cash/1]).
-export([invalid_currency_partial_capture/1]).
-export([invalid_amount_partial_capture/1]).
-export([invalid_permit_partial_capture_in_service/1]).
-export([invalid_permit_partial_capture_in_provider/1]).
-export([payment_hold_auto_capturing/1]).
-export([invalid_refund_party_status/1]).
-export([invalid_refund_shop_status/1]).
-export([payment_refund_idempotency/1]).
-export([payment_refund_success/1]).
-export([deadline_doesnt_affect_payment_refund/1]).
-export([payment_manual_refund/1]).
-export([payment_partial_refunds_success/1]).
-export([payment_refund_id_types/1]).
-export([payment_temporary_unavailability_retry_success/1]).
-export([payment_temporary_unavailability_too_many_retries/1]).
-export([invalid_amount_payment_partial_refund/1]).
-export([invalid_amount_partial_capture_and_refund/1]).
-export([ineligible_payment_partial_refund/1]).
-export([invalid_currency_payment_partial_refund/1]).
-export([cant_start_simultaneous_partial_refunds/1]).
-export([retry_temporary_unavailability_refund/1]).
-export([rounding_cashflow_volume/1]).
-export([payment_with_offsite_preauth_success/1]).
-export([payment_with_offsite_preauth_failed/1]).
-export([payment_with_tokenized_bank_card/1]).
-export([terms_retrieval/1]).
-export([payment_has_optional_fields/1]).
-export([payment_capture_failed/1]).
-export([payment_capture_retries_exceeded/1]).

-export([adhoc_repair_working_failed/1]).
-export([adhoc_repair_failed_succeeded/1]).
-export([adhoc_repair_force_removal/1]).
-export([adhoc_repair_invalid_changes_failed/1]).
-export([adhoc_repair_force_invalid_transition/1]).

-export([repair_fail_pre_processing_succeeded/1]).
-export([repair_skip_inspector_succeeded/1]).
-export([repair_fail_session_succeeded/1]).
-export([repair_fail_session_on_pre_processing/1]).
-export([repair_complex_succeeded_first/1]).
-export([repair_complex_succeeded_second/1]).

-export([consistent_account_balances/1]).
-export([consistent_history/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.


%% tests descriptions

-type config()         :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name()     :: hg_ct_helper:group_name().
-type test_return()    :: _ | no_return().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        invalid_party_status,
        invalid_shop_status,

        % With constant domain config
        {group, all_non_destructive_tests},

        payments_w_bank_card_issuer_conditions,
        payments_w_bank_conditions,

        % With variable domain config
        {group, adjustments},
        {group, refunds},
        rounding_cashflow_volume,
        terms_retrieval,

        consistent_account_balances,
        consistent_history
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {all_non_destructive_tests, [parallel], [
            {group, base_payments},
            payment_risk_score_check,
            payment_risk_score_check_fail,
            payment_risk_score_check_timeout,
            party_revision_check,

            invalid_payment_w_deprived_party,
            external_account_posting,
            terminal_cashflow_overrides_provider,

            {group, holds_management},

            {group, offsite_preauth_payment},

            payment_with_tokenized_bank_card,

            {group, adhoc_repairs},

            {group, repair_scenarios}
        ]},

        {base_payments, [parallel], [
            invoice_creation_idempotency,
            invalid_invoice_shop,
            invalid_invoice_amount,
            invalid_invoice_currency,
            invalid_invoice_template_cost,
            invalid_invoice_template_id,
            invoive_w_template_idempotency,
            invoice_w_template,
            invoice_cancellation,
            overdue_invoice_cancellation,
            invoice_cancellation_after_payment_timeout,
            invalid_payment_amount,

            payment_start_idempotency,
            payment_success,
            processing_deadline_reached_test,
            payment_success_empty_cvv,
            payment_success_additional_info,
            payment_w_terminal_success,
            payment_w_crypto_currency_success,
            payment_w_wallet_success,
            payment_w_customer_success,
            payment_w_another_shop_customer,
            payment_w_another_party_customer,
            payment_w_deleted_customer,
            payment_w_mobile_commerce,
            payment_suspend_timeout_failure,
            payment_success_on_second_try,
            payment_fail_after_silent_callback,
            payment_temporary_unavailability_retry_success,
            payment_temporary_unavailability_too_many_retries,
            payment_has_optional_fields,
            invoice_success_on_third_payment,
            payment_capture_failed,
            payment_capture_retries_exceeded
        ]},

        {adjustments, [parallel], [
            invalid_payment_adjustment,
            payment_adjustment_success
        ]},

        {refunds, [], [
            invalid_refund_party_status,
            invalid_refund_shop_status,
            {refunds_, [parallel], [
                retry_temporary_unavailability_refund,
                payment_refund_idempotency,
                payment_refund_success,
                deadline_doesnt_affect_payment_refund,
                payment_partial_refunds_success,
                invalid_amount_payment_partial_refund,
                invalid_amount_partial_capture_and_refund,
                invalid_currency_payment_partial_refund,
                cant_start_simultaneous_partial_refunds
            ]},
            ineligible_payment_partial_refund,
            payment_manual_refund,
            payment_refund_id_types
        ]},

        {holds_management, [parallel], [
            payment_hold_cancellation,
            payment_hold_double_cancellation,
            payment_hold_cancellation_captured,
            payment_hold_auto_cancellation,
            payment_hold_capturing,
            payment_hold_double_capturing,
            payment_hold_capturing_cancelled,
            deadline_doesnt_affect_payment_capturing,
            invalid_currency_partial_capture,
            invalid_amount_partial_capture,
            payment_hold_partial_capturing,
            payment_hold_partial_capturing_with_cart,
            payment_hold_partial_capturing_with_cart_missing_cash,
            payment_hold_auto_capturing,
            {group, holds_management_with_custom_config}
        ]},

        {holds_management_with_custom_config, [], [
            invalid_permit_partial_capture_in_service,
            invalid_permit_partial_capture_in_provider
        ]},

        {offsite_preauth_payment, [parallel], [
            payment_with_offsite_preauth_success,
            payment_with_offsite_preauth_failed
        ]},
        {adhoc_repairs, [parallel], [
            adhoc_repair_working_failed,
            adhoc_repair_failed_succeeded,
            adhoc_repair_force_removal,
            adhoc_repair_invalid_changes_failed,
            adhoc_repair_force_invalid_transition
        ]},
        {repair_scenarios, [parallel], [
            repair_fail_pre_processing_succeeded,
            repair_skip_inspector_succeeded,
            repair_fail_session_succeeded,
            repair_fail_session_on_pre_processing,
            repair_complex_succeeded_first,
            repair_complex_succeeded_second
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_invoice_payment', 'p', '_'}, x),
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),

    {Apps, Ret} = hg_ct_helper:start_apps([
        woody, scoper, dmt_client, party_client, hellgate, {cowboy, CowboySpec}
    ]),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    CustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID)),
    AnotherPartyID = hg_utils:unique_id(),
    AnotherPartyClient = hg_client_party:start(AnotherPartyID, hg_ct_helper:create_client(RootUrl, AnotherPartyID)),
    AnotherCustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, AnotherPartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    AnotherShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), AnotherPartyClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    {ok, _} = supervisor:start_child(SupPid, hg_dummy_fault_detector:child_spec()),
    _ = unlink(SupPid),
    ok = start_kv_store(SupPid),
    NewC = [
        {party_id, PartyID},
        {party_client, PartyClient},
        {shop_id, ShopID},
        {customer_client, CustomerClient},
        {another_party_id, AnotherPartyID},
        {another_party_client, AnotherPartyClient},
        {another_shop_id, AnotherShopID},
        {another_customer_client, AnotherCustomerClient},
        {root_url, RootUrl},
        {apps, Apps},
        {test_sup, SupPid}
        | C
    ],

    ok = start_proxies([{hg_dummy_provider, 1, NewC}, {hg_dummy_inspector, 2, NewC}]),
    NewC.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    SupPid = cfg(test_sup, C),
    ok = supervisor:terminate_child(SupPid, hg_dummy_fault_detector),
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)],
    exit(cfg(test_sup, C), shutdown).

%% tests

-include("invoice_events.hrl").
-include("payment_events.hrl").
-include("customer_events.hrl").

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(payment(ID, Revision), #domain_InvoicePayment{id = ID, party_revision = Revision}).
-define(customer(ID), #payproc_Customer{id = ID}).
-define(adjustment(ID), #domain_InvoicePaymentAdjustment{id = ID}).
-define(adjustment(ID, Status), #domain_InvoicePaymentAdjustment{id = ID, status = Status}).
-define(adjustment_revision(Revision), #domain_InvoicePaymentAdjustment{party_revision = Revision}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(invoice_state(Invoice, Payments), #payproc_Invoice{invoice = Invoice, payments = Payments}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(invoice_w_revision(Revision), #domain_Invoice{party_revision = Revision}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
-define(payment_w_context(Context), #domain_InvoicePayment{context = Context}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).
-define(trx_info(ID, Extra), #domain_TransactionInfo{id = ID, extra = Extra}).


-define(invalid_invoice_status(Status),
    {exception, #payproc_InvalidInvoiceStatus{status = Status}}).

-define(invalid_payment_status(Status),
    {exception, #payproc_InvalidPaymentStatus{status = Status}}).
-define(invalid_adjustment_status(Status),
    {exception, #payproc_InvalidPaymentAdjustmentStatus{status = Status}}).
-define(invalid_adjustment_pending(ID),
    {exception, #payproc_InvoicePaymentAdjustmentPending{id = ID}}).
-define(operation_not_permitted(),
    {exception, #payproc_OperationNotPermitted{}}).
-define(insufficient_account_balance(),
    {exception, #payproc_InsufficientAccountBalance{}}).
-define(invoice_payment_amount_exceeded(Maximum),
    {exception, #payproc_InvoicePaymentAmountExceeded{maximum = Maximum}}).
-define(inconsistent_refund_currency(Currency),
    {exception, #payproc_InconsistentRefundCurrency{currency = Currency}}).
-define(inconsistent_capture_currency(Currency),
    {exception, #payproc_InconsistentCaptureCurrency{payment_currency = Currency}}).
-define(amount_exceeded_capture_balance(Amount),
    {exception, #payproc_AmountExceededCaptureBalance{payment_amount = Amount}}).

-spec init_per_group(group_name(), config()) -> config().

init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) when
    Name == payment_adjustment_success;
    Name == rounding_cashflow_volume;
    Name == payments_w_bank_card_issuer_conditions;
    Name == payments_w_bank_conditions;
    Name == ineligible_payment_partial_refund;
    Name == invalid_permit_partial_capture_in_service;
    Name == invalid_permit_partial_capture_in_provider
->
    Revision = hg_domain:head(),
    Fixture = case Name of
        payment_adjustment_success ->
            get_adjustment_fixture(Revision);
        rounding_cashflow_volume ->
            get_cashflow_rounding_fixture(Revision);
        payments_w_bank_card_issuer_conditions ->
            payments_w_bank_card_issuer_conditions_fixture(Revision);
        payments_w_bank_conditions ->
            payments_w_bank_conditions_fixture(Revision);
        ineligible_payment_partial_refund ->
            construct_term_set_for_refund_eligibility_time(1);
        invalid_permit_partial_capture_in_service ->
            construct_term_set_for_partial_capture_service_permit();
        invalid_permit_partial_capture_in_provider ->
            construct_term_set_for_partial_capture_provider_permit(Revision)
    end,
    ok = hg_domain:upsert(Fixture),
    [{original_domain_revision, Revision} | init_per_testcase(C)];
init_per_testcase(_Name, C) ->
    init_per_testcase(C).

init_per_testcase(C) ->
    ApiClient = hg_ct_helper:create_client(cfg(root_url, C), cfg(party_id, C)),
    Client = hg_client_invoicing:start_link(ApiClient),
    ClientTpl = hg_client_invoice_templating:start_link(ApiClient),
    ok = hg_context:save(hg_context:create()),
    [{client, Client}, {client_tpl, ClientTpl} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    ok = hg_context:cleanup(),
    _ = case cfg(original_domain_revision, C) of
        Revision when is_integer(Revision) ->
            ok = hg_domain:reset(Revision);
        undefined ->
            ok
    end.

-spec invoice_creation_idempotency(config()) -> _ | no_return().

invoice_creation_idempotency(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceID = hg_utils:unique_id(),
    ExternalID = <<"123">>,
    InvoiceParams0 = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100000, <<"RUB">>}),
    InvoiceParams1 = InvoiceParams0#payproc_InvoiceParams{
        id = InvoiceID,
        external_id = ExternalID
    },
    Invoice1 = hg_client_invoicing:create(InvoiceParams1, Client),
    #payproc_Invoice{
        invoice = DomainInvoice
    } = Invoice1,
    #domain_Invoice{
        id = InvoiceID,
        external_id = ExternalID
    } = DomainInvoice,
    Invoice2 = hg_client_invoicing:create(InvoiceParams1, Client),
    Invoice1 = Invoice2.

-spec invalid_invoice_shop(config()) -> _ | no_return().

invalid_invoice_shop(C) ->
    Client = cfg(client, C),
    ShopID = genlib:unique(),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    {exception, #payproc_ShopNotFound{}} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_invoice_amount(config()) -> test_return().

invalid_invoice_amount(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, -10000),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount">>]
    }} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_invoice_currency(config()) -> test_return().

invalid_invoice_currency(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100, <<"KEK">>}),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid currency">>]
    }} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_party_status(config()) -> test_return().

invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100000, <<"RUB">>}),
    TplID = create_invoice_tpl(C),
    InvoiceParamsWithTpl = make_invoice_params_tpl(TplID),

    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create_with_tpl(InvoiceParamsWithTpl, Client),
    ok = hg_client_party:activate(PartyClient),

    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create_with_tpl(InvoiceParamsWithTpl, Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec invalid_shop_status(config()) -> test_return().

invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100000, <<"RUB">>}),
    TplID = create_invoice_tpl(C),
    InvoiceParamsWithTpl = make_invoice_params_tpl(TplID),

    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create_with_tpl(InvoiceParamsWithTpl, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),

    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create_with_tpl(InvoiceParamsWithTpl, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec invalid_invoice_template_cost(config()) -> _ | no_return().

invalid_invoice_template_cost(C) ->
    Client = cfg(client, C),
    Context = make_invoice_context(),

    Cost1 = make_tpl_cost(unlim, sale, "30%"),
    TplID = create_invoice_tpl(C, Cost1, Context),
    Params1 = make_invoice_params_tpl(TplID),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_NO_COST]
    }} = hg_client_invoicing:create_with_tpl(Params1, Client),

    Cost2 = make_tpl_cost(fixed, 100, <<"RUB">>),
    _ = update_invoice_tpl(TplID, Cost2, C),
    Params2 = make_invoice_params_tpl(TplID, make_cash(50, <<"RUB">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_COST]
    }} = hg_client_invoicing:create_with_tpl(Params2, Client),
    Params3 = make_invoice_params_tpl(TplID, make_cash(100, <<"KEK">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_COST]
    }} = hg_client_invoicing:create_with_tpl(Params3, Client),

    Cost3 = make_tpl_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"RUB">>}),
    _ = update_invoice_tpl(TplID, Cost3, C),
    Params4 = make_invoice_params_tpl(TplID, make_cash(50, <<"RUB">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_AMOUNT]
    }} = hg_client_invoicing:create_with_tpl(Params4, Client),
    Params5 = make_invoice_params_tpl(TplID, make_cash(50000, <<"RUB">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_AMOUNT]
    }} = hg_client_invoicing:create_with_tpl(Params5, Client),
    Params6 = make_invoice_params_tpl(TplID, make_cash(500, <<"KEK">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_CURRENCY]
    }} = hg_client_invoicing:create_with_tpl(Params6, Client).

-spec invalid_invoice_template_id(config()) -> _ | no_return().

invalid_invoice_template_id(C) ->
    Client = cfg(client, C),

    TplID1 = <<"Watsthat">>,
    Params1 = make_invoice_params_tpl(TplID1),
    {exception, #payproc_InvoiceTemplateNotFound{}} = hg_client_invoicing:create_with_tpl(Params1, Client),

    TplID2 = create_invoice_tpl(C),
    _ = delete_invoice_tpl(TplID2, C),
    Params2 = make_invoice_params_tpl(TplID2),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoicing:create_with_tpl(Params2, Client).

-spec invoive_w_template_idempotency(config()) -> _ | no_return().

invoive_w_template_idempotency(C) ->
    Client = cfg(client, C),
    TplCost1 = {_, FixedCost} = make_tpl_cost(fixed, 10000, <<"RUB">>),
    TplContext1 = make_invoice_context(<<"default context">>),
    TplID = create_invoice_tpl(C, TplCost1, TplContext1),
    #domain_InvoiceTemplate{
        owner_id = TplPartyID,
        shop_id  = TplShopID,
        context  = TplContext1
    } = get_invoice_tpl(TplID, C),
    InvoiceCost1 = FixedCost,
    InvoiceContext1 = make_invoice_context(),
    InvoiceID = hg_utils:unique_id(),
    ExternalID = hg_utils:unique_id(),

    Params = make_invoice_params_tpl(TplID, InvoiceCost1, InvoiceContext1),
    Params1 = Params#payproc_InvoiceWithTemplateParams{
        id = InvoiceID,
        external_id = ExternalID
    },
    ?invoice_state(#domain_Invoice{
        id          = InvoiceID,
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1,
        external_id = ExternalID
    }) = hg_client_invoicing:create_with_tpl(Params1, Client),

    OtherParams = make_invoice_params_tpl(TplID),
    Params2 = OtherParams#payproc_InvoiceWithTemplateParams{
        id = InvoiceID,
        external_id = hg_utils:unique_id()
    },
    ?invoice_state(#domain_Invoice{
        id          = InvoiceID,
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1,
        external_id = ExternalID
    }) = hg_client_invoicing:create_with_tpl(Params2, Client).

-spec invoice_w_template(config()) -> _ | no_return().

invoice_w_template(C) ->
    Client = cfg(client, C),
    TplCost1 = {_, FixedCost} = make_tpl_cost(fixed, 10000, <<"RUB">>),
    TplContext1 = make_invoice_context(<<"default context">>),
    TplID = create_invoice_tpl(C, TplCost1, TplContext1),
    #domain_InvoiceTemplate{
        owner_id = TplPartyID,
        shop_id  = TplShopID,
        context  = TplContext1
    } = get_invoice_tpl(TplID, C),
    InvoiceCost1 = FixedCost,
    InvoiceContext1 = make_invoice_context(<<"invoice specific context">>),

    Params1 = make_invoice_params_tpl(TplID, InvoiceCost1, InvoiceContext1),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client),

    Params2 = make_invoice_params_tpl(TplID),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = TplContext1
    }) = hg_client_invoicing:create_with_tpl(Params2, Client),

    TplCost2 = make_tpl_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"RUB">>}),
    _ = update_invoice_tpl(TplID, TplCost2, C),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client),

    TplCost3 = make_tpl_cost(unlim, sale, "146%"),
    _ = update_invoice_tpl(TplID, TplCost3, C),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client).

-spec invoice_cancellation(config()) -> test_return().

invoice_cancellation(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    InvoiceID = create_invoice(InvoiceParams, Client),
    ?invalid_invoice_status(_) = hg_client_invoicing:fulfill(InvoiceID, <<"perfect">>, Client),
    ok = hg_client_invoicing:rescind(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancellation(config()) -> test_return().

overdue_invoice_cancellation(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(1), 10000, C),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec invoice_cancellation_after_payment_timeout(config()) -> test_return().

invoice_cancellation_after_payment_timeout(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberdusk">>, make_due_date(3), 1000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    _UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    %% wait for payment timeout
    PaymentID = await_payment_process_timeout(InvoiceID, PaymentID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec invalid_payment_amount(config()) -> test_return().

invalid_payment_amount(C) ->
    Client = cfg(client, C),
    PaymentParams = make_payment_params(),
    InvoiceID1 = start_invoice(<<"rubberduck">>, make_due_date(10), 1, C),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount, less", _/binary>>]
    }} = hg_client_invoicing:start_payment(InvoiceID1, PaymentParams, Client),
    InvoiceID2 = start_invoice(<<"rubberduck">>, make_due_date(10), 100000000000000, C),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount, more", _/binary>>]
    }} = hg_client_invoicing:start_payment(InvoiceID2, PaymentParams, Client).

-spec payment_start_idempotency(config()) -> test_return().

payment_start_idempotency(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams0 = make_payment_params(),
    PaymentID1 = <<"1">>,
    ExternalID = <<"42">>,
    PaymentParams1 = PaymentParams0#payproc_InvoicePaymentParams{
        id = PaymentID1,
        external_id = ExternalID
    },
    ?payment_state(#domain_InvoicePayment{
        id = PaymentID1,
        external_id = ExternalID
    }) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams1, Client),
    ?payment_state(#domain_InvoicePayment{
        id = PaymentID1,
        external_id = ExternalID
    }) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams1, Client),
    PaymentParams2 = PaymentParams0#payproc_InvoicePaymentParams{id = <<"2">>},
    {exception, #payproc_InvoicePaymentPending{id = PaymentID1}} =
        hg_client_invoicing:start_payment(InvoiceID, PaymentParams2, Client),
    PaymentID1 = process_payment(InvoiceID, PaymentParams1, Client),
    PaymentID1 = await_payment_capture(InvoiceID, PaymentID1, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID1, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    ?payment_state(#domain_InvoicePayment{
        id = PaymentID1,
        external_id = ExternalID
    }) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams1, Client).

-spec payment_success(config()) -> test_return().

payment_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    Context = #'Content'{
        type = <<"application/x-erlang-binary">>,
        data = erlang:term_to_binary({you, 643, "not", [<<"welcome">>, here]})
    },
    PaymentParams = set_payment_context(Context, make_payment_params()),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(Payment)]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    ?payment_w_status(PaymentID, ?captured()) = Payment,
    ?payment_w_context(Context) = Payment.

-spec processing_deadline_reached_test(config()) -> test_return().

processing_deadline_reached_test(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    Context = #'Content'{
        type = <<"application/x-erlang-binary">>,
        data = erlang:term_to_binary({you, 643, "not", [<<"welcome">>, here]})
    },
    PaymentParams0 = set_payment_context(Context, make_payment_params()),
    Deadline = hg_datetime:format_now(),
    PaymentParams = PaymentParams0#payproc_InvoicePaymentParams{processing_deadline = Deadline},
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_sessions_restarts(PaymentID, ?processed(), InvoiceID, Client, 0),
    [?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, Failure})))] = next_event(InvoiceID, Client),
    ok = payproc_errors:match(
        'PaymentFailure',
        Failure,
        fun({authorization_failed, {processing_deadline_reached, _}}) -> ok end
    ).

-spec payment_success_empty_cvv(config()) -> test_return().

payment_success_empty_cvv(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(empty_cvv),
    PaymentParams = make_payment_params(PaymentTool, Session, instant),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_success_additional_info(config()) -> test_return().

payment_success_additional_info(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(empty_cvv),
    PaymentParams = make_payment_params(PaymentTool, Session, instant),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),

    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(Trx))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
    ] = next_event(InvoiceID, Client),
    #domain_TransactionInfo{additional_info = AdditionalInfo} = Trx,
    AdditionalInfo = hg_ct_fixture:construct_dummy_additional_info(),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),

    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_has_optional_fields(config()) -> test_return().

payment_has_optional_fields(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?payment_state(Payment) = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    #domain_InvoicePayment{owner_id = PartyID, shop_id = ShopID, route = Route} = Payment,
    false = Route =:= undefined.

-spec payment_capture_failed(config()) -> test_return().

payment_capture_failed(C) ->
    Client = cfg(client, C),
    Amount = 42000,
    Cost = ?cash(Amount, <<"RUB">>),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), Amount, C),
    PaymentParams = make_scenario_payment_params([good, fail]),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_capture_started(_)),
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 5000, Client),
    ?assertException(
        error,
        {{woody_error, _}, _},
        hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client)
    ),
    PaymentID = repair_failed_capture(InvoiceID, PaymentID, ?timeout_reason(), Cost, Client).

-spec payment_capture_retries_exceeded(config()) -> test_return().

payment_capture_retries_exceeded(C) ->
    Client = cfg(client, C),
    Amount = 42000,
    Cost = ?cash(Amount, <<"RUB">>),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), Amount, C),
    PaymentParams = make_scenario_payment_params([good, temp, temp, temp, temp]),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Reason = ?timeout_reason(),
    Target = ?captured(Reason, Cost),
    [
        ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cost, _)),
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cost), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_sessions_restarts(PaymentID, Target, InvoiceID, Client, 3),
    timeout = next_event(InvoiceID, 5000, Client),
    ?assertException(
        error,
        {{woody_error, _}, _},
        hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client)
    ),
    PaymentID = repair_failed_capture(InvoiceID, PaymentID, Reason, Cost, Client).

repair_failed_capture(InvoiceID, PaymentID, Reason, Cost, Client) ->
    Target = ?captured(Reason, Cost),
    Changes = [
        ?payment_ev(PaymentID, ?session_ev(Target, ?session_finished(?session_succeeded())))
    ],
    ok = repair_invoice(InvoiceID, Changes, Client),
    PaymentID = await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, 0).

-spec payment_w_terminal_success(config()) -> _ | no_return().

payment_w_terminal_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberruble">>, make_due_date(10), 42000, C),
    PaymentParams = make_terminal_payment_params(),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    %% simulate user interaction
    {URL, GoodForm} = get_post_request(UserInteraction),
    BadForm = #{<<"tag">> => <<"666">>},
    _ = assert_invalid_post_request({URL, BadForm}),
    _ = assert_success_post_request({URL, GoodForm}),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_w_crypto_currency_success(config()) -> _ | no_return().

payment_w_crypto_currency_success(C) ->
    Client = cfg(client, C),
    PayCash = 2000,
    InvoiceID = start_invoice(<<"cryptoduck">>, make_due_date(10), PayCash, C),
    PaymentParams = make_crypto_currency_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(low)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(CF))
    ] = next_event(InvoiceID, Client),
    ?cash(PayCash, <<"RUB">>) = get_cashflow_volume({provider, settlement}, {merchant, settlement}, CF),
    ?cash(40, <<"RUB">>) = get_cashflow_volume({system, settlement}, {provider, settlement}, CF),
    ?cash(90, <<"RUB">>) = get_cashflow_volume({merchant, settlement}, {system, settlement}, CF).

-spec payment_w_mobile_commerce(config()) -> _ | no_return().

payment_w_mobile_commerce(C) ->
    Client = cfg(client, C),
    PayCash = 1001,
    InvoiceID = start_invoice(<<"oatmeal">>, make_due_date(10), PayCash, C),
    PaymentParams = make_mobile_commerce_params(success),
    hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client).

-spec payment_suspend_timeout_failure(config()) -> _ | no_return().

payment_suspend_timeout_failure(C) ->
    Client = cfg(client, C),
    PayCash = 1001,
    InvoiceID = start_invoice(<<"oatmeal">>, make_due_date(10), PayCash, C),
    PaymentParams = make_mobile_commerce_params(failure),
    _ = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_failed({failure, _Failure})))),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, _Failure})))
    ] = next_event(InvoiceID, Client).

-spec payment_w_wallet_success(config()) -> _ | no_return().

payment_w_wallet_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"bubbleblob">>, make_due_date(10), 42000, C),
    PaymentParams = make_wallet_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_w_customer_success(config()) -> test_return().

payment_w_customer_success(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    CustomerID = make_customer_w_rec_tool(PartyID, ShopID, cfg(customer_client, C)),
    PaymentParams = make_customer_payment_params(CustomerID),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_w_another_shop_customer(config()) -> test_return().

payment_w_another_shop_customer(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    PartyClient = cfg(party_client, C),
    AnotherShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(AnotherShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    CustomerID = make_customer_w_rec_tool(PartyID, ShopID, cfg(customer_client, C)),
    PaymentParams = make_customer_payment_params(CustomerID),
    {exception, #'InvalidRequest'{}} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client).

-spec payment_w_another_party_customer(config()) -> test_return().

payment_w_another_party_customer(C) ->
    Client = cfg(client, C),
    AnotherPartyID = cfg(another_party_id, C),
    ShopID = cfg(shop_id, C),
    AnotherShopID = cfg(another_shop_id, C),
    CustomerID = make_customer_w_rec_tool(AnotherPartyID, AnotherShopID, cfg(another_customer_client, C)),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_customer_payment_params(CustomerID),
    {exception, #'InvalidRequest'{}} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client).

-spec payment_w_deleted_customer(config()) -> test_return().

payment_w_deleted_customer(C) ->
    Client = cfg(client, C),
    CustomerClient = cfg(customer_client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    CustomerID = make_customer_w_rec_tool(PartyID, ShopID, CustomerClient),
    ok = hg_client_customer:delete(CustomerID, CustomerClient),
    PaymentParams = make_customer_payment_params(CustomerID),
    {exception, #'InvalidRequest'{}} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client).

-spec payment_success_on_second_try(config()) -> test_return().

payment_success_on_second_try(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    %% simulate user interaction
    {URL, GoodForm} = get_post_request(UserInteraction),
    BadForm = #{<<"tag">> => <<"666">>},
    _ = assert_invalid_post_request({URL, BadForm}),
    %% make noop callback call
    _ = assert_success_post_request({URL, hg_dummy_provider:construct_silent_callback(GoodForm)}),
    %% ensure that suspend is still holding up
    _ = assert_success_post_request({URL, GoodForm}),
    %% ensure that callback is now invalid̋
    _ = assert_invalid_post_request({URL, GoodForm}),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec payment_fail_after_silent_callback(config()) -> _ | no_return().

payment_fail_after_silent_callback(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentID = start_payment(InvoiceID, make_tds_payment_params(), Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    {URL, Form} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, hg_dummy_provider:construct_silent_callback(Form)}),
    PaymentID = await_payment_process_timeout(InvoiceID, PaymentID, Client).

-spec payments_w_bank_card_issuer_conditions(config()) -> test_return().

payments_w_bank_card_issuer_conditions(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(1), <<"RUB">>, ?tmpl(4), ?pinst(1), PartyClient),
    %kaz success
    FirstInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1000, C),
    {{bank_card, BankCard}, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    KazBankCard = BankCard#domain_BankCard{
        issuer_country = kaz,
        metadata = #{<<?MODULE_STRING>> => {obj, #{{str, <<"vsn">>} => {i, 42}}}}
    },
    KazPaymentParams = make_payment_params({bank_card, KazBankCard}, Session, instant),
    FirstPayment = process_payment(FirstInvoice, KazPaymentParams, Client),
    FirstPayment = await_payment_capture(FirstInvoice, FirstPayment, Client),
    %kaz fail
    SecondInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {exception,
        {'InvalidRequest', [<<"Invalid amount, more than allowed maximum">>]}
    } = hg_client_invoicing:start_payment(SecondInvoice, KazPaymentParams, Client),
    %rus success
    ThirdInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {{bank_card, BankCard1}, Session1} = hg_dummy_provider:make_payment_tool(no_preauth),
    RusBankCard = BankCard1#domain_BankCard{
        issuer_country = rus,
        metadata = #{<<?MODULE_STRING>> => {obj, #{{str, <<"vsn">>} => {i, 42}}}}
    },
    RusPaymentParams = make_payment_params({bank_card, RusBankCard}, Session1, instant),
    SecondPayment = process_payment(ThirdInvoice, RusPaymentParams, Client),
    SecondPayment = await_payment_capture(ThirdInvoice, SecondPayment, Client),
    %fail with undefined issuer_country
    FourthInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {UndefBankCard, Session2} = hg_dummy_provider:make_payment_tool(no_preauth),
    UndefPaymentParams = make_payment_params(UndefBankCard, Session2, instant),
    ?assertException(%fix me
        error,
        {{woody_error, _}, _},
        hg_client_invoicing:start_payment(FourthInvoice, UndefPaymentParams, Client)
    ).

-spec payments_w_bank_conditions(config()) -> test_return().

payments_w_bank_conditions(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(1), <<"RUB">>, ?tmpl(4), ?pinst(1), PartyClient),
    %bank 1 success
    FirstInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1000, C),
    {{bank_card, BankCard}, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    TestBankCard = BankCard#domain_BankCard {
        bank_name = <<"TEST BANK">>
    },
    TestPaymentParams = make_payment_params({bank_card, TestBankCard}, Session, instant),
    FirstPayment = process_payment(FirstInvoice, TestPaymentParams, Client),
    FirstPayment = await_payment_capture(FirstInvoice, FirstPayment, Client),
    %bank 1 fail
    SecondInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {exception,
        {'InvalidRequest', [<<"Invalid amount, more than allowed maximum">>]}
    } = hg_client_invoicing:start_payment(SecondInvoice, TestPaymentParams, Client),
    %bank 1 /w different wildcard fail
    ThirdInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {{bank_card, BankCard1}, Session1} = hg_dummy_provider:make_payment_tool(no_preauth),
    WildBankCard = BankCard1#domain_BankCard {
        bank_name = <<"TESTBANK">>
    },
    WildPaymentParams = make_payment_params({bank_card, WildBankCard}, Session1, instant),
    {exception,
        {'InvalidRequest', [<<"Invalid amount, more than allowed maximum">>]}
    } = hg_client_invoicing:start_payment(ThirdInvoice, WildPaymentParams, Client),
    %some other bank success
    FourthInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 10000, C),
    {{bank_card, BankCard2}, Session2} = hg_dummy_provider:make_payment_tool(no_preauth),
    OthrBankCard = BankCard2#domain_BankCard {
        bank_name = <<"SOME OTHER BANK">>
    },
    OthrPaymentParams = make_payment_params({bank_card, OthrBankCard}, Session2, instant),
    ThirdPayment = process_payment(FourthInvoice, OthrPaymentParams, Client),
    ThirdPayment = await_payment_capture(FourthInvoice, ThirdPayment, Client),
    %test fallback to bins with undefined bank_name
    FifthInvoice = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 1001, C),
    {{bank_card, BankCard3}, Session3} = hg_dummy_provider:make_payment_tool(no_preauth),
    FallbackBankCard = BankCard3#domain_BankCard {
        bin = <<"42424242">>
    },
    FallbackPaymentParams = make_payment_params({bank_card, FallbackBankCard}, Session3, instant),
    {exception,
        {'InvalidRequest', [<<"Invalid amount, more than allowed maximum">>]}
    } = hg_client_invoicing:start_payment(FifthInvoice, FallbackPaymentParams, Client).


-spec invoice_success_on_third_payment(config()) -> test_return().

invoice_success_on_third_payment(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberdock">>, make_due_date(60), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID1 = start_payment(InvoiceID, PaymentParams, Client),
    %% wait for payment timeout and start new one after
    _ = await_payment_process_interaction(InvoiceID, PaymentID1, Client),
    PaymentID1 = await_payment_process_timeout(InvoiceID, PaymentID1, Client),
    PaymentID2 = start_payment(InvoiceID, PaymentParams, Client),
    %% wait for payment timeout and start new one after
    _ = await_payment_process_interaction(InvoiceID, PaymentID2, Client),
    PaymentID2 = await_payment_process_timeout(InvoiceID, PaymentID2, Client),
    PaymentID3 = start_payment(InvoiceID, PaymentParams, Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID3, Client),
    GoodPost = get_post_request(UserInteraction),
    %% simulate user interaction FTW!
    _ = assert_success_post_request(GoodPost),
    PaymentID3 = await_payment_process_finish(InvoiceID, PaymentID3, Client),
    PaymentID3 = await_payment_capture(InvoiceID, PaymentID3, Client).

%% @TODO modify this test by failures of inspector in case of wrong terminal choice
-spec payment_risk_score_check(config()) -> test_return().

payment_risk_score_check(C) ->
    Client = cfg(client, C),
    % Invoice w/ cost < 500000
    InvoiceID1 = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID1)) = hg_client_invoicing:start_payment(InvoiceID1, PaymentParams, Client),
    [
        ?payment_ev(PaymentID1, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID1, Client),
    [
        ?payment_ev(PaymentID1, ?risk_score_changed(low)), % low risk score...
        % ...covered with high risk coverage terminal
        ?payment_ev(PaymentID1, ?route_changed(?route(?prv(1), ?trm(1)))),
        ?payment_ev(PaymentID1, ?cash_flow_changed(_))
    ] = next_event(InvoiceID1, Client),
    [
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID1, Client),
    PaymentID1 = await_payment_process_finish(InvoiceID1, PaymentID1, Client),
    PaymentID1 = await_payment_capture(InvoiceID1, PaymentID1, Client),
    % Invoice w/ 500000 < cost < 100000000
    InvoiceID2 = start_invoice(<<"rubberbucks">>, make_due_date(10), 31337000, C),
    ?payment_state(?payment(PaymentID2)) = hg_client_invoicing:start_payment(InvoiceID2, PaymentParams, Client),
    [
        ?payment_ev(PaymentID2, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID2, Client),
    [
        ?payment_ev(PaymentID2, ?risk_score_changed(high)), % high risk score...
        % ...covered with the same terminal
        ?payment_ev(PaymentID2, ?route_changed(?route(?prv(101), ?trm(1)))),
        ?payment_ev(PaymentID2, ?cash_flow_changed(_))
    ] = next_event(InvoiceID2, Client),
    [
        ?payment_ev(PaymentID2, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID2, Client),
    PaymentID2 = await_payment_process_finish(InvoiceID2, PaymentID2, Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % Invoice w/ 100000000 =< cost
    InvoiceID3 = start_invoice(<<"rubbersocks">>, make_due_date(10), 100000000, C),
    ?payment_state(?payment(PaymentID3)) = hg_client_invoicing:start_payment(InvoiceID3, PaymentParams, Client),
    [
        ?payment_ev(PaymentID3, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID3, Client),
    [
        % fatal risk score is not going to be covered
        ?payment_ev(PaymentID3, ?risk_score_changed(fatal)),
        ?payment_ev(PaymentID3, ?payment_status_changed(?failed({failure, Failure})))
    ] = next_event(InvoiceID3, Client),
    ok = payproc_errors:match(
        'PaymentFailure',
        Failure,
        fun({no_route_found, _}) -> ok end
    ).

-spec payment_risk_score_check_fail(config()) -> test_return().

payment_risk_score_check_fail(C) ->
    payment_risk_score_check(4, C).

-spec payment_risk_score_check_timeout(config()) -> test_return().

payment_risk_score_check_timeout(C) ->
    payment_risk_score_check(5, C).

-spec party_revision_check(config()) -> test_return().

party_revision_check(C) ->
    {PartyID, PartyClient, Client, ShopID} = party_revision_check_init_params(C),
    {InvoiceRev, InvoiceID} = invoice_create_and_get_revision(PartyID, Client, ShopID),

    party_revision_increment(ShopID, PartyClient),

    {PaymentRev, PaymentID} = make_payment_and_get_revision(InvoiceID, Client),
    PaymentRev = InvoiceRev + 1,

    party_revision_increment(ShopID, PartyClient),

    AdjustmentRev = make_payment_adjustment_and_get_revision(PaymentID, InvoiceID, Client),
    AdjustmentRev = PaymentRev + 1,

    party_revision_increment(ShopID, PartyClient),

    % add some cash to make smooth refund after
    InvoiceParams2 = make_invoice_params(PartyID, ShopID, <<"rubbermoss">>, make_due_date(10), 200000),
    InvoiceID2 = create_invoice(InvoiceParams2, Client),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID2, Client),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),

    RefundRev = make_payment_refund_and_get_revision(PaymentID, InvoiceID, Client),
    RefundRev = AdjustmentRev + 1.

-spec invalid_payment_adjustment(config()) -> test_return().

invalid_payment_adjustment(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    %% start a smoker's payment
    PaymentParams = make_tds_payment_params(),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    %% no way to create adjustment for a payment not yet finished
    ?invalid_payment_status(?pending()) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client),
    _UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_process_timeout(InvoiceID, PaymentID, Client),
    %% no way to create adjustment for a failed payment
    ?invalid_payment_status(?failed(_)) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client).

-spec payment_adjustment_success(config()) -> test_return().

payment_adjustment_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    %% start a healthy man's payment
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(low)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(CF1))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    PrvAccount1 = get_cashflow_account({provider, settlement}, CF1),
    SysAccount1 = get_cashflow_account({system, settlement}, CF1),
    MrcAccount1 = get_cashflow_account({merchant, settlement}, CF1),
    %% update terminal cashflow
    ProviderRef = ?prv(100),
    Provider = hg_domain:get(hg_domain:head(), {provider, ProviderRef}),
    ProviderTerms = Provider#domain_Provider.payment_terms,
    ok = hg_domain:upsert(
        {provider, #domain_ProviderObject{
            ref = ProviderRef,
            data = Provider#domain_Provider{
                payment_terms = ProviderTerms#domain_PaymentsProvisionTerms{
                    cash_flow = {value,
                        get_adjustment_provider_cashflow(actual)
                    }
                }
            }
        }}
    ),
    %% update merchant fees
    PartyClient = cfg(party_client, C),
    Shop = hg_client_party:get_shop(cfg(shop_id, C), PartyClient),
    ok = hg_ct_helper:adjust_contract(Shop#domain_Shop.contract_id, ?tmpl(3), PartyClient),
    %% make an adjustment
    Params = make_adjustment_params(Reason = <<"imdrunk">>),
    ?adjustment(AdjustmentID, ?adjustment_pending()) = Adjustment =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, Params, Client),
    Adjustment = #domain_InvoicePaymentAdjustment{id = AdjustmentID, reason = Reason} =
        hg_client_invoicing:get_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_created(Adjustment)))
    ] = next_event(InvoiceID, Client),
    %% no way to create another one yet
    ?invalid_adjustment_pending(AdjustmentID) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_status_changed(?adjustment_processed())))
    ] = next_event(InvoiceID, Client),
    ok =
        hg_client_invoicing:capture_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    ?invalid_adjustment_status(?adjustment_captured(_)) =
        hg_client_invoicing:capture_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    ?invalid_adjustment_status(?adjustment_captured(_)) =
        hg_client_invoicing:cancel_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_status_changed(?adjustment_captured(_))))
    ] = next_event(InvoiceID, Client),
    %% verify that cash deposited correctly everywhere
    #domain_InvoicePaymentAdjustment{new_cash_flow = CF2} = Adjustment,
    PrvAccount2 = get_cashflow_account({provider, settlement}, CF2),
    SysAccount2 = get_cashflow_account({system, settlement}, CF2),
    MrcAccount2 = get_cashflow_account({merchant, settlement}, CF2),
    500  = MrcDiff = maps:get(own_amount, MrcAccount2) - maps:get(own_amount, MrcAccount1),
    -500 = PrvDiff = maps:get(own_amount, PrvAccount2) - maps:get(own_amount, PrvAccount1),
    SysDiff = MrcDiff + PrvDiff - 20,
    SysDiff = maps:get(own_amount, SysAccount2) - maps:get(own_amount, SysAccount1).

-spec payment_temporary_unavailability_retry_success(config()) -> test_return().

payment_temporary_unavailability_retry_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_scenario_payment_params([temp, temp, good, temp, temp]),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client, 2),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, ?timeout_reason(), Client, 2),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured(_Reason, _Cost)))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_temporary_unavailability_too_many_retries(config()) -> test_return().

payment_temporary_unavailability_too_many_retries(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_scenario_payment_params([temp, temp, temp, temp]),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    {failed, PaymentID, {failure, Failure}} =
        await_payment_process_failure(InvoiceID, PaymentID, Client, 3),
    ok = payproc_errors:match(
        'PaymentFailure',
        Failure,
        fun({authorization_failed, {temporarily_unavailable, _}}) -> ok end
    ).

get_cashflow_account(Type, CF) ->
    [ID] = [V || #domain_FinalCashFlowPosting{
        destination = #domain_FinalCashFlowAccount{
            account_id = V,
            account_type = T
        }
    } <- CF, T == Type],
    hg_ct_helper:get_balance(ID).

get_adjustment_fixture(Revision) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [

        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            fees = {value, [
                                ?cfpost(
                                    {merchant, settlement},
                                    {system, settlement},
                                    ?share(40, 1000, operation_amount)
                                )
                            ]}
                        }
                    }
                }]
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(100)
                ])}
            }}
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(100),
            data = #domain_Provider{
                name = <<"Adjustable">>,
                description = <<>>,
                abs_account = <<>>,
                terminal = {value, [?prvtrm(100)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
                        {exclusive, ?cash(100000000, <<"RUB">>)}
                    )},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_flow = {value,
                        get_adjustment_provider_cashflow(initial)
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(100),
            data = #domain_Terminal{
                name = <<"Adjustable Terminal">>,
                description = <<>>,
                risk_coverage = low
            }
        }}

    ].

get_adjustment_provider_cashflow(initial) ->
    [
        ?cfpost(
            {provider, settlement},
            {merchant, settlement},
            ?share(1, 1, operation_amount)
        ),
        ?cfpost(
            {system, settlement},
            {provider, settlement},
            ?share(21, 1000, operation_amount)
        )
    ];
get_adjustment_provider_cashflow(actual) ->
    [
        ?cfpost(
            {provider, settlement},
            {merchant, settlement},
            ?share(1, 1, operation_amount)
        ),
        ?cfpost(
            {system, settlement},
            {provider, settlement},
            ?share(16, 1000, operation_amount)
        ),
        ?cfpost(
            {system, settlement},
            {external, outcome},
            ?fixed(20, <<"RUB">>)
        )
    ].

-spec invalid_payment_w_deprived_party(config()) -> test_return().

invalid_payment_w_deprived_party(C) ->
    PartyID = <<"DEPRIVED ONE">>,
    RootUrl = cfg(root_url, C),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    InvoicingClient = hg_client_invoicing:start_link(hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(10), 42000),
    InvoiceID = create_invoice(InvoiceParams, InvoicingClient),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, InvoicingClient),
    PaymentParams = make_payment_params(),
    Exception = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, InvoicingClient),
    {exception, #'InvalidRequest'{}} = Exception.

-spec external_account_posting(config()) -> test_return().

external_account_posting(C) ->
    PartyID = <<"LGBT">>,
    RootUrl = cfg(root_url, C),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    InvoicingClient = hg_client_invoicing:start_link(hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubbermoss">>, make_due_date(10), 42000),
    InvoiceID = create_invoice(InvoiceParams, InvoicingClient),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, InvoicingClient),
    ?payment_state(
        ?payment(PaymentID)
    ) = hg_client_invoicing:start_payment(InvoiceID, make_payment_params(), InvoicingClient),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, InvoicingClient),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(low)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(CF))
    ] = next_event(InvoiceID, InvoicingClient),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, InvoicingClient),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, InvoicingClient),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, InvoicingClient),
    [AssistAccountID] = [
        AccountID ||
            #domain_FinalCashFlowPosting{
                destination = #domain_FinalCashFlowAccount{
                    account_type = {external, outcome},
                    account_id = AccountID
                },
                details = <<"Assist fee">>
            } <- CF
    ],
    #domain_ExternalAccountSet{
        accounts = #{?cur(<<"RUB">>) := #domain_ExternalAccount{outcome = AssistAccountID}}
    } = hg_domain:get(hg_domain:head(), {external_account_set, ?eas(2)}).


-spec terminal_cashflow_overrides_provider(config()) -> test_return().

terminal_cashflow_overrides_provider(C) ->
    PartyID = <<"LGBT">>,
    RootUrl = cfg(root_url, C),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    InvoicingClient = hg_client_invoicing:start_link(hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(4), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubbermoss">>, make_due_date(10), 42000),
    InvoiceID = create_invoice(InvoiceParams, InvoicingClient),
    _ = next_event(InvoiceID, InvoicingClient),
    _ = hg_client_invoicing:start_payment(InvoiceID, make_payment_params(), InvoicingClient),
    _ = next_event(InvoiceID, InvoicingClient),
    [ _, _, ?payment_ev(PaymentID, ?cash_flow_changed(CF)) ] = next_event(InvoiceID, InvoicingClient),
    _ = next_event(InvoiceID, InvoicingClient),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, InvoicingClient),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, InvoicingClient),
    [AssistAccountID] = [
        AccountID ||
            #domain_FinalCashFlowPosting{
                destination = #domain_FinalCashFlowAccount{
                    account_type = {external, outcome},
                    account_id = AccountID
                },
                details = <<"Kek">>
            } <- CF
    ],
    #domain_ExternalAccountSet{
        accounts = #{?cur(<<"RUB">>) := #domain_ExternalAccount{outcome = AssistAccountID}}
    } = hg_domain:get(hg_domain:head(), {external_account_set, ?eas(2)}).

%%

-spec invalid_refund_party_status(config()) -> _ | no_return().

invalid_refund_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, make_refund_params(), Client),
    ok = hg_client_party:activate(PartyClient),
    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, make_refund_params(), Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec invalid_refund_shop_status(config()) -> _ | no_return().

invalid_refund_shop_status(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyClient = cfg(party_client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, make_refund_params(), Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),
    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, make_refund_params(), Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).


-spec payment_refund_idempotency(config()) -> _ | no_return().

payment_refund_idempotency(C) ->
    Client = cfg(client, C),
    RefundParams0 = make_refund_params(),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    InvoiceID2 = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    RefundID = <<"1">>,
    ExternalID = <<"42">>,
    RefundParams1 = RefundParams0#payproc_InvoicePaymentRefundParams{
        id = RefundID,
        external_id = ExternalID
    },
    % try starting the same refund twice
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID, external_id = ExternalID} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client),
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID, external_id = ExternalID} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client),
    RefundParams2 = RefundParams0#payproc_InvoicePaymentRefundParams{id = <<"2">>},
    % can't start a different refund
    ?operation_not_permitted() = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams2, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID, Refund0, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    % check refund completed
    Refund1 = Refund0#domain_InvoicePaymentRefund{status = ?refund_succeeded()},
    Refund1 = hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    % get back a completed refund when trying to start a new one
    Refund1 = hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client).

-spec payment_refund_success(config()) -> _ | no_return().

payment_refund_success(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    RefundParams = make_refund_params(),
    % not finished yet
    ?invalid_payment_status(?processed()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    % not enough funds on the merchant account
    Failure = {failure, payproc_errors:construct('RefundFailure',
        {terms_violated, {insufficient_merchant_funds, #payprocerr_GeneralFailure{}}}
    )},
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID0} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID0, Refund0, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID0, ?refund_status_changed(?refund_failed(Failure))))
    ] = next_event(InvoiceID, Client),
    % top up merchant account
    InvoiceID2 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % create a refund finally
    Refund = #domain_InvoicePaymentRefund{id = RefundID} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID, Refund, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRefund{status = ?refund_succeeded()} =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    % no more refunds for you
    ?invalid_payment_status(?refunded()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client).

-spec deadline_doesnt_affect_payment_refund(config()) -> _ | no_return().

deadline_doesnt_affect_payment_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    ProcessingDeadline = 4000, % ms
    PaymentParams = set_processing_deadline(ProcessingDeadline, make_payment_params()),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    RefundParams = make_refund_params(),
    % not finished yet
    ?invalid_payment_status(?processed()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    timer:sleep(ProcessingDeadline),
    % not enough funds on the merchant account
    Failure = {failure, payproc_errors:construct('RefundFailure',
        {terms_violated, {insufficient_merchant_funds, #payprocerr_GeneralFailure{}}}
    )},
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID0} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID0, Refund0, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID0, ?refund_status_changed(?refund_failed(Failure))))
    ] = next_event(InvoiceID, Client),
    % top up merchant account
    InvoiceID2 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % create a refund finally
    Refund = #domain_InvoicePaymentRefund{id = RefundID} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID, Refund, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRefund{status = ?refund_succeeded()} =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client).


-spec payment_manual_refund(config()) -> _ | no_return().

payment_manual_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    TrxInfo = ?trx_info(<<"test">>, #{}),
    RefundParams = #payproc_InvoicePaymentRefundParams {
        reason = <<"manual">>,
        transaction_info = TrxInfo
    },
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    % not enough funds on the merchant account
    Failure = {failure, payproc_errors:construct('RefundFailure',
        {terms_violated, {insufficient_merchant_funds, #payprocerr_GeneralFailure{}}}
    )},
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID0} =
        hg_client_invoicing:refund_payment_manual(InvoiceID, PaymentID, RefundParams, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID0, ?refund_created(Refund0, _, TrxInfo)))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID0, ?refund_status_changed(?refund_failed(Failure))))
    ] = next_event(InvoiceID, Client),
    % top up merchant account
    InvoiceID2 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % prevent proxy access
    OriginalRevision = hg_domain:head(),
    Fixture = payment_manual_refund_fixture(OriginalRevision),
    ok = hg_domain:upsert(Fixture),
    % create refund
    Refund = #domain_InvoicePaymentRefund{id = RefundID} =
        hg_client_invoicing:refund_payment_manual(InvoiceID, PaymentID, RefundParams, Client),
    Refund =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_created(Refund, _, TrxInfo)))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_started()))),
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?trx_bound(TrxInfo)))),
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRefund{status = ?refund_succeeded()} =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    ?invalid_payment_status(?refunded()) =
        hg_client_invoicing:refund_payment_manual(InvoiceID, PaymentID, RefundParams, Client),
    % reenable proxy
    ok = hg_domain:reset(OriginalRevision).

-spec payment_partial_refunds_success(config()) -> _ | no_return().

payment_partial_refunds_success(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams0 = make_refund_params(43000, <<"RUB">>),
    % top up merchant account
    InvoiceID2 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 3000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % refund amount exceeds payment amount
    ?invoice_payment_amount_exceeded(_) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams0, Client),
    % first refund
    RefundParams1 = make_refund_params(10000, <<"RUB">>),
    Refund1 = #domain_InvoicePaymentRefund{id = RefundID1} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID1, Refund1, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID1, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    % refund amount exceeds payment amount
    RefundParams2 = make_refund_params(33000, <<"RUB">>),
    ?invoice_payment_amount_exceeded(?cash(32000, _)) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams2, Client),
    % second refund
    RefundParams3 = make_refund_params(30000, <<"RUB">>),
    Refund3 = #domain_InvoicePaymentRefund{id = RefundID3} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams3, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID3, Refund3, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID3, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    % check payment status = captured
    #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{status = ?captured()},
        refunds =
            [
                #domain_InvoicePaymentRefund{cash = ?cash(10000, <<"RUB">>), status = ?refund_succeeded()},
                #domain_InvoicePaymentRefund{cash = ?cash(30000, <<"RUB">>), status = ?refund_succeeded()}
            ]
    } = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client),
    % last refund
    RefundParams4 = make_refund_params(2000, <<"RUB">>),
    Refund4 = #domain_InvoicePaymentRefund{id = RefundID4} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams4, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID4, Refund4, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID4, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(_, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    % check payment status = refunded and all refunds
    #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{status = ?refunded()},
        refunds =
            [
                #domain_InvoicePaymentRefund{cash = ?cash(10000, <<"RUB">>), status = ?refund_succeeded()},
                #domain_InvoicePaymentRefund{cash = ?cash(30000, <<"RUB">>), status = ?refund_succeeded()},
                #domain_InvoicePaymentRefund{cash = ?cash(2000, <<"RUB">>), status = ?refund_succeeded()}
            ]
    } = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client),
    % no more refunds for you
    RefundParams5 = make_refund_params(1000, <<"RUB">>),
    ?invalid_payment_status(?refunded()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams5, Client),
    % Check sequence
    ?assertEqual(<<"1">>, RefundID1),
    ?assertEqual(<<"2">>, RefundID3),
    ?assertEqual(<<"3">>, RefundID4).

-spec invalid_currency_payment_partial_refund(config()) -> _ | no_return().

invalid_currency_payment_partial_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams1 = make_refund_params(50, <<"EUR">>),
    ?inconsistent_refund_currency(<<"EUR">>) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client).

-spec invalid_amount_payment_partial_refund(config()) -> _ | no_return().

invalid_amount_payment_partial_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceAmount = 42000,
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), InvoiceAmount, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams1 = make_refund_params(50, <<"RUB">>),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount, less than allowed minumum">>]
    }} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client),
    RefundParams2 = make_refund_params(40001, <<"RUB">>),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount, more than allowed maximum">>]
    }} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams2, Client),
    RefundAmount = 10000,
    %% make cart cost not equal to remaining invoice cost
    Cash = ?cash(InvoiceAmount - RefundAmount - 1, <<"RUB">>),
    Cart = ?cart(Cash, #{}),
    RefundParams3 = make_refund_params(RefundAmount, <<"RUB">>, Cart),
    {exception, #'InvalidRequest'{
        errors = [<<"Remaining payment amount not equal cart cost">>]
    }} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams3, Client),
    %% miss cash in refund params
    RefundParams4 = #payproc_InvoicePaymentRefundParams{
        reason = <<"ZANOZED">>,
        cart = Cart
    },
    {exception, #'InvalidRequest'{
        errors = [<<"Refund amount does not match with the cart total amount">>]
    }} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams4, Client).

 -spec invalid_amount_partial_capture_and_refund(config()) -> _ | no_return().

invalid_amount_partial_capture_and_refund(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    % do a partial capture
    Cash = ?cash(21000, <<"RUB">>),
    Reason = <<"ok">>,
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client),
    PaymentID = await_payment_partial_capture(InvoiceID, PaymentID, Reason, Cash, Client),
    % try to refund an amount that exceeds capture amount
    RefundParams = make_refund_params(42000, <<"RUB">>),
    ?invoice_payment_amount_exceeded(?cash(21000, _)) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client).

-spec cant_start_simultaneous_partial_refunds(config()) -> _ | no_return().

cant_start_simultaneous_partial_refunds(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams = make_refund_params(10000, <<"RUB">>),
    Refund1 = #domain_InvoicePaymentRefund{id = RefundID1} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    ?operation_not_permitted() =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID1, Refund1, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID1, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    Refund2 = #domain_InvoicePaymentRefund{id = RefundID2} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID2, Refund2, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID2, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{status = ?captured()},
        refunds =
            [
                #domain_InvoicePaymentRefund{cash = ?cash(10000, <<"RUB">>), status = ?refund_succeeded()},
                #domain_InvoicePaymentRefund{cash = ?cash(10000, <<"RUB">>), status = ?refund_succeeded()}
            ]
    } = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client).

-spec ineligible_payment_partial_refund(config()) -> _ | no_return().

ineligible_payment_partial_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(100), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams = make_refund_params(5000, <<"RUB">>),
    ?operation_not_permitted() =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client).

-spec retry_temporary_unavailability_refund(config()) -> _ | no_return().

retry_temporary_unavailability_refund(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_scenario_payment_params([good, good, temp, temp]),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    RefundParams1 = make_refund_params(1000, <<"RUB">>),
    Refund1 = #domain_InvoicePaymentRefund{id = RefundID1} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams1, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID1, Refund1, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID1, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client, 2),
    % check payment status still captured and all refunds
    #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{status = ?captured()},
        refunds =
            [
                #domain_InvoicePaymentRefund{cash = ?cash(1000, <<"RUB">>), status = ?refund_succeeded()}
            ]
    } = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_refund_id_types(config()) -> _ | no_return().

payment_refund_id_types(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params(), Client),
    TrxInfo = ?trx_info(<<"test">>, #{}),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    % top up merchant account
    InvoiceID2 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID2 = process_payment(InvoiceID2, make_payment_params(), Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % create refund
    RefundParams = #payproc_InvoicePaymentRefundParams{
        reason = <<"42">>,
        cash = ?cash(5000, <<"RUB">>)
    },
    % 0
    ManualRefundParams = RefundParams#payproc_InvoicePaymentRefundParams{transaction_info = TrxInfo},
    Refund0 = #domain_InvoicePaymentRefund{id = RefundID0} =
        hg_client_invoicing:refund_payment_manual(InvoiceID, PaymentID, ManualRefundParams, Client),
    Refund0 = hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID0, Client),
    PaymentID = await_partial_manual_refund_succeeded(Refund0, TrxInfo, InvoiceID, PaymentID, RefundID0, Client),
    % 1
    Refund1 = #domain_InvoicePaymentRefund{id = RefundID1} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund1 = hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID1, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID1, Refund1, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID1, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    % 2
    CustomIdManualParams = ManualRefundParams#payproc_InvoicePaymentRefundParams{id = <<"2">>},
    Refund2 = #domain_InvoicePaymentRefund{id = RefundID2} =
        hg_client_invoicing:refund_payment_manual(InvoiceID, PaymentID, CustomIdManualParams, Client),
    Refund2 = hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID2, Client),
    PaymentID = await_partial_manual_refund_succeeded(Refund2, TrxInfo, InvoiceID, PaymentID, RefundID2, Client),
    % 3
    CustomIdParams = RefundParams#payproc_InvoicePaymentRefundParams{id = <<"m3">>},
    {exception, #'InvalidRequest'{}} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, CustomIdParams, Client),
    Refund3 = #domain_InvoicePaymentRefund{id = RefundID3} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund3 = hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID3, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID3, Refund3, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID3, Client),
    PaymentID = await_refund_payment_process_finish(InvoiceID, PaymentID, Client),
    % Check ids
    ?assertEqual(<<"m1">>, RefundID0),
    ?assertEqual(<<"2">>, RefundID1),
    ?assertEqual(<<"m2">>, RefundID2),
    ?assertEqual(<<"3">>, RefundID3).
%%

-spec consistent_history(config()) -> test_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(cfg(root_url, C))),
    Events = hg_client_eventsink:pull_events(5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events).

-spec payment_hold_cancellation(config()) -> _ | no_return().

payment_hold_cancellation(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 10000, C),
    PaymentParams = make_payment_params({hold, capture}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    ok = hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client),
    PaymentID = await_payment_cancel(InvoiceID, PaymentID, <<"whynot">>, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_unpaid()),
        [?payment_state(?payment_w_status(PaymentID, ?cancelled()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec payment_hold_double_cancellation(config()) -> _ | no_return().

payment_hold_double_cancellation(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 10000, C),
    PaymentParams = make_payment_params({hold, capture}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    ?assertEqual(ok, hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client)),
    Result = hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client),
    ?assertMatch({exception, #payproc_InvalidPaymentStatus{}}, Result).

-spec payment_hold_cancellation_captured(config()) -> _ | no_return().

payment_hold_cancellation_captured(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params({hold, cancel}), Client),
    ?assertEqual(ok, hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client)),
    Result = hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client),
    ?assertMatch({exception, #payproc_InvalidPaymentStatus{}}, Result).

-spec payment_hold_auto_cancellation(config()) -> _ | no_return().

payment_hold_auto_cancellation(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(20), 10000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_cancel(InvoiceID, PaymentID, undefined, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_unpaid()),
        [?payment_state(?payment_w_status(PaymentID, ?cancelled()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec payment_hold_capturing(config()) -> _ | no_return().

payment_hold_capturing(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params({hold, cancel}), Client),
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, <<"ok">>, Client).

-spec payment_hold_double_capturing(config()) -> _ | no_return().

payment_hold_double_capturing(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params({hold, cancel}), Client),
    ?assertEqual(ok, hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client)),
    Result = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client),
    ?assertMatch({exception, #payproc_InvalidPaymentStatus{}}, Result).

-spec payment_hold_capturing_cancelled(config()) -> _ | no_return().

payment_hold_capturing_cancelled(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentID = process_payment(InvoiceID, make_payment_params({hold, cancel}), Client),
    ?assertEqual(ok, hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client)),
    Result = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client),
    ?assertMatch({exception, #payproc_InvalidPaymentStatus{}}, Result).

-spec deadline_doesnt_affect_payment_capturing(config()) -> _ | no_return().

deadline_doesnt_affect_payment_capturing(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    ProcessingDeadline = 4000, % ms
    PaymentParams = set_processing_deadline(ProcessingDeadline, make_payment_params({hold, cancel})),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    timer:sleep(ProcessingDeadline),
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, <<"ok">>, Client).

-spec payment_hold_partial_capturing(config()) -> _ | no_return().

payment_hold_partial_capturing(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"RUB">>),
    Reason = <<"ok">>,
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client),
    [
       ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cash, _)),
       ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cash), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, 0, Cash).

-spec payment_hold_partial_capturing_with_cart(config()) -> _ | no_return().

payment_hold_partial_capturing_with_cart(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"RUB">>),
    Cart = ?cart(Cash, #{}),
    Reason = <<"ok">>,
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Cart, Client),
    [
       ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cash, _)),
       ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cash, Cart), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, 0, Cash, Cart).

-spec payment_hold_partial_capturing_with_cart_missing_cash(config()) -> _ | no_return().

payment_hold_partial_capturing_with_cart_missing_cash(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"RUB">>),
    Cart = ?cart(Cash, #{}),
    Reason = <<"ok">>,
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, undefined, Cart, Client),
    [
       ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cash, _)),
       ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cash, Cart), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, 0, Cash, Cart).

-spec invalid_currency_partial_capture(config()) -> _ | no_return().

invalid_currency_partial_capture(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"USD">>),
    Reason = <<"ok">>,
    ?inconsistent_capture_currency(<<"RUB">>) =
        hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client).

-spec invalid_amount_partial_capture(config()) -> _ | no_return().

invalid_amount_partial_capture(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(100000, <<"RUB">>),
    Reason = <<"ok">>,
    ?amount_exceeded_capture_balance(42000) =
        hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client).

-spec invalid_permit_partial_capture_in_service(config()) -> _ | no_return().

invalid_permit_partial_capture_in_service(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(1), <<"RUB">>, ?tmpl(6), ?pinst(1), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"RUB">>),
    Reason = <<"ok">>,
    ?operation_not_permitted() = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client).

-spec invalid_permit_partial_capture_in_provider(config()) -> _ | no_return().

invalid_permit_partial_capture_in_provider(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10000, <<"RUB">>),
    Reason = <<"ok">>,
    ?operation_not_permitted() = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client).

-spec payment_hold_auto_capturing(config()) -> _ | no_return().

payment_hold_auto_capturing(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_tds_payment_params({hold, capture}),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    _ = assert_success_post_request(get_post_request(UserInteraction)),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    _ = assert_invalid_post_request(get_post_request(UserInteraction)),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, ?timeout_reason(), Client).

-spec rounding_cashflow_volume(config()) -> _ | no_return().

rounding_cashflow_volume(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(CF))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    ?cash(0, <<"RUB">>) = get_cashflow_volume({provider, settlement}, {merchant, settlement}, CF),
    ?cash(1, <<"RUB">>) = get_cashflow_volume({system, settlement}, {provider, settlement}, CF),
    ?cash(1, <<"RUB">>) = get_cashflow_volume({system, settlement}, {system, subagent}, CF),
    ?cash(1, <<"RUB">>) = get_cashflow_volume({system, settlement}, {external, outcome}, CF),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

get_cashflow_rounding_fixture(Revision) ->
    PaymentInstituition = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstituition#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(100)
                ])}
            }}
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(100),
            data = #domain_Provider{
                name = <<"Rounding">>,
                description = <<>>,
                abs_account = <<>>,
                terminal = {value, [?prvtrm(100)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
                        {exclusive, ?cash(100000000, <<"RUB">>)}
                    )},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share_with_rounding_method(1, 200000, operation_amount, round_half_towards_zero)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share_with_rounding_method(1, 200000, operation_amount, round_half_away_from_zero)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {system, subagent},
                            ?share_with_rounding_method(1, 200000, operation_amount, round_half_away_from_zero)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {external, outcome},
                            ?share(1, 200000, operation_amount)
                        )
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(100),
                data = #domain_Terminal{
                    name = <<"Rounding Terminal">>,
                description = <<>>,
                risk_coverage = low
            }
        }}
    ].

get_cashflow_volume(Source, Destination, CF) ->
    [Volume] = [V || #domain_FinalCashFlowPosting{
        source = #domain_FinalCashFlowAccount{account_type = S},
        destination = #domain_FinalCashFlowAccount{account_type = D},
        volume = V
    } <- CF, S == Source, D == Destination],
    Volume.

%%

-spec terms_retrieval(config()) -> _ | no_return().

terms_retrieval(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 1500, C),
    Timestamp = hg_datetime:format_now(),
    TermSet1 = hg_client_invoicing:compute_terms(InvoiceID, {timestamp, Timestamp}, Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, [
            ?pmt(bank_card, jcb),
            ?pmt(bank_card, mastercard),
            ?pmt(bank_card, visa),
            ?pmt(crypto_currency, bitcoin),
            ?pmt(digital_wallet, qiwi),
            ?pmt(empty_cvv_bank_card, visa),
            ?pmt(mobile, mts),
            ?pmt(payment_terminal, euroset),
            ?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))
        ]}
    }} = TermSet1,
    Revision = hg_domain:head(),
    ok = hg_domain:update(construct_term_set_for_cost(1000, 2000)),
    TermSet2 = hg_client_invoicing:compute_terms(InvoiceID, {timestamp, Timestamp}, Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, [?pmt(bank_card, visa)]}
    }} = TermSet2,
    ok = hg_domain:reset(Revision).

%%

-define(repair_set_timer(T),
    #repair_ComplexAction{timer = {set_timer, #repair_SetTimerAction{timer = T}}}).
-define(repair_mark_removal(),
    #repair_ComplexAction{remove = #repair_RemoveAction{}}).

-spec adhoc_repair_working_failed(config()) -> _ | no_return().

adhoc_repair_working_failed(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    {exception, #'InvalidRequest'{}} = repair_invoice(InvoiceID, [], Client),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec adhoc_repair_failed_succeeded(config()) -> _ | no_return().

adhoc_repair_failed_succeeded(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(unexpected_failure),
    PaymentParams = make_payment_params(PaymentTool, Session),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(PaymentID))))
    ]  = next_event(InvoiceID, Client),
    % assume no more events here since machine is FUBAR already
    timeout = next_event(InvoiceID, 2000, Client),
    Changes = [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
    ],
    ok = repair_invoice(InvoiceID, Changes, ?repair_set_timer({timeout, 0}), undefined, Client),
    Changes = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec adhoc_repair_force_removal(config()) -> _ | no_return().

adhoc_repair_force_removal(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    timeout = next_event(InvoiceID, 1000, Client),
    _ = ?assertEqual(ok, hg_invoice:fail(InvoiceID)),
    ?assertException(
        error,
        {{woody_error, {external, result_unexpected, _}}, _},
        hg_client_invoicing:rescind(InvoiceID, <<"LOL NO">>, Client)
    ),
    ok = repair_invoice(InvoiceID, [], ?repair_mark_removal(), undefined, Client),
    {exception, #payproc_InvoiceNotFound{}} = hg_client_invoicing:get(InvoiceID, Client).

-spec adhoc_repair_invalid_changes_failed(config()) -> _ | no_return().

adhoc_repair_invalid_changes_failed(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(unexpected_failure),
    PaymentParams = make_payment_params(PaymentTool, Session),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(PaymentID))))
    ] = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 1000, Client),
    InvalidChanges1 = [
        ?payment_ev(PaymentID, ?refund_ev(<<"42">>, ?refund_status_changed(?refund_succeeded())))
    ],
    ?assertException(
        error,
        {{woody_error, {external, result_unexpected, _}}, _},
        repair_invoice(InvoiceID, InvalidChanges1, Client)
    ),
    InvalidChanges2 = [
        ?payment_ev(PaymentID, ?payment_status_changed(?captured())),
        ?invoice_status_changed(?invoice_paid())
    ],
    ?assertException(
        error,
        {{woody_error, {external, result_unexpected, _}}, _},
        repair_invoice(InvoiceID, InvalidChanges2, Client)
    ),
    Changes = [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
    ],
    ?assertEqual(
        ok,
        repair_invoice(InvoiceID, Changes, Client)
    ),
    Changes = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec adhoc_repair_force_invalid_transition(config()) -> _ | no_return().

adhoc_repair_force_invalid_transition(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberdank">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    _ = ?assertEqual(ok, hg_invoice:fail(InvoiceID)),
    Failure = payproc_errors:construct(
        'PaymentFailure',
        {authorization_failed, {unknown, #payprocerr_GeneralFailure{}}}
    ),
    InvalidChanges = [
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, Failure}))),
        ?invoice_status_changed(?invoice_unpaid())
    ],
    ?assertException(
        error,
        {{woody_error, {external, result_unexpected, _}}, _},
        repair_invoice(InvoiceID, InvalidChanges, Client)
    ),
    Params = #payproc_InvoiceRepairParams{validate_transitions = false},
    ?assertEqual(
        ok,
        repair_invoice(InvoiceID, InvalidChanges, #repair_ComplexAction{}, Params, Client)
    ),
    ?invoice_state(
        ?invoice_w_status(?invoice_unpaid()),
        [?payment_state(?payment_w_status(PaymentID, ?failed({failure, Failure})))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_with_offsite_preauth_success(config()) -> test_return().

payment_with_offsite_preauth_success(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(preauth_3ds_offsite),
    PaymentParams = make_payment_params(PaymentTool, Session),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    timer:sleep(2000),
    {URL, Form} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, Form}),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_with_offsite_preauth_failed(config()) -> test_return().

payment_with_offsite_preauth_failed(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(3), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(preauth_3ds_offsite),
    PaymentParams = make_payment_params(PaymentTool, Session, instant),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    _UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?processed(), ?session_finished(?session_failed({failure, Failure})))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, Failure})))
    ] = next_event(InvoiceID, 8000, Client),
    ok = payproc_errors:match('PaymentFailure', Failure, fun({authorization_failed, _}) -> ok end),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec payment_with_tokenized_bank_card(config()) -> test_return().

payment_with_tokenized_bank_card(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_tokenized_bank_card_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec repair_fail_pre_processing_succeeded(config()) -> test_return().

repair_fail_pre_processing_succeeded(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(6), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    % Invoice
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ok = repair_invoice_with_scenario(InvoiceID, fail_pre_processing, Client),

    [
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, _Failure})))
    ] = next_event(InvoiceID, Client).

-spec repair_skip_inspector_succeeded(config()) -> test_return().

repair_skip_inspector_succeeded(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(6), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    % Invoice
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ok = repair_invoice_with_scenario(InvoiceID, skip_inspector, Client),

    [
        ?payment_ev(PaymentID, ?risk_score_changed(low)), % we send low risk score in create repair...
        ?payment_ev(PaymentID, ?route_changed(?route(?prv(2), ?trm(7)))),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec repair_fail_session_succeeded(config()) -> test_return().

repair_fail_session_succeeded(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(unexpected_failure),
    PaymentParams = make_payment_params(PaymentTool, Session),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(PaymentID))))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ok = repair_invoice_with_scenario(InvoiceID, fail_session, Client),

    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_failed({failure, Failure})))),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, Failure})))
    ] = next_event(InvoiceID, Client).

-spec repair_fail_session_on_pre_processing(config()) -> test_return().

repair_fail_session_on_pre_processing(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(7), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ?assertException(
        error,
        {{woody_error, {external, result_unexpected, _}}, _},
        repair_invoice_with_scenario(InvoiceID, fail_session, Client)
    ),
    ok = repair_invoice_with_scenario(InvoiceID, fail_pre_processing, Client),

    [
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, _Failure})))
    ] = next_event(InvoiceID, Client).

-spec repair_complex_succeeded_first(config()) -> test_return().

repair_complex_succeeded_first(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(6), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    % Invoice
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ok = repair_invoice_with_scenario(InvoiceID, complex, Client),

    [
        ?payment_ev(PaymentID, ?risk_score_changed(low)), % we send low risk score in create repair...
        ?payment_ev(PaymentID, ?route_changed(?route(?prv(2), ?trm(7)))),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec repair_complex_succeeded_second(config()) -> test_return().

repair_complex_succeeded_second(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubbercrack">>, make_due_date(10), 42000, C),
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(unexpected_failure),
    PaymentParams = make_payment_params(PaymentTool, Session),
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(PaymentID))))
    ] = next_event(InvoiceID, Client),

    timeout = next_event(InvoiceID, 2000, Client),
    ok = repair_invoice_with_scenario(InvoiceID, complex, Client),

    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_failed({failure, Failure})))),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed({failure, Failure})))
    ] = next_event(InvoiceID, Client).

%%

-spec consistent_account_balances(config()) -> test_return().

consistent_account_balances(C) ->
    PartyClient = cfg(party_client, C),
    Party = hg_client_party:get(PartyClient),
    Shops = maps:values(Party#domain_Party.shops),
    _ = [
        consistent_account_balance(AccountID, Shop) ||
        #domain_Shop{account = ShopAccount} = Shop <- Shops,
        #domain_ShopAccount{ settlement = AccountID1, guarantee = AccountID2} <- [ShopAccount],
        AccountID <- [AccountID1, AccountID2]
    ].

consistent_account_balance(AccountID, Comment) ->
    case hg_ct_helper:get_balance(AccountID) of
        #{own_amount := V, min_available_amount := V, max_available_amount := V} ->
            ok;
        #{} = Account ->
            erlang:error({"Inconsistent account balance", Account, Comment})
    end.

%%

next_event(InvoiceID, Client) ->
    %% timeout should be at least as large as hold expiration in construct_domain_fixture/0
    next_event(InvoiceID, 12000, Client).

next_event(InvoiceID, Timeout, Client) ->
    case hg_client_invoicing:pull_event(InvoiceID, Timeout, Client) of
        {ok, ?invoice_ev(Changes)} ->
            case filter_changes(Changes) of
                L when length(L) > 0 ->
                    L;
                [] ->
                    next_event(InvoiceID, Timeout, Client)
            end;
        Result ->
            Result
    end.

filter_changes(Changes) ->
    lists:filtermap(fun filter_change/1, Changes).

filter_change(?payment_ev(_, C)) ->
    filter_change(C);
filter_change(?refund_ev(_, C)) ->
    filter_change(C);
filter_change(?session_ev(_, ?proxy_st_changed(_))) ->
    false;
filter_change(?session_ev(_, ?session_suspended(_, _))) ->
    false;
filter_change(?session_ev(_, ?session_activated())) ->
    false;
filter_change(_) ->
    true.

%%

start_service_handler(Module, C, HandlerOpts) ->
    start_service_handler(Module, Module, C, HandlerOpts).

start_service_handler(Name, Module, C, HandlerOpts) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = maps:merge(HandlerOpts, #{hellgate_root_url => cfg(root_url, C)}),
    ChildSpec = hg_test_proxy:get_child_spec(Name, Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(cfg(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

start_proxies(Proxies) ->
    setup_proxies(lists:map(
        fun
            Mapper({Module, ProxyID, Context}) ->
                Mapper({Module, ProxyID, #{}, Context});
            Mapper({Module, ProxyID, ProxyOpts, Context}) ->
                construct_proxy(ProxyID, start_service_handler(Module, Context, #{}), ProxyOpts)
        end,
        Proxies
    )).

setup_proxies(Proxies) ->
    ok = hg_domain:upsert(Proxies).

start_kv_store(SupPid) ->
    ChildSpec = #{
        id => hg_kv_store,
        start => {hg_kv_store, start_link, [[]]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [hg_kv_store]
    },
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    ok.

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = ?prx(ID),
        data = #domain_ProxyDefinition{
            name              = Url,
            description       = Url,
            url               = Url,
            options           = Options
        }
    }}.

%%

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Cost).

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

make_invoice_params_tpl(TplID) ->
    hg_ct_helper:make_invoice_params_tpl(TplID).

make_invoice_params_tpl(TplID, Cost) ->
    hg_ct_helper:make_invoice_params_tpl(TplID, Cost).

make_invoice_params_tpl(TplID, Cost, Context) ->
    hg_ct_helper:make_invoice_params_tpl(TplID, Cost, Context).

make_invoice_context() ->
    hg_ct_helper:make_invoice_context().

make_invoice_context(Ctx) ->
    hg_ct_helper:make_invoice_context(Ctx).

make_cash(Amount, Currency) ->
    hg_ct_helper:make_cash(Amount, Currency).

make_tpl_cost(Type, P1, P2) ->
    hg_ct_helper:make_invoice_tpl_cost(Type, P1, P2).

create_invoice_tpl(Config) ->
    Cost = hg_ct_helper:make_invoice_tpl_cost(fixed, 100, <<"RUB">>),
    Context = make_invoice_context(),
    create_invoice_tpl(Config, Cost, Context).

create_invoice_tpl(Config, Cost, Context) ->
    Client = cfg(client_tpl, Config),
    PartyID = cfg(party_id, Config),
    ShopID = cfg(shop_id, Config),
    Lifetime = hg_ct_helper:make_lifetime(0, 1, 0),
    Product = <<"rubberduck">>,
    Details = hg_ct_helper:make_invoice_tpl_details(Product, Cost),
    Params = hg_ct_helper:make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details, Context),
    #domain_InvoiceTemplate{id = TplID} = hg_client_invoice_templating:create(Params, Client),
    TplID.

get_invoice_tpl(TplID, Config) ->
    Client = cfg(client_tpl, Config),
    hg_client_invoice_templating:get(TplID, Client).

update_invoice_tpl(TplID, Cost, Config) ->
    Client = cfg(client_tpl, Config),
    Product = <<"rubberduck">>,
    Details = hg_ct_helper:make_invoice_tpl_details(Product, Cost),
    Params = hg_ct_helper:make_invoice_tpl_update_params(#{details => Details}),
    hg_client_invoice_templating:update(TplID, Params, Client).

delete_invoice_tpl(TplID, Config) ->
    Client = cfg(client_tpl, Config),
    hg_client_invoice_templating:delete(TplID, Client).

make_terminal_payment_params() ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(terminal),
    make_payment_params(PaymentTool, Session, instant).

make_crypto_currency_payment_params() ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(crypto_currency),
    make_payment_params(PaymentTool, Session, instant).

make_mobile_commerce_params(success) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(mobile_commerce),
    make_payment_params(PaymentTool, Session, instant);
make_mobile_commerce_params(failure) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(mobile_commerce_failure),
    make_payment_params(PaymentTool, Session, instant).

make_wallet_payment_params() ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(digital_wallet),
    make_payment_params(PaymentTool, Session, instant).

make_tds_payment_params() ->
    make_tds_payment_params(instant).

make_tds_payment_params(FlowType) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(preauth_3ds),
    make_payment_params(PaymentTool, Session, FlowType).

make_customer_payment_params(CustomerID) ->
    #payproc_InvoicePaymentParams{
        payer = {customer, #payproc_CustomerPayerParams{
            customer_id = CustomerID
        }},
        flow = {instant, #payproc_InvoicePaymentParamsFlowInstant{}}
    }.

make_tokenized_bank_card_payment_params() ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(tokenized_bank_card),
    make_payment_params(PaymentTool, Session).

make_scenario_payment_params(Scenario) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool({scenario, Scenario}),
    make_payment_params(PaymentTool, Session, instant).

make_payment_params() ->
    make_payment_params(instant).

make_payment_params(FlowType) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(no_preauth),
    make_payment_params(PaymentTool, Session, FlowType).

make_payment_params(PaymentTool, Session) ->
    make_payment_params(PaymentTool, Session, instant).

make_payment_params(PaymentTool, Session, FlowType) ->
    Flow = case FlowType of
        instant ->
            {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
        {hold, OnHoldExpiration} ->
            {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
    end,
    #payproc_InvoicePaymentParams{
        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
            resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = Session,
                client_info = #domain_ClientInfo{}
            },
            contact_info = #domain_ContactInfo{}
        }},
        flow = Flow
    }.

set_payment_context(Context, Params = #payproc_InvoicePaymentParams{}) ->
    Params#payproc_InvoicePaymentParams{context = Context}.

make_refund_params() ->
    #payproc_InvoicePaymentRefundParams{
        reason = <<"ZANOZED">>
    }.

make_refund_params(Amount, Currency) ->
    #payproc_InvoicePaymentRefundParams{
        reason = <<"ZANOZED">>,
        cash = make_cash(Amount, Currency)
    }.

make_refund_params(Amount, Currency, Cart) ->
    #payproc_InvoicePaymentRefundParams{
        reason = <<"ZANOZED">>,
        cash = make_cash(Amount, Currency),
        cart = Cart
    }.

make_adjustment_params() ->
    make_adjustment_params(<<>>).

make_adjustment_params(Reason) ->
    make_adjustment_params(Reason, undefined).

make_adjustment_params(Reason, Revision) ->
    #payproc_InvoicePaymentAdjustmentParams{
        domain_revision = Revision,
        reason = Reason
    }.

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

create_invoice(InvoiceParams, Client) ->
    ?invoice_state(?invoice(InvoiceID)) = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

repair_invoice(InvoiceID, Changes, Client) ->
    repair_invoice(InvoiceID, Changes, undefined, undefined, Client).

repair_invoice(InvoiceID, Changes, Action, Params, Client) ->
    hg_client_invoicing:repair(InvoiceID, Changes, Action, Params, Client).

create_repair_scenario(fail_pre_processing) ->
    Failure = payproc_errors:construct('PaymentFailure', {no_route_found, {unknown, #payprocerr_GeneralFailure{}}}),
    {'fail_pre_processing', #'payproc_InvoiceRepairFailPreProcessing'{failure = Failure}};
create_repair_scenario(skip_inspector) ->
    {'skip_inspector', #'payproc_InvoiceRepairSkipInspector'{risk_score = low}};
create_repair_scenario(fail_session) ->
    Failure = payproc_errors:construct('PaymentFailure',
                {no_route_found, {unknown, #payprocerr_GeneralFailure{}}}
            ),
    {'fail_session', #'payproc_InvoiceRepairFailSession'{failure = Failure}};
create_repair_scenario(complex) ->
    {'complex', #'payproc_InvoiceRepairComplex'{scenarios =
    [
        create_repair_scenario(skip_inspector),
        create_repair_scenario(fail_session)
    ]}}.

repair_invoice_with_scenario(InvoiceID, Scenario, Client) ->
    hg_client_invoicing:repair_scenario(InvoiceID, create_repair_scenario(Scenario), Client).

start_invoice(Product, Due, Amount, C) ->
    start_invoice(cfg(shop_id, C), Product, Due, Amount, C).

start_invoice(ShopID, Product, Due, Amount, C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    InvoiceID.

start_payment(InvoiceID, PaymentParams, Client) ->
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    PaymentID.

process_payment(InvoiceID, PaymentParams, Client) ->
    process_payment(InvoiceID, PaymentParams, Client, 0).

process_payment(InvoiceID, PaymentParams, Client, Restarts) ->
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client, Restarts).

await_payment_session_started(InvoiceID, PaymentID, Client, Target) ->
    [
        ?payment_ev(PaymentID, ?session_ev(Target, ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_process_interaction(InvoiceID, PaymentID, Client) ->
    Events0 = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = Events0,
    Events1 = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?interaction_requested(UserInteraction)))
    ] = Events1,
    UserInteraction.

await_payment_process_finish(InvoiceID, PaymentID, Client) ->
    await_payment_process_finish(InvoiceID, PaymentID, Client, 0).

await_payment_process_finish(InvoiceID, PaymentID, Client, Restarts) ->
    PaymentID = await_sessions_restarts(PaymentID, ?processed(), InvoiceID, Client, Restarts),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(_)))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

get_payment_cost(InvoiceID, PaymentID, Client) ->
    #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{cost = Cost}
    } = hg_client_invoicing:get_payment(InvoiceID, PaymentID, Client),
    Cost.

await_payment_capture(InvoiceID, PaymentID, Client) ->
    await_payment_capture(InvoiceID, PaymentID, ?timeout_reason(), Client).

await_payment_capture(InvoiceID, PaymentID, Reason, Client) ->
    await_payment_capture(InvoiceID, PaymentID, Reason, Client, 0).

await_payment_capture(InvoiceID, PaymentID, Reason, Client, Restarts) ->
    Cost = get_payment_cost(InvoiceID, PaymentID, Client),
    [
        ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cost, _)),
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cost), ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts).

await_payment_partial_capture(InvoiceID, PaymentID, Reason, Cash, Client) ->
    await_payment_partial_capture(InvoiceID, PaymentID, Reason, Cash, Client, 0).

await_payment_partial_capture(InvoiceID, PaymentID, Reason, Cash, Client, Restarts) ->
    [
       ?payment_ev(PaymentID, ?payment_capture_started(Reason, Cash, _)),
       ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(Reason, Cash), ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts, Cash).

await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts) ->
    Cost = get_payment_cost(InvoiceID, PaymentID, Client),
    await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts, Cost).

await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts, Cost) ->
    await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts, Cost, undefined).

await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client, Restarts, Cost, Cart) ->
    Target = ?captured(Reason, Cost, Cart),
    PaymentID = await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts),
    [
        ?payment_ev(PaymentID, ?session_ev(Target, ?session_finished(?session_succeeded())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(Target)),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_cancel(InvoiceID, PaymentID, Reason, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled_with_reason(Reason), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled_with_reason(Reason), ?session_finished(?session_succeeded())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?payment_status_changed(?cancelled_with_reason(Reason)))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_process_timeout(InvoiceID, PaymentID, Client) ->
    {failed, PaymentID, ?operation_timeout()} = await_payment_process_failure(InvoiceID, PaymentID, Client),
    PaymentID.

await_payment_process_failure(InvoiceID, PaymentID, Client) ->
    await_payment_process_failure(InvoiceID, PaymentID, Client, 0).

await_payment_process_failure(InvoiceID, PaymentID, Client, Restarts) ->
    await_payment_process_failure(InvoiceID, PaymentID, Client, Restarts, ?processed()).

await_payment_process_failure(InvoiceID, PaymentID, Client, Restarts, Target) ->
    PaymentID = await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(Target, ?session_finished(?session_failed(Failure)))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed(Failure)))
    ] = next_event(InvoiceID, Client),
    {failed, PaymentID, Failure}.

refund_payment(InvoiceID, PaymentID, RefundID, Refund, Client) ->
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_created(Refund, _)))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_partial_manual_refund_succeeded(Refund, TrxInfo, InvoiceID, PaymentID, RefundID, Client) ->
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_created(Refund, _, TrxInfo)))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_started()))),
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?trx_bound(TrxInfo)))),
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_status_changed(?refund_succeeded())))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_refund_session_started(InvoiceID, PaymentID, RefundID, Client) ->
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_started())))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_refund_payment_process_finish(InvoiceID, PaymentID, Client) ->
    await_refund_payment_process_finish(InvoiceID, PaymentID, Client, 0).

await_refund_payment_process_finish(InvoiceID, PaymentID, Client, Restarts) ->
    PaymentID = await_sessions_restarts(PaymentID, ?refunded(), InvoiceID, Client, Restarts),
    [
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(_, ?refund_status_changed(?refund_succeeded())))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_sessions_restarts(PaymentID, _Target, _InvoiceID, _Client, 0) ->
    PaymentID;
await_sessions_restarts(PaymentID, ?refunded() = Target, InvoiceID, Client, Restarts) when Restarts > 0 ->
    [
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(Target, ?session_finished(?session_failed(_))))),
        ?payment_ev(PaymentID, ?refund_ev(_, ?session_ev(Target, ?session_started())))
    ] = next_event(InvoiceID, Client),
    await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts - 1);
await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts) when Restarts > 0 ->
    [
        ?payment_ev(PaymentID, ?session_ev(Target, ?session_finished(?session_failed(_)))),
        ?payment_ev(PaymentID, ?session_ev(Target, ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_sessions_restarts(PaymentID, Target, InvoiceID, Client, Restarts - 1).

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

assert_invalid_post_request(Req) ->
    {ok, 400, _RespHeaders, _ClientRef} = post_request(Req).

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form};
get_post_request({payment_terminal_reciept, #'PaymentTerminalReceipt'{short_payment_id = SPID}}) ->
    URL = hg_dummy_provider:get_callback_url(),
    {URL, #{<<"tag">> => SPID}}.

make_customer_w_rec_tool(PartyID, ShopID, Client) ->
    CustomerParams = hg_ct_helper:make_customer_params(PartyID, ShopID, <<"InvoicingTests">>),
    #payproc_Customer{id = CustomerID} =
        hg_client_customer:create(CustomerParams, Client),
    #payproc_CustomerBinding{id = BindingID} =
        hg_client_customer:start_binding(
            CustomerID,
            hg_ct_helper:make_customer_binding_params(hg_dummy_provider:make_payment_tool(no_preauth)),
            Client
        ),
    ok = wait_for_binding_success(CustomerID, BindingID, Client),
    CustomerID.

wait_for_binding_success(CustomerID, BindingID, Client) ->
    wait_for_binding_success(CustomerID, BindingID, 15000, Client).

wait_for_binding_success(CustomerID, BindingID, TimeLeft, Client) when TimeLeft > 0 ->
    Target = ?customer_binding_changed(BindingID, ?customer_binding_status_changed(?customer_binding_succeeded())),
    Started = genlib_time:ticks(),
    Event = hg_client_customer:pull_event(CustomerID, Client),
    R = case Event of
        {ok, ?customer_event(Changes)} ->
            lists:member(Target, Changes);
        _ ->
            false
    end,
    case R of
        true ->
            ok;
        false ->
            timer:sleep(200),
            Now = genlib_time:ticks(),
            TimeLeftNext = TimeLeft - (Now - Started) div 1000,
            wait_for_binding_success(CustomerID, BindingID, TimeLeftNext, Client)
    end;
wait_for_binding_success(_, _, _, _) ->
    timeout.

party_revision_check_init_params(C) ->
    PartyID = <<"RevChecker">>,
    RootUrl = cfg(root_url, C),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    Client = hg_client_invoicing:start_link(hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),
    {PartyID, PartyClient, Client, ShopID}.

invoice_create_and_get_revision(PartyID, Client, ShopID) ->
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"somePlace">>, make_due_date(10), 5000),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()) = ?invoice_w_revision(InvoiceRev))] =
        next_event(InvoiceID, Client),
    {InvoiceRev, InvoiceID}.

make_payment_and_get_revision(InvoiceID, Client) ->
    PaymentParams = make_payment_params(),
    ?payment_state(
        ?payment(PaymentID, PaymentRev)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_)),
        ?payment_ev(PaymentID, ?route_changed(_)),
        ?payment_ev(PaymentID, ?cash_flow_changed(_))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_session_started(InvoiceID, PaymentID, Client, ?processed()),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client, 0),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    {PaymentRev, PaymentID}.

make_payment_adjustment_and_get_revision(PaymentID, InvoiceID, Client) ->
    % make adjustment
    Params = make_adjustment_params(Reason = <<"imdrunk">>),
    ?adjustment(AdjustmentID, ?adjustment_pending()) = Adjustment = ?adjustment_revision(AdjustmentRev) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, Params, Client),
    Adjustment = #domain_InvoicePaymentAdjustment{id = AdjustmentID, reason = Reason} =
        hg_client_invoicing:get_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_created(Adjustment)))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_status_changed(?adjustment_processed())))
    ] = next_event(InvoiceID, Client),
    ok =
        hg_client_invoicing:capture_adjustment(InvoiceID, PaymentID, AdjustmentID, Client),
    [
        ?payment_ev(PaymentID, ?adjustment_ev(AdjustmentID, ?adjustment_status_changed(?adjustment_captured(_))))
    ] = next_event(InvoiceID, Client),
    AdjustmentRev.

make_payment_refund_and_get_revision(PaymentID, InvoiceID, Client) ->
    % create a refund finally
    RefundParams = make_refund_params(),
    Refund = #domain_InvoicePaymentRefund{id = RefundID, party_revision = RefundRev} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    PaymentID = refund_payment(InvoiceID, PaymentID, RefundID, Refund, Client),
    PaymentID = await_refund_session_started(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRefund{status = ?refund_succeeded()} =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    RefundRev.

party_revision_increment(ShopID, PartyClient) ->
    Shop = hg_client_party:get_shop(ShopID, PartyClient),
    ok = hg_ct_helper:adjust_contract(Shop#domain_Shop.contract_id, ?tmpl(1), PartyClient).

payment_risk_score_check(Cat, C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(Cat), <<"RUB">>, ?tmpl(2), ?pinst(2), PartyClient),
    InvoiceID1 = start_invoice(ShopID, <<"rubberduck">>, make_due_date(10), 42000, C),
    % Invoice
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID1)) = hg_client_invoicing:start_payment(InvoiceID1, PaymentParams, Client),
    [
        ?payment_ev(PaymentID1, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID1, Client),
    [
        ?payment_ev(PaymentID1, ?risk_score_changed(low)), % default low risk score...
        ?payment_ev(PaymentID1, ?route_changed(?route(?prv(2), ?trm(7)))),
        ?payment_ev(PaymentID1, ?cash_flow_changed(_))
    ] = next_event(InvoiceID1, Client),
    [
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID1, Client),
    PaymentID1 = await_payment_process_finish(InvoiceID1, PaymentID1, Client),
    PaymentID1 = await_payment_capture(InvoiceID1, PaymentID1, Client).

-spec construct_domain_fixture() -> [hg_domain:object()].

construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ?ordset([
                ?cat(1)
            ])},
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(<<"DEPRIVED ONE">>, undefined),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(bank_card, jcb),
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi),
                        ?pmt(empty_cvv_bank_card, visa),
                        ?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay)),
                        ?pmt(crypto_currency, bitcoin),
                        ?pmt(mobile, mts)
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     10, <<"RUB">>)},
                        {exclusive, ?cash(420000000, <<"RUB">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, operation_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 10}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                fees = {value, [
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?fixed(100, <<"RUB">>)
                    )
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ = {value, ?cashrng(
                                {inclusive, ?cash(      1000, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    ]}

                }
            }
        },
        recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ?ordset([
                ?cat(2),
                ?cat(3),
                ?cat(4),
                ?cat(5),
                ?cat(6),
                ?cat(7)
            ])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                % проверяем, что условие никогда не отрабатывает
                #domain_CashLimitDecision {
                    if_ = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                        definition = {empty_cvv_is, true}
                    }}}},
                    then_ = {value,
                        ?cashrng(
                            {inclusive, ?cash(0, <<"RUB">>)},
                            {inclusive, ?cash(0, <<"RUB">>)}
                        )
                    }
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     10, <<"RUB">>)},
                        {exclusive, ?cash(  4200000, <<"RUB">>)}
                    )}
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(      200, <<"USD">>)},
                        {exclusive, ?cash(   313370, <<"USD">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, operation_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, operation_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 3}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                fees = {value, [
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash( 1000, <<"RUB">>)},
                        {exclusive, ?cash(40000, <<"RUB">>)}
                    )}
                }
            }
        }
    },
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        hg_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),
        hg_ct_fixture:construct_category(?cat(4), <<"Offliner">>, live),
        hg_ct_fixture:construct_category(?cat(5), <<"Timeouter">>, live),
        hg_ct_fixture:construct_category(?cat(6), <<"MachineFailer">>, live),
        hg_ct_fixture:construct_category(?cat(7), <<"TempFailer">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, jcb)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(digital_wallet, qiwi)),
        hg_ct_fixture:construct_payment_method(?pmt(empty_cvv_bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(crypto_currency, bitcoin)),
        hg_ct_fixture:construct_payment_method(?pmt(mobile, mts)),
        hg_ct_fixture:construct_payment_method(?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),
        hg_ct_fixture:construct_inspector(?insp(3), <<"Fatalist">>, ?prx(2), #{<<"risk_score">> => <<"fatal">>}),
        hg_ct_fixture:construct_inspector(?insp(4), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}, low),
        hg_ct_fixture:construct_inspector(?insp(5), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"timeout">>}, low),
        hg_ct_fixture:construct_inspector(?insp(6), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}),
        hg_ct_fixture:construct_inspector(?insp(7), <<"TempFailer">>, ?prx(2),
            #{<<"link_state">> => <<"temporary_failure">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),
        hg_ct_fixture:construct_contract_template(?tmpl(3), ?trms(3)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_system_account_set(?sas(2)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3),
                    ?prv(4)
                ])},

                % TODO do we realy need this decision hell here?
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(4)}},
                                then_ = {value, ?insp(4)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(        0, <<"RUB">>)},
                                    {exclusive, ?cash(   500000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(   500000, <<"RUB">>)},
                                    {exclusive, ?cash(100000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash( 100000000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(3)}
                            }
                        ]}
                    }
                ]},
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
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3)
                ])},
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(4)}},
                                then_ = {value, ?insp(4)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(5)}},
                                then_ = {value, ?insp(5)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(6)}},
                                then_ = {value, ?insp(6)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(7)}},
                                then_ = {value, ?insp(7)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(        0, <<"RUB">>)},
                                    {exclusive, ?cash(   500000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(   500000, <<"RUB">>)},
                                    {exclusive, ?cash(100000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash( 100000000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(3)}
                            }
                        ]}
                    }
                ]},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {decisions, [
                    #domain_ExternalAccountSetDecision{
                        if_ = {condition, {party, #domain_PartyCondition{
                            id = <<"LGBT">>
                        }}},
                        then_ = {value, ?eas(2)}
                    },
                    #domain_ExternalAccountSetDecision{
                        if_ = {constant, true},
                        then_ = {value, ?eas(1)}
                    }
                ]},
                payment_institutions = ?ordset([?pinst(1), ?pinst(2)])
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TestTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = DefaultTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = []
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?prvtrm(1)
                ])},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(bank_card, jcb),
                        ?pmt(empty_cvv_bank_card, visa),
                        ?pmt(crypto_currency, bitcoin),
                        ?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, visa}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(18, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, mastercard}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(19, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, jcb}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system, #domain_PaymentSystemCondition{
                                    payment_system_is = visa,
                                    token_provider_is = applepay
                                }}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {crypto_currency, #domain_CryptoCurrencyCondition{
                                definition = {crypto_currency_is, bitcoin}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        }
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                    definition = {payment_system_is, visa}
                                }}}},
                                then_ = {value, ?hold_lifetime(12)}
                            }
                        ]}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                risk_coverage = high
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?prvtrm(6), ?prvtrm(7)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2),
                        ?cat(4),
                        ?cat(5),
                        ?cat(6)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        )
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                risk_coverage = low,
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash( 5000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {external, outcome},
                            ?fixed(20, <<"RUB">>),
                            <<"Assist fee">>
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(7),
            data = #domain_Terminal{
                name = <<"Terminal 7">>,
                description = <<"Terminal 7">>,
                risk_coverage = high,
                terms = #domain_PaymentsProvisionTerms{
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {external, outcome},
                            ?fixed(20, <<"RUB">>),
                            <<"Kek">>
                        )
                    ]}
                }
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = #domain_Provider{
                name = <<"Crovider">>,
                description = <<"Payment terminal provider">>,
                terminal = {value, [?prvtrm(10)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"crovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(10),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(4),
            data = #domain_Provider{
                name = <<"UnionTelecom">>,
                description = <<"Mobile commerce terminal provider">>,
                terminal = {decisions, [
                    #domain_TerminalDecision{
                        if_ = {condition,
                                {payment_tool, {mobile_commerce, #domain_MobileCommerceCondition{
                                    definition = {operator_is, mts}
                                }}}
                            },
                        then_ = {value, [?prvtrm(11)]}
                    }
                ]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"Union Telecom">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(mobile, mts)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(11),
            data = #domain_Terminal{
                name = <<"Parking Payment Terminal">>,
                description = <<"Mts">>,
                risk_coverage = low,
                options = #{
                    <<"goodPhone">> => <<"7891">>,
                    <<"prefix">>    => <<"1234567890">>
                }
            }
        }}
    ].

construct_term_set_for_cost(LowerBound, UpperBound) ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = {condition, {cost_in, ?cashrng(
                        {inclusive, ?cash(LowerBound, <<"RUB">>)},
                        {inclusive, ?cash(UpperBound, <<"RUB">>)}
                    )}},
                    then_ = {value, ordsets:from_list([?pmt(bank_card, visa)])}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([])}
                }
            ]}
        }
    },
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = ?trms(1),
        data = #domain_TermSetHierarchy{
            parent_terms = undefined,
            term_sets = [#domain_TimedTermSet{
                action_time = #'TimestampInterval'{},
                terms = TermSet
            }]
        }
    }}.

construct_term_set_for_refund_eligibility_time(Seconds) ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            refunds = #domain_PaymentRefundsServiceTerms{
                eligibility_time = {value, #'TimeSpan'{seconds = Seconds}}
            }
        }
    },
    [
        hg_ct_fixture:construct_contract_template(?tmpl(100), ?trms(100)),
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(100),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(2),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }}
    ].

%

payments_w_bank_card_issuer_conditions_fixture(Revision) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(100)
                ])}
            }}
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(100),
            data = #domain_Provider{
                name = <<"VTB21">>,
                description = <<>>,
                abs_account = <<>>,
                terminal = {value, [?prvtrm(100)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
                        {exclusive, ?cash(100000000, <<"RUB">>)}
                    )},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_ = {condition,
                                {payment_tool,
                                    {bank_card, #domain_BankCardCondition {
                                        definition = {issuer_country_is, kaz}
                                    }}
                                }
                            },
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(25, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_ = {constant, true},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(19, 1000, operation_amount)
                                )
                            ]}
                        }
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(100),
            data = #domain_Terminal{
                name = <<"VTB21">>,
                description = <<>>,
                risk_coverage = low
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(4),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            cash_limit = {decisions, [
                                #domain_CashLimitDecision {
                                    if_ = {condition,
                                        {payment_tool,
                                            {bank_card, #domain_BankCardCondition {
                                                definition = {issuer_country_is, kaz}
                                            }}
                                        }
                                    },
                                    then_ = {value,
                                        ?cashrng(
                                            {inclusive, ?cash(1000, <<"RUB">>)},
                                            {inclusive, ?cash(1000, <<"RUB">>)}
                                        )
                                    }
                                },
                                #domain_CashLimitDecision {
                                    if_   = {constant, true},
                                    then_ = {value,
                                        ?cashrng(
                                            {inclusive, ?cash(      1000, <<"RUB">>)},
                                            {exclusive, ?cash(1000000000, <<"RUB">>)}
                                        )
                                    }
                                }
                            ]}
                        }
                    }
                }]
            }
        }},
        hg_ct_fixture:construct_contract_template(?tmpl(4), ?trms(4))
    ].

payments_w_bank_conditions_fixture(_Revision) ->
    [
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(4),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            cash_limit = {decisions, [
                                #domain_CashLimitDecision {
                                    if_ = {condition,
                                        {payment_tool,
                                            {bank_card, #domain_BankCardCondition {
                                                definition = {issuer_bank_is, ?bank(1)}
                                            }}
                                        }
                                    },
                                    then_ = {value,
                                        ?cashrng(
                                            {inclusive, ?cash(1000, <<"RUB">>)},
                                            {inclusive, ?cash(1000, <<"RUB">>)}
                                        )
                                    }
                                },
                                #domain_CashLimitDecision {
                                    if_   = {constant, true},
                                    then_ = {value,
                                        ?cashrng(
                                            {inclusive, ?cash(      1000, <<"RUB">>)},
                                            {exclusive, ?cash(1000000000, <<"RUB">>)}
                                        )
                                    }
                                }
                            ]}
                        }
                    }
                }]
            }
        }},
        {bank, #domain_BankObject {
            ref = ?bank(1),
            data = #domain_Bank {
                name = <<"TEST BANK">>,
                description = <<"TEST BANK">>,
                bins = ordsets:from_list([<<"42424242">>]),
                binbase_id_patterns = ordsets:from_list([<<"TEST*BANK">>])
            }
        }},
        hg_ct_fixture:construct_contract_template(?tmpl(4), ?trms(4))
    ].

payment_manual_refund_fixture(_Revision) ->
    [
        {proxy, #domain_ProxyObject{
            ref = ?prx(1),
            data = #domain_ProxyDefinition{
                name        = <<"undefined">>,
                description = <<"undefined">>,
                url         = <<"undefined">>,
                options     = #{}
            }
        }}
    ].

construct_term_set_for_partial_capture_service_permit() ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 10}}
                    }
                ]},
                partial_captures = #domain_PartialCaptureServiceTerms{}
            }
        }
    },
    [
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(5),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }},
        hg_ct_fixture:construct_contract_template(?tmpl(6), ?trms(5))
    ].

construct_term_set_for_partial_capture_provider_permit(Revision) ->
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),
    [
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = PaymentInstitution#domain_PaymentInstitution{
                providers = {value, ?ordset([
                    ?prv(101)
                ])}
            }}
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(101),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?prvtrm(1)
                ])},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, visa}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(18, 1000, operation_amount)
                                )
                            ]}
                        }
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    },
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                    definition = {payment_system_is, visa}
                                }}}},
                                then_ = {value, ?hold_lifetime(12)}
                            }
                        ]},
                        partial_captures = #domain_PartialCaptureProvisionTerms{}
                    }
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        }}
    ].

% Deadline as timeout()
set_processing_deadline(Timeout, PaymentParams) ->
    Deadline = woody_deadline:to_binary(woody_deadline:from_timeout(Timeout)),
    PaymentParams#payproc_InvoicePaymentParams{processing_deadline = Deadline}.
