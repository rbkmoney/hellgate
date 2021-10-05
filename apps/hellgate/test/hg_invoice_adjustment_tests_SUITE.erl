-module(hg_invoice_adjustment_tests_SUITE).

-include("hg_ct_domain.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invoice_adjustment_capture/1]).
-export([invoice_adjustment_capture_new/1]).
-export([invoice_adjustment_cancel/1]).
-export([invoice_adjustment_cancel_new/1]).
-export([invoice_adjustment_existing_invoice_status/1]).
-export([invoice_adjustment_existing_invoice_status_new/1]).
-export([invoice_adjustment_invalid_invoice_status/1]).
-export([invoice_adjustment_invalid_invoice_status_new/1]).
-export([invoice_adjustment_invalid_adjustment_status/1]).
-export([invoice_adjustment_invalid_adjustment_status_new/1]).
-export([invoice_adjustment_payment_pending/1]).
-export([invoice_adjustment_payment_pending_new/1]).
-export([invoice_adjustment_pending_blocks_payment/1]).
-export([invoice_adjustment_pending_blocks_payment_new/1]).
-export([invoice_adjustment_pending_no_invoice_expiration/1]).
-export([invoice_adjustment_invoice_expiration_after_capture/1]).
-export([invoice_adjustment_invoice_expiration_after_capture_new/1]).

-behaviour(supervisor).

-export([init/1]).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%% tests descriptions

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name() :: hg_ct_helper:group_name().
-type test_return() :: _ | no_return().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, all_tests}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {all_tests, [parallel], [
            invoice_adjustment_capture,
            invoice_adjustment_capture_new,
            invoice_adjustment_cancel,
            invoice_adjustment_cancel_new,
            invoice_adjustment_existing_invoice_status,
            invoice_adjustment_existing_invoice_status_new,
            invoice_adjustment_invalid_invoice_status,
            invoice_adjustment_invalid_invoice_status_new,
            invoice_adjustment_invalid_adjustment_status,
            invoice_adjustment_invalid_adjustment_status_new,
            invoice_adjustment_payment_pending,
            invoice_adjustment_payment_pending_new,
            invoice_adjustment_pending_blocks_payment,
            invoice_adjustment_pending_blocks_payment_new,
            invoice_adjustment_pending_no_invoice_expiration,
            invoice_adjustment_invoice_expiration_after_capture_new,
            invoice_adjustment_invoice_expiration_after_capture
        ]}
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_invoice_payment', 'p', '_'}, x),
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),

    {Apps, Ret} = hg_ct_helper:start_apps([woody, scoper, dmt_client, party_client, hellgate, {cowboy, CowboySpec}]),
    _ = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),

    PartyID = hg_utils:unique_id(),
    PartyClient = {party_client:create_client(), party_client:create_context(user_info())},

    _ = timer:sleep(5000),

    ShopID = hg_ct_helper:create_party_and_shop(PartyID, ?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyClient),

    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    NewC = [
        {party_id, PartyID},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps},
        {test_sup, SupPid}
        | C
    ],

    ok = start_proxies([{hg_dummy_provider, 1, NewC}, {hg_dummy_inspector, 2, NewC}]),
    NewC.

user_info() ->
    #{user_info => #{id => <<"test">>, realm => <<"service">>}}.

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = hg_domain:cleanup(),
    _ = [application:stop(App) || App <- cfg(apps, C)],
    exit(cfg(test_sup, C), shutdown).

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    init_per_testcase(C).

init_per_testcase(C) ->
    ApiClient = hg_ct_helper:create_client(cfg(root_url, C), cfg(party_id, C)),
    Client = hg_client_invoicing:start_link(ApiClient),
    ClientTpl = hg_client_invoice_templating:start_link(ApiClient),
    ok = hg_context:save(hg_context:create()),
    [{client, Client}, {client_tpl, ClientTpl} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = hg_context:cleanup().

-include("invoice_events.hrl").
-include("payment_events.hrl").
-include("customer_events.hrl").

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-define(merchant_to_system_share_1, ?share(45, 1000, operation_amount)).
-define(merchant_to_system_share_2, ?share(100, 1000, operation_amount)).

%% ADJ

-spec invoice_adjustment_capture(config()) -> test_return().
invoice_adjustment_capture(C) ->
    invoice_adjustment_capture(C, visa).

-spec invoice_adjustment_capture_new(config()) -> test_return().
invoice_adjustment_capture_new(C) ->
    invoice_adjustment_capture(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_capture(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(10), make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_Invoice)] = next_event(InvoiceID, Client),
    PaymentID = process_payment(InvoiceID, make_payment_params(PmtSys), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    Unpaid = {unpaid, #domain_InvoiceUnpaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = Unpaid
            }}
    },
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch({pending, #domain_InvoiceAdjustmentPending{}}, Adjustment#domain_InvoiceAdjustment.status),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Processed))] = next_event(InvoiceID, Client),
    ?assertMatch({processed, #domain_InvoiceAdjustmentProcessed{}}, Processed),
    ok = hg_client_invoicing:capture_invoice_adjustment(InvoiceID, ID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Captured))] = next_event(InvoiceID, Client),
    ?assertMatch({captured, #domain_InvoiceAdjustmentCaptured{}}, Captured),
    #payproc_Invoice{invoice = #domain_Invoice{status = FinalStatus}} = hg_client_invoicing:get(InvoiceID, Client),
    ?assertMatch(Unpaid, FinalStatus).

-spec invoice_adjustment_cancel(config()) -> test_return().
invoice_adjustment_cancel(C) ->
    invoice_adjustment_cancel(C, visa).

-spec invoice_adjustment_cancel_new(config()) -> test_return().
invoice_adjustment_cancel_new(C) ->
    invoice_adjustment_cancel(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_cancel(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    PaymentID = process_payment(InvoiceID, make_payment_params(PmtSys), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    #payproc_Invoice{invoice = #domain_Invoice{status = InvoiceStatus}} = hg_client_invoicing:get(InvoiceID, Client),
    TargetInvoiceStatus = {unpaid, #domain_InvoiceUnpaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = TargetInvoiceStatus
            }}
    },
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch({pending, #domain_InvoiceAdjustmentPending{}}, Adjustment#domain_InvoiceAdjustment.status),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Processed))] = next_event(InvoiceID, Client),
    ?assertMatch({processed, #domain_InvoiceAdjustmentProcessed{}}, Processed),
    ok = hg_client_invoicing:cancel_invoice_adjustment(InvoiceID, ID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Cancelled))] = next_event(InvoiceID, Client),
    ?assertMatch({cancelled, #domain_InvoiceAdjustmentCancelled{}}, Cancelled),
    #payproc_Invoice{invoice = #domain_Invoice{status = FinalStatus}} = hg_client_invoicing:get(InvoiceID, Client),
    ?assertMatch(InvoiceStatus, FinalStatus).

-spec invoice_adjustment_invalid_invoice_status(config()) -> test_return().
invoice_adjustment_invalid_invoice_status(C) ->
    invoice_adjustment_invalid_invoice_status(C, visa).

-spec invoice_adjustment_invalid_invoice_status_new(config()) -> test_return().
invoice_adjustment_invalid_invoice_status_new(C) ->
    invoice_adjustment_invalid_invoice_status(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_invalid_invoice_status(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    PaymentID = process_payment(InvoiceID, make_payment_params(PmtSys), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = {unpaid, #domain_InvoiceUnpaid{}}
            }}
    },
    ok = hg_client_invoicing:fulfill(InvoiceID, <<"ok">>, Client),
    {exception, E} = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch(#payproc_InvoiceAdjustmentStatusUnacceptable{}, E).

-spec invoice_adjustment_existing_invoice_status(config()) -> test_return().
invoice_adjustment_existing_invoice_status(C) ->
    invoice_adjustment_existing_invoice_status(C, visa).

-spec invoice_adjustment_existing_invoice_status_new(config()) -> test_return().
invoice_adjustment_existing_invoice_status_new(C) ->
    invoice_adjustment_existing_invoice_status(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_existing_invoice_status(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    PaymentID = process_payment(InvoiceID, make_payment_params(PmtSys), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    Paid = {paid, #domain_InvoicePaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = Paid
            }}
    },
    {exception, E} = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch(#payproc_InvoiceAlreadyHasStatus{status = Paid}, E).

-spec invoice_adjustment_invalid_adjustment_status(config()) -> test_return().
invoice_adjustment_invalid_adjustment_status(C) ->
    invoice_adjustment_invalid_adjustment_status(C, visa).

-spec invoice_adjustment_invalid_adjustment_status_new(config()) -> test_return().
invoice_adjustment_invalid_adjustment_status_new(C) ->
    invoice_adjustment_invalid_adjustment_status(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_invalid_adjustment_status(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    PaymentID = process_payment(InvoiceID, make_payment_params(PmtSys), Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    #payproc_Invoice{invoice = #domain_Invoice{status = InvoiceStatus}} = hg_client_invoicing:get(InvoiceID, Client),
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = {cancelled, #domain_InvoiceCancelled{details = <<"hulk smash">>}}
            }}
    },
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch({pending, #domain_InvoiceAdjustmentPending{}}, Adjustment#domain_InvoiceAdjustment.status),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Processed))] = next_event(InvoiceID, Client),
    ?assertMatch({processed, #domain_InvoiceAdjustmentProcessed{}}, Processed),
    ok = hg_client_invoicing:cancel_invoice_adjustment(InvoiceID, ID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Cancelled))] = next_event(InvoiceID, Client),
    ?assertMatch({cancelled, #domain_InvoiceAdjustmentCancelled{}}, Cancelled),
    #payproc_Invoice{invoice = #domain_Invoice{status = FinalStatus}} = hg_client_invoicing:get(InvoiceID, Client),
    ?assertMatch(InvoiceStatus, FinalStatus),
    {exception, E} = hg_client_invoicing:cancel_invoice_adjustment(InvoiceID, ID, Client),
    ?assertMatch(#payproc_InvalidInvoiceAdjustmentStatus{status = Cancelled}, E).

-spec invoice_adjustment_payment_pending(config()) -> test_return().
invoice_adjustment_payment_pending(C) ->
    invoice_adjustment_payment_pending(C, visa).

-spec invoice_adjustment_payment_pending_new(config()) -> test_return().
invoice_adjustment_payment_pending_new(C) ->
    invoice_adjustment_payment_pending(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_payment_pending(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(10), make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    PaymentID = start_payment(InvoiceID, make_tds_payment_params(instant, PmtSys), Client),
    Paid = {paid, #domain_InvoicePaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = Paid
            }}
    },
    {exception, E} = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch(#payproc_InvoicePaymentPending{id = PaymentID}, E),
    UserInteraction = await_payment_process_interaction(InvoiceID, PaymentID, Client),
    {URL, GoodForm} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, GoodForm}),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec invoice_adjustment_pending_blocks_payment(config()) -> test_return().
invoice_adjustment_pending_blocks_payment(C) ->
    invoice_adjustment_pending_blocks_payment(C, visa).

-spec invoice_adjustment_pending_blocks_payment_new(config()) -> test_return().
invoice_adjustment_pending_blocks_payment_new(C) ->
    invoice_adjustment_pending_blocks_payment(C, visa).

invoice_adjustment_pending_blocks_payment(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_Invoice)] = next_event(InvoiceID, Client),
    PaymentParams = make_payment_params({hold, capture}, PmtSys),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    Cash = ?cash(10, <<"RUB">>),
    Reason = <<"ok">>,
    TargetInvoiceStatus = {cancelled, #domain_InvoiceCancelled{details = <<"hulk smash">>}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = TargetInvoiceStatus
            }}
    },
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, Reason, Cash, Client),
    PaymentID = await_payment_partial_capture(InvoiceID, PaymentID, Reason, Cash, Client),
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch({pending, #domain_InvoiceAdjustmentPending{}}, Adjustment#domain_InvoiceAdjustment.status),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Processed))] = next_event(InvoiceID, Client),
    ?assertMatch({processed, #domain_InvoiceAdjustmentProcessed{}}, Processed),
    {exception, E} = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    ?assertMatch(#payproc_InvoiceAdjustmentPending{id = ID}, E).

-spec invoice_adjustment_pending_no_invoice_expiration(config()) -> test_return().
invoice_adjustment_pending_no_invoice_expiration(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(5), make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    Paid = {paid, #domain_InvoicePaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = Paid
            }}
    },
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(_Processed))] = next_event(InvoiceID, Client),
    timeout = next_event(InvoiceID, 6000, Client).

-spec invoice_adjustment_invoice_expiration_after_capture(config()) -> test_return().
invoice_adjustment_invoice_expiration_after_capture(C) ->
    invoice_adjustment_invoice_expiration_after_capture(C, visa).

-spec invoice_adjustment_invoice_expiration_after_capture_new(config()) -> test_return().
invoice_adjustment_invoice_expiration_after_capture_new(C) ->
    invoice_adjustment_invoice_expiration_after_capture(C, ?pmt_sys(<<"visa-ref">>)).

invoice_adjustment_invoice_expiration_after_capture(C, PmtSys) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(10), make_cash(10000)),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(_)] = next_event(InvoiceID, Client),
    Context = #'Content'{
        type = <<"application/x-erlang-binary">>,
        data = erlang:term_to_binary({you, 643, "not", [<<"welcome">>, here]})
    },
    PaymentParams = set_payment_context(Context, make_payment_params(PmtSys)),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    Unpaid = {unpaid, #domain_InvoiceUnpaid{}},
    AdjustmentParams = #payproc_InvoiceAdjustmentParams{
        reason = <<"kek">>,
        scenario =
            {status_change, #domain_InvoiceAdjustmentStatusChange{
                target_status = Unpaid
            }}
    },
    Adjustment = hg_client_invoicing:create_invoice_adjustment(InvoiceID, AdjustmentParams, Client),
    ?assertMatch({pending, #domain_InvoiceAdjustmentPending{}}, Adjustment#domain_InvoiceAdjustment.status),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_created(Adjustment))] = next_event(InvoiceID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Processed))] = next_event(InvoiceID, Client),
    ?assertMatch({processed, #domain_InvoiceAdjustmentProcessed{}}, Processed),
    ok = hg_client_invoicing:capture_invoice_adjustment(InvoiceID, ID, Client),
    [?invoice_adjustment_ev(ID, ?invoice_adjustment_status_changed(Captured))] = next_event(InvoiceID, Client),
    ?assertMatch({captured, #domain_InvoiceAdjustmentCaptured{}}, Captured),
    #payproc_Invoice{invoice = #domain_Invoice{status = Unpaid}} = hg_client_invoicing:get(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(_))] = next_event(InvoiceID, Client).

%% ADJ

-spec construct_domain_fixture() -> [hg_domain:object()].
construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies =
                {value,
                    ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
            categories =
                {value,
                    ?ordset([
                        ?cat(1)
                    ])},
            payment_methods =
                {decisions, [
                    #domain_PaymentMethodDecision{
                        if_ = {constant, true},
                        then_ =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                                    ?pmt(bank_card_deprecated, visa)
                                ])}
                    }
                ]},
            cash_limit =
                {decisions, [
                    #domain_CashLimitDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(10, <<"RUB">>)},
                                    {exclusive, ?cash(420000000, <<"RUB">>)}
                                )}
                    }
                ]},
            fees =
                {decisions, [
                    #domain_CashFlowDecision{
                        if_ =
                            {condition,
                                {payment_tool,
                                    {bank_card, #domain_BankCardCondition{
                                        definition = {category_is, ?bc_cat(1)}
                                    }}}},
                        then_ =
                            {value, [
                                ?cfpost(
                                    {merchant, settlement},
                                    {system, settlement},
                                    ?merchant_to_system_share_2
                                )
                            ]}
                    },
                    #domain_CashFlowDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ =
                            {value, [
                                ?cfpost(
                                    {merchant, settlement},
                                    {system, settlement},
                                    ?merchant_to_system_share_1
                                )
                            ]}
                    }
                ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods =
                    {value,
                        ?ordset([
                            ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                            ?pmt(bank_card_deprecated, visa)
                        ])},
                lifetime =
                    {decisions, [
                        #domain_HoldLifetimeDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ = {value, #domain_HoldLifetime{seconds = 10}}
                        }
                    ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods =
                    {value,
                        ?ordset([
                            ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                            ?pmt(bank_card_deprecated, visa)
                        ])},
                fees =
                    {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?fixed(100, <<"RUB">>)
                        )
                    ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit =
                        {decisions, [
                            #domain_CashLimitDecision{
                                if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                then_ =
                                    {value,
                                        ?cashrng(
                                            {inclusive, ?cash(1000, <<"RUB">>)},
                                            {exclusive, ?cash(1000000000, <<"RUB">>)}
                                        )}
                            }
                        ]}
                }
            }
        },
        recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods =
                {value,
                    ordsets:from_list([
                        ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                        ?pmt(bank_card_deprecated, visa)
                    ])}
        }
    },
    [
        hg_ct_fixture:construct_bank_card_category(
            ?bc_cat(1),
            <<"Bank card category">>,
            <<"Corporative">>,
            [<<"*CORPORAT*">>]
        ),

        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card_deprecated, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, ?bank_card(<<"visa-ref">>))),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                payment_routing_rules = #domain_RoutingRules{
                    policies = ?ruleset(2),
                    prohibitions = ?ruleset(1)
                },
                inspector =
                    {decisions, [
                        #domain_InspectorDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ =
                                {decisions, [
                                    #domain_InspectorDecision{
                                        if_ =
                                            {condition,
                                                {cost_in,
                                                    ?cashrng(
                                                        {inclusive, ?cash(0, <<"RUB">>)},
                                                        {exclusive, ?cash(500000, <<"RUB">>)}
                                                    )}},
                                        then_ = {value, ?insp(1)}
                                    }
                                ]}
                        }
                    ]},
                residences = [],
                realm = test
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(1),
            data = #domain_RoutingRuleset{
                name = <<"Prohibitions: all is allow">>,
                decisions = {candidates, []}
            }
        }},
        {routing_rules, #domain_RoutingRulesObject{
            ref = ?ruleset(2),
            data = #domain_RoutingRuleset{
                name = <<"Prohibitions: all is allow">>,
                decisions =
                    {candidates, [
                        ?candidate({constant, true}, ?trm(1))
                    ]}
            }
        }},
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set =
                    {decisions, [
                        #domain_ExternalAccountSetDecision{
                            if_ = {constant, true},
                            then_ = {value, ?eas(1)}
                        }
                    ]},
                payment_institutions = ?ordset([?pinst(1)])
            }
        }},

        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                term_sets = [
                    #domain_TimedTermSet{
                        action_time = #'TimestampInterval'{},
                        terms = TestTermSet
                    }
                ]
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        currencies =
                            {value,
                                ?ordset([
                                    ?cur(<<"RUB">>)
                                ])},
                        categories =
                            {value,
                                ?ordset([
                                    ?cat(1)
                                ])},
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                                    ?pmt(bank_card_deprecated, visa)
                                ])},
                        cash_limit =
                            {value,
                                ?cashrng(
                                    {inclusive, ?cash(1000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )},
                        cash_flow =
                            {decisions, [
                                #domain_CashFlowDecision{
                                    if_ =
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition =
                                                        {payment_system, #domain_PaymentSystemCondition{
                                                            payment_system_is = ?pmt_sys(<<"visa-ref">>)
                                                        }}
                                                }}}},
                                    then_ =
                                        {value, [
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
                                    if_ =
                                        {condition,
                                            {payment_tool,
                                                {bank_card, #domain_BankCardCondition{
                                                    definition = {payment_system_is, visa}
                                                }}}},
                                    then_ =
                                        {value, [
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
                        holds = #domain_PaymentHoldsProvisionTerms{
                            lifetime =
                                {decisions, [
                                    #domain_HoldLifetimeDecision{
                                        if_ =
                                            {condition,
                                                {payment_tool,
                                                    {bank_card, #domain_BankCardCondition{
                                                        definition =
                                                            {payment_system, #domain_PaymentSystemCondition{
                                                                payment_system_is = ?pmt_sys(<<"visa-ref">>)
                                                            }}
                                                    }}}},
                                        then_ = {value, ?hold_lifetime(12)}
                                    },
                                    #domain_HoldLifetimeDecision{
                                        if_ =
                                            {condition,
                                                {payment_tool,
                                                    {bank_card, #domain_BankCardCondition{
                                                        definition = {payment_system_is, visa}
                                                    }}}},
                                        then_ = {value, ?hold_lifetime(12)}
                                    }
                                ]}
                        },
                        refunds = #domain_PaymentRefundsProvisionTerms{
                            cash_flow =
                                {value, [
                                    ?cfpost(
                                        {merchant, settlement},
                                        {provider, settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]},
                            partial_refunds = #domain_PartialRefundsProvisionTerms{
                                cash_limit =
                                    {value,
                                        ?cashrng(
                                            {inclusive, ?cash(10, <<"RUB">>)},
                                            {exclusive, ?cash(1000000000, <<"RUB">>)}
                                        )}
                            }
                        },
                        chargebacks = #domain_PaymentChargebackProvisionTerms{
                            cash_flow =
                                {value, [
                                    ?cfpost(
                                        {merchant, settlement},
                                        {provider, settlement},
                                        ?share(1, 1, operation_amount)
                                    )
                                ]}
                        }
                    },
                    recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                        categories = {value, ?ordset([?cat(1)])},
                        payment_methods =
                            {value,
                                ?ordset([
                                    ?pmt(bank_card, ?bank_card(<<"visa-ref">>)),
                                    ?pmt(bank_card_deprecated, visa)
                                ])},
                        cash_value = {value, ?cash(1000, <<"RUB">>)}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                risk_coverage = high,
                provider_ref = ?prv(1)
            }
        }},
        hg_ct_fixture:construct_payment_system(?pmt_sys(<<"visa-ref">>), <<"visa payment system">>)
    ].

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
    setup_proxies(
        lists:map(
            fun
                Mapper({Module, ProxyID, Context}) ->
                    Mapper({Module, ProxyID, #{}, Context});
                Mapper({Module, ProxyID, ProxyOpts, Context}) ->
                    construct_proxy(ProxyID, start_service_handler(Module, Context, #{}), ProxyOpts)
            end,
            Proxies
        )
    ).

setup_proxies(Proxies) ->
    _ = hg_domain:upsert(Proxies),
    ok.

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = ?prx(ID),
        data = #domain_ProxyDefinition{
            name = Url,
            description = Url,
            url = Url,
            options = Options
        }
    }}.

make_cash(Amount) ->
    hg_ct_helper:make_cash(Amount, <<"RUB">>).

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Cost).

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

create_invoice(InvoiceParams, Client) ->
    ?invoice_state(?invoice(InvoiceID)) = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

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
filter_change(?chargeback_ev(_, C)) ->
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

set_payment_context(Context, Params = #payproc_InvoicePaymentParams{}) ->
    Params#payproc_InvoicePaymentParams{context = Context}.

make_payment_params(PmtSys) ->
    make_payment_params(instant, PmtSys).

make_payment_params(FlowType, PmtSys) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(no_preauth, PmtSys),
    make_payment_params(PaymentTool, Session, FlowType).

make_tds_payment_params(FlowType, PmtSys) ->
    {PaymentTool, Session} = hg_dummy_provider:make_payment_tool(preauth_3ds, PmtSys),
    make_payment_params(PaymentTool, Session, FlowType).

make_payment_params(PaymentTool, Session, FlowType) ->
    Flow =
        case FlowType of
            instant ->
                {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
            {hold, OnHoldExpiration} ->
                {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
        end,
    #payproc_InvoicePaymentParams{
        payer =
            {payment_resource, #payproc_PaymentResourcePayerParams{
                resource = #domain_DisposablePaymentResource{
                    payment_tool = PaymentTool,
                    payment_session_id = Session,
                    client_info = #domain_ClientInfo{}
                },
                contact_info = #domain_ContactInfo{}
            }},
        flow = Flow
    }.

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form};
get_post_request({payment_terminal_reciept, #'PaymentTerminalReceipt'{short_payment_id = SPID}}) ->
    URL = hg_dummy_provider:get_callback_url(),
    {URL, #{<<"tag">> => SPID}}.

start_payment(InvoiceID, PaymentParams, Client) ->
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?risk_score_changed(_))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?route_changed(_))
    ] = next_event(InvoiceID, Client),
    [
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

-dialyzer({no_match, await_sessions_restarts/5}).

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

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).
