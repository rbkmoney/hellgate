-module(hg_invoice_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/domain.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([invalid_invoice_shop/1]).
-export([invalid_invoice_amount/1]).
-export([invalid_invoice_currency/1]).
-export([invalid_party_status/1]).
-export([invalid_shop_status/1]).
-export([invoice_cancellation/1]).
-export([overdue_invoice_cancellation/1]).
-export([invoice_cancellation_after_payment_timeout/1]).
-export([invalid_payment_amount/1]).
-export([payment_success/1]).
-export([payment_success_on_second_try/1]).
-export([invoice_success_on_third_payment/1]).
-export([payment_risk_score_check/1]).
-export([invalid_payment_adjustment/1]).
-export([payment_adjustment_success/1]).
-export([invalid_payment_w_deprived_party/1]).
-export([external_account_posting/1]).
-export([consistent_history/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

%%


%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

cfg(Key, C) ->
    case lists:keyfind(Key, 1, C) of
        {Key, V} -> V;
        _        -> undefined
    end.

-spec all() -> [test_case_name()].

all() ->
    [
        invalid_invoice_shop,
        invalid_invoice_amount,
        invalid_invoice_currency,
        invalid_party_status,
        invalid_shop_status,
        invoice_cancellation,
        overdue_invoice_cancellation,
        invoice_cancellation_after_payment_timeout,
        invalid_payment_amount,
        payment_success,
        payment_success_on_second_try,
        invoice_success_on_third_payment,

        payment_risk_score_check,

        invalid_payment_adjustment,
        payment_adjustment_success,

        invalid_payment_w_deprived_party,
        external_account_posting,

        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_client_party', '_', '_'}, x),
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, dmt_client, hellgate, {cowboy, CowboySpec}]),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(RootUrl)),
    ShopID = hg_ct_helper:create_party_and_shop(Client),
    [
        {party_id, PartyID},
        {party_client, Client},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps}
        | C
    ].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

%% tests

-include("invoice_events.hrl").
-include("payment_events.hrl").

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(adjustment(ID), #domain_InvoicePaymentAdjustment{id = ID}).
-define(adjustment(ID, Status), #domain_InvoicePaymentAdjustment{id = ID, status = Status}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(invoice_state(Invoice, Payments), #payproc_Invoice{invoice = Invoice, payments = Payments}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-define(invalid_invoice_status(Status),
    {exception, #payproc_InvalidInvoiceStatus{status = Status}}).

-define(invalid_payment_status(Status),
    {exception, #payproc_InvalidPaymentStatus{status = Status}}).
-define(invalid_adjustment_status(Status),
    {exception, #payproc_InvalidPaymentAdjustmentStatus{status = Status}}).
-define(invalid_adjustment_pending(ID),
    {exception, #payproc_InvoicePaymentAdjustmentPending{id = ID}}).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(payment_adjustment_success, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(get_adjustment_fixture(Revision)),
    [{original_domain_revision, Revision} | init_per_testcase(C)];
init_per_testcase(_Name, C) ->
    init_per_testcase(C).

init_per_testcase(C) ->
    PartyID = cfg(party_id, C),
    Client = hg_client_invoicing:start_link(make_userinfo(PartyID), hg_client_api:new(cfg(root_url, C))),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    [{client, Client}, {test_sup, SupPid} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    _ = case cfg(original_domain_revision, C) of
        Revision when is_integer(Revision) ->
            ok = hg_domain:reset(Revision);
        undefined ->
            ok
    end,
    _ = unlink(cfg(test_sup, C)),
    exit(cfg(test_sup, C), shutdown).

-spec invalid_invoice_shop(config()) -> _ | no_return().

invalid_invoice_shop(C) ->
    Client = cfg(client, C),
    ShopID = genlib:unique(),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    {exception, #payproc_ShopNotFound{}} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_invoice_amount(config()) -> _ | no_return().

invalid_invoice_amount(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, -10000),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid amount">>]
    }} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_invoice_currency(config()) -> _ | no_return().

invalid_invoice_currency(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100, <<"KEK">>}),
    {exception, #'InvalidRequest'{
        errors = [<<"Invalid currency">>]
    }} = hg_client_invoicing:create(InvoiceParams, Client).

-spec invalid_party_status(config()) -> _ | no_return().

invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100000, <<"RUB">>}),
    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    ok = hg_client_party:activate(PartyClient),
    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec invalid_shop_status(config()) -> _ | no_return().

invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, {100000, <<"RUB">>}),
    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),
    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoicing:create(InvoiceParams, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec invoice_cancellation(config()) -> _ | no_return().

invoice_cancellation(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, 10000),
    InvoiceID = create_invoice(InvoiceParams, Client),
    ?invalid_invoice_status(_) = hg_client_invoicing:fulfill(InvoiceID, <<"perfect">>, Client),
    ok = hg_client_invoicing:rescind(InvoiceID, <<"whynot">>, Client).

-spec overdue_invoice_cancellation(config()) -> _ | no_return().

overdue_invoice_cancellation(C) ->
    Client = cfg(client, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(1), 10000, C),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec invoice_cancellation_after_payment_timeout(config()) -> _ | no_return().

invoice_cancellation_after_payment_timeout(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberdusk">>, make_due_date(3), 1000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?interaction_requested(_)))
    ] = next_event(InvoiceID, Client),
    %% wait for payment timeout
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?captured(), ?session_finished(?session_failed(?operation_timeout())))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed(_)))
    ] = next_event(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec invalid_payment_amount(config()) -> _ | no_return().

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

-spec payment_success(config()) -> _ | no_return().

payment_success(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_success_on_second_try(config()) -> _ | no_return().

payment_success_on_second_try(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?captured(), ?interaction_requested(UserInteraction))
        )
    ] = next_event(InvoiceID, Client),
    %% simulate user interaction
    {URL, GoodForm} = get_post_request(UserInteraction),
    BadForm = #{<<"tag">> => <<"666">>},
    _ = assert_failed_post_request({URL, BadForm}),
    _ = assert_success_post_request({URL, GoodForm}),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client).

-spec invoice_success_on_third_payment(config()) -> _ | no_return().

invoice_success_on_third_payment(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberdock">>, make_due_date(60), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID1 = process_payment(InvoiceID, PaymentParams, Client),
    %% wait for payment timeout and start new one after
    PaymentID1 = await_payment_failure(InvoiceID, PaymentID1, Client),
    PaymentID2 = process_payment(InvoiceID, PaymentParams, Client),
    %% wait for payment timeout and start new one after
    PaymentID2 = await_payment_failure(InvoiceID, PaymentID2, Client),
    PaymentID3 = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(
            PaymentID3,
            ?session_ev(_, ?interaction_requested(UserInteraction))
        )
    ] = next_event(InvoiceID, Client),
    GoodPost = get_post_request(UserInteraction),
    %% simulate user interaction FTW!
    _ = assert_success_post_request(GoodPost),
    PaymentID3 = await_payment_capture(InvoiceID, PaymentID3, Client).

%% @TODO modify this test by failures of inspector in case of wrong terminal choice
-spec payment_risk_score_check(config()) -> _ | no_return().

payment_risk_score_check(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    % Invoice w/ cost < 500000
    InvoiceID1 = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID1)) = hg_client_invoicing:start_payment(InvoiceID1, PaymentParams, Client),
    [
        ?payment_ev(PaymentID1, ?payment_started(?payment_w_status(?pending()), low, _, _)),
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID1, Client),
    [
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?trx_bound(_))),
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID1, ?payment_status_changed(?processed())),
        ?payment_ev(PaymentID2, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID1, Client),
    PaymentID1 = await_payment_capture(InvoiceID1, PaymentID1, Client),
    % Invoice w/ cost > 500000
    InvoiceID2 = start_invoice(<<"rubberbucks">>, make_due_date(10), 31337000, C),
    ?payment_state(?payment(PaymentID2)) = hg_client_invoicing:start_payment(InvoiceID2, PaymentParams, Client),
    [
        ?payment_ev(PaymentID2, ?payment_started(?payment_w_status(?pending()), high, _, _)),
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID2, Client),
    [
        ?payment_ev(PaymentID2, ?session_ev(?processed(), ?trx_bound(_))),
        ?payment_ev(PaymentID2, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID2, ?payment_status_changed(?processed())),
        ?payment_ev(PaymentID1, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID2, Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client).

-spec invalid_payment_adjustment(config()) -> _ | no_return().

invalid_payment_adjustment(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    %% start a smoker's payment
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    %% no way to create adjustment for a payment not yet finished
    ?invalid_payment_status(?processed()) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client),
    PaymentID = await_payment_failure(InvoiceID, PaymentID, Client),
    %% no way to create adjustment for a failed payment
    ?invalid_payment_status(?failed(_)) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client).

-spec payment_adjustment_success(config()) -> _ | no_return().

payment_adjustment_success(C) ->
    Client = cfg(client, C),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    %% start a healthy man's payment
    PaymentParams = make_payment_params(),
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending()), _, _, CF1)),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(_))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?processed())),
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    PrvAccount1 = get_cashflow_account({provider, settlement}, CF1),
    SysAccount1 = get_cashflow_account({system, settlement}, CF1),
    %% update terminal cashflow
    Terminal = hg_domain:get(hg_domain:head(), {terminal, ?trm(100)}),
    ok = hg_domain:upsert(
        {terminal, #domain_TerminalObject{
            ref = ?trm(100),
            data = Terminal#domain_Terminal{cash_flow = get_adjustment_terminal_cashflow(actual)}
        }}
    ),
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
    500  = maps:get(own_amount, PrvAccount1) - maps:get(own_amount, PrvAccount2),
    -480 = maps:get(own_amount, SysAccount1) - maps:get(own_amount, SysAccount2).

get_cashflow_account(Type, CF) ->
    [ID] = [V || #domain_FinalCashFlowPosting{
        destination = #domain_FinalCashFlowAccount{
            account_id = V,
            account_type = T
        }
    } <- CF, T == Type],
    hg_ct_helper:get_account(ID).

get_adjustment_fixture(Revision) ->
    AccountRUB = hg_ct_fixture:construct_terminal_account(?cur(<<"RUB">>)),
    Globals = hg_domain:get(Revision, {globals, ?glob()}),
    [
        {globals, #domain_GlobalsObject{
            ref = ?glob(),
            data = Globals#domain_Globals{
                providers = {value, ordsets:from_list([?prv(100)])}
            }}
        },
        {provider, #domain_ProviderObject{
            ref = ?prv(100),
            data = #domain_Provider{
                name = <<"Adjustable">>,
                description = <<>>,
                abs_account = <<>>,
                terminal = {value, [?trm(100)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}}
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(100),
            data = #domain_Terminal{
                name = <<"Adjustable Terminal">>,
                description = <<>>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = get_adjustment_terminal_cashflow(initial),
                account = AccountRUB,
                risk_coverage = low
            }
        }}
    ].

get_adjustment_terminal_cashflow(initial) ->
    [
        ?cfpost(
            {provider, settlement},
            {merchant, settlement},
            ?share(1, 1, payment_amount)
        ),
        ?cfpost(
            {system, settlement},
            {provider, settlement},
            ?share(21, 1000, payment_amount)
        )
    ];
get_adjustment_terminal_cashflow(actual) ->
    [
        ?cfpost(
            {provider, settlement},
            {merchant, settlement},
            ?share(1, 1, payment_amount)
        ),
        ?cfpost(
            {system, settlement},
            {provider, settlement},
            ?share(16, 1000, payment_amount)
        ),
        ?cfpost(
            {system, settlement},
            {external, outcome},
            ?fixed(20, ?cur(<<"RUB">>))
        )
    ].

-spec invalid_payment_w_deprived_party(config()) -> _ | no_return().

invalid_payment_w_deprived_party(C) ->
    PartyID = <<"DEPRIVED ONE">>,
    ShopID = <<"TESTSHOP">>,
    RootUrl = cfg(root_url, C),
    UserInfo = make_userinfo(PartyID),
    PartyClient = hg_client_party:start(UserInfo, PartyID, hg_client_api:new(RootUrl)),
    InvoicingClient = hg_client_invoicing:start_link(UserInfo, hg_client_api:new(RootUrl)),
    ShopID = hg_ct_helper:create_party_and_shop(PartyClient),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubberduck">>, make_due_date(10), 42000),
    InvoiceID = create_invoice(InvoiceParams, InvoicingClient),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, InvoicingClient),
    PaymentParams = make_payment_params(),
    Exception = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, InvoicingClient),
    {exception, #'InvalidRequest'{}} = Exception.

-spec external_account_posting(config()) -> _ | no_return().

external_account_posting(C) ->
    PartyID = <<"LGBT">>,
    RootUrl = cfg(root_url, C),
    UserInfo = make_userinfo(PartyID),
    PartyClient = hg_client_party:start(UserInfo, PartyID, hg_client_api:new(RootUrl)),
    InvoicingClient = hg_client_invoicing:start_link(UserInfo, hg_client_api:new(RootUrl)),
    ShopID = hg_ct_helper:create_party_and_shop(PartyClient),
    ok = start_proxy(hg_dummy_provider, 1, C),
    ok = start_proxy(hg_dummy_inspector, 2, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, <<"rubbermoss">>, make_due_date(10), 42000),
    InvoiceID = create_invoice(InvoiceParams, InvoicingClient),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, InvoicingClient),
    ?payment_state(
        ?payment(PaymentID)
    ) = hg_client_invoicing:start_payment(InvoiceID, make_payment_params(), InvoicingClient),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending()), low, _, CF)),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, InvoicingClient),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(_))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?processed())),
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, InvoicingClient),
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
    _ = hg_context:set(woody_context:new()),
    #domain_ExternalAccountSet{
        accounts = #{?cur(<<"RUB">>) := #domain_ExternalAccount{outcome = AssistAccountID}}
    } = hg_domain:get(hg_domain:head(), {external_account_set, ?eas(2)}),
    hg_context:cleanup().

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(?config(root_url, C))),
    Events = hg_client_eventsink:pull_events(5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events).

%%

next_event(InvoiceID, Client) ->
    next_event(InvoiceID, 5000, Client).

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

filter_change(?payment_ev(_, ?session_ev(_, ?proxy_st_changed(_)))) ->
    false;
filter_change(?payment_ev(_, ?session_ev(_, ?session_suspended()))) ->
    false;
filter_change(?payment_ev(_, ?session_ev(_, ?session_activated()))) ->
    false;
filter_change(E) ->
    {true, E}.

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

start_proxy(Module, ProxyID, Context) ->
    start_proxy(Module, ProxyID, #{}, Context).
start_proxy(Module, ProxyID, ProxyOpts, Context) ->
    setup_proxy(start_service_handler(Module, Context, #{}), ProxyID, ProxyOpts).

setup_proxy(ProxyUrl, ProxyID, ProxyOpts) ->
    ok = hg_domain:upsert(construct_proxy(ProxyID, ProxyUrl, ProxyOpts)).

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

make_userinfo(PartyID) ->
    #payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}}.

make_invoice_params(PartyID, ShopID, Product, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Cost).

make_invoice_params(PartyID, ShopID, Product, Due, Cost) ->
    hg_ct_helper:make_invoice_params(PartyID, ShopID, Product, Due, Cost).

make_tds_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_tds_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_simple_payment_tool(),
    make_payment_params(PaymentTool, Session).

make_payment_params(PaymentTool, Session) ->
    #payproc_InvoicePaymentParams{
        payer = #domain_Payer{
            payment_tool = PaymentTool,
            session_id = Session,
            client_info = #domain_ClientInfo{},
            contact_info = #domain_ContactInfo{}
        }
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

start_invoice(Product, Due, Amount, C) ->
    start_invoice(cfg(shop_id, C), Product, Due, Amount, C).

start_invoice(ShopID, Product, Due, Amount, C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    InvoiceParams = make_invoice_params(PartyID, ShopID, Product, Due, Amount),
    InvoiceID = create_invoice(InvoiceParams, Client),
    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    InvoiceID.

process_payment(InvoiceID, PaymentParams, Client) ->
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending()))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(_)))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?processed())),
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_capture(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?captured())),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_failure(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?interaction_requested(_)))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?captured(), ?session_finished(?session_failed(Failure = ?operation_timeout())))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed(Failure)))
    ] = next_event(InvoiceID, Client),
    PaymentID.

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

assert_failed_post_request(Req) ->
    {ok, 500, _RespHeaders, _ClientRef} = post_request(Req).

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

get_post_request({'redirect', {'post_request', #'BrowserPostRequest'{uri = URL, form = Form}}}) ->
    {URL, Form}.

-spec construct_domain_fixture() -> [hg_domain:object()].

construct_domain_fixture() ->
    AccountUSD = hg_ct_fixture:construct_terminal_account(?cur(<<"USD">>)),
    AccountRUB = hg_ct_fixture:construct_terminal_account(?cur(<<"RUB">>)),
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(1)
            ])},
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(<<"DEPRIVED ONE">>, {shop_is, <<"TESTSHOP">>}),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(     1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(420000000, ?cur(<<"RUB">>))}
                    }}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                }
            ]}
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
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(1000, ?cur(<<"RUB">>))},
                        upper = {exclusive, ?cash(4200000, ?cur(<<"RUB">>))}
                    }}
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, #domain_CashRange{
                        lower = {inclusive, ?cash(      200, ?cur(<<"USD">>))},
                        upper = {exclusive, ?cash(   313370, ?cur(<<"USD">>))}
                    }}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, payment_amount)
                        )
                    ]}
                }
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
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),
        hg_ct_fixture:construct_proxy(?prx(3), <<"Merchant proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ordsets:from_list([
                    ?prv(1),
                    ?prv(2)
                ])},
                system_account_set = {value, ?sas(1)},
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
                default_contract_template = ?tmpl(2),
                common_merchant_proxy = ?prx(3), % FIXME
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, #domain_CashRange{
                                    lower = {inclusive, ?cash(        0, ?cur(<<"RUB">>))},
                                    upper = {exclusive, ?cash(   500000, ?cur(<<"RUB">>))}
                                }}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, #domain_CashRange{
                                    lower = {inclusive, ?cash(   500000, ?cur(<<"RUB">>))},
                                    upper = {exclusive, ?cash(100000000, ?cur(<<"RUB">>))}
                                }}},
                                then_ = {value, ?insp(2)}
                            }
                        ]}
                    }
                ]}
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
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?trm(1), ?trm(2), ?trm(3), ?trm(4)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = AccountUSD,
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(2),
            data = #domain_Terminal{
                name = <<"Brominal 2">>,
                description = <<"Brominal 2">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(18, 1000, payment_amount)
                    )
                ],
                account = AccountUSD,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(3),
            data = #domain_Terminal{
                name = <<"Brominal 3">>,
                description = <<"Brominal 3">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = AccountRUB,
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(4),
            data = #domain_Terminal{
                name = <<"Brominal 4">>,
                description = <<"Brominal 4">>,
                payment_method = ?pmt(bank_card, mastercard),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(19, 1000, payment_amount)
                    )
                ],
                account = AccountRUB,
                risk_coverage = low
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?trm(5), ?trm(6), ?trm(7), ?trm(8)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(5),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    )
                ],
                account = AccountRUB,
                risk_coverage = high
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(1),
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {external, outcome},
                        ?fixed(20, ?cur(<<"RUB">>)),
                        <<"Assist fee">>
                    )
                ],
                account = AccountRUB,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(7),
            data = #domain_Terminal{
                name = <<"Terminal 7">>,
                description = <<"Terminal 7">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(3),
                risk_coverage = high,
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    )
                ],
                account = AccountRUB
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(8),
            data = #domain_Terminal{
                name = <<"Terminal 8">>,
                description = <<"Terminal 8">>,
                payment_method = ?pmt(bank_card, visa),
                category = ?cat(2),
                risk_coverage = low,
                cash_flow = [
                    ?cfpost(
                        {provider, settlement},
                        {merchant, settlement},
                        ?share(1, 1, payment_amount)
                    ),
                    ?cfpost(
                        {system, settlement},
                        {provider, settlement},
                        ?share(16, 1000, payment_amount)
                    )
                ],
                account = AccountRUB
            }
        }}
    ].

