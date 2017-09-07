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
-export([invalid_invoice_template_cost/1]).
-export([invalid_invoice_template_id/1]).
-export([invoice_w_template/1]).
-export([invoice_cancellation/1]).
-export([overdue_invoice_cancellation/1]).
-export([invoice_cancellation_after_payment_timeout/1]).
-export([invalid_payment_amount/1]).
-export([payment_success/1]).
-export([payment_w_terminal_success/1]).
-export([payment_success_on_second_try/1]).
-export([payment_fail_after_silent_callback/1]).
-export([invoice_success_on_third_payment/1]).
-export([payment_risk_score_check/1]).
-export([invalid_payment_adjustment/1]).
-export([payment_adjustment_success/1]).
-export([invalid_payment_w_deprived_party/1]).
-export([external_account_posting/1]).
-export([payment_hold_cancellation/1]).
-export([payment_hold_auto_cancellation/1]).
-export([payment_hold_capturing/1]).
-export([payment_hold_auto_capturing/1]).
-export([payment_refund_success/1]).
-export([consistent_history/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.


%% tests descriptions

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [test_case_name()].

all() ->
    [
        invalid_invoice_shop,
        invalid_invoice_amount,
        invalid_invoice_currency,
        invalid_party_status,
        invalid_shop_status,
        invalid_invoice_template_cost,
        invalid_invoice_template_id,
        invoice_w_template,
        invoice_cancellation,
        overdue_invoice_cancellation,
        invoice_cancellation_after_payment_timeout,
        invalid_payment_amount,
        payment_success,
        payment_w_terminal_success,
        payment_success_on_second_try,
        payment_fail_after_silent_callback,
        invoice_success_on_third_payment,

        payment_risk_score_check,

        invalid_payment_adjustment,
        payment_adjustment_success,

        invalid_payment_w_deprived_party,
        external_account_posting,

        payment_hold_cancellation,
        payment_hold_auto_cancellation,
        payment_hold_capturing,
        payment_hold_auto_capturing,

        payment_refund_success,

        consistent_history
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_invoice_payment', 'merge_change', '_'}, x),
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

-define(ordset(Es), ordsets:from_list(Es)).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(payment_adjustment_success, C) ->
    Revision = hg_domain:head(),
    ok = hg_domain:upsert(get_adjustment_fixture(Revision)),
    [{original_domain_revision, Revision} | init_per_testcase(C)];
init_per_testcase(_Name, C) ->
    init_per_testcase(C).

init_per_testcase(C) ->
    PartyID = cfg(party_id, C),
    UserInfo = make_userinfo(PartyID),
    ApiClient = hg_client_api:new(cfg(root_url, C)),
    Client = hg_client_invoicing:start_link(UserInfo, ApiClient),
    ClientTpl = hg_client_invoice_templating:start_link(UserInfo, ApiClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    [{client, Client}, {client_tpl, ClientTpl}, {test_sup, SupPid} | C].

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

-spec invalid_shop_status(config()) -> _ | no_return().

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
    _ = update_invoice_tpl(TplID, #{cost => Cost2}, C),
    Params2 = make_invoice_params_tpl(TplID, make_cash(50, <<"RUB">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_COST]
    }} = hg_client_invoicing:create_with_tpl(Params2, Client),
    Params3 = make_invoice_params_tpl(TplID, make_cash(100, <<"KEK">>)),
    {exception, #'InvalidRequest'{
        errors = [?INVOICE_TPL_BAD_COST]
    }} = hg_client_invoicing:create_with_tpl(Params3, Client),

    Cost3 = make_tpl_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"RUB">>}),
    _ = update_invoice_tpl(TplID, #{cost => Cost3}, C),
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

-spec invoice_w_template(config()) -> _ | no_return().

invoice_w_template(C) ->
    Client = cfg(client, C),
    TplCost1 = {_, FixedCost} = make_tpl_cost(fixed, 10000, <<"RUB">>),
    TplContext1 = make_invoice_context(<<"default context">>),
    TplID = create_invoice_tpl(C, TplCost1, TplContext1),
    #domain_InvoiceTemplate{
        owner_id = TplPartyID,
        shop_id  = TplShopID,
        details  = TplDetails,
        cost     = TplCost1,
        context  = TplContext1
    } = get_invoice_tpl(TplID, C),
    InvoiceCost1 = FixedCost,
    InvoiceContext1 = make_invoice_context(<<"invoice specific context">>),

    Params1 = make_invoice_params_tpl(TplID, InvoiceCost1, InvoiceContext1),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        details     = TplDetails,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client),

    Params2 = make_invoice_params_tpl(TplID),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        details     = TplDetails,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = TplContext1
    }) = hg_client_invoicing:create_with_tpl(Params2, Client),

    TplCost2 = make_tpl_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"RUB">>}),
    _ = update_invoice_tpl(TplID, #{cost => TplCost2}, C),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        details     = TplDetails,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client),

    TplCost3 = make_tpl_cost(unlim, sale, "146%"),
    _ = update_invoice_tpl(TplID, #{cost => TplCost3}, C),
    ?invoice_state(#domain_Invoice{
        owner_id    = TplPartyID,
        shop_id     = TplShopID,
        details     = TplDetails,
        template_id = TplID,
        cost        = InvoiceCost1,
        context     = InvoiceContext1
    }) = hg_client_invoicing:create_with_tpl(Params1, Client).

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
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberdusk">>, make_due_date(3), 1000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
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
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_w_terminal_success(config()) -> _ | no_return().

payment_w_terminal_success(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberruble">>, make_due_date(10), 42000, C),
    PaymentParams = make_terminal_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec payment_success_on_second_try(config()) -> _ | no_return().

payment_success_on_second_try(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
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
    %% make noop callback call
    _ = assert_success_post_request({URL, hg_dummy_provider:construct_silent_callback(GoodForm)}),
    %% ensure that suspend is still holding up
    _ = assert_success_post_request({URL, GoodForm}),
    PaymentID = await_payment_capture_finish(InvoiceID, PaymentID, Client).

-spec payment_fail_after_silent_callback(config()) -> _ | no_return().

payment_fail_after_silent_callback(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberdick">>, make_due_date(20), 42000, C),
    PaymentID = process_payment(InvoiceID, make_tds_payment_params(), Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?captured(), ?interaction_requested(UserInteraction))
        )
    ] = next_event(InvoiceID, Client),
    {URL, Form} = get_post_request(UserInteraction),
    _ = assert_success_post_request({URL, hg_dummy_provider:construct_silent_callback(Form)}),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?captured(), ?session_finished(?session_failed(Failure = ?operation_timeout())))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed(Failure)))
    ] = next_event(InvoiceID, Client).

-spec invoice_success_on_third_payment(config()) -> _ | no_return().

invoice_success_on_third_payment(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
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
        ?payment_ev(PaymentID3, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(
            PaymentID3,
            ?session_ev(_, ?interaction_requested(UserInteraction))
        )
    ] = next_event(InvoiceID, Client),
    GoodPost = get_post_request(UserInteraction),
    %% simulate user interaction FTW!
    _ = assert_success_post_request(GoodPost),
    PaymentID3 = await_payment_capture_finish(InvoiceID, PaymentID3, Client).

%% @TODO modify this test by failures of inspector in case of wrong terminal choice
-spec payment_risk_score_check(config()) -> _ | no_return().

payment_risk_score_check(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
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
        ?payment_ev(PaymentID1, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID1, Client),
    PaymentID1 = await_payment_capture(InvoiceID1, PaymentID1, Client),
    % Invoice w/ 500000 < cost < 100000000
    InvoiceID2 = start_invoice(<<"rubberbucks">>, make_due_date(10), 31337000, C),
    ?payment_state(?payment(PaymentID2)) = hg_client_invoicing:start_payment(InvoiceID2, PaymentParams, Client),
    [
        ?payment_ev(PaymentID2, ?payment_started(?payment_w_status(?pending()), high, _, _)),
        ?payment_ev(PaymentID1, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID2, Client),
    [
        ?payment_ev(PaymentID2, ?session_ev(?processed(), ?trx_bound(_))),
        ?payment_ev(PaymentID2, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID2, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID2, Client),
    PaymentID2 = await_payment_capture(InvoiceID2, PaymentID2, Client),
    % Invoice w/ 100000000 =< cost
    InvoiceID3 = start_invoice(<<"rubbersocks">>, make_due_date(10), 100000000, C),
    Exception = hg_client_invoicing:start_payment(InvoiceID3, PaymentParams, Client),
    {exception, #'InvalidRequest'{errors = [<<"Fatal error">>]}} = Exception.

-spec invalid_payment_adjustment(config()) -> _ | no_return().

invalid_payment_adjustment(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 100000, C),
    %% start a smoker's payment
    PaymentParams = make_tds_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    %% no way to create adjustment for a payment not yet finished
    ?invalid_payment_status(?processed()) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client),
    PaymentID = await_payment_failure_finish(InvoiceID, PaymentID, Client),
    %% no way to create adjustment for a failed payment
    ?invalid_payment_status(?failed(_)) =
        hg_client_invoicing:create_adjustment(InvoiceID, PaymentID, make_adjustment_params(), Client).

-spec payment_adjustment_success(config()) -> _ | no_return().

payment_adjustment_success(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
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
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    PrvAccount1 = get_cashflow_account({provider, settlement}, CF1),
    SysAccount1 = get_cashflow_account({system, settlement}, CF1),
    %% update terminal cashflow
    ProviderRef = ?prv(100),
    Provider = hg_domain:get(hg_domain:head(), {provider, ProviderRef}),
    ProviderTerms = Provider#domain_Provider.terms,
    ok = hg_domain:upsert(
        {provider, #domain_ProviderObject{
            ref = ProviderRef,
            data = Provider#domain_Provider{
                terms = ProviderTerms#domain_PaymentsProvisionTerms{
                    cash_flow = {value,
                        get_adjustment_provider_cashflow(actual)
                    }
                }
            }
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
    Globals = hg_domain:get(Revision, {globals, ?glob()}),
    [
        {globals, #domain_GlobalsObject{
            ref = ?glob(),
            data = Globals#domain_Globals{
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
                terminal = {value, [?trm(100)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_PaymentsProvisionTerms{
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
            ?share(1, 1, payment_amount)
        ),
        ?cfpost(
            {system, settlement},
            {provider, settlement},
            ?share(21, 1000, payment_amount)
        )
    ];
get_adjustment_provider_cashflow(actual) ->
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
            ?fixed(20, <<"RUB">>)
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
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
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
    _ = hg_ct_helper:create_party_and_shop(PartyClient),
    ShopID = hg_ct_helper:create_battle_ready_shop(?cat(2), ?tmpl(2), PartyClient),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
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
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
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

-spec payment_refund_success(config()) -> _ | no_return().

payment_refund_success(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    RefundParams = make_refund_params(),
    ?invalid_payment_status(?processed()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    Refund = #domain_InvoicePaymentRefund{id = RefundID} =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client),
    Refund =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?refund_created(Refund, _))),
        ?payment_ev(PaymentID, ?refund_ev(RefundID, ?session_ev(?refunded(), ?session_started())))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?trx_bound(_)))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded())))),
        ?payment_ev(PaymentID, ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))),
        ?payment_ev(ID, ?payment_status_changed(?refunded()))
    ] = next_event(InvoiceID, Client),
    #domain_InvoicePaymentRefund{status = ?refund_succeeded()} =
        hg_client_invoicing:get_payment_refund(InvoiceID, PaymentID, RefundID, Client),
    ?invalid_payment_status(?refunded()) =
        hg_client_invoicing:refund_payment(InvoiceID, PaymentID, RefundParams, Client).

%%

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Client = hg_client_eventsink:start_link(hg_client_api:new(cfg(root_url, C))),
    Events = hg_client_eventsink:pull_events(5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events).

-spec payment_hold_cancellation(config()) -> _ | no_return().

payment_hold_cancellation(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(5), 10000, C),
    PaymentParams = make_payment_params({hold, capture}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    ok = hg_client_invoicing:cancel_payment(InvoiceID, PaymentID, <<"whynot">>, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled_with_reason(<<"whynot">>), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(
            PaymentID,
            ?session_ev(?cancelled_with_reason(<<"whynot">>), ?session_finished(?session_succeeded()))
        ),
        ?payment_ev(PaymentID, ?payment_status_changed(?cancelled_with_reason(<<"whynot">>)))
    ] = next_event(InvoiceID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_unpaid()),
        [?payment_state(?payment_w_status(PaymentID, ?cancelled()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec payment_hold_auto_cancellation(config()) -> _ | no_return().

payment_hold_auto_cancellation(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(5), 10000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?cancelled()))
    ] = next_event(InvoiceID, 3000, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_unpaid()),
        [?payment_state(?payment_w_status(PaymentID, ?cancelled()))]
    ) = hg_client_invoicing:get(InvoiceID, Client),
    [?invoice_status_changed(?invoice_cancelled(<<"overdue">>))] = next_event(InvoiceID, Client).

-spec payment_hold_capturing(config()) -> _ | no_return().

payment_hold_capturing(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, cancel}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    ok = hg_client_invoicing:capture_payment(InvoiceID, PaymentID, <<"ok">>, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured_with_reason(<<"ok">>), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured_with_reason(<<"ok">>), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?captured_with_reason(<<"ok">>))),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, Client).

-spec payment_hold_auto_capturing(config()) -> _ | no_return().

payment_hold_auto_capturing(C) ->
    Client = cfg(client, C),
    ok = start_proxies([{hg_dummy_provider, 1, C}, {hg_dummy_inspector, 2, C}]),
    InvoiceID = start_invoice(<<"rubberduck">>, make_due_date(10), 42000, C),
    PaymentParams = make_payment_params({hold, capture}),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?captured())),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, 3000, Client).

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

filter_change(?payment_ev(_, C)) ->
    filter_change(C);
filter_change(?refund_ev(_, C)) ->
    filter_change(C);
filter_change(?session_ev(_, ?proxy_st_changed(_))) ->
    false;
filter_change(?session_ev(_, ?session_suspended())) ->
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
    hg_ct_helper:make_userinfo(PartyID).

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
    create_invoice_tpl_(Config, []).

create_invoice_tpl(Config, Cost, Context) ->
    Lifetime = hg_ct_helper:make_lifetime(0, 1, 0),
    create_invoice_tpl_(Config, [Lifetime, Cost, Context]).

create_invoice_tpl_(Config, AdditionalParams) ->
    Client = cfg(client_tpl, Config),
    PartyID = cfg(party_id, Config),
    ShopID = cfg(shop_id, Config),
    Product = <<"rubberduck">>,
    Params = erlang:apply(hg_ct_helper, make_invoice_tpl_create_params, [PartyID, ShopID, Product | AdditionalParams]),
    #domain_InvoiceTemplate{id = TplID} = hg_client_invoice_templating:create(Params, Client),
    TplID.

get_invoice_tpl(TplID, Config) ->
    Client = cfg(client_tpl, Config),
    hg_client_invoice_templating:get(TplID, Client).

update_invoice_tpl(TplID, Diff, Config) ->
    Client = cfg(client_tpl, Config),
    Params = hg_ct_helper:make_invoice_tpl_update_params(Diff),
    hg_client_invoice_templating:update(TplID, Params, Client).

delete_invoice_tpl(TplID, Config) ->
    Client = cfg(client_tpl, Config),
    hg_client_invoice_templating:delete(TplID, Client).

make_terminal_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_terminal_payment_tool(),
    make_payment_params(PaymentTool, Session, instant).

make_tds_payment_params() ->
    {PaymentTool, Session} = hg_ct_helper:make_tds_payment_tool(),
    make_payment_params(PaymentTool, Session, instant).

make_payment_params() ->
    make_payment_params(instant).

make_payment_params(FlowType) ->
    {PaymentTool, Session} = hg_ct_helper:make_simple_payment_tool(),
    make_payment_params(PaymentTool, Session, FlowType).

make_payment_params(PaymentTool, Session, FlowType) ->
    Flow = case FlowType of
        instant ->
            {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
        {hold, OnHoldExpiration} ->
            {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
    end,
    #payproc_InvoicePaymentParams{
        payer = #domain_Payer{
            payment_tool = PaymentTool,
            session_id = Session,
            client_info = #domain_ClientInfo{},
            contact_info = #domain_ContactInfo{}
        },
        flow = Flow
    }.

make_refund_params() ->
    #payproc_InvoicePaymentRefundParams{
        reason = <<"ZANOZED">>
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
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_capture(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_payment_capture_finish(InvoiceID, PaymentID, Client).

await_payment_capture_finish(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?captured())),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_failure(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_payment_failure_finish(InvoiceID, PaymentID, Client).

await_payment_failure_finish(InvoiceID, PaymentID, Client) ->
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
                    if_   = ?partycond(<<"DEPRIVED ONE">>, {shop_is, <<"TESTSHOP">>}),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(payment_terminal, euroset)
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
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
                            ?share(45, 1000, payment_amount)
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
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?fixed(100, <<"RUB">>)
                    )
                ]}
            }
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
                ?cat(3)
            ])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
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
                ]}
            }
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
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),
        hg_ct_fixture:construct_proxy(?prx(3), <<"Merchant proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),
        hg_ct_fixture:construct_inspector(?insp(3), <<"Fatalist">>, ?prx(2), #{<<"risk_score">> => <<"fatal">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3)
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
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?trm(1)
                ])},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, {payment_system_is, visa}}}},
                            then_ = {value, [
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
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, {payment_system_is, mastercard}}}},
                            then_ = {value, [
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
                            ]}
                        }
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, {payment_system_is, visa}}}},
                                then_ = {value, ?hold_lifetime(5)}
                            }
                        ]}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, payment_amount)
                            )
                        ]}
                    }
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
                terminal = {value, [?trm(6), ?trm(7)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2)
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
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, payment_amount)
                        )
                    ]}
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
                risk_coverage = high
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = #domain_Provider{
                name = <<"Crovider">>,
                description = <<"Payment terminal provider">>,
                terminal = {value, [?trm(10)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"crovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
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
        }}
    ].
%