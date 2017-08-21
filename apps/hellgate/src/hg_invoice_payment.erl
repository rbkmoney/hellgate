%%% Invoice payment submachine
%%%
%%% TODO
%%%  - make proper submachine interface
%%%     - `init` / `start_session` should provide `next` or `done` to the caller
%%%  - distinguish between different error classes:
%%%     - regular operation error
%%%     - callback timeout
%%%     - internal error ?
%%%  - handle idempotent callbacks uniformly
%%%     - get rid of matches against session status
%%%  - tag machine with the provider trx
%%%     - distinguish between trx tags and callback tags
%%%     - tag namespaces
%%%  - clean the mess with error handling
%%%     - abuse transient error passthrough
%%%  - think about safe clamping of timers returned by some proxy
%%%  - why don't user interaction events imprint anything on the state?
%%%  - adjustments look and behave very much like claims over payments

-module(hg_invoice_payment).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).
-export([get_adjustments/1]).
-export([get_adjustment/2]).

%% Machine like

-export([init/3]).

-export([process_signal/3]).
-export([process_call/3]).

-export([start_session/1]).

-export([create_adjustment/3]).
-export([capture_adjustment/3]).
-export([cancel_adjustment/3]).

-export([capture/2]).
-export([cancel/2]).

-export([merge_change/2]).

-export([get_log_params/2]).

-export([marshal/1]).
-export([unmarshal/1]).

%%

-record(st, {
    payment          :: undefined | payment(),
    risk_score       :: undefined | risk_score(),
    route            :: undefined | route(),
    cash_flow        :: undefined | cash_flow(),
    trx              :: undefined | trx_info(),
    target           :: undefined | target(),
    sessions = #{}   :: #{target() => session()},
    adjustments = [] :: [adjustment()]
}).

-type st() :: #st{}.
-export_type([st/0]).

-type party()             :: dmsl_domain_thrift:'Party'().
-type invoice()           :: dmsl_domain_thrift:'Invoice'().
-type payment()           :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()        :: dmsl_domain_thrift:'InvoicePaymentID'().
-type adjustment()        :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()     :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params() :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type target()            :: dmsl_domain_thrift:'TargetInvoicePaymentStatus'().
-type risk_score()        :: dmsl_domain_thrift:'RiskScore'().
-type route()             :: dmsl_domain_thrift:'InvoicePaymentRoute'().
-type cash_flow()         :: dmsl_domain_thrift:'FinalCashFlow'().
-type trx_info()          :: dmsl_domain_thrift:'TransactionInfo'().
-type proxy_state()       :: dmsl_proxy_thrift:'ProxyState'().

-type session() :: #{
    target      := target(),
    status      := active | suspended | finished,
    trx         := trx_info(),
    proxy_state => proxy_state()
}.

%%

-include("domain.hrl").
-include("payment_events.hrl").

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().

%%

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

-spec get_adjustments(st()) -> [adjustment()].

get_adjustments(#st{adjustments = As}) ->
    As.

-spec get_adjustment(adjustment_id(), st()) -> adjustment() | no_return().

get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoicePaymentAdjustment.id, As) of
        Adjustment = #domain_InvoicePaymentAdjustment{} ->
            Adjustment;
        false ->
            throw(#payproc_InvoicePaymentAdjustmentNotFound{})
    end.

%%

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

-spec init(payment_id(), _, opts()) ->
    {payment(), hg_machine:result()}.

init(PaymentID, PaymentParams, Opts) ->
    hg_log_scope:scope(
        payment,
        fun() -> init_(PaymentID, PaymentParams, Opts) end,
        #{
            id => PaymentID
        }
    ).

-spec init_(payment_id(), _, opts()) ->
    {st(), hg_machine:result()}.

init_(PaymentID, PaymentParams, #{party := Party} = Opts) ->
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Revision = hg_domain:head(),
    PaymentTerms = get_merchant_payment_terms(Opts),
    VS0 = collect_varset(Party, Shop, #{}),
    VS1 = validate_payment_params(PaymentParams, {Revision, PaymentTerms}, VS0),
    VS2 = validate_payment_cost(Invoice, {Revision, PaymentTerms}, VS1),
    VS3 = validate_payment_flow(PaymentParams, PaymentTerms, VS2),
    CreatedAt = hg_datetime:format_now(),
    Flow = validate_flow(PaymentParams, PaymentTerms, CreatedAt),
    Payment = construct_payment(PaymentID, Invoice, Flow, PaymentParams, CreatedAt, Revision),
    {RiskScore, VS4} = inspect(Shop, Invoice, Payment, VS3),
    Route = validate_route(Payment, hg_routing:choose(VS4, Revision)),
    FinalCashflow = construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS4, Revision),
    _AccountsState = hg_accounting:plan(
        construct_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Events = [?payment_started(Payment, RiskScore, Route, FinalCashflow)],
    {collapse_changes(Events), {Events, hg_machine_action:new()}}.

get_merchant_payment_terms(Opts) ->
    Invoice = get_invoice(Opts),
    hg_party:get_payments_service_terms(
        get_shop_id(Invoice),
        get_party(Opts),
        get_created_at(Invoice)
    ).

construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS, Revision) ->
    hg_cashflow:finalize(
        collect_cash_flow({Revision, PaymentTerms}, Route, VS),
        collect_cash_flow_context(Invoice, Payment),
        collect_account_map(Invoice, Shop, Route, VS, Revision)
    ).

construct_payment(PaymentID, Invoice, Flow, PaymentParams, CreatedAt, Revision) ->
    #domain_InvoicePayment{
        id              = PaymentID,
        created_at      = CreatedAt,
        domain_revision = Revision,
        status          = ?pending(),
        cost            = Invoice#domain_Invoice.cost,
        payer           = PaymentParams#payproc_InvoicePaymentParams.payer,
        flow            = Flow
    }.

validate_payment_params(
    #payproc_InvoicePaymentParams{payer = #domain_Payer{payment_tool = PaymentTool}},
    Terms,
    VS
) ->
    VS1 = validate_payment_tool(PaymentTool, Terms, VS),
    VS1#{payment_tool => PaymentTool}.

validate_payment_tool(
    PaymentTool,
    {Revision, #domain_PaymentsServiceTerms{payment_methods = PaymentMethodSelector}},
    VS
) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    VS.

validate_payment_cost(
    #domain_Invoice{cost = Cash},
    {Revision, #domain_PaymentsServiceTerms{cash_limit = LimitSelector}},
    VS
) ->
    Limit = reduce_selector(cash_limit, LimitSelector, VS, Revision),
    ok = validate_limit(Cash, Limit),
    VS#{cost => Cash}.

validate_limit(Cash, CashRange) ->
    case hg_condition:test_cash_range(Cash, CashRange) of
        within ->
            ok;
        {exceeds, lower} ->
            throw_invalid_request(<<"Invalid amount, less than allowed minumum">>);
        {exceeds, upper} ->
            throw_invalid_request(<<"Invalid amount, more than allowed maximum">>)
    end.

validate_route(_Payment, Route = #domain_InvoicePaymentRoute{}) ->
    Route;
validate_route(Payment, undefined) ->
    error({misconfiguration, {'No route found for a payment', Payment}}).

validate_payment_flow(
    #payproc_InvoicePaymentParams{flow = {Type, _}},
    Terms,
    VS
) ->
    PaymentFlow = case Type of
        instant ->
            instant;
        hold ->
            {hold, validate_hold_lifetime(Terms)}
    end,
    VS#{payment_flow => PaymentFlow}.

validate_flow(PaymentParams, PaymentTerms, CreatedAt) ->
    case PaymentParams#payproc_InvoicePaymentParams.flow of
        {instant, _} ->
            ?invoice_payment_flow_instant();
        {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}} ->
            ?hold_lifetime(HoldLifetime) = validate_hold_lifetime(PaymentTerms),
            HeldUntil = hg_datetime:format_ts(hg_datetime:parse_ts(CreatedAt) + HoldLifetime),
            ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)
    end.

validate_hold_lifetime(#domain_PaymentsServiceTerms{hold_lifetime = undefined}) ->
    throw_invalid_request(<<"Holds are not available">>);
validate_hold_lifetime(#domain_PaymentsServiceTerms{hold_lifetime = HoldLifetime}) ->
    HoldLifetime.

collect_varset(Party, Shop = #domain_Shop{
    category = Category,
    account = #domain_ShopAccount{currency = Currency}
}, VS) ->
    VS#{
        party    => Party,
        shop     => Shop,
        category => Category,
        currency => Currency
    }.

collect_varset(Party, Shop, #domain_InvoicePayment{
    cost = Cash,
    payer = #domain_Payer{payment_tool = PaymentTool}
}, VS) ->
    VS0 = collect_varset(Party, Shop, VS),
    VS0#{
        cost         => Cash,
        payment_tool => PaymentTool
    }.

%%

collect_cash_flow(
    {Revision, #domain_PaymentsServiceTerms{fees = MerchantCashFlowSelector}},
    #domain_InvoicePaymentRoute{terminal = TerminalRef},
    VS
) ->
    #domain_Terminal{cash_flow = ProviderCashFlow} = hg_domain:get(Revision, {terminal, TerminalRef}),
    MerchantCashFlow = reduce_selector(merchant_cash_flow, MerchantCashFlowSelector, VS, Revision),
    MerchantCashFlow ++ ProviderCashFlow.

collect_cash_flow_context(
    #domain_Invoice{cost = InvoiceCost},
    #domain_InvoicePayment{cost = PaymentCost}
) ->
    #{
        invoice_amount => InvoiceCost,
        payment_amount => PaymentCost
    }.

collect_account_map(Invoice, Shop, Route, VS, Revision) ->
    TerminalRef = Route#domain_InvoicePaymentRoute.terminal,
    #domain_Terminal{account = ProviderAccount} = hg_domain:get(Revision, {terminal, TerminalRef}),
    #domain_Shop{account = MerchantAccount} = Shop,
    Currency = get_invoice_currency(Invoice),
    SystemAccount = choose_system_account(Currency, VS, Revision),
    M = #{
        {merchant , settlement} => MerchantAccount#domain_ShopAccount.settlement     ,
        {merchant , guarantee } => MerchantAccount#domain_ShopAccount.guarantee      ,
        {provider , settlement} => ProviderAccount#domain_TerminalAccount.settlement ,
        {system   , settlement} => SystemAccount#domain_SystemAccount.settlement
    },
    % External account probably can be optional for some payments
    case choose_external_account(Currency, VS, Revision) of
        #domain_ExternalAccount{income = Income, outcome = Outcome} ->
            M#{
                {external, income} => Income,
                {external, outcome} => Outcome
            };
        undefined ->
            M
    end.

choose_system_account(Currency, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    SystemAccountSetSelector = Globals#domain_Globals.system_account_set,
    SystemAccountSetRef = reduce_selector(system_account_set, SystemAccountSetSelector, VS, Revision),
    SystemAccountSet = hg_domain:get(Revision, {system_account_set, SystemAccountSetRef}),
    choose_account(
        system,
        Currency,
        SystemAccountSet#domain_SystemAccountSet.accounts
    ).

choose_external_account(Currency, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    ExternalAccountSetSelector = Globals#domain_Globals.external_account_set,
    case hg_selector:reduce(ExternalAccountSetSelector, VS, Revision) of
        {value, ExternalAccountSetRef} ->
            ExternalAccountSet = hg_domain:get(Revision, {external_account_set, ExternalAccountSetRef}),
            genlib_map:get(
                Currency,
                ExternalAccountSet#domain_ExternalAccountSet.accounts
            );
        _ ->
            undefined
    end.

choose_account(Name, Currency, Accounts) ->
    case maps:find(Currency, Accounts) of
        {ok, Account} ->
            Account;
        error ->
            error({misconfiguration, {'No account for a given currency', {Name, Currency}}})
    end.

construct_plan_id(
    #domain_Invoice{id = InvoiceID},
    #domain_InvoicePayment{id = PaymentID}
) ->
    <<InvoiceID/binary, ".", PaymentID/binary>>.

reduce_selector(Name, Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

%%

-spec start_session(target()) ->
    {ok, hg_machine:result()}.

start_session(Target) ->
    Events = [?session_ev(Target, ?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

-spec capture(st(), atom()) -> {ok, hg_machine:result()}.

capture(St, Reason) ->
    do_payment(St, ?captured_with_reason(hg_utils:format_reason(Reason))).

-spec cancel(st(), atom()) -> {ok, hg_machine:result()}.

cancel(St, Reason) ->
    do_payment(St, ?cancelled_with_reason(hg_utils:format_reason(Reason))).

do_payment(St, Target) ->
    Payment = get_payment(St),
    _ = assert_payment_status(processed, Payment),
    _ = assert_payment_flow(hold, Payment),
    start_session(Target).

-spec create_adjustment(adjustment_params(), st(), opts()) ->
    {adjustment(), hg_machine:result()}.

create_adjustment(Params, St, Opts) ->
    Payment = get_payment(St),
    Revision = get_adjustment_revision(Params),
    CashflowWas = get_cashflow(St),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_adjustment_pending(St),
    Party = get_party(Opts),
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    PaymentTerms = get_merchant_payment_terms(Opts),
    Route = get_route(St),
    VS = collect_varset(Party, Shop, Payment, #{}),
    Cashflow = construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS, Revision),
    ID = construct_adjustment_id(St),
    Adjustment = #domain_InvoicePaymentAdjustment{
        id                    = ID,
        status                = ?adjustment_pending(),
        created_at            = hg_datetime:format_now(),
        domain_revision       = Revision,
        reason                = Params#payproc_InvoicePaymentAdjustmentParams.reason,
        old_cash_flow_inverse = hg_cashflow:revert(CashflowWas),
        new_cash_flow         = Cashflow
    },
    _AccountsState = prepare_adjustment_cashflow(Adjustment, St, Opts),
    Event = ?adjustment_ev(ID, ?adjustment_created(Adjustment)),
    {Adjustment, {[Event], hg_machine_action:new()}}.

get_adjustment_revision(Params) ->
    hg_utils:select_defined(
        Params#payproc_InvoicePaymentAdjustmentParams.domain_revision,
        hg_domain:head()
    ).

construct_adjustment_id(#st{adjustments = As}) ->
    integer_to_binary(length(As) + 1).

assert_payment_status(Status, #domain_InvoicePayment{status = {Status, _}}) ->
    ok;
assert_payment_status(_, #domain_InvoicePayment{status = Status}) ->
    throw(#payproc_InvalidPaymentStatus{status = Status}).

assert_no_adjustment_pending(#st{adjustments = [Adjustment | _]}) ->
    assert_adjustment_finalized(Adjustment);
assert_no_adjustment_pending(#st{adjustments = []}) ->
    ok.

assert_adjustment_finalized(#domain_InvoicePaymentAdjustment{id = ID, status = {pending, _}}) ->
    throw(#payproc_InvoicePaymentAdjustmentPending{id = ID});
assert_adjustment_finalized(_) ->
    ok.

assert_payment_flow(hold, #domain_InvoicePayment{flow = ?invoice_payment_flow_hold(_, _)}) ->
    ok;
assert_payment_flow(_, _) ->
    throw(#payproc_InvalidOperation{}).

-spec capture_adjustment(adjustment_id(), st(), opts()) ->
    {ok, hg_machine:result()}.

capture_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, capture, St, Options).

-spec cancel_adjustment(adjustment_id(), st(), opts()) ->
    {ok, hg_machine:result()}.

cancel_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, cancel, St, Options).

finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(pending, Adjustment),
    _AccountsState = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
    Status = case Intent of
        capture ->
            ?adjustment_captured(hg_datetime:format_now());
        cancel ->
            ?adjustment_cancelled(hg_datetime:format_now())
    end,
    Event = ?adjustment_ev(ID, ?adjustment_status_changed(Status)),
    {ok, {[Event], hg_machine_action:new()}}.

prepare_adjustment_cashflow(Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    hg_accounting:plan(PlanID, Plan).

finalize_adjustment_cashflow(Intent, Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    case Intent of
        capture ->
            hg_accounting:commit(PlanID, Plan);
        cancel ->
            hg_accounting:rollback(PlanID, Plan)
    end.

get_adjustment_cashflow_plan(#domain_InvoicePaymentAdjustment{
    old_cash_flow_inverse = CashflowInverse,
    new_cash_flow         = Cashflow
}) ->
    [
        {1, CashflowInverse},
        {2, Cashflow}
    ].

assert_adjustment_status(Status, #domain_InvoicePaymentAdjustment{status = {Status, _}}) ->
    ok;
assert_adjustment_status(_, #domain_InvoicePaymentAdjustment{status = Status}) ->
    throw(#payproc_InvalidPaymentAdjustmentStatus{status = Status}).

construct_adjustment_plan_id(Adjustment, St, Options) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(Options)),
        get_payment_id(St),
        {adj, get_adjustment_id(Adjustment)}
    ]).

get_adjustment_id(#domain_InvoicePaymentAdjustment{id = ID}) ->
    ID.

get_adjustment_cashflow(#domain_InvoicePaymentAdjustment{new_cash_flow = Cashflow}) ->
    Cashflow.

%%

-spec process_signal(timeout, st(), opts()) ->
    {next | done, hg_machine:result()}.

process_signal(timeout, St, Options) ->
    hg_log_scope:scope(
        payment,
        fun() -> process_timeout(St, Options) end,
        get_st_meta(St)
    ).

process_timeout(St, Options) ->
    case get_target_session_status(St) of
        active ->
            Action = hg_machine_action:new(),
            process(Action, St, Options);
        suspended ->
            Action = hg_machine_action:new(),
            process_callback_timeout(Action, St, Options);
        finished ->
            process_finished_session(St)
    end.

-spec process_call({callback, _}, st(), opts()) ->
    {_, {next | done, hg_machine:result()}}. % FIXME

process_call({callback, Payload}, St, Options) ->
    hg_log_scope:scope(
        payment,
        fun() -> process_callback(Payload, St, Options) end,
        get_st_meta(St)
    ).

process_callback(Payload, St, Options) ->
    case get_target_session_status(St) of
        suspended ->
            Action = hg_machine_action:unset_timer(),
            handle_callback(Payload, Action, St, Options);
        active ->
            % there's ultimately no way how we could end up here
            error(invalid_session_status)
    end.

process_callback_timeout(Action, St, Options) ->
    Session = get_target_session(St),
    Result = handle_proxy_callback_timeout(Action, Session),
    finish_processing(Result, St, Options).

process_finished_session(St) ->
    Payment = get_payment(St),
    Target = case Payment#domain_InvoicePayment.flow of
        ?invoice_payment_flow_instant() ->
            ?captured();
        ?invoice_payment_flow_hold(OnHoldExpiration, _) ->
            case OnHoldExpiration of
                cancel ->
                    ?cancelled();
                capture ->
                    ?captured()
            end
    end,
    {ok, Result} = start_session(Target),
    {done, Result}.

process(Action0, St, Options) ->
    Session = get_target_session(St),
    ProxyContext = construct_proxy_context(Session, St, Options),
    {ok, ProxyResult} = issue_process_call(ProxyContext, St, Options),
    Result = handle_proxy_result(ProxyResult, Action0, Session),
    finish_processing(Result, St, Options).

handle_callback(Payload, Action0, St, Options) ->
    Session = get_target_session(St),
    ProxyContext = construct_proxy_context(Session, St, Options),
    {ok, CallbackResult} = issue_callback_call(Payload, ProxyContext, St, Options),
    {Response, Result} = handle_callback_result(CallbackResult, Action0, get_target_session(St)),
    {Response, finish_processing(Result, St, Options)}.

finish_processing({Events, Action}, St, Options) ->
    St1 = collapse_changes(Events, St),
    case get_target_session(St1) of
        #{status := finished, result := ?session_succeeded(), target := Target} ->
            case Target of
                {captured, _} ->
                    _AccountsState = commit_plan(St, Options);
                {cancelled, _} ->
                    _AccountsState = rollback_plan(St, Options);
                {processed, _} ->
                    ok
            end,
            NewAction = get_action(Target, Action, St),
            {done, {Events ++ [?payment_status_changed(Target)], NewAction}};
        #{status := finished, result := ?session_failed(Failure)} ->
            % TODO is it always rollback?
            _AccountsState = rollback_plan(St, Options),
            {done, {Events ++ [?payment_status_changed(?failed(Failure))], Action}};
        #{} ->
            {next, {Events, Action}}
    end.

get_action({processed, _}, Action, St) ->
    #domain_InvoicePayment{flow = Flow} = get_payment(St),
    case Flow of
        ?invoice_payment_flow_instant() ->
            hg_machine_action:set_timeout(0, Action);
        ?invoice_payment_flow_hold(_, HeldUntil) ->
            hg_machine_action:set_deadline(HeldUntil, Action)
    end;
get_action(_, Action, _) ->
    Action.

handle_callback_result(
    #prxprv_CallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    Events1 = wrap_session_events([?session_activated()], Session),
    {Events2, Action} = handle_proxy_result(ProxyResult, Action0, Session),
    {Response, {Events1 ++ Events2, Action}}.

handle_proxy_result(
    #prxprv_ProxyResult{intent = {_, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, Action0),
    {wrap_session_events(Events1 ++ Events2 ++ Events3, Session), Action}.

handle_proxy_callback_timeout(Action, Session) ->
    Events = [?session_finished(?session_failed(?operation_timeout()))],
    {wrap_session_events(Events, Session), Action}.

wrap_session_events(SessionEvents, #{target := Target}) ->
    [?session_ev(Target, Ev) || Ev <- SessionEvents].

bind_transaction(undefined, _Session) ->
    % no transaction yet
    [];
bind_transaction(Trx, #{trx := undefined}) ->
    % got transaction, nothing bound so far
    [?trx_bound(Trx)];
bind_transaction(Trx, #{trx := Trx}) ->
    % got the same transaction as one which has been bound previously
    [];
bind_transaction(Trx, #{trx := TrxWas}) ->
    % got transaction which differs from the bound one
    % verify against proxy contracts
    case Trx#domain_TransactionInfo.id of
        ID when ID =:= TrxWas#domain_TransactionInfo.id ->
            [?trx_bound(Trx)];
        _ ->
            error(proxy_contract_violated)
    end.

update_proxy_state(undefined) ->
    [];
update_proxy_state(ProxyState) ->
    [?proxy_st_changed(ProxyState)].

handle_proxy_intent(#'FinishIntent'{status = {success, _}}, Action) ->
    Events = [?session_finished(?session_succeeded())],
    {Events, Action};

handle_proxy_intent(#'FinishIntent'{status = {failure, Failure}}, Action) ->
    Events = [?session_finished(?session_failed(convert_failure(Failure)))],
    {Events, Action};

handle_proxy_intent(#'SleepIntent'{timer = Timer}, Action0) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = [],
    {Events, Action};

handle_proxy_intent(#'SuspendIntent'{tag = Tag, timeout = Timer, user_interaction = UserInteraction}, Action0) ->
    Action = try_set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended() | try_request_interaction(UserInteraction)],
    {Events, Action}.

try_set_timer(undefined, Action) ->
    Action;
try_set_timer(Timer, Action) ->
    hg_machine_action:set_timer(Timer, Action).

try_request_interaction(undefined) ->
    [];
try_request_interaction(UserInteraction) ->
    [?interaction_requested(UserInteraction)].

commit_plan(St, Options) ->
    finalize_plan(fun hg_accounting:commit/2, St, Options).

rollback_plan(St, Options) ->
    finalize_plan(fun hg_accounting:rollback/2, St, Options).

finalize_plan(Finalizer, St, Options) ->
    PlanID = construct_plan_id(get_invoice(Options), get_payment(St)),
    Cashflow = get_cashflow(St),
    Finalizer(PlanID, [{1, Cashflow}]).

%%

construct_proxy_context(Session, St, Options) ->
    Payment = get_payment(St),
    Trx = get_trx(St),
    #prxprv_Context{
        session = construct_session(Session),
        payment_info = construct_payment_info(Payment, Trx, Options),
        options = collect_proxy_options(St)
    }.

construct_session(Session = #{target := Target}) ->
    #prxprv_Session{
        target = Target,
        state = maps:get(proxy_state, Session, undefined)
    }.

construct_payment_info(Payment, Trx, Options) ->
    #prxprv_PaymentInfo{
        shop = construct_proxy_shop(Options),
        invoice = construct_proxy_invoice(Options),
        payment = construct_proxy_payment(Payment, Trx)
    }.

construct_proxy_payment(
    #domain_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    Trx
) ->
    #prxprv_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        trx = Trx,
        payer = Payer,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_invoice(
    #{invoice := #domain_Invoice{
        id = InvoiceID,
        created_at = CreatedAt,
        due = Due,
        details = Details,
        cost = Cost
    }}
) ->
    #prxprv_Invoice{
        id = InvoiceID,
        created_at =  CreatedAt,
        due =  Due,
        details = Details,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_shop(Options) ->
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        location = Location,
        category = ShopCategoryRef
    } = get_shop(Options),
    ShopCategory = hg_domain:get(
        hg_domain:head(),
        {category, ShopCategoryRef}
    ),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails,
        location = Location
    }.

construct_proxy_cash(#domain_Cash{
    amount = Amount,
    currency = CurrencyRef
}) ->
    Revision = hg_domain:head(),
    #prxprv_Cash{
        amount = Amount,
        currency = hg_domain:get(Revision, {currency, CurrencyRef})
    }.

collect_proxy_options(
    #st{
        route = #domain_InvoicePaymentRoute{provider = ProviderRef, terminal = TerminalRef}
    }
) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    lists:foldl(
        fun
            (undefined, M) ->
                M;
            (M1, M) ->
                maps:merge(M1, M)
        end,
        #{},
        [
            Terminal#domain_Terminal.options,
            Proxy#domain_Proxy.additional,
            ProxyDef#domain_ProxyDefinition.options
        ]
    ).

convert_failure(#'Failure'{code = Code, description = Description}) ->
    ?external_failure(Code, Description).

%%

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_shop_id(#domain_Invoice{shop_id = ShopID}) ->
    ShopID.

get_created_at(#domain_Invoice{created_at = Dt}) ->
    Dt.

get_party(#{party := Party}) ->
    Party.

get_shop(#{party := Party, invoice := Invoice}) ->
    ShopID = Invoice#domain_Invoice.shop_id,
    Shops = Party#domain_Party.shops,
    maps:get(ShopID, Shops).

get_invoice_currency(#domain_Invoice{cost = #domain_Cash{currency = Currency}}) ->
    Currency.

%%

get_payment_id(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    ID.

%%

-spec throw_invalid_request(binary()) -> no_return().

throw_invalid_request(Why) ->
    throw(#'InvalidRequest'{errors = [Why]}).

%%

-spec merge_change(change(), st() | undefined) -> st().

merge_change(Event, undefined) ->
    merge_change(Event, #st{});

merge_change(?payment_started(Payment, RiskScore, Route, Cashflow), St) ->
    St#st{payment = Payment, risk_score = RiskScore, route = Route, cash_flow = Cashflow};
merge_change(?payment_status_changed(Status), St = #st{payment = Payment}) ->
    St#st{payment = Payment#domain_InvoicePayment{status = Status}};
merge_change(?adjustment_ev(ID, Event), St) ->
    merge_adjustment_change(ID, Event, St);
merge_change(?session_ev(Target, ?session_started()), St) ->
    % FIXME why the hell dedicated handling
    set_session(Target, create_session(Target, get_trx(St)), St#st{target = Target});
merge_change(?session_ev(Target, Event), St) ->
    Session = merge_session_change(Event, get_session(Target, St)),
    St1 = set_session(Target, Session, St),
    case get_session_status(Session) of
        finished ->
            % FIXME leaky transactions
            set_trx(get_session_trx(Session), St1);
        _ ->
            St1
    end.

merge_adjustment_change(_ID, ?adjustment_created(Adjustment), St = #st{adjustments = As}) ->
    St#st{adjustments = [Adjustment | As]};
merge_adjustment_change(ID, ?adjustment_status_changed(Status), St0) ->
    Adjustment = get_adjustment(ID, St0),
    St1 = set_adjustment(Adjustment#domain_InvoicePaymentAdjustment{status = Status}, St0),
    case Status of
        ?adjustment_captured(_) ->
            set_cashflow(get_adjustment_cashflow(Adjustment), St1);
        ?adjustment_cancelled(_) ->
            St1
    end.

get_cashflow(#st{cash_flow = FinalCashflow}) ->
    FinalCashflow.

set_cashflow(Cashflow, St = #st{}) ->
    St#st{cash_flow = Cashflow}.

get_trx(#st{trx = Trx}) ->
    Trx.

set_trx(Trx, St = #st{}) ->
    St#st{trx = Trx}.

set_adjustment(Adjustment, St = #st{adjustments = As}) ->
    ID = get_adjustment_id(Adjustment),
    St#st{adjustments = lists:keyreplace(ID, #domain_InvoicePaymentAdjustment.id, As, Adjustment)}.

merge_session_change(?session_finished(Result), Session) ->
    Session#{status := finished, result => Result};
merge_session_change(?session_activated(), Session) ->
    Session#{status := active};
merge_session_change(?session_suspended(), Session) ->
    Session#{status := suspended};
merge_session_change(?trx_bound(Trx), Session) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session) ->
    Session.

create_session(Target, Trx) ->
    #{
        target => Target,
        status => active,
        trx    => Trx
    }.

get_target_session(St) ->
    get_session(get_target(St), St).

get_session(Target, #st{sessions = Sessions}) ->
    maps:get(Target, Sessions, undefined).

set_session(Target, Session, St = #st{sessions = Sessions}) ->
    St#st{sessions = Sessions#{Target => Session}}.

get_target_session_status(St) ->
    get_session_status(get_target_session(St)).

get_session_status(#{status := Status}) ->
    Status.

get_session_trx(#{trx := Trx}) ->
    Trx.

get_target(#st{target = Target}) ->
    Target.

%%

collapse_changes(Changes) ->
    collapse_changes(Changes, undefined).

collapse_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

%%

issue_process_call(ProxyContext, St, Opts) ->
    issue_call('ProcessPayment', [ProxyContext], St, Opts).

issue_callback_call(Payload, ProxyContext, St, Opts) ->
    issue_call('HandlePaymentCallback', [Payload, ProxyContext], St, Opts).

issue_call(Func, Args, St, Opts) ->
    CallOpts = get_call_options(St, Opts),
    hg_woody_wrapper:call('ProviderProxy', Func, Args, CallOpts).

get_call_options(St, _Opts) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider(get_route(St))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#st{route = Route}) ->
    Route.

get_route_provider(#domain_InvoicePaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

inspect(Shop, Invoice, Payment = #domain_InvoicePayment{domain_revision = Revision}, VS) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    InspectorSelector = Globals#domain_Globals.inspector,
    InspectorRef = reduce_selector(inspector, InspectorSelector, VS, Revision),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    RiskScore = hg_inspector:inspect(Shop, Invoice, Payment, Inspector),
    {RiskScore, VS#{risk_score => RiskScore}}.

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.

%%

-spec get_log_params(change(), st()) ->
    {ok, #{type := invoice_payment_event, params := list(), message := string()}} | undefined.

get_log_params(?payment_started(Payment, _, _, Cashflow), _) ->
    Params = [{accounts, get_partial_remainders(Cashflow)}],
    make_log_params(invoice_payment_started, Payment, Params);
get_log_params(?payment_status_changed({Status, _}), State) ->
    Payment = get_payment(State),
    Cashflow = get_cashflow(State),
    Params = [{status, Status}, {accounts, get_partial_remainders(Cashflow)}],
    make_log_params(invoice_payment_status_changed, Payment, Params);
get_log_params(_, _) ->
    undefined.

make_log_params(EventType, Payment, Params) ->
    #domain_InvoicePayment{
        id = ID,
        cost = ?cash(Amount, ?currency(SymbolicCode))
    } = Payment,
    Result = #{
        type => invoice_payment_event,
        params => [{type, EventType}, {id, ID}, {cost, [{amount, Amount}, {currency, SymbolicCode}]} | Params],
        message => get_message(EventType)
    },
    {ok, Result}.

get_partial_remainders(CashFlow) ->
    Reminders = maps:to_list(hg_cashflow:get_partial_remainders(CashFlow)),
    lists:map(
        fun ({Account, Cash}) ->
            ?cash(Amount, ?currency(SymbolicCode)) = Cash,
            Remainder = [{remainder, [{amount, Amount}, {currency, SymbolicCode}]}],
            {get_account_key(Account), Remainder}
        end,
        Reminders
    ).

get_account_key({AccountParty, AccountType}) ->
    list_to_binary(lists:concat([atom_to_list(AccountParty), ".", atom_to_list(AccountType)])).

get_message(invoice_payment_started) ->
    "Invoice payment is started";
get_message(invoice_payment_status_changed) ->
    "Invoice payment status is changed".

%% Marshalling

-include("msgpack_marshalling.hrl").

-spec marshal(change()) ->
    term().

marshal(Change) ->
    marshal(change, Change).

%% Changes

marshal(change, ?payment_started(Payment, RiskScore, Route, Cashflow)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_started"},
        {str, "payment"} => marshal(payment, Payment),
        {str, "risk_score"} => marshal(risk_score, RiskScore),
        {str, "route"} => marshal(route, Route),
        {str, "cash_flow"} => marshal(final_cash_flow, Cashflow)
    });
marshal(change, ?payment_status_changed(Status)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_status_changed"},
        {str, "status"} => marshal(status, Status)
    });
marshal(change, ?session_ev(Target, Payload)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_session_change"},
        {str, "target"} => marshal(status, Target),
        {str, "payload"} => marshal(session_change, Payload)
    });
marshal(change, ?adjustment_ev(AdjustmentID, Payload)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_adjustment_change"},
        {str, "id"} => {str, AdjustmentID},
        {str, "payload"} => marshal(adj_change, Payload)
    });

%% Change components

marshal(payment, #domain_InvoicePayment{} = Payment) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "id"} => {str, Payment#domain_InvoicePayment.id},
        {str, "created_at"} => {str, Payment#domain_InvoicePayment.created_at},
        {str, "domain_revision"} => Payment#domain_InvoicePayment.domain_revision,
        {str, "cost"} => hg_cash:marshal(Payment#domain_InvoicePayment.cost),
        {str, "payer"} => marshal(payer, Payment#domain_InvoicePayment.payer),
        {str, "flow"} => marshal(flow, Payment#domain_InvoicePayment.flow)
    });

marshal(flow, ?invoice_payment_flow_instant()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "type"} => {str, "instant"}});
marshal(flow, ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "type"} => {str, "hold"},
        {str, "on_hold_expiration"} => marshal(on_hold_expiration, OnHoldExpiration),
        {str, "held_until"} => {str, HeldUntil}
    });

marshal(status, ?pending()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "status"} => {str, "pending"}});
marshal(status, ?processed()) ->
   ?WRAP_VERSION_DATA(2, #{{str, "status"} => {str, "processed"}});
marshal(status, ?failed(Failure)) ->
    ?WRAP_VERSION_DATA(2, #{{str, "status"} => {str, "failed"}, {str, "failure"} => marshal(failure, Failure)});
marshal(status, ?captured_with_reason(Reason)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "status"} => {str, "captured"},
        {str, "reason"} => marshal(str, Reason)
    });
marshal(status, ?cancelled_with_reason(Reason)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "status"} => {str, "cancelled"},
        {str, "reason"} => marshal(str, Reason)
    });

marshal(session_change, ?session_started()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "change"} => {str, "invoice_payment_session_started"}});
marshal(session_change, ?session_finished(Result)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_session_finished"},
        {str, "result"} => marshal(session_status, Result)
    });
marshal(session_change, ?session_suspended()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "change"} => {str, "invoice_payment_session_suspended"}});
marshal(session_change, ?session_activated()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "change"} => {str, "invoice_payment_session_activated"}});
marshal(session_change, ?trx_bound(Trx)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_session_transaction_bound"},
        {str, "trx"} => marshal(trx, Trx)
    });
marshal(session_change, ?proxy_st_changed(ProxySt)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_session_proxy_state_changed"},
        {str, "proxy_state"} => ProxySt
    });
marshal(session_change, ?interaction_requested(UserInteraction)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_session_interaction_requested"},
        {str, "interaction"} => marshal(interaction, UserInteraction)
    });
marshal(session_status, ?session_succeeded()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "status"} => {str, "succeeded"}});
marshal(session_status, ?session_failed(PayloadFailure)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "status"} => {str, "failed"},
        {str, "failure"} => marshal(failure, PayloadFailure)
    });

marshal(adj_change, ?adjustment_created(Adjustment)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_adjustment_created"},
        {str, "adjustment"} => marshal(adj, Adjustment)
    });
marshal(adj_change, ?adjustment_status_changed(Status)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "change"} => {str, "invoice_payment_adjustment_status_changed"},
        {str, "status"} => marshal(adj_status, Status)
    });

marshal(adj, #domain_InvoicePaymentAdjustment{} = Adjustment) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "id"} => {str, Adjustment#domain_InvoicePaymentAdjustment.id},
        {str, "created_at"} => {str, Adjustment#domain_InvoicePaymentAdjustment.created_at},
        {str, "domain_revision"} => Adjustment#domain_InvoicePaymentAdjustment.domain_revision,
        {str, "reason"} => {str, Adjustment#domain_InvoicePaymentAdjustment.reason},
        {str, "old_cash_flow_inverse"} => marshal(final_cash_flow,
            Adjustment#domain_InvoicePaymentAdjustment.old_cash_flow_inverse),
        {str, "new_cash_flow"} => marshal(final_cash_flow,
            Adjustment#domain_InvoicePaymentAdjustment.new_cash_flow)
    });

marshal(adj_status, ?adjustment_pending()) ->
    ?WRAP_VERSION_DATA(2, #{{str, "status"} => {str, "pending"}});
marshal(adj_status, ?adjustment_captured(At)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "status"} => {str, "captured"},
        {str, "at"} => {str, At}
    });
marshal(adj_status, ?adjustment_cancelled(At)) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "status"} => {str, "cancelled"},
        {str, "at"} => {str, At}
    });

marshal(route, #domain_InvoicePaymentRoute{} = Route) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "provider"} => marshal(provider_ref, Route#domain_InvoicePaymentRoute.provider),
        {str, "terminal"} => marshal(terminal_ref, Route#domain_InvoicePaymentRoute.terminal)
    });

marshal(provider_ref, #domain_ProviderRef{id = ObjectID}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "id"} => ObjectID});

marshal(terminal_ref, #domain_TerminalRef{id = ObjectID}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "id"} => ObjectID});

marshal(final_cash_flow, CashFlow) ->
    ?WRAP_VERSION_DATA(2, [marshal(final_cash_flow_posting, CashFlowPosting) || CashFlowPosting <- CashFlow]);

marshal(final_cash_flow_posting, #domain_FinalCashFlowPosting{} = CashFlowPosting) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "source"} =>
            marshal(final_cash_flow_account, CashFlowPosting#domain_FinalCashFlowPosting.source),
        {str, "destination"} =>
            marshal(final_cash_flow_account, CashFlowPosting#domain_FinalCashFlowPosting.destination),
        {str, "volume"} => hg_cash:marshal(CashFlowPosting#domain_FinalCashFlowPosting.volume),
        {str, "details"} => marshal(str, CashFlowPosting#domain_FinalCashFlowPosting.details)
    });

marshal(final_cash_flow_account, #domain_FinalCashFlowAccount{} = CashFlowAccount) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "account_type"} =>
            marshal(account_type, CashFlowAccount#domain_FinalCashFlowAccount.account_type),
        {str, "account_id"} => CashFlowAccount#domain_FinalCashFlowAccount.account_id
    });

marshal(account_type, {merchant, settlement}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "merchant"} => {str, "settlement"}});
marshal(account_type, {merchant, guarantee}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "merchant"} => {str, "guarantee"}});
marshal(account_type, {provider, settlement}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "provider"} => {str, "settlement"}});
marshal(account_type, {system, settlement}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "system"} => {str, "settlement"}});
marshal(account_type, {external, income}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "external"} => {str, "income"}});
marshal(account_type, {external, outcome}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "external"} => {str, "outcome"}});

marshal(payer, #domain_Payer{} = Payer) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "payment_tool"} => marshal(payment_tool, Payer#domain_Payer.payment_tool),
        {str, "session_id"} => {str, Payer#domain_Payer.session_id},
        {str, "client_info"} => marshal(client_info, Payer#domain_Payer.client_info),
        {str, "contact_info"} => marshal(contact_info, Payer#domain_Payer.contact_info)
    });

marshal(payment_tool, {bank_card, #domain_BankCard{} = BankCard}) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "token"} => {str, BankCard#domain_BankCard.token},
        {str, "payment_system"} => marshal(payment_system, BankCard#domain_BankCard.payment_system),
        {str, "bin"} => {str, BankCard#domain_BankCard.bin},
        {str, "masked_pan"} => {str, BankCard#domain_BankCard.masked_pan}
    });

marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "ip_address"} => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        {str, "fingerprint"} => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

marshal(contact_info, #domain_ContactInfo{} = ContactInfo) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "phone_number"} => marshal(str, ContactInfo#domain_ContactInfo.phone_number),
        {str, "email"} => marshal(str, ContactInfo#domain_ContactInfo.email)
    });

marshal(trx, #domain_TransactionInfo{} = TransactionInfo) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "id"} => {str, TransactionInfo#domain_TransactionInfo.id},
        {str, "timestamp"} => marshal(str, TransactionInfo#domain_TransactionInfo.timestamp),
        {str, "extra"} => marshal(map_string, TransactionInfo#domain_TransactionInfo.extra)
    });

marshal(map_string, MapString) ->
    maps:from_list([{{str, K}, {str, V}} || {K, V} <- maps:to_list(MapString)]);

marshal(interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "redirect"} => #{{str, "get_request"} => #{{str, "uri"} => {str, URI}}}});
marshal(interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    ?WRAP_VERSION_DATA(2, #{{str, "redirect"} => #{{str, "post_request"} => #{
        {str, "uri"} => {str, URI},
        {str, "form"} => marshal(map_string, Form)
    }}});

marshal(failure, {operation_timeout, _}) ->
    ?WRAP_VERSION_DATA(2, {str, "operation_timeout"});
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    ?WRAP_VERSION_DATA(2, #{
        {str, "code"} => {str, ExternalFailure#domain_ExternalFailure.code},
        {str, "description"} => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    });

marshal(on_hold_expiration, cancel) ->
    {str, "cancel"};
marshal(on_hold_expiration, capture) ->
    {str, "capture"};

marshal(risk_score, low) ->
    {str, "low"};
marshal(risk_score, high) ->
    {str, "high"};
marshal(risk_score, fatal) ->
    {str, "fatal"};

marshal(payment_system, visa) ->
    {str, "visa"};
marshal(payment_system, mastercard) ->
    {str, "mastercard"};
marshal(payment_system, visaelectron) ->
    {str, "visaelectron"};
marshal(payment_system, maestro) ->
    {str, "maestro"};
marshal(payment_system, forbrugsforeningen) ->
    {str, "forbrugsforeningen"};
marshal(payment_system, dankort) ->
    {str, "dankort"};
marshal(payment_system, amex) ->
    {str, "amex"};
marshal(payment_system, dinersclub) ->
    {str, "dinersclub"};
marshal(payment_system, discover) ->
    {str, "discover"};
marshal(payment_system, unionpay) ->
    {str, "unionpay"};
marshal(payment_system, jcb) ->
    {str, "jcb"};
marshal(payment_system, nspkmir) ->
    {str, "nspkmir"};

%% Optional components

marshal(str, undefined) ->
    undefined;
marshal(str, String) ->
    {str, String}.

%% Unmarshalling

-spec unmarshal(term()) -> change().

unmarshal(Change) ->
    unmarshal(change, Change).

unmarshal(Type, #{{str, "version"} := Version, {str, "data"} := Data}) ->
    unmarshal(Version, Type, Data);

unmarshal(on_hold_expiration, {str, "cancel"}) ->
    cancel;
unmarshal(on_hold_expiration, {str, "capture"}) ->
    capture;

unmarshal(risk_score, {str, "low"}) ->
    low;
unmarshal(risk_score, {str, "high"}) ->
    high;
unmarshal(risk_score, {str, "fatal"}) ->
    fatal;

unmarshal(payment_system, {str, "visa"}) ->
    visa;
unmarshal(payment_system, {str, "mastercard"}) ->
    mastercard;
unmarshal(payment_system, {str, "visaelectron"}) ->
    visaelectron;
unmarshal(payment_system, {str, "maestro"}) ->
    maestro;
unmarshal(payment_system, {str, "forbrugsforeningen"}) ->
    forbrugsforeningen;
unmarshal(payment_system, {str, "dankort"}) ->
    dankort;
unmarshal(payment_system, {str, "amex"}) ->
    amex;
unmarshal(payment_system, {str, "dinersclub"}) ->
    dinersclub;
unmarshal(payment_system, {str, "discover"}) ->
    discover;
unmarshal(payment_system, {str, "unionpay"}) ->
    unionpay;
unmarshal(payment_system, {str, "jcb"}) ->
    jcb;
unmarshal(payment_system, {str, "nspkmir"}) ->
    nspkmir;

unmarshal(map_string, MapString) ->
    maps:from_list([{?BIN(K), ?BIN(V)} || {{str, K}, {str, V}} <- maps:to_list(MapString)]);

%% Optional components

unmarshal(str, undefined) ->
    undefined;
unmarshal(str, {str, String}) ->
    ?BIN(String).

%% Changes

unmarshal(1, change, ?payment_started(Payment, RiskScore, Route, Cashflow)) ->
    NewPayment =
        #domain_InvoicePayment{
        id              = Payment#domain_InvoicePayment.id,
        created_at      = Payment#domain_InvoicePayment.created_at,
        domain_revision = Payment#domain_InvoicePayment.domain_revision,
        status          = Payment#domain_InvoicePayment.status,
        cost            = Payment#domain_InvoicePayment.cost,
        payer           = Payment#domain_InvoicePayment.payer,
        flow            = ?invoice_payment_flow_instant()
    },
    ?payment_started(NewPayment, RiskScore, Route, Cashflow);
unmarshal(1, change, Change) ->
    Change;
unmarshal(2, change, #{
    {str, "change"} := {str, "invoice_payment_started"},
    {str, "payment"} := Payment,
    {str, "risk_score"} := RiskScore,
    {str, "route"} := Route,
    {str, "cash_flow"} := Cashflow
}) ->
    ?payment_started(
        unmarshal(payment, Payment),
        unmarshal(risk_score, RiskScore),
        unmarshal(route, Route),
        unmarshal(final_cash_flow, Cashflow)
    );
unmarshal(2, change, #{
    {str, "change"} := {str, "invoice_payment_status_changed"},
    {str, "status"} := Status
}) ->
    ?payment_status_changed(unmarshal(status, Status));
unmarshal(2, change, #{
    {str, "change"} := {str, "invoice_payment_session_change"},
    {str, "payload"} := Payload,
    {str, "target"} := Target
}) ->
    ?session_ev(unmarshal(status, Target), unmarshal(session_change, Payload));
unmarshal(2, change, #{
    {str, "change"} := {str, "invoice_payment_adjustment_change"},
    {str, "id"} := {str, AdjustmentID},
    {str, "payload"} := Payload
}) ->
    ?adjustment_ev(?BIN(AdjustmentID), unmarshal(adj_change, Payload));

%% Change components

unmarshal(2, payment, #{
    {str, "id"} := {str, PaymentID},
    {str, "created_at"} := {str, CreatedAt},
    {str, "domain_revision"} := Revision,
    {str, "cost"} := Cash,
    {str, "payer"} := Payer,
    {str, "flow"} := Flow
}) ->
    #domain_InvoicePayment{
        id              = ?BIN(PaymentID),
        created_at      = ?BIN(CreatedAt),
        domain_revision = Revision,
        cost            = hg_cash:unmarshal(Cash),
        payer           = unmarshal(payer, Payer),
        status          = ?pending(),
        flow            = unmarshal(flow, Flow)
    };

unmarshal(2, flow, #{{str, "type"} := {str, "instant"}}) ->
    ?invoice_payment_flow_instant();
unmarshal(2, flow, #{
    {str, "type"} := {str, "hold"},
    {str, "on_hold_expiration"} := OnHoldExpiration,
    {str, "held_until"} := {str, HeldUntil}
}) ->
    ?invoice_payment_flow_hold(unmarshal(on_hold_expiration, OnHoldExpiration), ?BIN(HeldUntil));

unmarshal(2, status, #{{str, "status"} := {str, "pending"}}) ->
    ?pending();
unmarshal(2, status, #{{str, "status"} := {str, "processed"}}) ->
    ?processed();
unmarshal(2, status, #{
    {str, "status"} := {str, "failed"},
    {str, "failure"} := Failure
}) ->
    ?failed(unmarshal(failure, Failure));
unmarshal(2, status, #{
    {str, "status"} := {str, "captured"},
    {str, "reason"} := Reason
}) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(2, status, #{
    {str, "status"} := {str, "cancelled"},
    {str, "reason"} := Reason
}) ->
    ?cancelled_with_reason(unmarshal(str, Reason));

unmarshal(2, session_change, #{{str, "change"} := {str, "invoice_payment_session_started"}}) ->
    ?session_started();
unmarshal(2, session_change, #{
    {str, "change"} := {str, "invoice_payment_session_finished"},
    {str, "result"} := Result
}) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(2, session_change, #{{str, "change"} := {str, "invoice_payment_session_suspended"}}) ->
    ?session_suspended();
unmarshal(2, session_change, #{{str, "change"} := {str, "invoice_payment_session_activated"}}) ->
    ?session_activated();
unmarshal(2, session_change, #{
    {str, "change"} := {str, "invoice_payment_session_transaction_bound"},
    {str, "trx"} := Trx
}) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(2, session_change, #{
    {str, "change"} := {str, "invoice_payment_session_proxy_state_changed"},
    {str, "proxy_state"} := ProxySt
}) ->
    ?proxy_st_changed(ProxySt);
unmarshal(2, session_change, #{
    {str, "change"} := {str, "invoice_payment_session_interaction_requested"},
    {str, "interaction"} := UserInteraction
}) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(2, session_status, #{{str, "status"} := {str, "succeeded"}}) ->
    ?session_succeeded();
unmarshal(2, session_status, #{
    {str, "status"} := {str, "failed"},
    {str, "failure"} := Failure
}) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(2, adj_change, #{
    {str, "change"} := {str, "invoice_payment_adjustment_created"},
    {str, "adjustment"} := Adjustment
}) ->
    ?adjustment_created(unmarshal(adj, Adjustment));
unmarshal(2, adj_change, #{
    {str, "change"} := {str, "invoice_payment_adjustment_status_changed"},
    {str, "status"} := Status
}) ->
    ?adjustment_status_changed(unmarshal(adj_status, Status));

unmarshal(2, adj, #{
    {str, "id"} := {str, AdjustmentID},
    {str, "created_at"} := {str, CreatedAt},
    {str, "domain_revision"} := Revision,
    {str, "reason"} := {str, Reason},
    {str, "old_cash_flow_inverse"} := OldCashFlowInverse,
    {str, "new_cash_flow"} := NewCashFlow
}) ->
    #domain_InvoicePaymentAdjustment{
        id                    = ?BIN(AdjustmentID),
        status                = ?adjustment_pending(),
        created_at            = ?BIN(CreatedAt),
        domain_revision       = Revision,
        reason                = ?BIN(Reason),
        old_cash_flow_inverse = unmarshal(final_cash_flow, OldCashFlowInverse),
        new_cash_flow         = unmarshal(final_cash_flow, NewCashFlow)
    };

unmarshal(2, adj_status, #{{str, "status"} := {str, "pending"}}) ->
    ?adjustment_pending();
unmarshal(2, adj_status, #{
    {str, "status"} := {str, "captured"},
    {str, "at"} := {str, At}
}) ->
    ?adjustment_captured(?BIN(At));
unmarshal(2, adj_status, #{
    {str, "status"} := {str, "cancelled"},
    {str, "at"} := {str, At}
}) ->
    ?adjustment_cancelled(?BIN(At));
unmarshal(2, route, #{
    {str, "provider"} := Provider,
    {str, "terminal"} := Terminal
}) ->
    #domain_InvoicePaymentRoute{
        provider = unmarshal(provider_ref, Provider),
        terminal = unmarshal(terminal_ref, Terminal)
    };
unmarshal(2, provider_ref, #{{str, "id"} := ObjectID}) ->
    #domain_ProviderRef{id = ObjectID};
unmarshal(2, terminal_ref, #{{str, "id"} := ObjectID}) ->
    #domain_TerminalRef{id = ObjectID};

unmarshal(2, final_cash_flow, CashFlow) ->
    [unmarshal(final_cash_flow_posting, CashFlowPosting) || CashFlowPosting <- CashFlow];

unmarshal(2, final_cash_flow_posting, #{
    {str, "source"} := Source,
    {str, "destination"} := Destination,
    {str, "volume"} := Volume,
    {str, "details"} := Details
}) ->
    #domain_FinalCashFlowPosting{
        source = unmarshal(final_cash_flow_account, Source),
        destination = unmarshal(final_cash_flow_account, Destination),
        volume = hg_cash:unmarshal(Volume),
        details = unmarshal(str, Details)
    };

unmarshal(2, final_cash_flow_account, #{
    {str, "account_type"} := AccountType,
    {str, "account_id"} := AccountId
}) ->
    #domain_FinalCashFlowAccount{
        account_type = unmarshal(account_type, AccountType),
        account_id = AccountId
    };

unmarshal(2, account_type, #{{str, "merchant"} := {str, "settlement"}}) ->
    {merchant, settlement};
unmarshal(2, account_type, #{{str, "merchant"} := {str, "guarantee"}}) ->
    {merchant, guarantee};
unmarshal(2, account_type, #{{str, "provider"} := {str, "settlement"}}) ->
    {provider, settlement};
unmarshal(2, account_type, #{{str, "system"} := {str, "settlement"}}) ->
    {system, settlement};
unmarshal(2, account_type, #{{str, "external"} := {str, "income"}}) ->
    {external, income};
unmarshal(2, account_type, #{{str, "external"} := {str, "outcome"}}) ->
    {external, outcome};

unmarshal(2, payer, #{
    {str, "payment_tool"} := PaymentTool,
    {str, "session_id"} := {str, SessionId},
    {str, "client_info"} := ClientInfo,
    {str, "contact_info"} := ContractInfo
}) ->
    #domain_Payer{
        payment_tool = unmarshal(payment_tool, PaymentTool),
        session_id = ?BIN(SessionId),
        client_info = unmarshal(client_info, ClientInfo),
        contact_info = unmarshal(contact_info, ContractInfo)
    };

unmarshal(2, payment_tool, #{
    {str, "token"} := {str, Token},
    {str, "payment_system"} := PaymentSystem,
    {str, "bin"} := {str, Bin},
    {str, "masked_pan"} := {str, MaskedPan}
}) ->
    {bank_card, #domain_BankCard{
        token = ?BIN(Token),
        payment_system = unmarshal(payment_system, PaymentSystem),
        bin = ?BIN(Bin),
        masked_pan = ?BIN(MaskedPan)
    }};

unmarshal(2, client_info, #{
    {str, "ip_address"} := IpAddress,
    {str, "fingerprint"} := Fingerprint
}) ->
    #domain_ClientInfo{
        ip_address = unmarshal(str, IpAddress),
        fingerprint = unmarshal(str, Fingerprint)
    };

unmarshal(2, contact_info, #{
    {str, "phone_number"} := PhoneNumber,
    {str, "email"} := Email
}) ->
    #domain_ContactInfo{
        phone_number = unmarshal(str, PhoneNumber),
        email = unmarshal(str, Email)
    };

unmarshal(2, trx, #{
    {str, "id"} := {str, ID},
    {str, "timestamp"} := Timestamp,
    {str, "extra"} := Extra
}) ->
    #domain_TransactionInfo{
        id = ?BIN(ID),
        timestamp = unmarshal(str, Timestamp),
        extra = unmarshal(map_string, Extra)
    };

unmarshal(2, interaction, #{{str, "redirect"} := #{{str, "get_request"} := #{{str, "uri"} := {str, URI}}}}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(2, interaction, #{{str, "redirect"} := #{{str, "post_request"} := #{
    {str, "uri"} := {str, URI},
    {str, "form"} := Form
}}}) ->
    {redirect, {post_request, #'BrowserPostRequest'{uri = ?BIN(URI), form = unmarshal(map_string, Form)}}};

unmarshal(2, failure, {str, "operation_timeout"}) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(2, failure, #{
    {str, "code"} := {str, Code},
    {str, "description"} := Description
}) ->
    {external_failure, #domain_ExternalFailure{code = ?BIN(Code), description = unmarshal(str, Description)}}.