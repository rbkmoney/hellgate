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
    VS3 = validate_payment_flow(PaymentParams, {Revision, PaymentTerms}, VS2),
    CreatedAt = hg_datetime:format_now(),
    Flow = validate_flow(PaymentParams, {Revision, PaymentTerms}, CreatedAt, VS3),
    Payment = construct_payment(PaymentID, Invoice, Flow, PaymentParams, CreatedAt, Revision),
    case inspect(Shop, Invoice, Payment, VS3) of
        {RiskScore, VS4} when RiskScore == low; RiskScore == high ->
            Route = validate_route(Payment, hg_routing:choose(VS4, Revision)),
            FinalCashflow = construct_final_cashflow(Invoice, Payment, Shop, PaymentTerms, Route, VS4, Revision),
            _AccountsState = hg_accounting:plan(
                construct_plan_id(Invoice, Payment),
                {1, FinalCashflow}
            ),
            Events = [?payment_started(Payment, RiskScore, Route, FinalCashflow)],
            {collapse_changes(Events), {Events, hg_machine_action:new()}};
        {fatal, _} ->
            throw_invalid_request(<<"Fatal error">>)
    end.

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
    {Revision, Terms},
    VS
) ->
    PaymentFlow = case Type of
        instant ->
            instant;
        hold ->
            {hold, validate_hold_lifetime(Terms, VS, Revision)}
    end,
    VS#{payment_flow => PaymentFlow}.

validate_flow(PaymentParams, {Revision, Terms}, CreatedAt, VS) ->
    case PaymentParams#payproc_InvoicePaymentParams.flow of
        {instant, _} ->
            ?invoice_payment_flow_instant();
        {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}} ->
            ?hold_lifetime(HoldLifetime) = validate_hold_lifetime(Terms, VS, Revision),
            HeldUntil = hg_datetime:format_ts(hg_datetime:parse_ts(CreatedAt) + HoldLifetime),
            ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)
    end.

validate_hold_lifetime(#domain_PaymentsServiceTerms{hold_lifetime = undefined}, _, _) ->
    throw_invalid_request(<<"Holds are not available">>);
validate_hold_lifetime(#domain_PaymentsServiceTerms{hold_lifetime = HoldLifetimeSelector}, VS, Revision) ->
    reduce_selector_for_holds(HoldLifetimeSelector, VS, Revision).

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

reduce_selector_for_holds(Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        _ ->
            throw_invalid_request(<<"Holds are not available">>)
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
    Action = hg_machine_action:new(),
    case get_target_session_status(St) of
        suspended ->
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

handle_proxy_result(
    #prxprv_ProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, Action0),
    {wrap_session_events(Events1 ++ Events2 ++ Events3, Session), Action}.

handle_callback_result(
    #prxprv_CallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

handle_proxy_callback_result(
    #prxprv_CallbackProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {Events3, Action} = handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0)),
    {wrap_session_events([?session_activated()] ++ Events1 ++ Events2 ++ Events3, Session), Action};
handle_proxy_callback_result(
    #prxprv_CallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState),
    {wrap_session_events(Events1 ++ Events2, Session), Action0}.

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
    Action = set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended() | try_request_interaction(UserInteraction)],
    {Events, Action}.

set_timer(Timer, Action) ->
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

-include("legacy_structures.hrl").
%% Marshalling

-spec marshal(change()) ->
    hg_msgpack_marshalling:value().

marshal(Change) ->
    marshal(change, Change).

%% Changes

marshal(change, ?payment_started(Payment, RiskScore, Route, Cashflow)) ->
    [2, #{
        <<"change">>        => <<"started">>,
        <<"payment">>       => marshal(payment, Payment),
        <<"risk_score">>    => marshal(risk_score, RiskScore),
        <<"route">>         => hg_routing:marshal(Route),
        <<"cash_flow">>     => hg_cashflow:marshal(Cashflow)
    }];
marshal(change, ?payment_status_changed(Status)) ->
    [2, #{
        <<"change">>        => <<"status_changed">>,
        <<"status">>        => marshal(status, Status)
    }];
marshal(change, ?session_ev(Target, Payload)) ->
    [2, #{
        <<"change">>        => <<"session_change">>,
        <<"target">>        => marshal(status, Target),
        <<"payload">>       => marshal(session_change, Payload)
    }];
marshal(change, ?adjustment_ev(AdjustmentID, Payload)) ->
    [2, #{
        <<"change">>        => <<"adjustment_change">>,
        <<"id">>            => marshal(str, AdjustmentID),
        <<"payload">>       => marshal(adj_change, Payload)
    }];

%% Change components

marshal(payment, #domain_InvoicePayment{} = Payment) ->
    genlib_map:compact(#{
        <<"id">>                => marshal(str, Payment#domain_InvoicePayment.id),
        <<"created_at">>        => marshal(str, Payment#domain_InvoicePayment.created_at),
        <<"domain_revision">>   => marshal(str, Payment#domain_InvoicePayment.domain_revision),
        <<"cost">>              => hg_cash:marshal(Payment#domain_InvoicePayment.cost),
        <<"payer">>             => marshal(payer, Payment#domain_InvoicePayment.payer),
        <<"flow">>              => marshal(flow, Payment#domain_InvoicePayment.flow),
        <<"context">>           => hg_content:marshal(Payment#domain_InvoicePayment.context)
    });

marshal(flow, ?invoice_payment_flow_instant()) ->
    #{<<"type">> => <<"instant">>};
marshal(flow, ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)) ->
    #{
        <<"type">>                  => <<"hold">>,
        <<"on_hold_expiration">>    => marshal(on_hold_expiration, OnHoldExpiration),
        <<"held_until">>            => marshal(str, HeldUntil)
    };

marshal(status, ?pending()) ->
    <<"pending">>;
marshal(status, ?processed()) ->
    <<"processed">>;
marshal(status, ?failed(Failure)) ->
    [
        <<"failed">>,
        marshal(failure, Failure)
    ];
marshal(status, ?captured_with_reason(Reason)) ->
    [
        <<"captured">>,
        marshal(str, Reason)
    ];
marshal(status, ?cancelled_with_reason(Reason)) ->
    [
        <<"cancelled">>,
        marshal(str, Reason)
    ];

marshal(session_change, ?session_started()) ->
    [2, <<"started">>];
marshal(session_change, ?session_finished(Result)) ->
    [2, [
        <<"finished">>,
        marshal(session_status, Result)
    ]];
marshal(session_change, ?session_suspended()) ->
    [2, <<"suspended">>];
marshal(session_change, ?session_activated()) ->
    [2, <<"activated">>];
marshal(session_change, ?trx_bound(Trx)) ->
    [2, [
        <<"transaction_bound">>,
        marshal(trx, Trx)
    ]];
marshal(session_change, ?proxy_st_changed(ProxySt)) ->
    [2, [
        <<"proxy_state_changed">>,
        marshal(bin, {bin, ProxySt})
    ]];
marshal(session_change, ?interaction_requested(UserInteraction)) ->
    [2, [
        <<"interaction_requested">>,
        marshal(interaction, UserInteraction)
    ]];

marshal(session_status, ?session_succeeded()) ->
    <<"succeeded">>;
marshal(session_status, ?session_failed(PayloadFailure)) ->
    [
        <<"failed">>,
        marshal(failure, PayloadFailure)
    ];

marshal(adj_change, ?adjustment_created(Adjustment)) ->
    [2, [
        <<"created">>,
        marshal(adj, Adjustment)
    ]];
marshal(adj_change, ?adjustment_status_changed(Status)) ->
    [2, [
        <<"status_changed">>,
        marshal(adj_status, Status)
    ]];

marshal(adj, #domain_InvoicePaymentAdjustment{} = Adjustment) ->
    #{
        <<"id">>                    => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.id),
        <<"created_at">>            => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.created_at),
        <<"domain_revision">>       => marshal(int, Adjustment#domain_InvoicePaymentAdjustment.domain_revision),
        <<"reason">>                => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.reason),
        <<"old_cash_flow_inverse">> => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.old_cash_flow_inverse),
        <<"new_cash_flow">>         => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.new_cash_flow)
    };

marshal(adj_status, ?adjustment_pending()) ->
    <<"pending">>;
marshal(adj_status, ?adjustment_captured(At)) ->
    [
        <<"captured">>,
        marshal(str, At)
    ];
marshal(adj_status, ?adjustment_cancelled(At)) ->
    [
        <<"cancelled">>,
        marshal(str, At)
    ];

marshal(payer, #domain_Payer{} = Payer) ->
    #{
        <<"payment_tool">>  => hg_payment_tool:marshal(Payer#domain_Payer.payment_tool),
        <<"session_id">>    => marshal(str, Payer#domain_Payer.session_id),
        <<"client_info">>   => marshal(client_info, Payer#domain_Payer.client_info),
        <<"contact_info">>  => marshal(contact_info, Payer#domain_Payer.contact_info)
    };

marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    genlib_map:compact(#{
        <<"ip_address">>    => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        <<"fingerprint">>   => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

marshal(contact_info, #domain_ContactInfo{} = ContactInfo) ->
    genlib_map:compact(#{
        <<"phone_number">>  => marshal(str, ContactInfo#domain_ContactInfo.phone_number),
        <<"email">>         => marshal(str, ContactInfo#domain_ContactInfo.email)
    });

marshal(trx, #domain_TransactionInfo{} = TransactionInfo) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, TransactionInfo#domain_TransactionInfo.id),
        <<"timestamp">>     => marshal(str, TransactionInfo#domain_TransactionInfo.timestamp),
        <<"extra">>         => marshal(map_str, TransactionInfo#domain_TransactionInfo.extra)
    });

marshal(interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    #{<<"redirect">> =>
        [
            <<"get_request">>,
            marshal(str, URI)
        ]
    };
marshal(interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    #{<<"redirect">> =>
        [
            <<"post_request">>,
            #{
                <<"uri">>   => marshal(str, URI),
                <<"form">>  => marshal(map_str, Form)
            }
        ]
    };

marshal(failure, {operation_timeout, _}) ->
    [2, <<"operation_timeout">>];
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [2, [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })]];

marshal(on_hold_expiration, cancel) ->
    <<"cancel">>;
marshal(on_hold_expiration, capture) ->
    <<"capture">>;

marshal(risk_score, low) ->
    <<"low">>;
marshal(risk_score, high) ->
    <<"high">>;
marshal(risk_score, fatal) ->
    <<"fatal">>;

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) -> change().

unmarshal(Change) ->
    unmarshal(change, Change).

%% Changes

unmarshal(change, [2, #{
    <<"change">>        := <<"started">>,
    <<"payment">>       := Payment,
    <<"risk_score">>    := RiskScore,
    <<"route">>         := Route,
    <<"cash_flow">>     := Cashflow
}]) ->
    ?payment_started(
        unmarshal(payment, Payment),
        unmarshal(risk_score, RiskScore),
        hg_routing:unmarshal(Route),
        hg_cashflow:unmarshal(Cashflow)
    );
unmarshal(change, [2, #{
    <<"change">>    := <<"status_changed">>,
    <<"status">>    := Status
}]) ->
    ?payment_status_changed(unmarshal(status, Status));
unmarshal(change, [2, #{
    <<"change">>    := <<"session_change">>,
    <<"payload">>   := Payload,
    <<"target">>    := Target
}]) ->
    ?session_ev(unmarshal(status, Target), unmarshal(session_change, Payload));
unmarshal(change, [2, #{
    <<"change">>    := <<"adjustment_change">>,
    <<"id">>        := AdjustmentID,
    <<"payload">>   := Payload
}]) ->
    ?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adj_change, Payload));

unmarshal(change, [1, ?legacy_payment_started(Payment, RiskScore, Route, Cashflow)]) ->
    ?payment_started(
        unmarshal(payment, Payment),
        unmarshal(risk_score, RiskScore),
        hg_routing:unmarshal([1, Route]),
        hg_cashflow:unmarshal([1, Cashflow])
    );
unmarshal(change, [1, ?legacy_payment_status_changed(Status)]) ->
    ?payment_status_changed(unmarshal(status, Status));
unmarshal(change, [1, ?legacy_session_ev(Target, Payload)]) ->
    ?session_ev(unmarshal(status, Target), unmarshal(session_change, [1, Payload]));
unmarshal(change, [1, ?legacy_adjustment_ev(AdjustmentID, Payload)]) ->
    ?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adj_change, [1, Payload]));

%% Payment

unmarshal(payment, #{
    <<"id">>                := ID,
    <<"created_at">>        := CreatedAt,
    <<"domain_revision">>   := Revision,
    <<"cost">>              := Cash,
    <<"payer">>             := Payer,
    <<"flow">>              := Flow
} = Payment) ->
    Context = maps:get(<<"context">>, Payment, undefined),
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        cost            = hg_cash:unmarshal(Cash),
        payer           = unmarshal(payer, Payer),
        status          = ?pending(),
        flow            = unmarshal(flow, Flow),
        context         = hg_content:unmarshal(Context)
    };

unmarshal(payment,
    ?legacy_payment(ID, CreatedAt, Revision, Status, Payer, Cash, Context)
) ->
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        status          = unmarshal(status, Status),
        cost            = hg_cash:unmarshal([1, Cash]),
        payer           = unmarshal(payer, Payer),
        flow            = ?invoice_payment_flow_instant(),
        context         = hg_content:unmarshal(Context)
    };

%% Flow

unmarshal(flow, #{<<"type">> := <<"instant">>}) ->
    ?invoice_payment_flow_instant();
unmarshal(flow, #{
    <<"type">>                  := <<"hold">>,
    <<"on_hold_expiration">>    := OnHoldExpiration,
    <<"held_until">>            := HeldUntil
}) ->
    ?invoice_payment_flow_hold(
        unmarshal(on_hold_expiration, OnHoldExpiration),
        unmarshal(str, HeldUntil));

%% Payment status

unmarshal(status, <<"pending">>) ->
    ?pending();
unmarshal(status, <<"processed">>) ->
    ?processed();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?failed(unmarshal(failure, Failure));
unmarshal(status, [<<"captured">>, Reason]) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(status, [<<"cancelled">>, Reason]) ->
    ?cancelled_with_reason(unmarshal(str, Reason));

unmarshal(status, ?legacy_pending()) ->
    ?pending();
unmarshal(status, ?legacy_processed()) ->
    ?processed();
unmarshal(status, ?legacy_failed(Failure)) ->
    ?failed(unmarshal(failure, [1, Failure]));
unmarshal(status, ?legacy_captured(Reason)) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(status, ?legacy_cancelled(Reason)) ->
    ?cancelled_with_reason(unmarshal(str, Reason));

%% Session change

unmarshal(session_change, [2, <<"started">>]) ->
    ?session_started();
unmarshal(session_change, [2, [<<"finished">>, Result]]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [2, <<"suspended">>]) ->
    ?session_suspended();
unmarshal(session_change, [2, <<"activated">>]) ->
    ?session_activated();
unmarshal(session_change, [2, [<<"transaction_bound">>, Trx]]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [2, [<<"proxy_state_changed">>, {bin, ProxySt}]]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [2, [<<"interaction_requested">>, UserInteraction]]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(session_change, [1, ?legacy_session_started()]) ->
    ?session_started();
unmarshal(session_change, [1, ?legacy_session_finished(Result)]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [1, ?legacy_session_suspended()]) ->
    ?session_suspended();
unmarshal(session_change, [1, ?legacy_session_activated()]) ->
    ?session_activated();
unmarshal(session_change, [1, ?legacy_trx_bound(Trx)]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [1, ?legacy_proxy_st_changed(ProxySt)]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [1, ?legacy_interaction_requested(UserInteraction)]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

%% Session status

unmarshal(session_status, <<"succeeded">>) ->
    ?session_succeeded();
unmarshal(session_status, [<<"failed">>, Failure]) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(session_status, ?legacy_session_succeeded()) ->
    ?session_succeeded();
unmarshal(session_status, ?legacy_session_failed(Failure)) ->
    ?session_failed(unmarshal(failure, Failure));

%% Adjustment change

unmarshal(adj_change, [2, [<<"created">>, Adjustment]]) ->
    ?adjustment_created(unmarshal(adj, Adjustment));
unmarshal(adj_change, [2, [<<"status_changed">>, Status]]) ->
    ?adjustment_status_changed(unmarshal(adj_status, Status));

unmarshal(adj_change, [1, ?legacy_adjustment_created(Adjustment)]) ->
    ?adjustment_created(unmarshal(adj, Adjustment));
unmarshal(adj_change, [1, ?legacy_adjustment_status_changed(Status)]) ->
    ?adjustment_status_changed(unmarshal(adj_status, Status));

%% Adjustment

unmarshal(adj, #{
    <<"id">>                    := ID,
    <<"created_at">>            := CreatedAt,
    <<"domain_revision">>       := Revision,
    <<"reason">>                := Reason,
    <<"old_cash_flow_inverse">> := OldCashFlowInverse,
    <<"new_cash_flow">>         := NewCashFlow
}) ->
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = ?adjustment_pending(),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal(OldCashFlowInverse),
        new_cash_flow         = hg_cashflow:unmarshal(NewCashFlow)
    };

unmarshal(adj,
    ?legacy_adjustment(ID, Status, CreatedAt, Revision, Reason, NewCashFlow, OldCashFlowInverse)
) ->
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = unmarshal(adj_status, Status),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal([1, OldCashFlowInverse]),
        new_cash_flow         = hg_cashflow:unmarshal([1, NewCashFlow])
    };

%% Adjustment status

unmarshal(adj_status, <<"pending">>) ->
    ?adjustment_pending();
unmarshal(adj_status, [<<"captured">>, At]) ->
    ?adjustment_captured(At);
unmarshal(adj_status, [<<"cancelled">>, At]) ->
    ?adjustment_cancelled(At);

unmarshal(adj_status, ?legacy_adjustment_pending()) ->
    ?adjustment_pending();
unmarshal(adj_status, ?legacy_adjustment_captured(At)) ->
    ?adjustment_captured(At);
unmarshal(adj_status, ?legacy_adjustment_cancelled(At)) ->
    ?adjustment_cancelled(At);

%% Payer

unmarshal(payer, #{
    <<"payment_tool">>  := PaymentTool,
    <<"session_id">>    := SessionId,
    <<"client_info">>   := ClientInfo,
    <<"contact_info">>  := ContractInfo
}) ->
    #domain_Payer{
        payment_tool    = hg_payment_tool:unmarshal(PaymentTool),
        session_id      = unmarshal(str, SessionId),
        client_info     = unmarshal(client_info, ClientInfo),
        contact_info    = unmarshal(contact_info, ContractInfo)
    };

unmarshal(payer, ?legacy_payer(PaymentTool, SessionId, ClientInfo, ContractInfo)) ->
    #domain_Payer{
        payment_tool    = hg_payment_tool:unmarshal([1, PaymentTool]),
        session_id      = unmarshal(str, SessionId),
        client_info     = unmarshal(client_info, ClientInfo),
        contact_info    = unmarshal(contact_info, ContractInfo)
    };

%% Client info

unmarshal(client_info, ?legacy_client_info(IpAddress, Fingerprint)) ->
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

unmarshal(client_info, ClientInfo) ->
    IpAddress = maps:get(<<"ip_address">>, ClientInfo, undefined),
    Fingerprint = maps:get(<<"fingerprint">>, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

%% Contract info

unmarshal(contact_info, ?legacy_contract_info(PhoneNumber, Email)) ->
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(contact_info, ContractInfo) ->
    PhoneNumber = maps:get(<<"phone_number">>, ContractInfo, undefined),
    Email = maps:get(<<"email">>, ContractInfo, undefined),
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(trx, #{
    <<"id">>    := ID,
    <<"extra">> := Extra
} = TRX) ->
    Timestamp = maps:get(<<"timestamp">>, TRX, undefined),
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(trx, ?legacy_trx(ID, Timestamp, Extra)) ->
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(interaction, #{<<"redirect">> := [<<"get_request">>, URI]}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, #{<<"redirect">> := [<<"post_request">>, #{
    <<"uri">>   := URI,
    <<"form">>  := Form
}]}) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};

unmarshal(interaction, ?legacy_get_request(URI)) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, ?legacy_post_request(URI, Form)) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};

unmarshal(failure, [2, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [2, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(failure, [1, ?legacy_operation_timeout()]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, ?legacy_external_failure(Code, Description)]) ->
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(on_hold_expiration, <<"cancel">>) ->
    cancel;
unmarshal(on_hold_expiration, <<"capture">>) ->
    capture;

unmarshal(on_hold_expiration, OnHoldExpiration) when is_atom(OnHoldExpiration) ->
    OnHoldExpiration;

unmarshal(risk_score, <<"low">>) ->
    low;
unmarshal(risk_score, <<"high">>) ->
    high;
unmarshal(risk_score, <<"fatal">>) ->
    fatal;

unmarshal(risk_score, RiskScore) when is_atom(RiskScore) ->
    RiskScore;

unmarshal(_, Other) ->
    Other.