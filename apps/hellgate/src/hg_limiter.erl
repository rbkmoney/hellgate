-module(hg_limiter).

-include("domain.hrl").
-include_lib("limiter_proto/include/lim_limiter_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type turnover_selector() :: dmsl_domain_thrift:'TurnoverLimitSelector'().
-type turnover_limit() :: dmsl_domain_thrift:'TurnoverLimit'().
-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type route() :: dmsl_domain_thrift:'PaymentRoute'().
-type refund() :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type cash() :: dmsl_domain_thrift:'Cash'().

-export([get_turnover_limits/1]).
-export([check_limits/3]).
-export([hold_payment_limits/4]).
-export([hold_refund_limits/4]).
-export([commit_payment_limits/5]).
-export([commit_refund_limits/4]).
-export([rollback_payment_limits/4]).
-export([rollback_refund_limits/4]).

-spec get_turnover_limits(turnover_selector() | undefined) -> [turnover_limit()].
get_turnover_limits(undefined) ->
    logger:info("Operation limits haven't been set on provider terms."),
    [];
get_turnover_limits({value, Limits}) ->
    Limits;
get_turnover_limits(Ambiguous) ->
    error({misconfiguration, {'Could not reduce selector to a value', Ambiguous}}).

-spec check_limits([turnover_limit()], invoice(), payment()) ->
    {ok, [hg_limiter_client:limit()]}
    | {error, {limit_overflow, [binary()]}}.
check_limits(TurnoverLimits, Invoice, Payment) ->
    Context = gen_limit_context(Invoice, Payment),
    try
        check_limits_(TurnoverLimits, Context, [])
    catch
        throw:limit_overflow ->
            IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
            {error, {limit_overflow, IDs}}
    end.

check_limits_([], _, Limits) ->
    {ok, Limits};
check_limits_([T | TurnoverLimits], Context, Acc) ->
    #domain_TurnoverLimit{id = LimitID} = T,
    Clock = get_latest_clock(),
    Limit = hg_limiter_client:get(LimitID, Clock, Context),
    #limiter_Limit{
        amount = LimiterAmount
    } = Limit,
    UpperBoundary = T#domain_TurnoverLimit.upper_boundary,
    case LimiterAmount < UpperBoundary of
        true ->
            check_limits_(TurnoverLimits, Context, [Limit | Acc]);
        false ->
            logger:info("Limit with id ~p overflowed, amount ~p upper boundary ~p", [
                LimitID,
                LimiterAmount,
                UpperBoundary
            ]),
            throw(limit_overflow)
    end.

-spec hold_payment_limits([turnover_limit()], route(), invoice(), payment()) -> ok.
hold_payment_limits(TurnoverLimits, Route, Invoice, Payment) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_payment_changes(IDs, Route, Invoice, Payment),
    Context = gen_limit_context(Invoice, Payment),
    hold(LimitChanges, get_latest_clock(), Context).

-spec hold_refund_limits([turnover_limit()], invoice(), payment(), refund()) -> ok.
hold_refund_limits(TurnoverLimits, Invoice, Payment, Refund) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_refund_changes(IDs, Invoice, Payment, Refund),
    Context = gen_limit_refund_context(Invoice, Payment, Refund),
    hold(LimitChanges, get_latest_clock(), Context).

-spec commit_payment_limits([turnover_limit()], route(), invoice(), payment(), cash() | undefined) -> ok.
commit_payment_limits(TurnoverLimits, Route, Invoice, Payment, CapturedCash) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_payment_changes(IDs, Route, Invoice, Payment),
    Context = gen_limit_context(Invoice, Payment, CapturedCash),
    commit(LimitChanges, get_latest_clock(), Context).

-spec commit_refund_limits([turnover_limit()], invoice(), payment(), refund()) -> ok.
commit_refund_limits(TurnoverLimits, Invoice, Payment, Refund) ->
    IDs = [T#domain_TurnoverLimit.id || T <- TurnoverLimits],
    LimitChanges = gen_limit_refund_changes(IDs, Invoice, Payment, Refund),
    Context = gen_limit_refund_context(Invoice, Payment, Refund),
    commit(LimitChanges, get_latest_clock(), Context).

-spec rollback_payment_limits([turnover_limit()], route(), invoice(), payment()) -> ok.
rollback_payment_limits(TurnoverLimits, Route, Invoice, Payment) ->
    #domain_InvoicePayment{cost = Cash} = Payment,
    CapturedCash = Cash#domain_Cash{
        amount = 0
    },
    commit_payment_limits(TurnoverLimits, Route, Invoice, Payment, CapturedCash).

-spec rollback_refund_limits([turnover_limit()], invoice(), payment(), refund()) -> ok.
rollback_refund_limits(TurnoverLimits, Invoice, Payment, Refund0) ->
    Refund1 = set_refund_amount(0, Refund0),
    commit_refund_limits(TurnoverLimits, Invoice, Payment, Refund1).

-spec hold([hg_limiter_client:limit_change()], hg_limiter_client:clock(), hg_limiter_client:context()) -> ok.
hold(LimitChanges, Clock, Context) ->
    lists:foreach(
        fun(LimitChange) ->
            hg_limiter_client:hold(LimitChange, Clock, Context)
        end,
        LimitChanges
    ).

-spec commit([hg_limiter_client:limit_change()], hg_limiter_client:clock(), hg_limiter_client:context()) -> ok.
commit(LimitChanges, Clock, Context) ->
    lists:foreach(
        fun(LimitChange) ->
            hg_limiter_client:commit(LimitChange, Clock, Context)
        end,
        LimitChanges
    ).

gen_limit_context(Invoice, Payment) ->
    gen_limit_context(Invoice, Payment, undefined).

gen_limit_context(Invoice, Payment, CapturedCash) ->
    InvoiceCtx = marshal(invoice, Invoice),
    PaymentCtx0 = marshal(payment, Payment),

    PaymentCtx1 = PaymentCtx0#limiter_context_InvoicePayment{
        capture_cost = maybe_marshal(cost, CapturedCash)
    },
    #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice_payment, #limiter_context_PaymentProcessingOperationInvoicePayment{}},
            invoice = InvoiceCtx#limiter_context_Invoice{effective_payment = PaymentCtx1}
        }
    }.

gen_limit_refund_context(Invoice, Payment, Refund) ->
    InvoiceCtx = marshal(invoice, Invoice),
    PaymentCtx0 = marshal(payment, Payment),
    RefundCtx = marshal(refund, Refund),

    PaymentCtx1 = PaymentCtx0#limiter_context_InvoicePayment{
        effective_refund = RefundCtx
    },
    #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice_payment_refund, #limiter_context_PaymentProcessingOperationInvoicePaymentRefund{}},
            invoice = InvoiceCtx#limiter_context_Invoice{effective_payment = PaymentCtx1}
        }
    }.

gen_limit_payment_changes(LimitIDs, Route, Invoice, Payment) ->
    [
        #limiter_LimitChange{
            id = ID,
            change_id = construct_limit_change_id(ID, Route, Invoice, Payment)
        }
     || ID <- LimitIDs
    ].

gen_limit_refund_changes(LimitIDs, Invoice, Payment, Refund) ->
    [
        #limiter_LimitChange{
            id = ID,
            change_id = construct_limit_refund_change_id(ID, Invoice, Payment, Refund)
        }
     || ID <- LimitIDs
    ].

construct_limit_change_id(LimitID, Route, Invoice, Payment) ->
    ?route(ProviderRef, TerminalRef) = Route,
    ComplexID = hg_utils:construct_complex_id([
        LimitID,
        genlib:to_binary(get_provider_id(ProviderRef)),
        genlib:to_binary(get_terminal_id(TerminalRef)),
        get_invoice_id(Invoice),
        get_payment_id(Payment)
    ]),
    genlib_string:join($., [<<"limiter">>, ComplexID]).

construct_limit_refund_change_id(LimitID, Invoice, Payment, Refund) ->
    ComplexID = hg_utils:construct_complex_id([
        LimitID,
        get_invoice_id(Invoice),
        get_payment_id(Payment),
        {refund_session, get_refund_id(Refund)}
    ]),
    genlib_string:join($., [<<"limiter">>, ComplexID]).

get_provider_id(#domain_ProviderRef{id = ID}) ->
    ID.

get_terminal_id(#domain_TerminalRef{id = ID}) ->
    ID.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

get_refund_id(#domain_InvoicePaymentRefund{id = ID}) ->
    ID.

get_latest_clock() ->
    {latest, #limiter_LatestClock{}}.

set_refund_amount(Amount, Refund) ->
    IFDefined = fun
        (undefined, _) ->
            undefined;
        (Value, Fun) ->
            Fun(Value)
    end,
    #domain_InvoicePaymentRefund{
        cash = Cash,
        cart = InvoiceCart
    } = Refund,
    Refund#domain_InvoicePaymentRefund{
        cash = IFDefined(Cash, fun(C) -> set_cash_amount(Amount, C) end),
        cart = IFDefined(InvoiceCart, fun(Cart) ->
            #domain_InvoiceCart{lines = Lines} = Cart,
            lists:foldl(
                fun(Line, Acc) ->
                    #domain_InvoiceLine{price = Price} = Line,
                    Line1 = Line#domain_InvoiceLine{
                        price = set_cash_amount(Amount, Price)
                    },
                    [Line1 | Acc]
                end,
                [],
                Lines
            )
        end)
    }.

set_cash_amount(Amount, Cash) ->
    Cash#domain_Cash{amount = Amount}.

marshal(invoice, Invoice) ->
    #limiter_context_Invoice{
        id = Invoice#domain_Invoice.id,
        owner_id = Invoice#domain_Invoice.owner_id,
        shop_id = Invoice#domain_Invoice.shop_id,
        cost = marshal(cost, Invoice#domain_Invoice.cost),
        created_at = Invoice#domain_Invoice.created_at
    };
marshal(payment, Payment) ->
    #limiter_context_InvoicePayment{
        id = Payment#domain_InvoicePayment.id,
        owner_id = Payment#domain_InvoicePayment.owner_id,
        shop_id = Payment#domain_InvoicePayment.shop_id,
        cost = marshal(cost, Payment#domain_InvoicePayment.cost),
        created_at = Payment#domain_InvoicePayment.created_at,
        flow = marshal(flow, Payment#domain_InvoicePayment.flow),
        payer = marshal(payer, Payment#domain_InvoicePayment.payer)
    };
marshal(refund, Refund) ->
    #limiter_context_InvoicePaymentRefund{
        id = Refund#domain_InvoicePaymentRefund.id,
        cost = marshal(cost, Refund#domain_InvoicePaymentRefund.cash),
        created_at = Refund#domain_InvoicePaymentRefund.created_at
    };
marshal(cost, Cost) ->
    #limiter_base_Cash{
        amount = Cost#domain_Cash.amount,
        currency = marshal(currency, Cost#domain_Cash.currency)
    };
marshal(currency, #domain_CurrencyRef{symbolic_code = Currency}) ->
    #limiter_base_CurrencyRef{
        symbolic_code = Currency
    };
marshal(flow, Flow) ->
    case Flow of
        {instant, #domain_InvoicePaymentFlowInstant{}} ->
            {instant, #limiter_context_InvoicePaymentFlowInstant{}};
        {hold, #domain_InvoicePaymentFlowHold{}} ->
            {hold, #limiter_context_InvoicePaymentFlowHold{}}
    end;
marshal(payer, Payer) ->
    case Payer of
        {payment_resource, #domain_PaymentResourcePayer{}} ->
            {payment_resource, #limiter_context_PaymentResourcePayer{}};
        {customer, #domain_CustomerPayer{}} ->
            {customer, #limiter_context_CustomerPayer{}};
        {recurrent, #domain_RecurrentPayer{}} ->
            {recurrent, #limiter_context_RecurrentPayer{}}
    end.

maybe_marshal(_, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).
