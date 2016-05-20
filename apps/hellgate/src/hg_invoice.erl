-module(hg_invoice).

%% Public

-export([create/3]).
-export([get/3]).
-export([get_events/4]).
-export([start_payment/4]).
-export([fulfill/4]).
-export([void/4]).

%% Machine callbacks

-behaviour(hg_machine).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

create(UserInfo, InvoiceParams, Opts) ->
    hg_dispatcher:start_machine(?MODULE, {InvoiceParams, UserInfo}, Opts).

get(UserInfo, InvoiceID, Opts) ->
    % call_machine(InvoiceID, {get, UserInfo}, Opts).
    error(noimpl).

get_events(UserInfo, InvoiceID, Range, Opts) ->
    % call_machine(InvoiceID, {get_events, Range, UserInfo}, Opts).
    error(noimpl).

start_payment(UserInfo, InvoiceID, PaymentParams, Opts) ->
    hg_dispatcher:call_machine(?MODULE, InvoiceID, {start_payment, PaymentParams, UserInfo}, Opts).

fulfill(UserInfo, InvoiceID, Reason, Opts) ->
    hg_dispatcher:call_machine(?MODULE, InvoiceID, {fulfill, Reason, UserInfo}, Opts).

void(UserInfo, InvoiceID, Reason, Opts) ->
    hg_dispatcher:call_machine(?MODULE, InvoiceID, {void, Reason, UserInfo}, Opts).

%%

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").
-include("hg_duration.hrl").

init(ID, {_UserInfo, InvoiceParams}) ->
    Invoice = create_invoice(ID, InvoiceParams),
    Event = emit_invoice_changed_event(Invoice),
    {ok, {Event, hg_action:set_timeout(?MINUTE)}}.

%%

process_signal(timeout, History) ->
    {Invoice, Payments} = collapse_history(History),
    case {get_invoice_status(Invoice), Payments} of
        {unpaid, []} ->
            % expired
            try_change_status(fun (I) -> cancel_invoice(overdue, I) end, Invoice);
        {unpaid, [Payment]} ->
            % payment succeeded
            PaymentNext = set_payment_status(succeeded, Payment),
            InvoiceNext = set_invoice_status(paid, Invoice),
            Events = [
                emit_payment_changed_event(PaymentNext),
                emit_invoice_changed_event(InvoiceNext)
            ],
            {ok, {Events, hg_action:new()}};
        _ ->
            % nothing specific
            Event = emit_invoice_changed_event(Invoice),
            {ok, {Event, hg_action:new()}}
    end;

process_signal({repair, _}, History) ->
    {Invoice, _} = collapse_history(History),
    Event = emit_invoice_changed_event(Invoice),
    {ok, {Event, hg_action:set_timeout(?MINUTE)}}.

%%

process_call({start_payment, PaymentParams, _UserInfo}, History) ->
    {Invoice, Payments} = collapse_history(History),
    case {get_invoice_status(Invoice), Payments} of
        {unpaid, []} ->
            Payment = create_payment(PaymentParams, Invoice),
            Event = emit_payment_changed_event(Payment),
            Response = {ok, get_payment_id(Payment)},
            {ok, Response, {Event, hg_action:set_timeout(?seconds(5))}};
        {unpaid, [Payment]} ->
            % TODO: need something like 'payment pending'
            [Payment] = Payments,
            Event = emit_payment_changed_event(Payment),
            Response = {ok, get_payment_id(Payment)},
            {ok, Response, {Event, hg_action:set_timeout(?seconds(5))}};
        _ ->
            Event = emit_invoice_changed_event(Invoice),
            Response = raise_invalid_invoice_status(Invoice),
            {ok, Response, {Event, hg_action:new()}}
    end;

process_call({fulfill, Reason, _UserInfo}, History) ->
    {Invoice, _} = collapse_history(History),
    try_change_status(fun (I) -> fulfill_invoice(Reason, I) end, Invoice);

process_call({void, Reason, _UserInfo}, History) ->
    {Invoice, _} = collapse_history(History),
    try_change_status(fun (I) -> cancel_invoice({void, Reason}, I) end, Invoice).

try_change_status(WithFun, Invoice) ->
    case WithFun(Invoice) of
        {ok, InvoiceNext} ->
            Event = emit_invoice_changed_event(InvoiceNext),
            {ok, ok, {Event, hg_action:new()}};
        error ->
            Event = emit_invoice_changed_event(Invoice),
            Response = raise_invalid_invoice_status(Invoice),
            {ok, Response, {Event, hg_action:new()}}
    end.

%%

create_invoice(ID, V = #'InvoiceParams'{}) ->
    #'Invoice'{
        id          = ID,
        status      = unpaid,
        product     = V#'InvoiceParams'.product,
        description = V#'InvoiceParams'.description,
        cost        = V#'InvoiceParams'.cost,
        context     = V#'InvoiceParams'.context
    }.

create_payment(V = #'InvoicePaymentParams'{}, Invoice) ->
    #'InvoicePayment'{
        id           = create_payment_id(Invoice),
        status       = pending,
        payer        = V#'InvoicePaymentParams'.payer,
        payment_tool = V#'InvoicePaymentParams'.payment_tool,
        session      = V#'InvoicePaymentParams'.session
    }.

create_payment_id(Invoice) ->
    ID = get_invoice_id(Invoice),
    <<ID/binary, ":", "0">>.

get_invoice_id(#'Invoice'{id = ID}) ->
    ID.

get_invoice_status(#'Invoice'{status = Status}) ->
    Status.
set_invoice_status(Status, V = #'Invoice'{}) ->
    V#'Invoice'{status = Status}.

get_payment_id(#'InvoicePayment'{id = ID}) ->
    ID.

set_payment_status(Status, V = #'InvoicePayment'{}) ->
    V#'InvoicePayment'{status = Status}.

cancel_invoice(Reason, V = #'Invoice'{status = unpaid}) ->
    {ok, V#'Invoice'{status = cancelled, details = format_reason(Reason)}};
cancel_invoice(_Reason, _Invoice) ->
    error.

fulfill_invoice(Reason, V = #'Invoice'{status = paid}) ->
    {ok, V#'Invoice'{status = fulfilled, details = format_reason(Reason)}};
fulfill_invoice(_Reason, _Invoice) ->
    error.

emit_payment_changed_event(Payment) ->
    #'InvoicePaymentStatusChanged'{payment = Payment}.

emit_invoice_changed_event(Invoice) ->
    #'InvoiceStatusChanged'{invoice = Invoice}.

raise_invalid_invoice_status(Invoice) ->
    {exception, #'InvalidInvoiceStatus'{status = get_invoice_status(Invoice)}}.

collapse_history(History) ->
    lists:foldl(fun merge_history/2, {undefined, []}, History).

merge_history(#'InvoicePaymentStatusChanged'{payment = Payment}, {Invoice, Payments}) ->
    {Invoice, lists:keystore(get_payment_id(Payment), #'InvoicePayment'.id, Payments, Payment)};
merge_history(#'InvoiceStatusChanged'{invoice = Invoice}, {_WasInvoice, Payments}) ->
    {Invoice, Payments};
merge_history(_, Acc) ->
    Acc.

%% TODO: fix this dirty hack
format_reason({Pre, V}) ->
    genlib:format("~s: ~s", [Pre, genlib:to_binary(V)]);
format_reason(V) ->
    genlib:to_binary(V).
