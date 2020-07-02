%%% Invoice adjustment submachine

-module(hg_invoice_adjustment).

-export([create/4]).
-export([capture/3]).
-export([cancel/3]).

-type invoice_state()
    :: hg_invoice:st().

% -spec create_adjustment(hg_datetime:timestamp(), adjustment_params(), invoice_state(), opts()) ->
%     {adjustment(), result()}.
create_adjustment(Timestamp, Params, InvoiceState, Opts) ->
    _ = assert_no_adjustment_pending(InvoiceState),
    create_adjustment_(Timestamp, Params, InvoiceState, Opts).

% -spec capture_adjustment(adjustment_id(), st(), opts()) ->
%     {ok, result()}.
capture_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, capture, St, Options).

% -spec cancel_adjustment(adjustment_id(), st(), opts()) ->
%     {ok, result()}.
cancel_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, cancel, St, Options).

% -spec finalize_adjustment(adjustment_id(), capture | cancel, st(), opts()) ->
%     {ok, result()}.
finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(processed, Adjustment),
    ok = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
    Status = ok,
    % case Intent of
    %     capture ->
    %         ?adjustment_captured(hg_datetime:format_now());
    %     cancel ->
    %         ?adjustment_cancelled(hg_datetime:format_now())
    % end,
    Event = ?adjustment_ev(ID, ?adjustment_status_changed(Status)),
    {ok, {[Event], hg_machine_action:new()}}.



assert_no_adjustment_pending(#st{adjustments = As}) ->
    lists:foreach(fun assert_adjustment_finalized/1, As).

assert_adjustment_finalized(#domain_InvoicePaymentAdjustment{id = ID, status = {Status, _}}) when
    Status =:= pending;
    Status =:= processed
->
    throw(#payproc_InvoicePaymentAdjustmentPending{id = ID});
assert_adjustment_finalized(_) ->
    ok.
