-module(hg_invoice_adjustment).

-include("invoice_events.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export(
    [ create/2
    , capture/0
    , cancel/0
    ]).

-type adjustment()
    :: dmsl_domain_thrift:'InvoiceAdjustment'().

-type id()
    :: dmsl_domain_thrift:'InvoiceAdjustmentID'().
-type params()
    :: dmsl_payment_processing_thrift:'InvoiceAdjustmentParams'().
-type adjustment_state()
    :: dmsl_domain_thrift:'InvoiceAdjustmentState'().

-type captured()
    :: {captured, dmsl_domain_thrift:'InvoiceAdjustmentCaptured'()}.
-type cancelled()
    :: {cancelled, dmsl_domain_thrift:'InvoiceAdjustmentCancelled'()}.

-type result()
    :: {[change()], action()}.

-type change()
    :: dmsl_payment_processing_thrift:'InvoiceAdjustmentChangePayload'().
-type action()
    :: hg_machine_action:t().

% API

-spec create(id(), params()) ->
    {adjustment(), result()}.
create(ID, Params) ->
    Adjustment = build_adjustment(ID, Params),
    Changes = [?invoice_adjustment_created(Adjustment)],
    Action = hg_machine_action:instant(),
    {Adjustment, {Changes, Action}}.

-spec capture() ->
    {ok, result()}.
capture() ->
    AdjustmentTarget = build_adjustment_target(captured),
    Changes = [?invoice_adjustment_status_changed(AdjustmentTarget)],
    Action = hg_machine_action:new(),
    {ok, {Changes, Action}}.

-spec cancel() ->
    {ok, result()}.
cancel() ->
    AdjustmentTarget = build_adjustment_target(cancelled),
    Changes = [?invoice_adjustment_status_changed(AdjustmentTarget)],
    Action = hg_machine_action:new(),
    {ok, {Changes, Action}}.

% Internal

-spec build_adjustment(id(), params()) ->
    adjustment().
build_adjustment(ID, Params) ->
    #domain_InvoiceAdjustment{
        id = ID,
        reason = Params#payproc_InvoiceAdjustmentParams.reason,
        status = {pending, #domain_InvoiceAdjustmentPending{}},
        created_at = hg_datetime:format_now(),
        domain_revision = hg_domain:head(),
        state = build_adjustment_state(Params)
    }.

-spec build_adjustment_state(params()) ->
    adjustment_state().
build_adjustment_state(Params) ->
    case Params#payproc_InvoiceAdjustmentParams.scenario of
        {status_change, StatusChange} ->
            {status_change, #domain_InvoiceAdjustmentStatusChangeState{scenario = StatusChange}}
    end.

-spec build_adjustment_target(captured)  -> captured();
                             (cancelled) -> cancelled().
build_adjustment_target(captured) ->
    {captured, #domain_InvoiceAdjustmentCaptured{at = hg_datetime:format_now()}};
build_adjustment_target(cancelled) ->
    {cancelled, #domain_InvoiceAdjustmentCancelled{at = hg_datetime:format_now()}}.
