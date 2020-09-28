-module(hg_invoice_adjustment).

-include("invoice_events.hrl").

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export(
    [
        create/2,
        create/3,
        capture/0,
        capture/1,
        cancel/0,
        cancel/1
    ]
).

-export([get_log_params/1]).

-type adjustment() ::
    dmsl_domain_thrift:'InvoiceAdjustment'().

-type id() ::
    dmsl_domain_thrift:'InvoiceAdjustmentID'().

-type params() ::
    dmsl_payment_processing_thrift:'InvoiceAdjustmentParams'().

-type adjustment_state() ::
    dmsl_domain_thrift:'InvoiceAdjustmentState'().

-type captured() ::
    {captured, dmsl_domain_thrift:'InvoiceAdjustmentCaptured'()}.

-type cancelled() ::
    {cancelled, dmsl_domain_thrift:'InvoiceAdjustmentCancelled'()}.

-type result() ::
    {[change()], action()}.

-type change() ::
    dmsl_payment_processing_thrift:'InvoiceAdjustmentChangePayload'().

-type action() ::
    hg_machine_action:t().

-type log_params() :: #{
    type := invoice_adjustment_event,
    params := list(),
    message := string()
}.

% API

-spec create(id(), params()) -> {adjustment(), result()}.
create(ID, Params) ->
    Timestamp = hg_datetime:format_now(),
    create(ID, Params, Timestamp).

-spec create(id(), params(), hg_datetime:timestamp()) -> {adjustment(), result()}.
create(ID, Params, Timestamp) ->
    Adjustment = build_adjustment(ID, Params, Timestamp),
    Changes = [?invoice_adjustment_created(Adjustment)],
    Action = hg_machine_action:instant(),
    {Adjustment, {Changes, Action}}.

-spec capture() -> {ok, result()}.
capture() ->
    capture(hg_datetime:format_now()).

-spec capture(hg_datetime:timestamp()) -> {ok, result()}.
capture(Timestamp) ->
    AdjustmentTarget = build_adjustment_target(captured, Timestamp),
    Changes = [?invoice_adjustment_status_changed(AdjustmentTarget)],
    Action = hg_machine_action:new(),
    {ok, {Changes, Action}}.

-spec cancel() -> {ok, result()}.
cancel() ->
    cancel(hg_datetime:format_now()).

-spec cancel(hg_datetime:timestamp()) -> {ok, result()}.
cancel(Timestamp) ->
    AdjustmentTarget = build_adjustment_target(cancelled, Timestamp),
    Changes = [?invoice_adjustment_status_changed(AdjustmentTarget)],
    Action = hg_machine_action:new(),
    {ok, {Changes, Action}}.

-spec get_log_params(change()) -> {ok, log_params()} | undefined.
get_log_params(Change) ->
    do_get_log_params(Change).

% Internal

-spec build_adjustment(id(), params(), hg_datetime:timestamp()) -> adjustment().
build_adjustment(ID, Params, Timestamp) ->
    #domain_InvoiceAdjustment{
        id = ID,
        reason = Params#payproc_InvoiceAdjustmentParams.reason,
        status = {pending, #domain_InvoiceAdjustmentPending{}},
        created_at = Timestamp,
        domain_revision = hg_domain:head(),
        state = build_adjustment_state(Params)
    }.

-spec build_adjustment_state(params()) -> adjustment_state().
build_adjustment_state(Params) ->
    case Params#payproc_InvoiceAdjustmentParams.scenario of
        {status_change, StatusChange} ->
            {status_change, #domain_InvoiceAdjustmentStatusChangeState{scenario = StatusChange}}
    end.

-spec build_adjustment_target
    (captured, hg_datetime:timestamp()) -> captured();
    (cancelled, hg_datetime:timestamp()) -> cancelled().
build_adjustment_target(captured, Timestamp) ->
    {captured, #domain_InvoiceAdjustmentCaptured{at = Timestamp}};
build_adjustment_target(cancelled, Timestamp) ->
    {cancelled, #domain_InvoiceAdjustmentCancelled{at = Timestamp}}.

-spec do_get_log_params(change()) -> {ok, log_params()} | undefined.
do_get_log_params(?invoice_adjustment_created(Adjustment)) ->
    Params = #{
        adjustment => Adjustment,
        event_type => invoice_adjustment_created
    },
    make_log_params(Params);
do_get_log_params(?invoice_adjustment_status_changed(Status)) ->
    Params = #{
        adjustment_status => Status,
        event_type => invoice_adjustment_status_changed
    },
    make_log_params(Params);
do_get_log_params(_) ->
    undefined.

-spec make_log_params(Params :: map()) -> {ok, log_params()}.
make_log_params(#{event_type := EventType} = Params) ->
    LogParams = maps:fold(
        fun(K, V, Acc) ->
            build_log_param(K, V) ++ Acc
        end,
        [],
        Params
    ),
    Message = get_log_message(EventType),
    {ok, #{
        type => invoice_adjustment_event,
        message => Message,
        params => LogParams
    }}.

-spec build_log_param(atom(), term()) -> [{atom(), term()}].
build_log_param(adjustment, #domain_InvoiceAdjustment{} = Adjustment) ->
    [
        {id, Adjustment#domain_InvoiceAdjustment.id},
        {reason, Adjustment#domain_InvoiceAdjustment.reason}
        | build_log_param(state, Adjustment#domain_InvoiceAdjustment.state)
    ];
build_log_param(state, {status_change, State}) ->
    Scenario = State#domain_InvoiceAdjustmentStatusChangeState.scenario,
    TargetStatus = Scenario#domain_InvoiceAdjustmentStatusChange.target_status,
    build_log_param(target_status, TargetStatus);
build_log_param(adjustment_status, {Status, _Details}) ->
    [{status, Status}];
build_log_param(target_status, {Status, _Details}) ->
    [{status, Status}];
build_log_param(_Key, _Value) ->
    [].

-spec get_log_message(EventType) -> string() when
    EventType ::
        invoice_adjustment_created |
        invoice_adjustment_status_changed.
get_log_message(invoice_adjustment_created) ->
    "Invoice adjustment created";
get_log_message(invoice_adjustment_status_changed) ->
    "Invoice adjustment status changed".
