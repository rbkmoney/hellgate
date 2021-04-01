-module(hg_invoice_repair).

-include("domain.hrl").
-include("payment_events.hrl").

-include_lib("damsel/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([check_for_action/2]).
-export([get_repair_state/3]).

%% dmsl types

-type risk_score() :: dmsl_domain_thrift:'RiskScore'().
-type proxy_result() :: dmsl_proxy_provider_thrift:'PaymentProxyResult'().
-type scenario() :: dmsl_payment_processing_thrift:'InvoiceRepairScenario'().

%% user types

-type action_type() ::
    fail_pre_processing
    | skip_inspector
    | fail_session.

-type scenario_result() ::
    hg_invoice_payment:machine_result()
    | proxy_result()
    | risk_score().

%% exported types

-export_type([scenario/0]).
-export_type([action_type/0]).

%% Repair

-spec check_for_action(action_type(), scenario()) -> call | {result, scenario_result()}.
check_for_action(ActionType, {complex, #payproc_InvoiceRepairComplex{scenarios = Scenarios}}) ->
    check_complex_list(ActionType, Scenarios);
check_for_action(
    fail_pre_processing,
    {fail_pre_processing, #payproc_InvoiceRepairFailPreProcessing{failure = Failure}}
) ->
    {result, {done, {[?payment_status_changed(?failed({failure, Failure}))], hg_machine_action:instant()}}};
check_for_action(skip_inspector, {skip_inspector, #payproc_InvoiceRepairSkipInspector{risk_score = RiskScore}}) ->
    {result, RiskScore};
check_for_action(fail_session, {fail_session, #payproc_InvoiceRepairFailSession{failure = Failure}}) ->
    ProxyResult = #prxprv_PaymentProxyResult{intent = {finish, #'prxprv_FinishIntent'{status = {failure, Failure}}}},
    {result, ProxyResult};
check_for_action(_Type, _Scenario) ->
    call.

check_complex_list(_ActionType, []) ->
    call;
check_complex_list(ActionType, [Scenario | Rest]) ->
    case check_for_action(ActionType, Scenario) of
        {result, _Value} = Result ->
            Result;
        call ->
            check_complex_list(ActionType, Rest)
    end.

%% create repair event

-spec get_repair_state(hg_invoice_payment:activity(), scenario(), hg_invoice_payment:st()) -> hg_invoice_payment:st().
get_repair_state(Activity, Scenario, St) ->
    check_activity_compatibility(Scenario, Activity),
    hg_invoice_payment:set_repair_scenario(Scenario, St).

check_activity_compatibility({complex, #payproc_InvoiceRepairComplex{scenarios = Scenarios}}, Activity) ->
    lists:foreach(fun(Sc) -> check_activity_compatibility(Sc, Activity) end, Scenarios);
check_activity_compatibility({fail_pre_processing, #payproc_InvoiceRepairFailPreProcessing{}}, Activity) when
    Activity =:= {payment, new} orelse
        Activity =:= {payment, risk_scoring} orelse
        Activity =:= {payment, routing} orelse
        Activity =:= {payment, cash_flow_building}
->
    ok;
check_activity_compatibility({skip_inspector, #payproc_InvoiceRepairSkipInspector{}}, Activity) when
    Activity =:= {payment, new} orelse
        Activity =:= {payment, risk_scoring}
->
    ok;
check_activity_compatibility({fail_session, #payproc_InvoiceRepairFailSession{}}, Activity) when
    Activity =:= {payment, processing_session}
->
    ok;
check_activity_compatibility({fail_session, #payproc_InvoiceRepairFailSession{}}, {refund_session, _}) ->
    ok;
check_activity_compatibility(Scenario, Activity) ->
    throw({exception, {activity_not_compatible_with_scenario, Activity, Scenario}}).
