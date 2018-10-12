-module(hg_invoice_repair).
-include("domain.hrl").
-include("payment_events.hrl").
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([check_for_action/2]).
-export([get_repair_changes/2]).

%% dmsl types

-type risk_score()      :: dmsl_domain_thrift:'RiskScore'().
-type proxy_result()    :: dmsl_proxy_provider_thrift:'PaymentProxyResult'().
-type change()          :: dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().
-type scenario()        :: dmsl_payment_processing_thrift:'InvoiceRepairScenario'().

%% user types

-type action_type() ::
    fail_pre_processing |
    skip_inspector |
    fail_adapter.

-type scenario_result() :: call |
    hg_invoice_payment:machine_result() |
    proxy_result() |
    risk_score().

%% exported types

-export_type([scenario/0]).
-export_type([action_type/0]).

%% Repair

-spec check_for_action(action_type(), scenario()) -> call | {result, scenario_result()}.
check_for_action(Type, Scenario) when is_list(Scenario) ->
    Map = lists:foldl(fun (Sc, Result) ->
        case check_for_action(Type, Sc) of
            call ->
                Result;
            SomeResult ->
                Result#{result => SomeResult}
        end
    end, #{}, Scenario),
    case maps:is_key(result, Map) of
        true ->
            maps:get(result, Map);
        false ->
            call
    end;

%% Story parts

check_for_action(ActionType, #payproc_InvoiceRepairComplex{scenarios = Scenarios}) ->
    check_for_action(ActionType, Scenarios);
check_for_action(fail_pre_processing, #payproc_InvoiceRepairFailPreProcessing{failure = Failure}) ->
    {result, {done, {[?payment_status_changed(?failed(Failure))], hg_machine_action:instant()}}};
check_for_action(skip_inspector, #payproc_InvoiceRepairSkipInspector{risk_score = RiskScore}) ->
    {result, RiskScore};
check_for_action(fail_adapter, #payproc_InvoiceRepairFailAdapter{failure = Failure}) ->
    ProxyResult = #prxprv_PaymentProxyResult{intent = {mysteryIntent,
        #'prxprv_FinishIntent'{status = {failure, Failure}}}},
    {result, ProxyResult};
check_for_action(_Type, _Scenario) ->
    call.

%% create repair event

-spec get_repair_changes(scenario(), hg_invoice_payment:activity()) -> change().

get_repair_changes(Scenario, Activity) ->
    check_activity_compatibility(Scenario, Activity),
    ?payment_new_repair(Scenario).

check_activity_compatibility(Scenario, Activity) when is_list(Scenario) ->
    lists:foreach(fun (Sc) -> check_activity_compatibility(Sc, Activity) end, Scenario);
check_activity_compatibility(#payproc_InvoiceRepairComplex{scenarios = Scenarios}, Activity) ->
    check_activity_compatibility(Scenarios, Activity);
check_activity_compatibility(#payproc_InvoiceRepairFailPreProcessing{}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} orelse
         Activity =:= {payment, routing} orelse
         Activity =:= {payment, cash_flow_building} ->
    ok;
check_activity_compatibility(#payproc_InvoiceRepairSkipInspector{}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} ->
    ok;
check_activity_compatibility(#payproc_InvoiceRepairFailAdapter{}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} orelse
         Activity =:= {payment, routing} orelse
         Activity =:= {payment, cash_flow_building} orelse
         Activity =:= {payment, processing_session} ->
    ok;
check_activity_compatibility(#payproc_InvoiceRepairFailAdapter{}, {refund_session, _}) ->
    ok;
check_activity_compatibility(Scenario, Activity) ->
    throw({exception, {activity_not_compatible_with_scenario, Activity, Scenario}}).

