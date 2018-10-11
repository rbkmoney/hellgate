-module(hg_invoice_repair).
-include("domain.hrl").
-include("payment_events.hrl").
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([check_for_action/2]).
-export([get_repair_changes/2]).

%% dmsl types

-type risk_score()      :: dmsl_domain_thrift:'RiskScore'().
-type failure()         :: dmsl_domain_thrift:'Failure'().
-type proxy_result()    :: dmsl_proxy_provider_thrift:'PaymentProxyResult'().
-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().

%% user types

-type action_type() ::
    fail_pre_processing |
    skip_inspector |
    fail_adapter.

-type scenario_result() ::
    risk_score().

-type scenario() ::
    {fail_pre_processing, failure()} |
    {skip_inspector, risk_score()} |
    {fail_adapter, proxy_result()}.

%% exported types

-export_type([scenario/0]).
-export_type([action_type/0]).

%% Repair

-spec check_for_action(action_type(), [scenario()] | scenario()) -> call | {result, scenario_result()}.
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

check_for_action(fail_pre_processing, {fail_pre_processing, Failure}) ->
    {result, {done, {[?payment_status_changed(?failed(Failure))], hg_machine_action:instant()}}};
check_for_action(skip_inspector, {skip_inspector, RiskScore}) ->
    {result, RiskScore};
check_for_action(fail_adapter, {fail_adapter, ProxyResult}) ->
    {result, ProxyResult};
check_for_action(_Type, _Scenario) ->
    call.

%% create repair event

-spec get_repair_changes([scenario()], hg_invoice_payment:activity()) -> change().

get_repair_changes(ScenarioData0, Activity) ->
    ScenarioData = scenario_to_list(ScenarioData0),
    lists:foreach(fun (Sc) ->
        check_activity_compatibility(Sc, Activity)
    end, ScenarioData),
    ?payment_new_repair(ScenarioData).

check_activity_compatibility({fail_pre_processing, _Args}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} orelse
         Activity =:= {payment, routing} orelse
         Activity =:= {payment, cash_flow_building} ->
    ok;
check_activity_compatibility({skip_inspector, _Args}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} ->
    ok;
check_activity_compatibility({fail_adapter, _Args}, Activity)
    when Activity =:= {payment, new} orelse
         Activity =:= {payment, risk_scoring} orelse
         Activity =:= {payment, routing} orelse
         Activity =:= {payment, cash_flow_building} orelse
         Activity =:= {payment, processing_session} ->
    ok;
check_activity_compatibility({fail_adapter, _Args}, {refund_session, _}) ->
    ok;
check_activity_compatibility(Scenario, Activity) ->
    throw({exception, {activity_not_compatible_with_scenario, Activity, Scenario}}).

%% Utils

scenario_to_list(ScenarioData) when is_list(ScenarioData) ->
    ScenarioData;
scenario_to_list(ScenarioData) ->
    [ScenarioData].

