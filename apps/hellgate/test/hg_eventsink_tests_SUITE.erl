-module(hg_eventsink_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([no_events/1]).
-export([events_observed/1]).
-export([consistent_history/1]).

%%

-define(c(Key, C), begin element(2, lists:keyfind(Key, 1, C)) end).

%% tests descriptions

-type config() :: [{atom(), term()}].

-type test_case_name() :: atom().
-type group_name() :: atom().

-spec all() -> [{group, group_name()}].

all() ->
    [
        {group, initial},
        {group, history}
    ].

-spec groups() -> [{group_name(), [test_case_name()]}].

groups() ->
    [
        {initial, [], [no_events, events_observed]},
        {history, [], [consistent_history]}
    ].

%% starting / stopping

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([woody, scoper, dmt_client, party_client, hellgate, party_management]),
    ok = hg_domain:insert(construct_domain_fixture()),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- ?c(apps, C)].

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    RootUrl = ?c(root_url, C),
    PartyID = hg_utils:unique_id(),
    [
        {party_id, PartyID},
        {eventsink_client, hg_client_eventsink:start_link(create_api(RootUrl, PartyID))},
        {partymgmt_client, pm_client_party:start_link(PartyID, create_api(RootUrl, PartyID))} | C
    ].

create_api(RootUrl, PartyID) ->
    hg_ct_helper:create_client(RootUrl, PartyID).

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%% tests

-define(event(ID, Source, Seq, Payload),
    #payproc_Event{
        id = ID,
        source = Source,
        payload = Payload,
        sequence   = Seq
    }
).

-define(party_event(ID, PartyID, Seq, Payload),
    ?event(ID, {party_id, PartyID}, Seq, Payload)
).

-spec no_events(config()) -> _ | no_return().

no_events(C) ->
    Client = ?c(eventsink_client, C),
    case hg_client_eventsink:pull_history(Client) of
        [] ->
            none = hg_client_eventsink:get_last_event_id(Client);
        Events = [_ | _] ->
            ?event(EventID, _, _, _) = lists:last(Events),
            EventID = hg_client_eventsink:get_last_event_id(Client)
    end.

-spec events_observed(config()) -> _ | no_return().

events_observed(C) ->
    EventsinkClient = ?c(eventsink_client, C),
    PartyMgmtClient = ?c(partymgmt_client, C),
    _History = hg_client_eventsink:pull_history(EventsinkClient),
    _ShopID = hg_ct_helper:create_party_and_shop(?cat(1), <<"RUB">>, ?tmpl(1), ?pinst(1), PartyMgmtClient),
    Events = hg_client_eventsink:pull_events(10, EventsinkClient),
    Seqs = [Seq || ?event(_, _, Seq, _) <- Events],
    Seqs = lists:sort(Seqs),
    IDs = [ID || ?event(ID, _, _, _) <- Events],
    IDs = lists:sort(IDs).

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Events = hg_client_eventsink:pull_history(?c(eventsink_client, C)),
    ok = hg_eventsink_history:assert_total_order(Events).

-spec construct_domain_fixture() -> [hg_domain:object()].

construct_domain_fixture() ->
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>),
        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = test
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, ?eas(1)},
                payment_institutions = ?ordset([?pinst(1)])
            }
        }},

        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
                            categories = {value, ordsets:from_list([?cat(1)])}
                        }
                    }
                }]
            }
        }}
    ].
