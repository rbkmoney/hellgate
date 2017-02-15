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

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
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
    UserInfo = #payproc_UserInfo{id = PartyID, type = {external_user, #payproc_ExternalUser{}}},
    [
        {party_id, PartyID},
        {eventsink_client, hg_client_eventsink:start_link(create_api(RootUrl))},
        {partymgmt_client, hg_client_party:start_link(UserInfo, PartyID, create_api(RootUrl))} | C
    ].

create_api(RootUrl) ->
    hg_client_api:new(RootUrl).

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%% tests

-include("party_events.hrl").

-define(event(ID, InvoiceID, Seq, Payload),
    #payproc_Event{
        id = ID,
        source = {party, InvoiceID},
        sequence = Seq,
        payload = Payload
    }
).

-spec no_events(config()) -> _ | no_return().

no_events(C) ->
    none = hg_client_eventsink:get_last_event_id(?c(eventsink_client, C)).

-spec events_observed(config()) -> _ | no_return().

events_observed(C) ->
    EventsinkClient = ?c(eventsink_client, C),
    PartyMgmtClient = ?c(partymgmt_client, C),
    PartyID = ?c(party_id, C),
    _ShopID = hg_ct_helper:create_party_and_shop(PartyMgmtClient),
    Events = hg_client_eventsink:pull_events(10, 3000, EventsinkClient),
    [?event(_ID, PartyID, 1, ?party_ev(?party_created(_))) | _] = Events,
    IDs = [ID || ?event(ID, _, _, _) <- Events],
    IDs = lists:sort(IDs).

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    Events = hg_client_eventsink:pull_events(5000, 500, ?c(eventsink_client, C)),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).

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

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ordsets:from_list([])},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(1),
                common_merchant_proxy = ?prx(1),
                inspector = {value, ?insp(1)}
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = #domain_PartyPrototypeRef{id = 42},
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    }
                },
                test_contract_template = ?tmpl(1)
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
