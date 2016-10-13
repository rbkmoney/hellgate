-module(hg_eventsink_tests_SUITE).
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
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?c(apps, C)].

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    RootUrl = ?c(root_url, C),
    UserInfo = #payproc_UserInfo{id = <<?MODULE_STRING>>},
    [
        {eventsink_client, hg_client_eventsink:start_link(create_api(RootUrl))},
        {invoicing_client, hg_client_invoicing:start_link(UserInfo, create_api(RootUrl))} | C
    ].

create_api(RootUrl) ->
    hg_client_api:new(RootUrl).

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

%% tests

-include("invoice_events.hrl").

-define(event(ID, InvoiceID, Seq, Payload),
    #payproc_Event{
        id = ID,
        source = {invoice, InvoiceID},
        sequence = Seq,
        payload = Payload
    }
).

-spec no_events(config()) -> _ | no_return().

no_events(C) ->
    none = hg_client_eventsink:get_last_event_id(?c(eventsink_client, C)).

-spec events_observed(config()) -> _ | no_return().

events_observed(C) ->
    InvoicingClient = ?c(invoicing_client, C),
    EventsinkClient = ?c(eventsink_client, C),
    InvoiceParams = hg_ct_helper:make_invoice_params(<<?MODULE_STRING>>, <<"rubberduck">>, 10000),
    {ok, InvoiceID1} = hg_client_invoicing:create(InvoiceParams, InvoicingClient),
    ok = hg_client_invoicing:rescind(InvoiceID1, <<"die">>, InvoicingClient),
    {ok, InvoiceID2} = hg_client_invoicing:create(InvoiceParams, InvoicingClient),
    ok = hg_client_invoicing:rescind(InvoiceID2, <<"noway">>, InvoicingClient),
    {ok, Events1} = hg_client_eventsink:pull_events(2, 3000, EventsinkClient),
    {ok, Events2} = hg_client_eventsink:pull_events(2, 3000, EventsinkClient),
    [
        ?event(ID1, InvoiceID1, 1, ?invoice_ev(?invoice_created(_))),
        ?event(ID2, InvoiceID1, 2, ?invoice_ev(?invoice_status_changed(?cancelled(_))))
    ] = Events1,
    [
        ?event(ID3, InvoiceID2, 1, ?invoice_ev(?invoice_created(_))),
        ?event(ID4, InvoiceID2, 2, ?invoice_ev(?invoice_status_changed(?cancelled(_))))
    ] = Events2,
    IDs = [ID1, ID2, ID3, ID4],
    IDs = lists:sort(IDs).

-spec consistent_history(config()) -> _ | no_return().

consistent_history(C) ->
    {ok, Events} = hg_client_eventsink:pull_events(5000, 500, ?c(eventsink_client, C)),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).
