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

-define(config(Key), begin element(2, lists:keyfind(Key, 1, C)) end).

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

-include_lib("hg_proto/include/hg_payment_processing_thrift.hrl").

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, hellgate]),
    [{root_url, maps:get(hellgate_root_url, Ret)}, {apps, Apps} | C].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps)].

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    UserInfo = #payproc_UserInfo{id = <<?MODULE_STRING>>},
    Client = hg_client:start_link(?config(root_url), UserInfo),
    [{client, Client} | C].

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
    Client = ?config(client),
    none = hg_client:get_last_event_id(Client).

-spec events_observed(config()) -> _ | no_return().

events_observed(C) ->
    Client = ?config(client),
    InvoiceParams = hg_ct_helper:make_invoice_params(<<?MODULE_STRING>>, <<"rubberduck">>, 10000),
    {ok, InvoiceID1} = hg_client:create_invoice(InvoiceParams, Client),
    ok = hg_client:rescind_invoice(InvoiceID1, <<"die">>, Client),
    {ok, InvoiceID2} = hg_client:create_invoice(InvoiceParams, Client),
    ok = hg_client:rescind_invoice(InvoiceID2, <<"noway">>, Client),
    {ok, Events1} = hg_client:pull_events(2, 3000, Client),
    {ok, Events2} = hg_client:pull_events(2, 3000, Client),
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
    Client = ?config(client),
    {ok, Events} = hg_client:pull_events(_N = 5000, 1000, Client),
    ok = hg_eventsink_history:assert_total_order(Events),
    ok = hg_eventsink_history:assert_contiguous_sequences(Events).
