-module(pm_client_event_poller).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([new/2]).
-export([poll/4]).

-export_type([st/1]).

%%

-type event_id() :: integer().

-type rpc() :: {Name :: atom(), woody:func(), [_]}.

-opaque st(Event) :: #{
    rpc           := rpc(),
    get_event_id  := get_event_id(Event),
    last_event_id => integer()
}.

-type get_event_id(Event) :: fun((Event) -> event_id()).

-define(POLL_INTERVAL, 1000).

-spec new(rpc(), get_event_id(Event)) ->
    st(Event).

new(RPC, GetEventID) ->
    #{
        rpc          => RPC,
        get_event_id => GetEventID
    }.

-spec poll(pos_integer(), non_neg_integer(), pm_client_api:t(), st(Event)) ->
    {[Event] | {exception | error, _}, pm_client_api:t(), st(Event)}.

poll(N, Timeout, Client, St) ->
    poll(N, Timeout, [], Client, St).

poll(_, Timeout, Acc, Client, St) when Timeout < 0 ->
    {Acc, Client, St};
poll(N, Timeout, Acc, Client, St) ->
    StartTs = genlib_time:ticks(),
    Range = construct_range(St, N),
    {Result, ClientNext} = call(Range, Client, St),
    case Result of
        {ok, Events} when length(Events) == N ->
            StNext = update_last_event_id(Events, St),
            {Acc ++ Events, ClientNext, StNext};
        {ok, Events} when is_list(Events) ->
            TimeoutLeft = wait_timeout(StartTs, Timeout),
            StNext = update_last_event_id(Events, St),
            poll(N - length(Events), TimeoutLeft, Acc ++ Events, ClientNext, StNext);
        _Error ->
            {Result, ClientNext, St}
    end.

construct_range(St, N) ->
    #payproc_EventRange{'after' = get_last_event_id(St), limit = N}.

wait_timeout(StartTs, TimeoutWas) ->
    _ = timer:sleep(?POLL_INTERVAL),
    TimeoutWas - (genlib_time:ticks() - StartTs) div 1000.

update_last_event_id([], St) ->
    St;
update_last_event_id(Events, St = #{get_event_id := GetEventID}) ->
    St#{last_event_id => GetEventID(lists:last(Events))}.

call(Range, Client, #{rpc := {Name, Function, Args}}) ->
    pm_client_api:call(Name, Function, Args ++ [Range], Client).

get_last_event_id(St) ->
    maps:get(last_event_id, St, undefined).
