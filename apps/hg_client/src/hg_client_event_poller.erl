-module(hg_client_event_poller).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([new/3]).
-export([poll/4]).

-export_type([t/0]).

%%

-type events() :: [dmsl_payment_processing_thrift:'Event'()].

-type api_call() :: {Name :: atom(), woody:func(), [_]}.
-opaque t() :: {api_call(), undefined | integer()}.

-spec new(Name :: atom(), woody:func(), [_]) ->
    t().

new(Name, Function, Args) ->
    {{Name, Function, Args}, undefined}.

-spec poll(pos_integer(), pos_integer(), hg_client_api:t(), t()) ->
    {events() | {exception | error, _}, hg_client_api:t(), t()}.

-define(POLL_INTERVAL, 1000).

poll(N, Timeout, Client, St) ->
    poll(N, Timeout, [], Client, St, St).

poll(_, Timeout, Acc, Client, _, St) when Timeout =< 0 ->
    {Acc, Client, St};
poll(N, Timeout, Acc, Client, StWas, {Call = {Name, Function, Args}, After}) ->
    StartTs = genlib_time:ticks(),
    Range = construct_range(After, N, Acc),
    {Result, ClientNext} = hg_client_api:call(Name, Function, Args ++ [Range], Client),
    case Result of
        {ok, Events} when length(Events) == N ->
            {Acc ++ Events, ClientNext, {Call, get_last_event_id(After, Events)}};
        {ok, Events} when is_list(Events) ->
            StNext = {Call, get_last_event_id(After, Events)},
            TimeoutLeft = wait_timeout(StartTs, Timeout),
            poll(N - length(Events), TimeoutLeft, Acc ++ Events, ClientNext, StWas, StNext);
        _Error ->
            {Result, ClientNext, StWas}
    end.

construct_range(After, N, []) ->
    #payproc_EventRange{'after' = After, limit = N};
construct_range(_After, N, Events) ->
    #payproc_Event{id = LastEvent} = lists:last(Events),
    #payproc_EventRange{'after' = LastEvent, limit = N}.

wait_timeout(StartTs, TimeoutWas) ->
    _ = timer:sleep(?POLL_INTERVAL),
    TimeoutWas - (genlib_time:ticks() - StartTs) div 1000.

get_last_event_id(After, []) ->
    After;
get_last_event_id(_After, Events) ->
    #payproc_Event{id = EventID} = lists:last(Events),
    EventID.
