-module(hg_machine).

%% TODO: merge with dispatcher?

-type id() :: binary().
-type args() :: _.
-type event() :: _.

-type history() :: [event()].
-type result() :: {event(), hg_action:t()}.

-callback init(id(), args()) ->
    {ok, result()}.

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history()) ->
    {ok, result()}.

-type call() :: _.
-type response() :: _.

-callback process_call(call(), history()) ->
    {ok, response(), result()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([history/0]).
-export_type([result/0]).
