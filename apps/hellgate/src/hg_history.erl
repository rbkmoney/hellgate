-module(hg_history).

-export([get_public_history/5]).

%%

-spec get_public_history(GetFun, PublishFun, EventID, N, Context) -> {PublicHistory, Context} when
    GetFun        :: fun((EventID, N, Context) -> {{History, EventID}, Context}),
    PublishFun    :: fun((Event) -> {true, PublicEvent} | false),
    EventID       :: integer(),
    N             :: non_neg_integer(),
    History       :: [Event],
    PublicHistory :: [PublicEvent],
    Event         :: term(),
    PublicEvent   :: term(),
    Context       :: woody_client:context().

get_public_history(GetFun, PublishFun, AfterID, undefined, Context0) ->
    {{History0, _LastID}, Context} = GetFun(AfterID, undefined, Context0),
    History = publish_history(PublishFun, History0),
    {History, Context};

get_public_history(_GetFun, _PublishFun, _AfterID, 0, Context) ->
    {[], Context};
get_public_history(GetFun, PublishFun, AfterID, N, Context0) ->
    {{History0, LastID}, Context1} = GetFun(AfterID, N, Context0),
    History = publish_history(PublishFun, History0),
    case length(History0) of
        N when length(History) =:= N ->
            {History, Context1};
        N ->
            Left = N - length(History),
            {HistoryRest, Context2} = get_public_history(GetFun, PublishFun, LastID, Left, Context1),
            {History ++ HistoryRest, Context2};
        M when M < N ->
            {History, Context1}
    end.

publish_history(PublishFun, History) ->
    lists:filtermap(PublishFun, History).
