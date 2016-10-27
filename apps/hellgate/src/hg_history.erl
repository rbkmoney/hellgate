-module(hg_history).

-export([get_public_history/4]).

%%

-spec get_public_history(GetFun, PublishFun, EventID, N) -> PublicHistory when
    GetFun        :: fun((EventID, N) -> {History, EventID}),
    PublishFun    :: fun((Event) -> {true, PublicEvent} | false),
    EventID       :: integer(),
    N             :: non_neg_integer(),
    History       :: [Event],
    PublicHistory :: [PublicEvent],
    Event         :: term(),
    PublicEvent   :: term().

get_public_history(GetFun, PublishFun, AfterID, undefined) ->
    {History0, _LastID} = GetFun(AfterID, undefined),
    publish_history(PublishFun, History0);

get_public_history(_GetFun, _PublishFun, _AfterID, 0) ->
    [];
get_public_history(GetFun, PublishFun, AfterID, N) ->
    {History0, LastID} = GetFun(AfterID, N),
    History = publish_history(PublishFun, History0),
    case length(History0) of
        N when length(History) =:= N ->
            History;
        N ->
            Left = N - length(History),
            HistoryRest = get_public_history(GetFun, PublishFun, LastID, Left),
            History ++ HistoryRest;
        M when M < N ->
            History
    end.

publish_history(PublishFun, History) ->
    lists:filtermap(PublishFun, History).
