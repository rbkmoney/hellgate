-module(hg_content).
-include_lib("dmsl/include/dmsl_base_thrift.hrl").

-export([marshal/1]).
-export([unmarshal/1]).

-type content() :: dmsl_base_thrift:'Content'().

%% Marshalling

-spec marshal(content()) ->
    term().

marshal(Content) ->
    marshal(content, Content).

marshal(content, #'Content'{type = Type, data = Data}) ->
    [
        marshal(str, Type),
        marshal(bin, {bin, Data})
    ];
marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(term()) ->
    content().

unmarshal(Content) ->
    unmarshal(content, Content).

unmarshal(content, [Type, {bin, Data}]) ->
    #'Content'{
        type = unmarshal(str, Type),
        data = unmarshal(bin, Data)
    };
unmarshal(_, Other) ->
    Other.