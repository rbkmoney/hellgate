-module(hg_proto).

%% TODO:
%%  - specs

-export([serialize/2]).
-export([deserialize/2]).

%%

-spec serialize(any(), term()) -> {ok, binary()} | {error, any()}.

serialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:write(Proto, {Type, Data}) of
        {NewProto, ok} ->
            {_, Result} = thrift_protocol:close_transport(NewProto),
            {ok, Result};
        {_NewProto, {error, _Reason} = Error} ->
            Error
    end.

-spec deserialize(any(), binary()) -> {ok, term()} | {error, any()}.

deserialize(Type, Data) ->
    {ok, Trans} = thrift_membuffer_transport:new(Data),
    {ok, Proto} = new_protocol(Trans),
    case thrift_protocol:read(Proto, Type) of
        {_NewProto, {ok, Result}} ->
            {ok, Result};
        {_NewProto, {error, _Reason} = Error} ->
            Error
    end.

new_protocol(Trans) ->
    thrift_binary_protocol:new(Trans, [{strict_read, true}, {strict_write, true}]).
