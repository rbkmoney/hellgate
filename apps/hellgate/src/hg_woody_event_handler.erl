-module(hg_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").
-include_lib("woody/src/woody_defs.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, RawMeta, Opts) ->
    FormattedMeta = format_meta(RawMeta),
    file:write_file("/Users/s.elin/Projects/rbkmoney/hellgate/formatted_test.out", io_lib:format("~p:~n~s~n", [RawMeta,FormattedMeta]), [append]),
    scoper_woody_event_handler:handle_event(Event, RpcID, RawMeta, Opts).

format_meta(#{args := _Args, type := call} = RawMeta) ->
    FormattedMeta =
        try
            format_message(RawMeta)
        catch
            _:E:S -> io_lib:format("ERROR: ~p: ~p~n", [E, S])
        end,
    lists:flatten(FormattedMeta);
format_meta(RawMeta) -> io_lib:format("WARNING: unhandled meta: ~p~n", [RawMeta]).

format_message(RawMeta) ->
    {Module, Service} = maps:get(service_schema, RawMeta),
    Service = maps:get(service, RawMeta),
    Function = maps:get(function, RawMeta),
    FunctionArgs = maps:get(args, RawMeta),
    {struct, struct, FunctionArgsMeta} = Module:function_info(Service, Function, params_type),
    io_lib:format("~s:~s(~s)", [Service, Function, format_args(FunctionArgsMeta,FunctionArgs)]).

format_args(FunctionArgsMeta, FunctionArgs) ->
    FormattedArgs = format_(FunctionArgsMeta, FunctionArgs),
    string:join(lists:reverse(FormattedArgs), ", ").

format_(FunctionArgsMeta, FunctionArgs) ->
    FormattedArgs = lists:foldl(
        fun
            ({{_Fid, _Required, _Type, _Name, undefined}, undefined}, Acc) ->
                Acc;
            ({{_Fid, _Required, _Type, Name, Default}, undefined}, Acc) ->
                [io_lib:format("~s = ~p", [Name, Default]) | Acc];
            ({{_Fid, _Required, string, Name, _Default}, Value}, Acc) ->
                [io_lib:format("~s = '~s'", [Name, Value]) | Acc];
            ({{_Fid, _Required, {struct, struct, {Module, Struct}}, Name, _Default}, Value}, Acc) ->
                [io_lib:format("~s = ~s", [Name, format_struct(Module, Struct, Value)]) | Acc];
            ({{_Fid, _Required, {struct, union, {Module, Struct}}, Name, _Default}, Value}, Acc) ->
                [io_lib:format("~s = ~s", [Name, format_union(Module, Struct, Value)]) | Acc];
            ({{_Fid, _Required, {struct, enum, {Module, Struct}}, Name, _Default}, Value}, Acc) ->
                [io_lib:format("~s = ~s", [Name, format_enum(Module, Struct, Value)]) | Acc];
            ({{_Fid, _Required, _Type, Name, _Default}, Value}, Acc) ->
                %% All other types such as i32, i64, bool, etc. format here
                [io_lib:format("~s = ~p", [Name, Value]) | Acc]
        end,
        [], lists:zip(FunctionArgsMeta, FunctionArgs)
    ),
    FormattedArgs.

format_struct(Module, Struct, StructValue) ->
    {struct, struct, StructMeta} = Module:struct_info(Struct),
    case StructMeta of
        [] -> atom_to_list(Struct);
        StructMeta ->
            ValueList = tl(tuple_to_list(StructValue)), %% Remove record name
            FormattedArgs = format_(StructMeta, ValueList),
            lists:flatten([atom_to_list(Struct),"{", string:join(lists:reverse(FormattedArgs), ", "), "}"])
    end.

%% Filter and map Values direct to its value
format_union(_Module, 'Value', {bin, UnionValue}) ->
    case size(UnionValue) of
        0 ->  "<<>>";
        L when L =< 10 -> UnionValue;
        _ -> "<<...>>"
    end;

format_union(Module, Struct, {Type, UnionValue}) ->
    {struct, union, StructMeta} = Module:struct_info(Struct),
    case lists:keysearch(Type, 4, StructMeta) of
        {value, {_, _, {struct, struct, {M, S}}, _, _}} ->
            ValueList = tl(tuple_to_list(UnionValue)), %% Remove record name
            case M:struct_info(S) of
                {struct, struct, []} -> atom_to_list(S);
                {struct, struct, UnionMeta} ->
                    FormattedArgs = format_(UnionMeta, ValueList),
                    lists:flatten([atom_to_list(S), "{", string:join(lists:reverse(FormattedArgs), ", "), "}"])
            end;
        {value, {_, _, string, Name, _}} when is_binary(UnionValue) ->
            io_lib:format("~s{~s = ~s}", [Struct, Name, UnionValue]);
        {value, {_, _, _Type, Name, _}} ->
            io_lib:format("~s{~s = ~p}", [Struct, Name, UnionValue])
    end.

format_enum(Module, Struct, {Type, EnumValue}) ->
    {struct, enum, StructMeta} = Module:struct_info(Struct),
    {value, {_, _, {struct, struct, {M, S}}, Name, _}} = lists:keysearch(Type, 4, StructMeta),
    {enum, EnumInfo} = M:enum_info(S),
    {value, {Value, _}} = lists:keysearch(EnumValue, 2, EnumInfo),
    io_lib:format("~s{~s = ~s}",[Struct,Name,Value]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec args_test_() -> _.
args_test_() -> [
    ?_assertEqual(
        "PartyManagement:Create(party_id = '1CQdDqPROyW', params = PartyParams{contact_info = PartyContactInfo{email = 'hg_ct_helper'}})",
        format_meta(
            #{args =>
            [undefined,<<"1CQdDqPROyW">>,
            {payproc_PartyParams,{domain_PartyContactInfo,<<"hg_ct_helper">>}}],
            deadline => undefined,execution_start_time => 1565596875497,
            function => 'Create',
            metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                <<"user-identity.realm">> => <<"external">>},
            role => server,service => 'PartyManagement',
            service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
            type => call}
        )
    ),
    ?_assertEqual(
        "PartyManagement:Get(party_id = '1CQdDqPROyW')",
        format_meta(
            #{args => [undefined,<<"1CQdDqPROyW">>],
                deadline => undefined,execution_start_time => 1565596875696,
                function => 'Get',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "CustomerManagement:Create(params = CustomerParams{party_id = '1CQdDqPROyW', shop_id = '1CQdDwgt3R3', contact_info = ContactInfo{email = 'invalid_shop'}, metadata = Null})",
        format_meta(
            #{args =>
            [{payproc_CustomerParams,<<"1CQdDqPROyW">>,<<"1CQdDwgt3R3">>,
                {domain_ContactInfo,undefined,<<"invalid_shop">>},
                {nl,{json_Null}}}],
                deadline => undefined,execution_start_time => 1565596876258,
                function => 'Create',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'CustomerManagement',
                service_schema => {dmsl_payment_processing_thrift,'CustomerManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "PartyManagement:GetRevision(user = UserInfo{id = '1CQdDqPROyW', type = ExternalUser}, party_id = '1CQdDqPROyW')",
        format_meta(
            #{args =>
            [{payproc_UserInfo,<<"1CQdDqPROyW">>,
                {external_user,{payproc_ExternalUser}}},
                <<"1CQdDqPROyW">>],
                deadline => {{{2019,8,12},{8,1,46}},263},
                execution_start_time => 1565596876266,function => 'GetRevision',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "PartyManagement:Checkout(user = UserInfo{id = '1CQdDqPROyW', type = ExternalUser}, party_id = '1CQdDqPROyW', revision = PartyRevisionParam{revision = 1})",
        format_meta(
            #{args =>
            [{payproc_UserInfo,<<"1CQdDqPROyW">>,
                {external_user,{payproc_ExternalUser}}},
                <<"1CQdDqPROyW">>,
                {revision,1}],
                deadline => {{{2019,8,12},{8,1,46}},263},
                execution_start_time => 1565596876292,function => 'Checkout',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "PartyManagement:Block(party_id = '1CQdDqPROyW', reason = '')",
        format_meta(
            #{args => [undefined,<<"1CQdDqPROyW">>,<<>>],
                deadline => undefined,execution_start_time => 1565596876383,
                function => 'Block',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "PartyManagement:Unblock(party_id = '1CQdDqPROyW', reason = '')",
        format_meta(
            #{args => [undefined,<<"1CQdDqPROyW">>,<<>>],
                deadline => undefined,execution_start_time => 1565596876458,
                function => 'Unblock',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQdDqPROyW">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        "Processor:ProcessSignal(a = SignalArgs{signal = InitSignal{arg = <<...>>}, machine = Machine{ns = 'party', id = '1CQxZsCgLJY', history = [], history_range = HistoryRange{direction = forward}, aux_state = Content{data = <<>>}, aux_state_legacy = <<>>}})",
        format_meta(
            #{args =>
            [{mg_stateproc_SignalArgs,
                {init,
                    {mg_stateproc_InitSignal,
                        {bin,
                            <<131,109,0,0,0,24,12,0,1,11,0,1,0,0,0,12,104,103,95,
                                99,116,95,104,101,108,112,101,114,0,0>>}}},
                {mg_stateproc_Machine,<<"party">>,<<"1CQxZsCgLJY">>,[],
                    {mg_stateproc_HistoryRange,undefined,undefined,forward},
                    {mg_stateproc_Content,undefined,{bin,<<>>}},
                    undefined,
                    {bin,<<>>}}}],
                deadline => {{{2019,8,12},{12,46,36}},433},
                execution_start_time => 1565613966542,function => 'ProcessSignal',
                metadata =>
                #{<<"user-identity.id">> => <<"1CQxZsCgLJY">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'Processor',
                service_schema => {mg_proto_state_processing_thrift,'Processor'},
                type => call}
        )
    )

].

-endif.