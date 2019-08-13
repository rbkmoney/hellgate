-module(hg_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").
-include_lib("woody/src/woody_defs.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

-define(MAX_BIN_LENGTH, 10).

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
    file:write_file("/Users/s.elin/Projects/rbkmoney/hellgate/formatted_test.out", io_lib:format("~s~n", [FormattedMeta]), [append]),
    scoper_woody_event_handler:handle_event(Event, RpcID, RawMeta, Opts).

format_meta(#{type := call} = RawMeta) ->
    FormattedMeta =
        try
            format_message(RawMeta)
        catch
            _:E:S -> io_lib:format("ERROR: ~p: ~p~n", [E, S])
        end,
    lists:flatten(FormattedMeta);
format_meta(_RawMeta) -> "". %% RawMeta.

format_message(#{result := {ok, Result}, status := ok, type := call} = RawMeta) ->
    {Module, Service} = maps:get(service_schema, RawMeta),
    Service = maps:get(service, RawMeta),
    Function = maps:get(function, RawMeta),
    case Module:function_info(Service, Function, reply_type) of
        {struct, struct, []} ->
            [];
        {struct, struct, {ReplyModule, ReplyType}} ->
            io_lib:format("~s", [format_struct(ReplyModule, ReplyType, Result)]);
        {list, {struct, struct, {ReplyModule, ReplyType}}} ->
            FormattedList = lists:map(fun(Entry) -> format_struct(ReplyModule, ReplyType, Entry) end, Result),
            io_lib:format("[~s]", [string:join(FormattedList, ", ")]);
        _ ->
            io_lib:format("~p", [Result])
    end;
%%format_message(#{result := Result, status := error, type := call} = RawMeta) ->
%%    {Module, Service} = maps:get(service_schema, RawMeta),
%%    Service = maps:get(service, RawMeta),
%%    Function = maps:get(function, RawMeta),
%%    {struct, struct, ExceptionsMeta} = Module:function_info(Service, Function, exceptions),
%%    io_lib:format("~s", [format_exception(ExceptionsMeta,Result)]);
format_message(#{args := FunctionArgs, type := call} = RawMeta) ->
    {Module, Service} = maps:get(service_schema, RawMeta),
    Service = maps:get(service, RawMeta),
    Function = maps:get(function, RawMeta),
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
            ({{_Fid, _Required, {list, {struct, union, {Module, Struct}}}, Name, _Default}, ValueList}, Acc) ->
                FormattedValueList =
                    lists:foldr(
                        fun(Value, FormattedAcc) ->
                            [format_union(Module, Struct, Value) | FormattedAcc]
                        end, [], ValueList),
                FormattedValue = string:join(FormattedValueList, ", "),
                [io_lib:format("~s = [~s]", [Name, FormattedValue]) | Acc];
            ({{_Fid, _Required, {list, {struct, struct, {Module, Struct}}}, Name, _Default}, ValueList}, Acc) ->
                FormattedValueList =
                    lists:foldr(
                        fun(Value, FormattedAcc) ->
                            [format_struct(Module, Struct, Value) | FormattedAcc]
                        end, [], ValueList),
                FormattedValue = string:join(FormattedValueList, ", "),
                [io_lib:format("~s = [~s]", [Name, FormattedValue]) | Acc];
            ({{_Fid, _Required, {map, string, {struct, struct,{Module,Struct}}}, Name, _Default}, ValueMap}, Acc) ->
                MapData = maps:to_list(ValueMap),
                Result =
                    lists:foldr(
                        fun({K, V}, Acc1) ->
                            [io_lib:format("~s => ~s", [K, format_struct(Module, Struct, V)]) | Acc1]
                        end,
                        [], MapData
                    ),
                FormattedResult = lists:flatten("#{",[string:join(Result, ", "), "}"]),
                [io_lib:format("~s = ~s", [Name, FormattedResult]) | Acc];
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
format_union(_Module, 'Value', Value) ->
    format_value(Value);

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
        {value, {_, _, {list, {struct, union, {M, S}}}, Name, _}} ->
            FormattedValueList =
                lists:foldr(
                    fun(Value, FormattedAcc) ->
                        FormattedUnion = format_union(M, S, Value),
                        [io_lib:format("~s{~s = '~s'}", [Struct, Name, FormattedUnion])| FormattedAcc]
                    end, [], UnionValue),
            FormattedValue = string:join(FormattedValueList, ", "),
            io_lib:format("~s = [~s]", [Name, FormattedValue]);
        {value, {_, _, {struct, union, {M, S}}, _, _}} ->
            format_union(M, S, UnionValue);
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

format_value({nl, _Null}) -> 'Null';
format_value({bin, Bin}) when size(Bin) == 0 -> "<<>>";
format_value({bin, Bin}) when size(Bin) =< ?MAX_BIN_LENGTH -> io_lib:format("~p", [Bin]);
format_value({bin, _Bin}) -> "<<...>>";
format_value({i, N}) -> N;
format_value({str, S}) when is_binary(S)-> binary_to_list(S);
format_value({str, S}) -> S;
format_value({obj, S}) ->
    ObjData = maps:to_list(S),
    Result =
        lists:foldr(
            fun({K, V}, Acc) ->
                [io_lib:format("~s => ~p", [format_value(K), format_value(V)]) | Acc]
            end,
            [], ObjData
        ),
    lists:flatten("#{",[string:join(Result, ", "), "}"]);
format_value({arr, S}) ->
    Result = lists:map(
        fun
            ({i, _} = Entry) ->
                %% Avoid crash on string concatenation
                integer_to_list(format_value(Entry));
            (Entry) ->
                format_value(Entry)
        end, S),
    lists:flatten("[",[string:join(Result, ", "), "]"]).

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
        lists:flatten([
            "Processor:ProcessSignal(a = SignalArgs{signal = InitSignal{arg = <<...>>}, machine = Machine{ns = 'party', ",
            "id = '1CQxZsCgLJY', history = [], history_range = HistoryRange{direction = forward}, aux_state = Content{data = <<>>}, ",
            "aux_state_legacy = <<>>}})"
        ]),
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
    ),
    ?_assertEqual(
        lists:flatten([
            "PartyManagement:CreateClaim(party_id = '1CR1Xziml7o', changeset = [ContractModificationUnit{id = '1CR1Y2ZcrA0', ",
            "modification = ContractParams{template = ContractTemplateRef{id = 1}, payment_institution = PaymentInstitutionRef{id = 1}, ",
            "contractor = RussianLegalEntity{registered_name = 'Hoofs & Horns OJSC', registered_number = '1234509876', ",
            "inn = '1213456789012', actual_address = 'Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO', post_address = 'NaN', ",
            "representative_position = 'Director', representative_full_name = 'Someone', representative_document = '100$ banknote', ",
            "russian_bank_account = RussianBankAccount{account = '4276300010908312893', bank_name = 'SomeBank', ",
            "bank_post_account = '123129876', bank_bik = '66642666'}}}}, ContractModificationUnit{id = '1CR1Y2ZcrA0', ",
            "modification = PayoutToolModificationUnit{payout_tool_id = '1CR1Y2ZcrA1', modification = PayoutToolParams{",
            "currency = CurrencyRef{symbolic_code = 'RUB'}, tool_info = RussianBankAccount{account = '4276300010908312893', ",
            "bank_name = 'SomeBank', bank_post_account = '123129876', bank_bik = '66642666'}}}}, ShopModificationUnit{id = '1CR1Y2ZcrA2', ",
            "modification = ShopParams{category = CategoryRef{id = 1}, location = ShopLocation{url = }, details = ShopDetails{",
            "name = 'Battle Ready Shop'}, contract_id = '1CR1Y2ZcrA0', payout_tool_id = '1CR1Y2ZcrA1'}}, ShopModificationUnit{",
            "id = '1CR1Y2ZcrA2', modification = ShopAccountParams{currency = CurrencyRef{symbolic_code = 'RUB'}}}])"
        ]),
        format_meta(
            #{args =>
            [undefined,<<"1CR1Xziml7o">>,
                [{contract_modification,
                    {payproc_ContractModificationUnit,<<"1CR1Y2ZcrA0">>,
                        {creation,
                            {payproc_ContractParams,undefined,
                                {domain_ContractTemplateRef,1},
                                {domain_PaymentInstitutionRef,1},
                                {legal_entity,
                                    {russian_legal_entity,
                                        {domain_RussianLegalEntity,<<"Hoofs & Horns OJSC">>,
                                            <<"1234509876">>,<<"1213456789012">>,
                                            <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,<<"NaN">>,
                                            <<"Director">>,<<"Someone">>,<<"100$ banknote">>,
                                            {domain_RussianBankAccount,<<"4276300010908312893">>,
                                                <<"SomeBank">>,<<"123129876">>,<<"66642666">>}}}}}}}},
                    {contract_modification,
                        {payproc_ContractModificationUnit,<<"1CR1Y2ZcrA0">>,
                            {payout_tool_modification,
                                {payproc_PayoutToolModificationUnit,<<"1CR1Y2ZcrA1">>,
                                    {creation,
                                        {payproc_PayoutToolParams,
                                            {domain_CurrencyRef,<<"RUB">>},
                                            {russian_bank_account,
                                                {domain_RussianBankAccount,<<"4276300010908312893">>,
                                                    <<"SomeBank">>,<<"123129876">>,<<"66642666">>}}}}}}}},
                    {shop_modification,
                        {payproc_ShopModificationUnit,<<"1CR1Y2ZcrA2">>,
                            {creation,
                                {payproc_ShopParams,
                                    {domain_CategoryRef,1},
                                    {url,<<>>},
                                    {domain_ShopDetails,<<"Battle Ready Shop">>,undefined},
                                    <<"1CR1Y2ZcrA0">>,<<"1CR1Y2ZcrA1">>}}}},
                    {shop_modification,
                        {payproc_ShopModificationUnit,<<"1CR1Y2ZcrA2">>,
                            {shop_account_creation,
                                {payproc_ShopAccountParams,{domain_CurrencyRef,<<"RUB">>}}}}}]],
                deadline => undefined,execution_start_time => 1565617299263,
                function => 'CreateClaim',
                metadata =>
                #{<<"user-identity.id">> => <<"1CR1Xziml7o">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                type => call}
        )
    ),
    ?_assertEqual(
        lists:flatten([
            "Processor:ProcessCall(a = CallArgs{arg = <<...>>, machine = Machine{ns = 'party', id = '1CSHThTEJ84', ",
            "history = [Event{id = 1, created_at = '2019-08-13T07:52:11.080519Z', data = [#{ct => \"application/x-erlang-binary\", ",
            "vsn => 6}, <<...>>]}], history_range = HistoryRange{limit = 10, direction = backward}, aux_state = Content{data = ",
            "#{aux_state => \"<<...>>\", ct => \"application/x-erlang-binary\"}}, aux_state_legacy = #{aux_state => \"<<...>>\", ",
            "ct => \"application/x-erlang-binary\"}}})"
        ]),
        format_meta(
            #{args =>
            [{mg_stateproc_CallArgs,
                {bin,
                    <<131,104,4,100,0,11,116,104,114,105,102,116,95,99,97,108,108,
                        100,0,16,112,97,114,116,121,95,109,97,110,97,103,101,109,101,
                        110,116,104,2,100,0,15,80,97,114,116,121,77,97,110,97,103,
                        101,109,101,110,116,100,0,11,67,114,101,97,116,101,67,108,97,
                        105,109,109,0,0,2,145,11,0,2,0,0,0,11,49,67,83,72,84,104,84,
                        69,74,56,52,15,0,3,12,0,0,0,4,12,0,4,11,0,1,0,0,0,11,49,67,
                        83,72,84,106,75,108,51,52,75,12,0,2,12,0,1,12,0,2,8,0,1,0,0,
                        0,1,0,12,0,3,8,0,1,0,0,0,1,0,12,0,1,12,0,1,12,0,1,11,0,1,0,0,
                        0,18,72,111,111,102,115,32,38,32,72,111,114,110,115,32,79,74,
                        83,67,11,0,2,0,0,0,10,49,50,51,52,53,48,57,56,55,54,11,0,3,0,
                        0,0,13,49,50,49,51,52,53,54,55,56,57,48,49,50,11,0,4,0,0,0,
                        48,78,101,122,97,104,117,97,108,99,111,121,111,116,108,32,49,
                        48,57,32,80,105,115,111,32,56,44,32,67,101,110,116,114,111,
                        44,32,48,54,48,56,50,44,32,77,69,88,73,67,79,11,0,5,0,0,0,3,
                        78,97,78,11,0,6,0,0,0,8,68,105,114,101,99,116,111,114,11,0,7,
                        0,0,0,7,83,111,109,101,111,110,101,11,0,8,0,0,0,13,49,48,48,
                        36,32,98,97,110,107,110,111,116,101,12,0,9,11,0,1,0,0,0,19,
                        52,50,55,54,51,48,48,48,49,48,57,48,56,51,49,50,56,57,51,11,
                        0,2,0,0,0,8,83,111,109,101,66,97,110,107,11,0,3,0,0,0,9,49,
                        50,51,49,50,57,56,55,54,11,0,4,0,0,0,8,54,54,54,52,50,54,54,
                        54,0,0,0,0,0,0,0,0,12,0,4,11,0,1,0,0,0,11,49,67,83,72,84,106,
                        75,108,51,52,75,12,0,2,12,0,4,11,0,1,0,0,0,11,49,67,83,72,84,
                        106,75,108,51,52,76,12,0,2,12,0,1,12,0,1,11,0,1,0,0,0,3,82,
                        85,66,0,12,0,2,12,0,1,11,0,1,0,0,0,19,52,50,55,54,51,48,48,
                        48,49,48,57,48,56,51,49,50,56,57,51,11,0,2,0,0,0,8,83,111,
                        109,101,66,97,110,107,11,0,3,0,0,0,9,49,50,51,49,50,57,56,55,
                        54,11,0,4,0,0,0,8,54,54,54,52,50,54,54,54,0,0,0,0,0,0,0,0,12,
                        0,6,11,0,1,0,0,0,11,49,67,83,72,84,106,75,108,51,52,77,12,0,
                        2,12,0,5,12,0,1,8,0,1,0,0,0,1,0,12,0,6,11,0,1,0,0,0,0,0,12,0,
                        2,11,0,1,0,0,0,17,66,97,116,116,108,101,32,82,101,97,100,121,
                        32,83,104,111,112,0,11,0,3,0,0,0,11,49,67,83,72,84,106,75,
                        108,51,52,75,11,0,4,0,0,0,11,49,67,83,72,84,106,75,108,51,52,
                        76,0,0,0,0,12,0,6,11,0,1,0,0,0,11,49,67,83,72,84,106,75,108,
                        51,52,77,12,0,2,12,0,12,12,0,1,11,0,1,0,0,0,3,82,85,66,0,0,0,
                        0,0,0>>},
                {mg_stateproc_Machine,<<"party">>,<<"1CSHThTEJ84">>,
                    [{mg_stateproc_Event,1,<<"2019-08-13T07:52:11.080519Z">>,
                        undefined,
                        {arr,
                            [{obj,
                                #{{str,<<"ct">>} =>
                                {str,<<"application/x-erlang-binary">>},
                                    {str,<<"vsn">>} => {i,6}}},
                                {bin,
                                    <<131,104,2,100,0,13,112,97,114,116,121,95,
                                        99,104,97,110,103,101,115,108,0,0,0,2,104,
                                        2,100,0,13,112,97,114,116,121,95,99,114,
                                        101,97,116,101,100,104,4,100,0,20,112,97,
                                        121,112,114,111,99,95,80,97,114,116,121,67,
                                        114,101,97,116,101,100,109,0,0,0,11,49,67,
                                        83,72,84,104,84,69,74,56,52,104,2,100,0,23,
                                        100,111,109,97,105,110,95,80,97,114,116,
                                        121,67,111,110,116,97,99,116,73,110,102,
                                        111,109,0,0,0,12,104,103,95,99,116,95,104,
                                        101,108,112,101,114,109,0,0,0,27,50,48,49,
                                        57,45,48,56,45,49,51,84,48,55,58,53,50,58,
                                        49,49,46,48,55,50,56,51,53,90,104,2,100,0,
                                        16,114,101,118,105,115,105,111,110,95,99,
                                        104,97,110,103,101,100,104,3,100,0,28,112,
                                        97,121,112,114,111,99,95,80,97,114,116,121,
                                        82,101,118,105,115,105,111,110,67,104,97,
                                        110,103,101,100,109,0,0,0,27,50,48,49,57,
                                        45,48,56,45,49,51,84,48,55,58,53,50,58,49,
                                        49,46,48,55,50,56,51,53,90,97,0,106>>}]}}],
                    {mg_stateproc_HistoryRange,undefined,10,backward},
                    {mg_stateproc_Content,undefined,
                        {obj,
                            #{{str,<<"aux_state">>} =>
                            {bin,
                                <<131,116,0,0,0,2,100,0,20,112,97,114,116,
                                    121,95,114,101,118,105,115,105,111,110,95,
                                    105,110,100,101,120,116,0,0,0,0,100,0,14,
                                    115,110,97,112,115,104,111,116,95,105,110,
                                    100,101,120,106>>},
                                {str,<<"ct">>} =>
                                {str,<<"application/x-erlang-binary">>}}}},
                    undefined,
                    {obj,
                        #{{str,<<"aux_state">>} =>
                        {bin,
                            <<131,116,0,0,0,2,100,0,20,112,97,114,116,121,95,
                                114,101,118,105,115,105,111,110,95,105,110,100,
                                101,120,116,0,0,0,0,100,0,14,115,110,97,112,
                                115,104,111,116,95,105,110,100,101,120,106>>},
                            {str,<<"ct">>} =>
                            {str,<<"application/x-erlang-binary">>}}}}}],
                deadline => {{{2019,8,13},{7,52,41}},105},
                execution_start_time => 1565682731109,function => 'ProcessCall',
                metadata =>
                #{<<"user-identity.id">> => <<"1CSHThTEJ84">>,
                    <<"user-identity.realm">> => <<"external">>},
                role => server,service => 'Processor',
                service_schema => {mg_proto_state_processing_thrift,'Processor'},
                type => call}
        )
    )
].

-spec result_test_() -> _.
result_test_() -> [
    ?_assertEqual(
        lists:flatten([
            "CallResult{response = <<131,100,0,2,111,107>>, change = MachineStateChange{aux_state = Content{",
            "data = #{aux_state => \"<<...>>\", ct => \"application/x-erlang-binary\"}}, events = [Content{",
            "data = [#{ct => \"application/x-erlang-binary\", vsn => 6}, <<...>>]}]}, action = ComplexAction{}}"
            ]),
        format_meta(
            #{
                deadline => {{{2019,8,13},{11,19,32}},986},
                execution_start_time => 1565695142994,function => 'ProcessCall',
                metadata =>
                #{<<"user-identity.id">> => <<"1CSWG2vduGe">>,
                    <<"user-identity.realm">> => <<"external">>},
                result =>
                {ok,{mg_stateproc_CallResult,
                    {bin,<<131,100,0,2,111,107>>},
                    {mg_stateproc_MachineStateChange,
                        {mg_stateproc_Content,undefined,
                            {obj,
                                #{{str,<<"aux_state">>} =>
                                {bin,
                                    <<131,116,0,0,0,2,100,0,20,112,97,114,116,
                                        121,95,114,101,118,105,115,105,111,110,
                                        95,105,110,100,101,120,116,0,0,0,7,97,0,
                                        104,2,97,1,97,1,97,1,104,2,97,2,97,2,97,
                                        2,104,2,97,3,97,3,97,3,104,2,97,4,97,4,
                                        97,4,104,2,97,5,97,5,97,5,104,2,97,6,97,
                                        6,97,6,104,2,97,7,97,7,100,0,14,115,110,
                                        97,112,115,104,111,116,95,105,110,100,
                                        101,120,106>>},
                                    {str,<<"ct">>} =>
                                    {str,<<"application/x-erlang-binary">>}}}},
                        [{mg_stateproc_Content,undefined,
                            {arr,
                                [{obj,
                                    #{{str,<<"ct">>} =>
                                    {str,<<"application/x-erlang-binary">>},
                                        {str,<<"vsn">>} => {i,6}}},
                                    {bin,
                                        <<131,104,2,100,0,13,112,97,114,116,121,
                                            95,99,104,97,110,103,101,115,108,0,0,0,
                                            2,104,2,100,0,13,115,104,111,112,95,98,
                                            108,111,99,107,105,110,103,104,3,100,0,
                                            20,112,97,121,112,114,111,99,95,83,104,
                                            111,112,66,108,111,99,107,105,110,103,
                                            109,0,0,0,11,49,67,83,87,71,56,106,48,
                                            52,119,77,104,2,100,0,7,98,108,111,99,
                                            107,101,100,104,3,100,0,14,100,111,109,
                                            97,105,110,95,66,108,111,99,107,101,100,
                                            109,0,0,0,0,109,0,0,0,27,50,48,49,57,45,
                                            48,56,45,49,51,84,49,49,58,49,57,58,48,
                                            51,46,48,49,53,50,50,50,90,104,2,100,0,
                                            16,114,101,118,105,115,105,111,110,95,
                                            99,104,97,110,103,101,100,104,3,100,0,
                                            28,112,97,121,112,114,111,99,95,80,97,
                                            114,116,121,82,101,118,105,115,105,111,
                                            110,67,104,97,110,103,101,100,109,0,0,0,
                                            27,50,48,49,57,45,48,56,45,49,51,84,49,
                                            49,58,49,57,58,48,51,46,48,49,53,50,50,
                                            50,90,97,6,106>>}]}}],
                        undefined,undefined},
                    {mg_stateproc_ComplexAction,undefined,undefined,undefined,
                        undefined}}},
                role => server,service => 'Processor',
                service_schema => {mg_proto_state_processing_thrift,'Processor'},
                status => ok,type => call}
        )
    ),
    ?_assertEqual(
        lists:flatten([
            "Party{id = '1CSWG2vduGe', contact_info = PartyContactInfo{email = 'hg_ct_helper'}, ",
            "created_at = '2019-08-13T11:19:01.249440Z', blocking = Unblocked{reason = '', ",
            "since = '2019-08-13T11:19:02.655869Z'}, suspension = Active{since = '2019-08-13T11:19:02.891892Z'}, ",
            "contractors = #{}, contracts = #{1CSWG8j04wK => Contract{id = '1CSWG8j04wK', ",
            "payment_institution = PaymentInstitutionRef{id = 1}, created_at = '2019-08-13T11:19:01.387269Z', ",
            "status = ContractActive, terms = TermSetHierarchyRef{id = 1}, adjustments = [], ",
            "payout_tools = [PayoutTool{id = '1CSWG8j04wL', created_at = '2019-08-13T11:19:01.387269Z', ",
            "currency = CurrencyRef{symbolic_code = 'RUB'}, payout_tool_info = RussianBankAccount{",
            "account = '4276300010908312893', bank_name = 'SomeBank', bank_post_account = '123129876', ",
            "bank_bik = '66642666'}}], contractor = RussianLegalEntity{registered_name = 'Hoofs & Horns OJSC', ",
            "registered_number = '1234509876', inn = '1213456789012', actual_address = 'Nezahualcoyotl 109 Piso 8, ",
            "Centro, 06082, MEXICO', post_address = 'NaN', representative_position = 'Director', representative_full_name = ",
            "'Someone', representative_document = '100$ banknote', russian_bank_account = RussianBankAccount{",
            "account = '4276300010908312893', bank_name = 'SomeBank', bank_post_account = '123129876', ",
            "bank_bik = '66642666'}}}}, shops = #{1CSWG8j04wM => Shop{id = '1CSWG8j04wM', ",
            "created_at = '2019-08-13T11:19:01.387269Z', blocking = Blocked{reason = '', ",
            "since = '2019-08-13T11:19:03.015222Z'}, suspension = Active{since = '2019-08-13T11:19:01.387269Z'}, ",
            "details = ShopDetails{name = 'Battle Ready Shop'}, location = ShopLocation{url = }, ",
            "category = CategoryRef{id = 1}, account = ShopAccount{currency = CurrencyRef{symbolic_code = 'RUB'}, ",
            "settlement = 7, guarantee = 6, payout = 8}, contract_id = '1CSWG8j04wK', payout_tool_id = '1CSWG8j04wL'}}, ",
            "wallets = #{}, revision = 6}"
            ]),
        format_meta(
            #{args =>
            [{payproc_UserInfo,<<"1CSWG2vduGe">>,
                {external_user,{payproc_ExternalUser}}},
                <<"1CSWG2vduGe">>,
                {revision,6}],
                deadline => {{{2019,8,13},{11,19,33}},42},
                execution_start_time => 1565695143068,function => 'Checkout',
                metadata =>
                #{<<"user-identity.id">> => <<"1CSWG2vduGe">>,
                    <<"user-identity.realm">> => <<"external">>},
                result =>
                {ok,
                    {domain_Party,<<"1CSWG2vduGe">>,
                        {domain_PartyContactInfo,<<"hg_ct_helper">>},
                        <<"2019-08-13T11:19:01.249440Z">>,
                        {unblocked,{domain_Unblocked,<<>>,<<"2019-08-13T11:19:02.655869Z">>}},
                        {active,{domain_Active,<<"2019-08-13T11:19:02.891892Z">>}},
                        #{},
                        #{<<"1CSWG8j04wK">> =>
                        {domain_Contract,<<"1CSWG8j04wK">>,undefined,
                            {domain_PaymentInstitutionRef,1},
                            <<"2019-08-13T11:19:01.387269Z">>,undefined,undefined,
                            {active,{domain_ContractActive}},
                            {domain_TermSetHierarchyRef,1},
                            [],
                            [{domain_PayoutTool,<<"1CSWG8j04wL">>,
                                <<"2019-08-13T11:19:01.387269Z">>,
                                {domain_CurrencyRef,<<"RUB">>},
                                {russian_bank_account,
                                    {domain_RussianBankAccount,<<"4276300010908312893">>,
                                        <<"SomeBank">>,<<"123129876">>,<<"66642666">>}}}],
                            undefined,undefined,
                            {legal_entity,
                                {russian_legal_entity,
                                    {domain_RussianLegalEntity,<<"Hoofs & Horns OJSC">>,
                                        <<"1234509876">>,<<"1213456789012">>,
                                        <<"Nezahualcoyotl 109 Piso 8, Centro, 06082, MEXICO">>,<<"NaN">>,
                                        <<"Director">>,<<"Someone">>,<<"100$ banknote">>,
                                        {domain_RussianBankAccount,<<"4276300010908312893">>,
                                            <<"SomeBank">>,<<"123129876">>,<<"66642666">>}}}}}},
                        #{<<"1CSWG8j04wM">> =>
                        {domain_Shop,<<"1CSWG8j04wM">>,<<"2019-08-13T11:19:01.387269Z">>,
                            {blocked,{domain_Blocked,<<>>,<<"2019-08-13T11:19:03.015222Z">>}},
                            {active,{domain_Active,<<"2019-08-13T11:19:01.387269Z">>}},
                            {domain_ShopDetails,<<"Battle Ready Shop">>,undefined},
                            {url,<<>>},
                            {domain_CategoryRef,1},
                            {domain_ShopAccount,{domain_CurrencyRef,<<"RUB">>},7,6,8},
                            <<"1CSWG8j04wK">>,<<"1CSWG8j04wL">>,undefined}},
                        #{},6}},
                role => server,service => 'PartyManagement',
                service_schema => {dmsl_payment_processing_thrift,'PartyManagement'},
                status => ok,type => call}
        )
    ),
    ?_assertEqual(
        lists:flatten([
            "SignalResult{change = MachineStateChange{aux_state = Content{data = #{}}, ",
            "events = [Content{data = [[2, #{change => \"created\", ",
            "contact_info => [35,123,[\"email\",32,61,62,32,\"\\\"create_customer\\\"\"],\"}\"], ",
            "created_at => \"2019-08-13T11:19:03.714218Z\", customer_id => \"1CSWGJ3N8Ns\", ",
            "metadata => 'Null', owner_id => \"1CSWG2vduGe\", shop_id => \"1CSWG8j04wM\"}]]}]}, ",
            "action = ComplexAction{}}"
        ]),
        format_meta(
            #{args =>
            [{mg_stateproc_SignalArgs,
                {init,
                    {mg_stateproc_InitSignal,
                        {bin,
                            <<131,109,0,0,0,71,11,0,1,0,0,0,11,49,67,83,87,71,50,118,100,117,71,
                                101,11,0,2,0,0,0,11,49,67,83,87,71,56,106,48,52,119,77,12,0,3,11,
                                0,2,0,0,0,15,99,114,101,97,116,101,95,99,117,115,116,111,109,101,
                                114,0,12,0,4,12,0,1,0,0,0>>}}},
                {mg_stateproc_Machine,<<"customer">>,<<"1CSWGJ3N8Ns">>,[],
                    {mg_stateproc_HistoryRange,undefined,undefined,forward},
                    {mg_stateproc_Content,undefined,{bin,<<>>}},
                    undefined,
                    {bin,<<>>}}}],
                deadline => {{{2019,8,13},{11,19,33}},606},
                execution_start_time => 1565695143707,function => 'ProcessSignal',
                metadata =>
                #{<<"user-identity.id">> => <<"1CSWG2vduGe">>,
                    <<"user-identity.realm">> => <<"external">>},
                result =>
                {ok,
                    {mg_stateproc_SignalResult,
                        {mg_stateproc_MachineStateChange,
                            {mg_stateproc_Content,undefined,{obj,#{}}},
                            [{mg_stateproc_Content,undefined,
                                {arr,
                                    [{arr,
                                        [{i,2},
                                            {obj,
                                                #{{str,<<"change">>} => {str,<<"created">>},
                                                    {str,<<"contact_info">>} =>
                                                    {obj,#{{str,<<"email">>} => {str,<<"create_customer">>}}},
                                                    {str,<<"created_at">>} =>
                                                    {str,<<"2019-08-13T11:19:03.714218Z">>},
                                                    {str,<<"customer_id">>} => {str,<<"1CSWGJ3N8Ns">>},
                                                    {str,<<"metadata">>} => {nl,{mg_msgpack_Nil}},
                                                    {str,<<"owner_id">>} => {str,<<"1CSWG2vduGe">>},
                                                    {str,<<"shop_id">>} => {str,<<"1CSWG8j04wM">>}}}]}]}}],
                            undefined,undefined},
                        {mg_stateproc_ComplexAction,undefined,undefined,undefined,undefined}}},
                role => server,service => 'Processor',
                service_schema => {mg_proto_state_processing_thrift,'Processor'},
                status => ok,type => call}
        )
%%    ),
%%    ?_assertEqual(
%%        "InvalidUser{}",
%%        format_meta(
%%            #{args =>
%%            [{payproc_CustomerParams,<<"1CSWGACOup6">>,<<"1CSWGACOup7">>,
%%                {domain_ContactInfo,undefined,<<"invalid_user">>},
%%                {nl,{json_Null}}}],
%%                class => business,deadline => undefined,
%%                execution_start_time => 1565695141702,function => 'Create',ignore => false,
%%                metadata =>
%%                #{<<"user-identity.id">> => <<"1CSWG2vduGe">>,
%%                    <<"user-identity.realm">> => <<"external">>},
%%                result => {payproc_InvalidUser},
%%                role => server,service => 'CustomerManagement',
%%                service_schema => {dmsl_payment_processing_thrift,'CustomerManagement'},
%%                status => error,type => call}
%%        )
    )
].

-endif.