-module(hg_machine).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type msgp()    :: hg_msgpack_marshalling:msgpack_value().

-type id() :: mg_proto_base_thrift:'ID'().
-type tag() :: {tag, mg_proto_base_thrift:'Tag'()}.
-type ref() :: id() | tag().
-type ns() :: mg_proto_base_thrift:'Namespace'().
-type args() :: _.

-type event(T) :: {event_id(), timestamp(), T}.
-type event() :: event(event_payload()).
-type event_id() :: mg_proto_base_thrift:'EventID'().
-type event_payload() :: #{
    data := msgp(),
    format_version := pos_integer() | undefined
}.
-type timestamp() :: mg_proto_base_thrift:'Timestamp'().
-type history() :: [event()].
-type auxst() :: msgp().

-type history_range() :: mg_proto_state_processing_thrift:'HistoryRange'().
-type direction()     :: mg_proto_state_processing_thrift:'Direction'().
-type descriptor()    :: mg_proto_state_processing_thrift:'MachineDescriptor'().

-type machine() :: #{
    id          := id(),
    history     := history(),
    aux_state   := auxst()
}.

-type result() :: #{
    events    => [event_payload()],
    action    => hg_machine_action:t(),
    auxst     => auxst()
}.

-callback namespace() ->
    ns().

-callback init(args(), machine()) ->
    result().

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), machine()) ->
    result().

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), machine()) ->
    {response(), result()}.

-type context() :: #{
    client_context => woody_context:ctx()
}.

-export_type([id/0]).
-export_type([ref/0]).
-export_type([tag/0]).
-export_type([ns/0]).
-export_type([event_id/0]).
-export_type([event_payload/0]).
-export_type([event/0]).
-export_type([event/1]).
-export_type([history/0]).
-export_type([auxst/0]).
-export_type([signal/0]).
-export_type([result/0]).
-export_type([context/0]).
-export_type([response/0]).
-export_type([machine/0]).

-export([start/3]).
-export([call/3]).
-export([call/6]).
-export([repair/3]).
-export([get_history/2]).
-export([get_history/4]).
-export([get_history/5]).
-export([get_machine/5]).

%% Dispatch

-export([get_child_spec/1]).
-export([get_service_handlers/2]).
-export([get_handler_module/1]).

-export([start_link/1]).
-export([init/1]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Internal types

-type mg_event() :: mg_proto_state_processing_thrift:'Event'().
-type mg_event_payload() :: mg_proto_state_processing_thrift:'EventBody'().

%%

-spec start(ns(), id(), term()) ->
    {ok, term()} | {error, exists | term()} | no_return().

start(Ns, ID, Args) ->
    call_automaton('Start', [Ns, ID, wrap_args(Args)]).

-spec call(ns(), ref(), Args :: term()) ->
    {ok, term()} | {error, notfound | failed} | no_return().

call(Ns, Ref, Args) ->
    call(Ns, Ref, Args, undefined, undefined, forward).

-spec call(
    ns(),
    ref(),
    Args :: term(),
    After :: event_id() | undefined,
    Limit :: integer() | undefined,
    Direction :: forward | backward
) ->
    {ok, term()} | {error, notfound | failed} | no_return().

call(Ns, Ref, Args, After, Limit, Direction) ->
    HistoryRange = #mg_stateproc_HistoryRange{
        'after' = After,
        'limit' = Limit,
        'direction' = Direction
    },
    Descriptor = prepare_descriptor(Ns, Ref, HistoryRange),
    case call_automaton('Call', [Descriptor, wrap_args(Args)]) of
        {ok, Response} ->
            % should be specific to a processing interface already
            {ok, unmarshal_term(Response)};
        {error, _} = Error ->
            Error
    end.

-spec repair(ns(), ref(), term()) ->
    {ok, term()} | {error, notfound | failed | working} | no_return().

repair(Ns, Ref, Args) ->
    Descriptor = prepare_descriptor(Ns, Ref, #mg_stateproc_HistoryRange{}),
    call_automaton('Repair', [Descriptor, wrap_args(Args)]).

-spec get_history(ns(), ref()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref) ->
    get_history(Ns, Ref, undefined, undefined, forward).

-spec get_history(ns(), ref(), undefined | event_id(), undefined | non_neg_integer()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref, AfterID, Limit) ->
    get_history(Ns, Ref, AfterID, Limit, forward).

-spec get_history(ns(), ref(), undefined | event_id(), undefined | non_neg_integer(), direction()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref, AfterID, Limit, Direction) ->
    case get_machine(Ns, Ref, AfterID, Limit, Direction) of
        {ok, #{history := History}} ->
            {ok, History};
        Error ->
            Error
    end.

-spec get_machine(ns(), ref(), undefined | event_id(), undefined | non_neg_integer(), direction()) ->
    {ok, machine()} | {error, notfound} | no_return().

get_machine(Ns, Ref, AfterID, Limit, Direction) ->
    Range = #mg_stateproc_HistoryRange{'after' = AfterID, limit = Limit, direction = Direction},
    Descriptor = prepare_descriptor(Ns, Ref, Range),
    case call_automaton('GetMachine', [Descriptor]) of
        {ok, #mg_stateproc_Machine{} = Machine} ->
            {ok, unmarshal_machine(Machine)};
        Error ->
            Error
    end.

%%

call_automaton(Function, Args) ->
    case hg_woody_wrapper:call(automaton, Function, Args) of
        {ok, _} = Result ->
            Result;
        {exception, #mg_stateproc_MachineAlreadyExists{}} ->
            {error, exists};
        {exception, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound};
        {exception, #mg_stateproc_MachineFailed{}} ->
            {error, failed};
        {exception, #mg_stateproc_MachineAlreadyWorking{}} ->
            {error, working}
    end.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(machine,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(func(), woody:args(), #{ns := ns()}) -> term() | no_return().

handle_function_('ProcessSignal', [Args], #{ns := Ns} = _Opts) ->
    #mg_stateproc_SignalArgs{signal = {Type, Signal}, machine = #mg_stateproc_Machine{id = ID} = Machine} = Args,
    scoper:add_meta(#{
        namespace => Ns,
        id => ID,
        activity => signal,
        signal => Type
    }),
    dispatch_signal(Ns, Signal, unmarshal_machine(Machine));

handle_function_('ProcessCall', [Args], #{ns := Ns} = _Opts) ->
    #mg_stateproc_CallArgs{arg = Payload, machine = #mg_stateproc_Machine{id = ID} = Machine} = Args,
    scoper:add_meta(#{
        namespace => Ns,
        id => ID,
        activity => call
    }),
    dispatch_call(Ns, Payload, unmarshal_machine(Machine)).

%%

-spec dispatch_signal(ns(), Signal, machine()) ->
    Result when
        Signal ::
            mg_proto_state_processing_thrift:'InitSignal'() |
            mg_proto_state_processing_thrift:'TimeoutSignal'() |
            mg_proto_state_processing_thrift:'RepairSignal'(),
        Result ::
            mg_proto_state_processing_thrift:'SignalResult'().

dispatch_signal(Ns, #mg_stateproc_InitSignal{arg = Payload}, Machine) ->
    Args = unwrap_args(Payload),
    _ = log_dispatch(init, Args, Machine),
    Module = get_handler_module(Ns),
    Result = Module:init(Args, Machine),
    marshal_signal_result(Result, Machine);

dispatch_signal(Ns, #mg_stateproc_TimeoutSignal{}, Machine) ->
    _ = log_dispatch(timeout, Machine),
    Module = get_handler_module(Ns),
    Result = Module:process_signal(timeout, Machine),
    marshal_signal_result(Result, Machine);

dispatch_signal(Ns, #mg_stateproc_RepairSignal{arg = Payload}, Machine) ->
    Args = unwrap_args(Payload),
    _ = log_dispatch(repair, Args, Machine),
    Module = get_handler_module(Ns),
    Result = Module:process_signal({repair, Args}, Machine),
    marshal_signal_result(Result, Machine).

marshal_signal_result(Result = #{}, #{aux_state := AuxStWas}) ->
    _ = lager:debug("signal result = ~p", [Result]),
    Change = #mg_stateproc_MachineStateChange{
        events = marshal_events(maps:get(events, Result, [])),
        aux_state = marshal_aux_st_format(maps:get(auxst, Result, AuxStWas))
    },
    #mg_stateproc_SignalResult{
        change = Change,
        action = maps:get(action, Result, hg_machine_action:new())
    }.

-spec dispatch_call(ns(), Call, machine()) ->
    Result when
        Call :: mg_proto_state_processing_thrift:'Args'(),
        Result :: mg_proto_state_processing_thrift:'CallResult'().

dispatch_call(Ns, Payload, Machine) ->
    Args = unwrap_args(Payload),
    _ = log_dispatch(call, Args, Machine),
    Module = get_handler_module(Ns),
    Result = Module:process_call(Args, Machine),
    marshal_call_result(Result, Machine).

marshal_call_result({Response, Result}, #{aux_state := AuxStWas}) ->
    _ = lager:debug("call response = ~p with result = ~p", [Response, Result]),
    Change = #mg_stateproc_MachineStateChange{
        events = marshal_events(maps:get(events, Result, [])),
        aux_state = marshal_aux_st_format(maps:get(auxst, Result, AuxStWas))
    },
    #mg_stateproc_CallResult{
        change = Change,
        action = maps:get(action, Result, hg_machine_action:new()),
        response = marshal_term(Response)
    }.

%%

-type service_handler() ::
    {Path :: string(), {woody:service(), {module(), hg_woody_wrapper:handler_opts()}}}.

-spec get_child_spec([MachineHandler :: module()]) ->
    supervisor:child_spec().

get_child_spec(MachineHandlers) ->
    #{
        id => hg_machine_dispatch,
        start => {?MODULE, start_link, [MachineHandlers]},
        type => supervisor
    }.

-spec get_service_handlers([MachineHandler :: module()], map()) ->
    [service_handler()].

get_service_handlers(MachineHandlers, Opts) ->
    [get_service_handler(H, Opts) || H <- MachineHandlers].

get_service_handler(MachineHandler, Opts) ->
    Ns = MachineHandler:namespace(),
    FullOpts = maps:merge(#{ns => Ns, handler => ?MODULE}, Opts),
    {Path, Service} = hg_proto:get_service_spec(processor, #{namespace => Ns}),
    {Path, {Service, {hg_woody_wrapper, FullOpts}}}.

%%

-define(TABLE, hg_machine_dispatch).

-spec start_link([module()]) ->
    {ok, pid()}.

start_link(MachineHandlers) ->
    supervisor:start_link(?MODULE, MachineHandlers).

-spec init([module()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(MachineHandlers) ->
    _ = ets:new(?TABLE, [protected, named_table, {read_concurrency, true}]),
    true = ets:insert_new(?TABLE, [{MH:namespace(), MH} || MH <- MachineHandlers]),
    {ok, {#{}, []}}.

%%

-spec get_handler_module(ns()) -> module().

get_handler_module(Ns) ->
    ets:lookup_element(?TABLE, Ns, 2).

log_dispatch(Operation, #{id := ID, history := History, aux_state := AuxSt}) ->
    lager:debug(
        "dispatch ~p with id = ~p, history = ~p, aux state = ~p",
        [Operation, ID, History, AuxSt]
    ).

log_dispatch(Operation, Args, #{id := ID, history := History, aux_state := AuxSt}) ->
    lager:debug(
        "dispatch ~p with id = ~p, args = ~p, history = ~p, aux state = ~p",
        [Operation, ID, Args, History, AuxSt]
    ).

unmarshal_machine(#mg_stateproc_Machine{id = ID, history = History} = Machine) ->
    AuxState = get_aux_state(Machine),
    #{
        id        => ID,
        history   => unmarshal_events(History),
        aux_state => AuxState
    }.

-spec marshal_events([event_payload()]) ->
    [mg_event_payload()].
marshal_events(Events) when is_list(Events) ->
    [marshal_event(Event) || Event <- Events].

-spec marshal_event(event_payload()) ->
    mg_event_payload().
marshal_event(#{format_version := Format, data := Data}) ->
    #mg_stateproc_Content{
        format_version = Format,
        data = mg_msgpack_marshalling:marshal(Data)
    }.

marshal_aux_st_format(AuxSt) ->
    #mg_stateproc_Content{
        format_version = undefined,
        data = mg_msgpack_marshalling:marshal(AuxSt)
    }.

-spec unmarshal_events([mg_event()]) ->
    [event()].
unmarshal_events(Events) when is_list(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(mg_event()) ->
    event().
unmarshal_event(#mg_stateproc_Event{id = ID, created_at = Dt, format_version = Format, data = Payload}) ->
    {ID, Dt, #{format_version => Format, data => mg_msgpack_marshalling:unmarshal(Payload)}}.

unmarshal_aux_st(Data) ->
    mg_msgpack_marshalling:unmarshal(Data).

get_aux_state(#mg_stateproc_Machine{aux_state = #mg_stateproc_Content{format_version = undefined, data = Data}}) ->
    unmarshal_aux_st(Data).

wrap_args(Args) ->
    marshal_term(Args).

unwrap_args(Payload) ->
    unmarshal_term(Payload).

marshal_term(V) ->
    {bin, term_to_binary(V)}.

unmarshal_term({bin, B}) ->
    binary_to_term(B).

-spec prepare_descriptor(ns(), ref(), history_range()) -> descriptor().
prepare_descriptor(NS, Ref, Range) ->
    #mg_stateproc_MachineDescriptor{
        ns = NS,
        ref = prepare_ref(Ref),
        range = Range
    }.

prepare_ref(ID) when is_binary(ID) ->
    {id, ID};
prepare_ref({tag, Tag}) ->
    {tag, Tag}.
