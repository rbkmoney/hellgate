-module(hg_machine).

-type id() :: mg_proto_base_thrift:'ID'().
-type tag() :: {tag, mg_proto_base_thrift:'Tag'()}.
-type ref() :: id() | tag().
-type ns() :: mg_proto_base_thrift:'Namespace'().
-type args() :: _.

-type machine() :: mg_proto_state_processing_thrift:'Machine'().

-type event() :: event(_).
-type event(T) :: {event_id(), timestamp(), T}.
-type event_id() :: mg_proto_base_thrift:'EventID'().
-type timestamp() :: mg_proto_base_thrift:'Timestamp'().

-type history() :: history(_).
-type history(T) :: [event(T)].

-type history_range() :: mg_proto_state_processing_thrift:'HistoryRange'().
-type descriptor()    :: mg_proto_state_processing_thrift:'MachineDescriptor'().

-type result(T) :: {[T], hg_machine_action:t()}.
-type result() :: result(_).

-callback namespace() ->
    ns().

-callback init(id(), args()) ->
    result().

-type signal() ::
    timeout | {repair, args()}.

-callback process_signal(signal(), history()) ->
    result().

-type call() :: _.
-type response() :: ok | {ok, term()} | {exception, term()}.

-callback process_call(call(), history()) ->
    {response(), result()}.

-type context() :: #{
    client_context => woody_context:ctx()
}.

-export_type([id/0]).
-export_type([ref/0]).
-export_type([tag/0]).
-export_type([ns/0]).
-export_type([event_id/0]).
-export_type([event/0]).
-export_type([event/1]).
-export_type([history/0]).
-export_type([history/1]).
-export_type([signal/0]).
-export_type([result/0]).
-export_type([result/1]).
-export_type([context/0]).
-export_type([response/0]).

-export([start/3]).
-export([call/3]).
-export([get_history/2]).
-export([get_history/4]).

%% Dispatch

-export([get_child_spec/1]).
-export([get_service_handlers/1]).
-export([get_handler_module/1]).

-export([start_link/1]).
-export([init/1]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%%

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").


%%

-spec start(ns(), id(), term()) ->
    {ok, term()} | {error, exists | term()} | no_return().

start(Ns, ID, Args) ->
    call_automaton('Start', [Ns, ID, wrap_args(Args)]).

-spec call(ns(), ref(), term()) ->
    {ok, term()} | {error, notfound | failed} | no_return().

call(Ns, Ref, Args) ->
    Descriptor = prepare_descriptor(Ns, Ref, #'HistoryRange'{}),
    case call_automaton('Call', [Descriptor, wrap_args(Args)]) of
        {ok, Response} ->
            % should be specific to a processing interface already
            {ok, unmarshal_term(Response)};
        {error, _} = Error ->
            Error
    end.

-spec get_history(ns(), ref()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref) ->
    get_history(Ns, Ref, #'HistoryRange'{}).

-spec get_history(ns(), ref(), undefined | event_id(), undefined | non_neg_integer()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref, AfterID, Limit) ->
    get_history(Ns, Ref, #'HistoryRange'{'after' = AfterID, limit = Limit}).

get_history(Ns, Ref, Range) ->
    Descriptor = prepare_descriptor(Ns, Ref, Range),
    case call_automaton('GetMachine', [Descriptor]) of
        {ok, #'Machine'{history = History}} when is_list(History) ->
            {ok, unmarshal(History)};
        Error ->
            Error
    end.

%%

call_automaton(Function, Args) ->
    case hg_woody_wrapper:call(automaton, Function, Args) of
        {ok, _} = Result ->
            Result;
        {exception, #'MachineAlreadyExists'{}} ->
            {error, exists};
        {exception, #'MachineNotFound'{}} ->
            {error, notfound};
        {exception, #'MachineFailed'{}} ->
            {error, failed}
    end.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(machine,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(func(), woody:args(), #{ns := ns()}) -> term() | no_return().

handle_function_('ProcessSignal', [Args], #{ns := Ns} = _Opts) ->
    #'SignalArgs'{signal = {Type, Signal}, machine = #'Machine'{id = ID} = Machine} = Args,
    hg_log_scope:set_meta(#{
        namespace => Ns,
        id => ID,
        activity => signal,
        signal => Type
    }),
    dispatch_signal(Ns, Signal, Machine);

handle_function_('ProcessCall', [Args], #{ns := Ns} = _Opts) ->
    #'CallArgs'{arg = Payload, machine = #'Machine'{id = ID} = Machine} = Args,
    hg_log_scope:set_meta(#{
        namespace => Ns,
        id => ID,
        activity => call
    }),
    dispatch_call(Ns, Payload, Machine).

%%

-spec dispatch_signal(ns(), Signal, machine()) ->
    Result when
        Signal ::
            mg_proto_state_processing_thrift:'InitSignal'() |
            mg_proto_state_processing_thrift:'TimeoutSignal'() |
            mg_proto_state_processing_thrift:'RepairSignal'(),
        Result ::
            mg_proto_state_processing_thrift:'SignalResult'().

dispatch_signal(Ns, #'InitSignal'{arg = Payload}, #'Machine'{id = ID}) ->
    Args = unwrap_args(Payload),
    _ = lager:debug("dispatch init with id = ~s and args = ~p", [ID, Args]),
    Module = get_handler_module(Ns),
    Result = Module:init(ID, Args),
    marshal_signal_result(Result);

dispatch_signal(Ns, #'TimeoutSignal'{}, #'Machine'{history = History}) ->
    _ = lager:debug("dispatch timeout with history = ~p", [History]),
    Module = get_handler_module(Ns),
    Result = Module:process_signal(timeout, unmarshal(History)),
    marshal_signal_result(Result);

dispatch_signal(Ns, #'RepairSignal'{arg = Payload}, #'Machine'{history = History}) ->
    Args = unwrap_args(Payload),
    _ = lager:debug("dispatch repair with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    Result = Module:process_signal({repair, Args}, unmarshal(History)),
    marshal_signal_result(Result).

marshal_signal_result({Events, Action}) ->
    _ = lager:debug("signal result with events = ~p and action = ~p", [Events, Action]),
    Change = #'MachineStateChange'{
        events = marshal(Events),
        aux_state = {nl, #msgpack_Nil{}} %%% @TODO get state from process signal?
    },
    #'SignalResult'{
        change = Change,
        action = Action
    }.

-spec dispatch_call(ns(), Call, machine()) ->
    Result when
        Call :: mg_proto_state_processing_thrift:'Args'(),
        Result :: mg_proto_state_processing_thrift:'CallResult'().

dispatch_call(Ns, Payload, #'Machine'{history = History}) ->
    Args = unwrap_args(Payload),
    _ = lager:debug("dispatch call with args = ~p and history: ~p", [Args, History]),
    Module = get_handler_module(Ns),
    Result = Module:process_call(Args, unmarshal(History)),
    marshal_call_result(Result).

marshal_call_result({Response, {Events, Action}}) ->
    _ = lager:debug("call response = ~p with event = ~p and action = ~p", [Response, Events, Action]),
    Change = #'MachineStateChange'{
        events = marshal(Events),
        aux_state = {nl, #msgpack_Nil{}} %%% @TODO get state from process signal?
    },

    #'CallResult'{
        change = Change,
        action = Action,
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

-spec get_service_handlers([MachineHandler :: module()]) ->
    [service_handler()].

get_service_handlers(MachineHandlers) ->
    lists:map(fun get_service_handler/1, MachineHandlers).

get_service_handler(MachineHandler) ->
    Ns = MachineHandler:namespace(),
    {Path, Service} = hg_proto:get_service_spec(processor, #{namespace => Ns}),
    {Path, {Service, {hg_woody_wrapper, #{ns => Ns, handler => ?MODULE}}}}.

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

marshal(Events) when is_list(Events) ->
    [hg_msgpack_marshalling:marshal(Event) || Event <- Events].

unmarshal(#'Event'{id = ID, created_at = Dt, event_payload = Payload}) ->
    {ID, Dt, hg_msgpack_marshalling:unmarshal(Payload)};
unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events].

%%

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
    #'MachineDescriptor'{
        ns = NS,
        ref = prepare_ref(Ref),
        range = Range
    }.

prepare_ref(ID) when is_binary(ID) ->
    {id, ID};
prepare_ref({tag, Tag}) ->
    {tag, Tag}.
