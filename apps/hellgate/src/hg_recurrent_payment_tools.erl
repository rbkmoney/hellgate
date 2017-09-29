%%%
%%% Payment processing machine
%%%

-module(hg_recurrent_payment_tools).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

-define(NS, <<"recurrent_payment_tools">>).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).
-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).
-export([namespace     /0]).
-export([init          /2]).
-export([process_signal/2]).
-export([process_call  /2]).

%% Types
-record(st, {
    rec_payment_tool :: undefined | rec_payment_tool(),
    route            :: undefined | route(),
    risk_score       :: undefined | risk_score(),
    session          :: undefined | session()
}).
-type st() :: #st{}.
-export_type([st/0]).

-type rec_payment_tool_id()     :: dmsl_payment_processing_thrift:'RecurrentPaymentToolID'().
-type rec_payment_tool()        :: dmsl_payment_processing_thrift:'RecurrentPaymentTool'().
-type rec_payment_tool_event()  :: dmsl_payment_processing_thrift:'RecurrentPaymentToolEvent'().
% -type rec_payment_tool_change() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolChange'().
-type rec_payment_tool_params() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolParams'().

-type route()      :: dmsl_domain_thrift:'PaymentRoute'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().

-type session() :: #{
    status      := active | suspended | finished,
    trx         => trx_info(),
    proxy_state => proxy_state()
}.

-type proxy_state()   :: dmsl_proxy_thrift:'ProxyState'().
-type trx_info()      :: dmsl_domain_thrift:'TransactionInfo'().

% -type msgpack_ev() :: hg_msgpack_marshalling:value().

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_handler:handler_opts()) ->
    term() | no_return().
handle_function('GetEvents', [#payproc_EventRange{'after' = After, limit = Limit}], _Opts) ->
    case hg_event_sink:get_events(After, Limit) of
        {ok, Events} ->
            Events;
        {error, event_not_found} ->
            throw(#payproc_EventNotFound{})
    end;
handle_function('GetLastEventID', [], _Opts) ->
    case hg_event_sink:get_last_event_id() of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#payproc_NoLastEvent{})
    end;
handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(recurrent_payment_tools,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('Create', RecurrentPaymentToolParams, _Opts) ->
    RecPaymentToolID = hg_utils:unique_id(),
    ok = set_meta(RecPaymentToolID),
    ok = start(RecPaymentToolID, RecurrentPaymentToolParams),
    get_rec_payment_tool(get_state(RecPaymentToolID));
handle_function_('Abandon', [RecPaymentToolID], _Opts) ->
    ok = set_meta(RecPaymentToolID),
    call(RecPaymentToolID, abandon);
handle_function_('Get', [RecPaymentToolID], _Opts) ->
    ok = set_meta(RecPaymentToolID),
    get_rec_payment_tool(get_state(RecPaymentToolID));
handle_function_('GetEvents', [RecPaymentToolID, Range], _Opts) ->
    ok = set_meta(RecPaymentToolID),
    get_public_history(RecPaymentToolID, Range).

get_public_history(RecPaymentToolID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    [publish_rec_payment_tool_event(RecPaymentToolID, Ev) || Ev <- get_history(RecPaymentToolID, AfterID, Limit)].

publish_rec_payment_tool_event(RecPaymentToolID, Event) ->
    {ID, Dt, Payload} = unmarshal(Event),
    #payproc_CustomerEvent{
        id = ID,
        created_at = Dt,
        source = RecPaymentToolID,
        payload = Payload
    }.

%%

set_meta(ID) ->
    hg_log_scope:set_meta(#{rec_payment_tool_id => ID}).

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

-spec map_error({ok, _Result} | {error, _Error}) ->
    _Result | no_return().
map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_RecurrentPaymentToolNotFound{});
map_error({error, Reason}) ->
    error(Reason).

%%

get_history(RecPaymentToolID) ->
    History = hg_machine:get_history(?NS, RecPaymentToolID),
    unmarshal(map_history_error(History)).

get_history(RecPaymentToolID, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, RecPaymentToolID, AfterID, Limit),
    unmarshal(map_history_error(History)).

get_state(RecPaymentToolID) ->
    collapse_history(get_history(RecPaymentToolID)).

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Events}, St0) ->
            lists:foldl(fun apply_change/2, St0, Events)
        end,
        #st{},
        History
    ).

%%

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_CustomerNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

-include("domain.hrl").
-include("recurrent_payment_tools.hrl").

%% hg_machine callbacks

-spec namespace() ->
    hg_machine:ns().
namespace() ->
    ?NS.

-spec init(rec_payment_tool_id(), rec_payment_tool_params()) ->
    hg_machine:result(rec_payment_tool_event()).
init(RecPaymentToolID, Params) ->
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    {Party, Shop} = get_party_shop(Params),
    MerchantTerms = get_merchant_payments_terms(Shop, Party, CreatedAt, Revision),
    VS0 = collect_varset(Party, Shop, #{}),
    {RecPaymentTool ,  VS1} = create_rec_payment_tool(RecPaymentToolID, CreatedAt, Params, MerchantTerms, VS0, Revision),
    {RiskScore      ,  VS2} = validate_risk_score(inspect(RecPaymentTool, VS1), VS1),
    {Route          , _VS3} = validate_route(hg_routing:choose(VS2, Revision), RecPaymentTool, VS2),
    {ok, {Changes, Action}} = start_session(),
    handle_result(#{
        changes => [?recurrent_payment_tool_has_created(RecPaymentTool, RiskScore, Route) | Changes],
        action => Action
    }).

get_party_shop(Params) ->
    PartyID = Params#payproc_RecurrentPaymentToolParams.party_id,
    ShopID = Params#payproc_RecurrentPaymentToolParams.shop_id,
    Party = hg_party_machine:get_party(PartyID),
    Shop = hg_party:get_shop(ShopID, Party),
    {Party, Shop}.

get_merchant_payments_terms(Shop, Party, CreatedAt, Revision) ->
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_contract_active(Contract),
    TermSet = hg_party:get_terms(Contract, CreatedAt, Revision),
    TermSet#domain_TermSet.payments.

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    % FIXME no such exception on the service interface
    throw(#payproc_InvalidContractStatus{status = Status}).

collect_varset(Party, Shop = #domain_Shop{
    category = Category,
    account = #domain_ShopAccount{currency = Currency}
}, VS) ->
    VS#{
        party    => Party,
        shop     => Shop,
        category => Category,
        currency => Currency
    }.

inspect(_RecPaymentTool, _VS) ->
    % FIXME please senpai
    high.

validate_risk_score(RiskScore, VS) when RiskScore == low; RiskScore == high ->
    {RiskScore, VS#{risk_score => RiskScore}}.

validate_route(Route = #domain_PaymentRoute{}, _RecPaymentTool, VS) ->
    {Route, VS};
validate_route(undefined, RecPaymentTool, _VS) ->
    error({misconfiguration, {'No route found for a recurrent payment tool', RecPaymentTool}}).

start_session() ->
    Events = [?session_ev(?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

-spec process_signal(hg_machine:signal(), hg_machine:history(rec_payment_tool_event())) ->
    hg_machine:result(rec_payment_tool_event()).
process_signal(Signal, History) ->
    handle_result(handle_signal(Signal, collapse_history(unmarshal(History)))).

handle_signal(timeout, St) ->
    process_timeout(St).

process_timeout(St) ->
    Action = hg_machine_action:new(),
    case get_session_status(get_session(St)) of
        active ->
            process(Action, St);
        suspended ->
            process_callback_timeout(Action, St)
    end.

get_session(#st{session = Session}) ->
    Session.

get_session_status(Session) ->
    maps:get(status, Session).

process(Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, ProxyResult} = hg_proxy_provider:generate_token(ProxyContext, get_route(St)),
    Result = handle_proxy_result(ProxyResult, Action, get_session(St)),
    finish_processing(Result, St).

process_callback_timeout(Action, St) ->
    Result = handle_proxy_callback_timeout(Action),
    finish_processing(Result, St).

get_route(#st{route = Route}) ->
    Route.

%%

construct_proxy_context(St) ->
    #prxprv_RecurrentTokenContext{
        session    = construct_session(St),
        token_info = construct_token_info(St),
        options    = hg_proxy_provider:collect_proxy_options(St)
    }.

construct_session(St) ->
    #prxprv_RecurrentTokenSession{
        state = maps:get(proxy_state, get_session(St), undefined)
    }.

construct_token_info(St) ->
    #prxprv_RecurrentTokenInfo{
        payment_tool = construct_proxy_payment_tool(get_rec_payment_tool(St)),
        trx          = get_session_trx(get_session(St))
    }.

get_session_trx(#{trx := Trx}) ->
    Trx.

get_rec_payment_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

construct_proxy_payment_tool(
    #payproc_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        minimal_payment_cost = Cash
    }
) ->
    #prxprv_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        minimal_payment_cost = Cash
    }.

%%

handle_proxy_result(
    #prxprv_RecurrentTokenProxyResult{
        intent = {_Type, Intent},
        trx = Trx,
        next_state = ProxyState
    },
    Action0,
    Session
) ->
    Changes1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState),
    {Changes3, Action} = hg_proxy_provider:handle_proxy_intent(Intent, Action0),
    Changes = Changes1 ++ Changes2 ++ Changes3,
    case Intent of
        #prxprv_RecurrentTokenFinishIntent{status = {success, {token, Token}}} ->
            make_proxy_result(Changes, Action, Token);
        _ ->
            make_proxy_result(Changes, Action)
    end.

handle_callback_result(
    #prxprv_RecurrentTokenCallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

handle_proxy_callback_result(
    #prxprv_RecurrentTokenProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Changes1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState),
    {Changes3, Action} = hg_proxy_provider:handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0)),
    {wrap_session_events([?session_activated()] ++ Changes1 ++ Changes2 ++ Changes3), Action};
handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Changes1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState),
    {wrap_session_events(Changes1 ++ Changes2), Action0}.

make_proxy_result(Changes, Action) ->
    make_proxy_result(Changes, Action, undefined).

make_proxy_result(Changes, Action, Token) ->
    {wrap_session_events(Changes), Action, Token}.

%%

handle_proxy_callback_timeout(Action) ->
    Changes = [?session_finished(?session_failed(?operation_timeout()))],
    {wrap_session_events(Changes), Action}.

wrap_session_events(SessionEvents) ->
    [?session_ev(Ev) || Ev <- SessionEvents].

%%

finish_processing({Changes, Action, Token}, St) ->
    St1 = apply_changes(Changes, St),
    case get_session(St1) of
        #{status := finished, result := ?session_succeeded()} ->
            {done, {Changes ++ [?recurrent_payment_tool_has_acquired(Token)], Action}};
        #{status := finished, result := ?session_failed(Failure)} ->
            {done, {Changes ++ [?recurrent_payment_tool_has_failed(Failure)], Action}};
        #{} ->
            {next, {Changes, Action}}
    end.

apply_changes(Changes, St) ->
    lists:foldl(fun apply_change/2, St, Changes).

apply_change(Event, undefined) ->
    apply_change(Event, #st{});

apply_change(?recurrent_payment_tool_has_created(RecPaymentTool, RiskScore, Route), St) ->
    St#st{
        rec_payment_tool = RecPaymentTool,
        risk_score = RiskScore,
        route = Route
    };
apply_change(?recurrent_payment_tool_has_acquired(Token), St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    St#st{
        rec_payment_tool = RecPaymentTool#payproc_RecurrentPaymentTool{
            rec_token = Token,
            status = ?recurrent_payment_tool_acquired()
        }
    };
apply_change(?recurrent_payment_tool_has_abandoned(), St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    St#st{
        rec_payment_tool = RecPaymentTool#payproc_RecurrentPaymentTool{
            status = ?recurrent_payment_tool_abandoned()
        }
    };
apply_change(?session_ev(?session_started()), St) ->
    St#st{session = create_session()};
apply_change(?session_ev(Event), St) ->
    Session = merge_session_change(Event, get_session(St)),
    St#st{session = Session}.

merge_session_change(?session_finished(Result), Session) ->
    Session#{status := finished, result => Result};
merge_session_change(?session_activated(), Session) ->
    Session#{status := active};
merge_session_change(?session_suspended(), Session) ->
    Session#{status := suspended};
merge_session_change(?trx_bound(Trx), Session) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session) ->
    Session.

%%

create_session() ->
    #{
        status => active
    }.

-type call() :: abandon.

-spec process_call(call(), hg_machine:history(rec_payment_tool_event())) ->
    {hg_machine:response(), hg_machine:result(rec_payment_tool_event())}.
process_call(Call, History) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, {[], hg_machine_action:new()}}
    end.

handle_call(abandon, St) ->
    ok = assert_rec_payment_tool_status(recurrent_payment_tool_acquired, St),
    #{
        response => ok,
        changes  => [?recurrent_payment_tool_has_abandoned()]
    };
handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

dispatch_callback({provider, Payload}, St) ->
    process_callback(Payload, St);
dispatch_callback(_Callback, _St) ->
    throw(invalid_callback).

process_callback(Payload, St) ->
    Action = hg_machine_action:new(),
    case get_session_status(get_session(St)) of
        suspended ->
            handle_callback(Payload, Action, St);
        _ ->
            throw(invalid_callback)
    end.

handle_callback(Payload, Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, CallbackResult} = hg_proxy_provider:handle_recurrent_token_callback(
        Payload,
        ProxyContext,
        get_route(St)
    ),
    {Response, Result} = handle_callback_result(CallbackResult, Action, get_session(St)),
    {Response, finish_processing(Result, St)}.

handle_result(Result) ->
    Changes = maps:get(changes, Result, []),
    Action = maps:get(action, Result, hg_machine_action:new()),
    case maps:find(response, Result) of
        {ok, Response} ->
            {{ok, Response}, {[marshal(Changes)], Action}};
        error ->
            {[marshal(Changes)], Action}
    end.

%%

assert_rec_payment_tool_status(recurrent_payment_tool_acquired, St) ->
    ?recurrent_payment_tool_acquired() = get_rec_payment_tool_status(get_rec_payment_tool(St)).

get_rec_payment_tool_status(RecPaymentTool) ->
    RecPaymentTool#payproc_RecurrentPaymentTool.status.

%%

create_rec_payment_tool(RecPaymentToolID, CreatedAt, Params, Terms, VS0, Revision) ->
    PaymentResource = Params#payproc_RecurrentPaymentToolParams.disposable_payment_resource,
    VS1 = validate_payment_tool(
        get_payment_tool(PaymentResource),
        Terms#domain_PaymentsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS2 = validate_cost(
        Terms#domain_PaymentsServiceTerms.cash_limit,
        VS1,
        Revision
    ),
    {#payproc_RecurrentPaymentTool{
        id                 = RecPaymentToolID,
        shop_id            = Params#payproc_RecurrentPaymentToolParams.shop_id,
        party_id           = Params#payproc_RecurrentPaymentToolParams.party_id,
        domain_revision    = Revision,
        status             = ?recurrent_payment_tool_created(),
        created_at         = CreatedAt,
        payment_resource   = PaymentResource,
        rec_token          = undefined
    }, VS2}.

validate_payment_tool(PaymentTool, PaymentMethodSelector, VS, Revision) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw(#'InvalidRequest'{errors = [<<"Invalid payment method">>]}),
    VS#{payment_tool => PaymentTool}.

validate_cost(CashLimitSelector, VS, Revision) ->
    CashLimit = reduce_selector(cash_limit, CashLimitSelector, VS, Revision),
    % FIXME
    {_Exclusiveness, Cash} = CashLimit#domain_CashRange.lower,
    VS#{cost => Cash}.

reduce_selector(Name, Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

get_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.

%%
%% Marshalling
%%

marshal(Changes) when is_list(Changes) ->
    [marshal(change, Change) || Change <- Changes].

marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, change}, Payload)}.

unmarshal(_, Other) ->
    Other.


%%
%% Event sink
%%

