%%%
%%% Payment processing machine
%%%

-module(hg_recurrent_paytool).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").

-define(NS, <<"recurrent_paytools">>).

%% Public interface

-export([assert_operation_permitted/2]).

-export([process_callback/2]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).
-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).
-export([namespace     /0]).
-export([init          /2]).
-export([process_signal/3]).
-export([process_call  /3]).

%% Types
-record(st, {
    rec_payment_tool     :: undefined | rec_payment_tool(),
    route                :: undefined | route(),
    risk_score           :: undefined | risk_score(),
    session              :: undefined | session(),
    minimal_payment_cost :: undefined | cash()
}).
-type st() :: #st{}.
-export_type([st/0]).

-type rec_payment_tool_id()     :: dmsl_payment_processing_thrift:'RecurrentPaymentToolID'().
-type rec_payment_tool()        :: dmsl_payment_processing_thrift:'RecurrentPaymentTool'().
-type rec_payment_tool_params() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolParams'().

-type route()      :: dmsl_domain_thrift:'PaymentRoute'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().
-type cash()       :: dmsl_domain_thrift:'Cash'().

-type shop()           :: dmsl_domain_thrift:'Shop'().
-type party()          :: dmsl_domain_thrift:'Party'().
-type merchant_terms() :: dmsl_domain_thrift:'RecurrentPaytoolsServiceTerms'().
-type payment_tool()   :: dmsl_domain_thrift:'PaymentTool'().

-type session() :: #{
    status      := active | suspended | finished,
    result      => session_result(),
    trx         => undefined | trx_info(),
    proxy_state => proxy_state()
}.

-type proxy_state()    :: dmsl_proxy_thrift:'ProxyState'().
-type trx_info()       :: dmsl_domain_thrift:'TransactionInfo'().
-type session_result() :: dmsl_payment_processing_thrift:'SessionResult'().


%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().
handle_function('GetEvents', [#payproc_EventRange{'after' = After, limit = Limit}], _Opts) ->
    case hg_event_sink:get_events(?NS, After, Limit) of
        {ok, Events} ->
            publish_events(Events);
        {error, event_not_found} ->
            throw(#payproc_EventNotFound{})
    end;
handle_function('GetLastEventID', [], _Opts) ->
    case hg_event_sink:get_last_event_id(?NS) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#payproc_NoLastEvent{})
    end;
handle_function(Func, Args, Opts) ->
    scoper:scope(recurrent_payment_tools,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('Create', [RecurrentPaymentToolParams], _Opts) ->
    RecPaymentToolID = hg_utils:unique_id(),
    ok = set_meta(RecPaymentToolID),
    Party = ensure_party_accessible(RecurrentPaymentToolParams),
    Shop = ensure_shop_exists(RecurrentPaymentToolParams),
    ok = assert_party_shop_operable(Shop, Party),
    MerchantTerms = assert_operation_permitted(Shop, Party),
    PaymentTool = validate_payment_tool(
        get_payment_tool(RecurrentPaymentToolParams#payproc_RecurrentPaymentToolParams.payment_resource),
        MerchantTerms#domain_RecurrentPaytoolsServiceTerms.payment_methods,
        collect_varset(Party, Shop, #{})
    ),
    ok = start(RecPaymentToolID, [PaymentTool, RecurrentPaymentToolParams]),
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
    #payproc_RecurrentPaymentToolEvent{
        id = ID,
        created_at = Dt,
        source = RecPaymentToolID,
        payload = Payload
    }.

%%

set_meta(ID) ->
    scoper:add_meta(#{id => ID}).

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, ID, Args)).

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
    throw(#payproc_RecurrentPaymentToolNotFound{}).

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

-spec init(rec_payment_tool_id(), [payment_tool() | rec_payment_tool_params()]) ->
    hg_machine:result().
init(RecPaymentToolID, [PaymentTool, Params]) ->
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    {Party, Shop} = get_party_shop(Params),
    PaymentInstitution = get_payment_institution(Shop, Party, Revision),
    RecPaymentTool = create_rec_payment_tool(RecPaymentToolID, CreatedAt, Params, Revision),
    VS0 = collect_varset(Party, Shop, #{payment_tool => PaymentTool}),
    {RiskScore     ,  VS1} = validate_risk_score(inspect(RecPaymentTool, VS0), VS0),
    {Route         , _VS2} = validate_route(
        hg_routing:choose(recurrent_paytool, PaymentInstitution, VS1, Revision),
        RecPaymentTool,
        VS1
    ),
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

get_payment_institution(Shop, Party, Revision) ->
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    PaymentInstitutionRef = Contract#domain_Contract.payment_institution,
    hg_domain:get(Revision, {payment_institution, PaymentInstitutionRef}).

get_merchant_recurrent_paytools_terms(Shop, Party, CreatedAt, Revision) ->
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_contract_active(Contract),
    #domain_TermSet{recurrent_paytools = Terms} = hg_party:get_terms(Contract, CreatedAt, Revision),
    Terms.

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
        party        => Party,
        shop         => Shop,
        category     => Category,
        currency     => Currency
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

-spec process_signal(hg_machine:signal(), hg_machine:history(), hg_machine:auxst()) ->
    hg_machine:result().
process_signal(Signal, History, _AuxSt) ->
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
        options    = hg_proxy_provider:collect_proxy_options(get_route(St))
    }.

construct_session(St) ->
    #prxprv_RecurrentTokenSession{
        state = maps:get(proxy_state, get_session(St), undefined)
    }.

construct_token_info(St) ->
    #prxprv_RecurrentTokenInfo{
        payment_tool = construct_proxy_payment_tool(St),
        trx          = get_session_trx(get_session(St))
    }.

get_session_trx(#{trx := Trx}) ->
    Trx;
get_session_trx(_) ->
    undefined.

get_rec_payment_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

construct_proxy_payment_tool(St) ->
    #payproc_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource
    } = get_rec_payment_tool(St),
    #prxprv_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        minimal_payment_cost = construct_proxy_cash(get_route(St))
    }.

construct_proxy_cash(Route) ->
    Revision = hg_domain:head(),
    ProviderTerms = hg_routing:get_rec_paytools_terms(Route, Revision),
    #domain_Cash{
        amount = Amount,
        currency = CurrencyRef
    } = get_minimal_payment_cost(ProviderTerms, #{}, Revision),
    #prxprv_Cash{
        amount = Amount,
        currency = hg_domain:get(Revision, {currency, CurrencyRef})
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
        #prxprv_RecurrentTokenFinishIntent{status = {'success', #prxprv_RecurrentTokenSuccess{token = Token}}} ->
            make_proxy_result(Changes, Action, Token);
        _ ->
            make_proxy_result(Changes, Action)
    end.

handle_callback_result(
    #prxprv_RecurrentTokenCallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_result(ProxyResult, hg_machine_action:unset_timer(Action0), Session)}.

make_proxy_result(Changes, Action) ->
    make_proxy_result(Changes, Action, undefined).

make_proxy_result(Changes, Action, Token) ->
    {wrap_session_events(Changes), Action, Token}.

%%

handle_proxy_callback_timeout(Action) ->
    Changes = [?session_finished(?session_failed(?operation_timeout()))],
    make_proxy_result(Changes, Action).

wrap_session_events(SessionEvents) ->
    [?session_ev(Ev) || Ev <- SessionEvents].

%%

finish_processing({Changes, Action, Token}, St) ->
    St1 = apply_changes(Changes, St),
    case get_session(St1) of
        #{status := finished, result := ?session_succeeded()} ->
            #{
                changes => Changes ++ [?recurrent_payment_tool_has_acquired(Token)],
                action  => Action
            };
        #{status := finished, result := ?session_failed(Failure)} ->
            #{
                changes => Changes ++ [?recurrent_payment_tool_has_failed(Failure)],
                action  => Action
            };
        #{} ->
            #{
                changes => Changes,
                action  => Action
            }
    end.

apply_changes(Changes, St) ->
    lists:foldl(fun apply_change/2, St, Changes).

apply_change(Event, undefined) ->
    apply_change(Event, #st{});

apply_change(?recurrent_payment_tool_has_created(RecPaymentTool, RiskScore, Route), St) ->
    St#st{
        rec_payment_tool = RecPaymentTool#payproc_RecurrentPaymentTool{
            route = Route
        },
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
apply_change(?recurrent_payment_tool_has_failed(Failure), St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    St#st{
        rec_payment_tool = RecPaymentTool#payproc_RecurrentPaymentTool{
            status = ?recurrent_payment_tool_failed(Failure)
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
        status => active,
        trx => undefined
    }.

-type call() :: abandon.

-spec process_call(call(), hg_machine:history(), hg_machine:auxst()) ->
    {hg_machine:response(), hg_machine:result()}.
process_call(Call, History, _AuxSt) ->
    St = collapse_history(unmarshal(History)),
    try handle_result(handle_call(Call, St)) catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

handle_call(abandon, St) ->
    ok = assert_rec_payment_tool_status(acquired, St),
    Changes = [?recurrent_payment_tool_has_abandoned()],
    St1 = apply_changes(Changes, St),
    #{
        response => get_rec_payment_tool(St1),
        changes  => Changes
    };
handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

dispatch_callback({provider, Payload}, St) ->
    Action = hg_machine_action:new(),
    case get_session_status(get_session(St)) of
        suspended ->
            ProxyContext = construct_proxy_context(St),
            {ok, CallbackResult} = hg_proxy_provider:handle_recurrent_token_callback(
                Payload,
                ProxyContext,
                get_route(St)
            ),
            {Response, Result} = handle_callback_result(CallbackResult, Action, get_session(St)),
            maps:merge(#{response => Response}, finish_processing(Result, St));
        _ ->
            throw(invalid_callback)
    end.


-type tag()               :: dmsl_base_thrift:'Tag'().
-type callback()          :: _. %% FIXME
-type callback_response() :: _. %% FIXME

-spec process_callback(tag(), callback()) ->
    {ok, callback_response()} | {error, invalid_callback | notfound | failed} | no_return().

process_callback(Tag, Callback) ->
    case hg_machine:call(?NS, {tag, Tag}, {callback, Callback}) of
        {ok, {ok, _} = Ok} ->
            Ok;
        {ok, {exception, invalid_callback}} ->
            {error, invalid_callback};
        {error, _} = Error ->
            Error
    end.

handle_result(Params) ->
    Result = handle_result_changes(Params, handle_result_action(Params, #{})),
    case maps:find(response, Params) of
        {ok, Response} ->
            {{ok, Response}, Result};
        error ->
            Result
    end.

handle_result_changes(#{changes := Changes = [_ | _]}, Acc) ->
    Acc#{events => [marshal(Changes)]};
handle_result_changes(#{}, Acc) ->
    Acc.

handle_result_action(#{action := Action}, Acc) ->
    Acc#{action => Action};
handle_result_action(#{}, Acc) ->
    Acc.

%%

ensure_party_accessible(#payproc_RecurrentPaymentToolParams{party_id = PartyID}) ->
    hg_invoice_utils:assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    Party.

ensure_shop_exists(#payproc_RecurrentPaymentToolParams{shop_id = ShopID, party_id = PartyID}) ->
    Party = hg_party_machine:get_party(PartyID),
    Shop = hg_invoice_utils:assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    Shop.

validate_payment_tool(PaymentTool, PaymentMethodSelector, VS) ->
    Revision = hg_domain:head(),
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw(#payproc_InvalidPaymentMethod{}),
    PaymentTool.

assert_party_shop_operable(Shop, Party) ->
    ok = assert_party_operable(Party),
    ok = assert_shop_operable(Shop),
    ok.

assert_party_operable(Party) ->
    Party = hg_invoice_utils:assert_party_operable(Party),
    ok.

assert_shop_operable(Shop) ->
    Shop = hg_invoice_utils:assert_shop_operable(Shop),
    ok.

assert_rec_payment_tool_status(StatusName, St) ->
    assert_rec_payment_tool_status_(StatusName, get_rec_payment_tool_status(get_rec_payment_tool(St))).

assert_rec_payment_tool_status_(StatusName, {StatusName, _}) ->
    ok;
assert_rec_payment_tool_status_(_StatusName, Status) ->
    throw(#payproc_InvalidRecurrentPaymentToolStatus{status = Status}).

-spec assert_operation_permitted(shop(), party()) -> merchant_terms().

assert_operation_permitted(Shop, Party) ->
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    Terms = get_merchant_recurrent_paytools_terms(Shop, Party, CreatedAt, Revision),
    case Terms of
        undefined ->
            throw(#payproc_OperationNotPermitted{});
        Terms ->
            Terms
    end.

get_rec_payment_tool_status(RecPaymentTool) ->
    RecPaymentTool#payproc_RecurrentPaymentTool.status.

%%

create_rec_payment_tool(RecPaymentToolID, CreatedAt, Params, Revision) ->
    PaymentResource = Params#payproc_RecurrentPaymentToolParams.payment_resource,
    #payproc_RecurrentPaymentTool{
        id                   = RecPaymentToolID,
        shop_id              = Params#payproc_RecurrentPaymentToolParams.shop_id,
        party_id             = Params#payproc_RecurrentPaymentToolParams.party_id,
        domain_revision      = Revision,
        status               = ?recurrent_payment_tool_created(),
        created_at           = CreatedAt,
        payment_resource     = PaymentResource,
        rec_token            = undefined,
        route                = undefined
    }.

get_minimal_payment_cost(ProviderTerms, VS, Revision) ->
    {Cash, _VS} = validate_cost(
        ProviderTerms#domain_RecurrentPaytoolsProvisionTerms.cash_value,
        VS,
        Revision
    ),
    Cash.

validate_cost(CashValueSelector, VS, Revision) ->
    Cash = reduce_selector(cash_value, CashValueSelector, VS, Revision),
    {Cash, VS#{cash_value => Cash}}.

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

%%

marshal(change, ?recurrent_payment_tool_has_created(RecPaymentTool, RiskScore, Route)) ->
    [1, #{
        <<"change">>           => <<"created">>,
        <<"rec_payment_tool">> => marshal(rec_payment_tool, RecPaymentTool),
        <<"risk_score">>       => marshal(risk_score, RiskScore),
        <<"route">>            => hg_routing:marshal(Route)
    }];
marshal(change, ?recurrent_payment_tool_has_acquired(Token)) ->
    [1, #{
        <<"change">> => <<"acquired">>,
        <<"token">>  => marshal(str, Token)
    }];
marshal(change, ?recurrent_payment_tool_has_abandoned()) ->
    [1, #{
        <<"change">> => <<"abandoned">>
    }];
marshal(change, ?recurrent_payment_tool_has_failed(Failure)) ->
    [1, #{
        <<"change">> => <<"failed">>,
        <<"failure">> => marshal(failure, Failure)
    }];
marshal(change, ?session_ev(Payload)) ->
    [1, #{
        <<"change">> => <<"session_change">>,
        <<"payload">> => marshal(session_change, Payload)
    }];

%%

marshal(rec_payment_tool, #payproc_RecurrentPaymentTool{} = RecPaymentTool) ->
    #{
        <<"id">> => marshal(str, RecPaymentTool#payproc_RecurrentPaymentTool.id),
        <<"shop_id">> => marshal(str, RecPaymentTool#payproc_RecurrentPaymentTool.shop_id),
        <<"party_id">> => marshal(str, RecPaymentTool#payproc_RecurrentPaymentTool.party_id),
        <<"domain_revision">> => marshal(int, RecPaymentTool#payproc_RecurrentPaymentTool.domain_revision),
        <<"status">> => marshal(status, RecPaymentTool#payproc_RecurrentPaymentTool.status),
        <<"created_at">> => marshal(str, RecPaymentTool#payproc_RecurrentPaymentTool.created_at),
        <<"payment_resource">> => marshal(
            disposable_payment_resource,
            RecPaymentTool#payproc_RecurrentPaymentTool.payment_resource
        ),
        <<"rec_token">> => marshal(str, RecPaymentTool#payproc_RecurrentPaymentTool.rec_token),
        <<"route">> => hg_routing:marshal(RecPaymentTool#payproc_RecurrentPaymentTool.route)
    };

marshal(risk_score, low) ->
    <<"low">>;
marshal(risk_score, high) ->
    <<"high">>;
marshal(risk_score, fatal) ->
    <<"fatal">>;

marshal(failure, {operation_timeout, _}) ->
    <<"operation_timeout">>;
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })];

%% Session change

marshal(session_change, ?session_started()) ->
    <<"started">>;
marshal(session_change, ?session_finished(Result)) ->
    [
        <<"finished">>,
        marshal(session_status, Result)
    ];
marshal(session_change, ?session_suspended()) ->
    <<"suspended">>;
marshal(session_change, ?session_activated()) ->
    <<"activated">>;
marshal(session_change, ?trx_bound(Trx)) ->
    [
        <<"transaction_bound">>,
        marshal(trx, Trx)
    ];
marshal(session_change, ?proxy_st_changed(ProxySt)) ->
    [
        <<"proxy_state_changed">>,
        marshal(bin, {bin, ProxySt})
    ];
marshal(session_change, ?interaction_requested(UserInteraction)) ->
    [
        <<"interaction_requested">>,
        marshal(interaction, UserInteraction)
    ];

marshal(session_status, ?session_succeeded()) ->
    <<"succeeded">>;
marshal(session_status, ?session_failed(PayloadFailure)) ->
    [
        <<"failed">>,
        marshal(failure, PayloadFailure)
    ];

%%

marshal(trx, #domain_TransactionInfo{} = TransactionInfo) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, TransactionInfo#domain_TransactionInfo.id),
        <<"timestamp">>     => marshal(str, TransactionInfo#domain_TransactionInfo.timestamp),
        <<"extra">>         => marshal(map_str, TransactionInfo#domain_TransactionInfo.extra)
    });

marshal(interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    #{<<"redirect">> =>
        [
            <<"get_request">>,
            marshal(str, URI)
        ]
    };
marshal(interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    #{<<"redirect">> =>
        [
            <<"post_request">>,
            #{
                <<"uri">>   => marshal(str, URI),
                <<"form">>  => marshal(map_str, Form)
            }
        ]
    };

%%

marshal(status, ?recurrent_payment_tool_created()) ->
    <<"created">>;
marshal(status, ?recurrent_payment_tool_acquired()) ->
    <<"acquired">>;
marshal(status, ?recurrent_payment_tool_abandoned()) ->
    <<"abandoned">>;
marshal(status, ?recurrent_payment_tool_failed(Failure)) ->
    [
        <<"failed">>,
        marshal(failure, Failure)
    ];

marshal(disposable_payment_resource, #domain_DisposablePaymentResource{} = PaymentResource) ->
    #{
        <<"payment_tool">> => hg_payment_tool:marshal(PaymentResource#domain_DisposablePaymentResource.payment_tool),
        <<"payment_session_id">> => marshal(str, PaymentResource#domain_DisposablePaymentResource.payment_session_id),
        <<"client_info">> => marshal(client_info, PaymentResource#domain_DisposablePaymentResource.client_info)
    };

%%

marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    genlib_map:compact(#{
        <<"ip_address">>    => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        <<"fingerprint">>   => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

%%

marshal(_, Other) ->
    Other.

%%
%% Unmarshalling
%%

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, changes}, Payload)}.

%%

unmarshal({list, changes}, Changes) when is_list(Changes) ->
    [unmarshal(change, Change) || Change <- Changes];

%%

unmarshal(change, [1, #{
    <<"change">>           := <<"created">>,
    <<"rec_payment_tool">> := RecPaymentTool,
    <<"risk_score">>       := RiskScore,
    <<"route">>            := Route
}]) ->
    ?recurrent_payment_tool_has_created(
        unmarshal(rec_payment_tool, RecPaymentTool),
        unmarshal(risk_score, RiskScore),
        hg_routing:unmarshal(Route)
    );
unmarshal(change, [1, #{
    <<"change">> := <<"acquired">>,
    <<"token">>  := Token
}]) ->
    ?recurrent_payment_tool_has_acquired(unmarshal(str, Token));
unmarshal(change, [1, #{
    <<"change">> := <<"abandoned">>
}]) ->
    ?recurrent_payment_tool_has_abandoned();
unmarshal(change, [1, #{
    <<"change">>  := <<"failed">>,
    <<"failure">> := Failure
}]) ->
    ?recurrent_payment_tool_has_failed(unmarshal(failure, Failure));
unmarshal(change, [1, #{
    <<"change">>    := <<"session_change">>,
    <<"payload">>   := Payload
}]) ->
    ?session_ev(unmarshal(session_change, Payload));

%%

unmarshal(rec_payment_tool, #{
    <<"id">>                   := ID,
    <<"shop_id">>              := ShopID,
    <<"party_id">>             := PartyID,
    <<"domain_revision">>      := Revision,
    <<"status">>               := Status,
    <<"created_at">>           := CreatedAt,
    <<"payment_resource">>     := PaymentResource,
    <<"rec_token">>            := RecToken,
    <<"route">>                := Route
}) ->
    #payproc_RecurrentPaymentTool{
        id                   = unmarshal(str, ID),
        shop_id              = unmarshal(str, ShopID),
        party_id             = unmarshal(str, PartyID),
        domain_revision      = unmarshal(int, Revision),
        status               = unmarshal(status, Status),
        created_at           = unmarshal(str, CreatedAt),
        payment_resource     = unmarshal(disposable_payment_resource, PaymentResource),
        rec_token            = unmarshal(str, RecToken),
        route                = hg_routing:unmarshal(Route)
    };

unmarshal(risk_score, <<"low">>) ->
    low;
unmarshal(risk_score, <<"high">>) ->
    high;
unmarshal(risk_score, <<"fatal">>) ->
    fatal;

unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

%% Session change

unmarshal(session_change, <<"started">>) ->
    ?session_started();
unmarshal(session_change, [<<"finished">>, Result]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, <<"suspended">>) ->
    ?session_suspended();
unmarshal(session_change, <<"activated">>) ->
    ?session_activated();
unmarshal(session_change, [<<"transaction_bound">>, Trx]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [<<"proxy_state_changed">>, {bin, ProxySt}]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [<<"interaction_requested">>, UserInteraction]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(session_status, <<"succeeded">>) ->
    ?session_succeeded();
unmarshal(session_status, [<<"failed">>, Failure]) ->
    ?session_failed(unmarshal(failure, Failure));

%%

unmarshal(trx, #{
    <<"id">>    := ID,
    <<"extra">> := Extra
} = TRX) ->
    Timestamp = maps:get(<<"timestamp">>, TRX, undefined),
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(interaction, #{<<"redirect">> := [<<"get_request">>, URI]}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, #{<<"redirect">> := [<<"post_request">>, #{
    <<"uri">>   := URI,
    <<"form">>  := Form
}]}) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};

%%

unmarshal(status, <<"created">>) ->
    ?recurrent_payment_tool_created();
unmarshal(status, <<"acquired">>) ->
    ?recurrent_payment_tool_acquired();
unmarshal(status, <<"abandoned">>) ->
    ?recurrent_payment_tool_abandoned();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?recurrent_payment_tool_failed(unmarshal(failure, Failure));

unmarshal(disposable_payment_resource, #{
    <<"payment_tool">> := PaymentTool,
    <<"payment_session_id">> := PaymentSessionId,
    <<"client_info">> := ClientInfo
}) ->
    #domain_DisposablePaymentResource{
        payment_tool = hg_payment_tool:unmarshal(PaymentTool),
        payment_session_id = unmarshal(str, PaymentSessionId),
        client_info = unmarshal(client_info, ClientInfo)
    };

%%

unmarshal(client_info, ClientInfo) ->
    IpAddress = maps:get(<<"ip_address">>, ClientInfo, undefined),
    Fingerprint = maps:get(<<"fingerprint">>, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

%%

unmarshal(_, Other) ->
    Other.

%%
%% Event sink
%%

publish_events(Events) ->
    [publish_event(Event) || Event <- Events].

publish_event({ID, Ns, SourceID, {EventID, Dt, Payload}}) ->
    hg_event_provider:publish_event(Ns, ID, SourceID, {EventID, Dt, hg_msgpack_marshalling:unmarshal(Payload)}).
