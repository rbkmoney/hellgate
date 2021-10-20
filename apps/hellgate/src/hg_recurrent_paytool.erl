%%%
%%% Payment processing machine
%%%

-module(hg_recurrent_paytool).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_proxy_provider_thrift.hrl").

-define(NS, <<"recurrent_paytools">>).

%% Public interface

-export([assert_operation_permitted/3]).
-export([validate_paytool_params/1]).

-export([process_callback/2]).

%% Woody handler called by hg_woody_wrapper

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Machine callbacks

-behaviour(hg_machine).

-export([namespace/0]).
-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).
-export([process_repair/2]).

%% Types
-record(st, {
    rec_payment_tool :: undefined | rec_payment_tool(),
    route :: undefined | route(),
    risk_score :: undefined | risk_score(),
    session :: undefined | session(),
    minimal_payment_cost :: undefined | cash()
}).

-type st() :: #st{}.

-export_type([st/0]).

-type rec_payment_tool() :: dmsl_payment_processing_thrift:'RecurrentPaymentTool'().
-type rec_payment_tool_change() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolChange'().
-type rec_payment_tool_params() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolParams'().

-type route() :: dmsl_domain_thrift:'PaymentRoute'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type shop() :: dmsl_domain_thrift:'Shop'().
-type party() :: dmsl_domain_thrift:'Party'().
-type merchant_terms() :: dmsl_domain_thrift:'RecurrentPaytoolsServiceTerms'().
-type domain_revision() :: hg_domain:revision().
-type action() :: hg_machine_action:t().
-type timeout_behaviour() :: dmsl_timeout_behaviour_thrift:'TimeoutBehaviour'().

-type session() :: #{
    status := active | suspended | finished,
    result => session_result(),
    trx => undefined | trx_info(),
    proxy_state => proxy_state(),
    timeout_behaviour => timeout_behaviour()
}.

-type proxy_state() :: dmsl_proxy_provider_thrift:'ProxyState'().
-type trx_info() :: dmsl_domain_thrift:'TransactionInfo'().
-type session_result() :: dmsl_payment_processing_thrift:'SessionResult'().

-type tag() :: dmsl_base_thrift:'Tag'().
-type callback() :: {provider, dmsl_proxy_provider_thrift:'Callback'()}.
-type callback_response() :: dmsl_proxy_provider_thrift:'CallbackResponse'().
-type proxy_callback_result() :: dmsl_proxy_provider_thrift:'RecurrentTokenCallbackResult'().
-type token() :: dmsl_domain_thrift:'Token'().

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function('GetEvents', {#payproc_EventRange{'after' = After, limit = Limit}}, _Opts) ->
    case hg_event_sink:get_events(?NS, After, Limit) of
        {ok, Events} ->
            publish_rec_payment_tool_events(Events);
        {error, event_not_found} ->
            throw(#payproc_EventNotFound{})
    end;
handle_function('GetLastEventID', {}, _Opts) ->
    case hg_event_sink:get_last_event_id(?NS) of
        {ok, ID} ->
            ID;
        {error, no_last_event} ->
            throw(#payproc_NoLastEvent{})
    end;
handle_function(Func, Args, Opts) ->
    scoper:scope(
        recurrent_payment_tools,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

handle_function_('Create', {RecurrentPaymentToolParams}, _Opts) ->
    RecPaymentToolID = get_paytool_id(RecurrentPaymentToolParams),
    ok = set_meta(RecPaymentToolID),
    RecurrentPaymentToolParams0 = ensure_params_domain_revision_defined(RecurrentPaymentToolParams),
    _ = validate_paytool_params(RecurrentPaymentToolParams0),
    ok = start(RecPaymentToolID, RecurrentPaymentToolParams0),
    get_rec_payment_tool(get_state(RecPaymentToolID));
handle_function_('Abandon', {RecPaymentToolID}, _Opts) ->
    ok = set_meta(RecPaymentToolID),
    call(RecPaymentToolID, abandon);
handle_function_('Get', {RecPaymentToolID}, _Opts) ->
    ok = set_meta(RecPaymentToolID),
    get_rec_payment_tool(get_state(RecPaymentToolID));
handle_function_('GetEvents', {RecPaymentToolID, Range}, _Opts) ->
    ok = set_meta(RecPaymentToolID),
    get_public_history(RecPaymentToolID, Range).

-spec validate_paytool_params(rec_payment_tool_params()) -> ok | no_return().
validate_paytool_params(RecurrentPaymentToolParams) ->
    DomainRevison = RecurrentPaymentToolParams#payproc_RecurrentPaymentToolParams.domain_revision,
    Party = ensure_party_accessible(RecurrentPaymentToolParams),
    Shop = ensure_shop_exists(RecurrentPaymentToolParams, Party),
    ok = assert_party_shop_operable(Shop, Party),
    MerchantTerms = assert_operation_permitted(Shop, Party, DomainRevison),
    _PaymentTool = validate_payment_tool(
        get_payment_tool(RecurrentPaymentToolParams#payproc_RecurrentPaymentToolParams.payment_resource),
        MerchantTerms#domain_RecurrentPaytoolsServiceTerms.payment_methods
    ),
    ok.

-spec ensure_params_domain_revision_defined(rec_payment_tool_params()) -> rec_payment_tool_params().
ensure_params_domain_revision_defined(Params) ->
    DomainRevision = Params#payproc_RecurrentPaymentToolParams.domain_revision,
    Params#payproc_RecurrentPaymentToolParams{
        domain_revision = ensure_domain_revision_defined(DomainRevision)
    }.

get_paytool_id(#payproc_RecurrentPaymentToolParams{id = ID}) ->
    ID.

get_public_history(RecPaymentToolID, #payproc_EventRange{'after' = AfterID, limit = Limit}) ->
    Events = get_history(RecPaymentToolID, AfterID, Limit),
    [publish_rec_payment_tool_event(RecPaymentToolID, Ev) || Ev <- Events].

publish_rec_payment_tool_event(RecPaymentToolID, Event) ->
    {ID, Dt, Payload} = Event,
    #payproc_RecurrentPaymentToolEvent{
        id = ID,
        created_at = Dt,
        source = RecPaymentToolID,
        payload = Payload
    }.

%%

set_meta(ID) ->
    scoper:add_meta(#{id => ID}).

start(ID, Params) ->
    EncodedParams = marshal_recurrent_paytool_params(Params),
    map_start_error(hg_machine:start(?NS, ID, EncodedParams)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, ID, Args)).

-spec map_error(ok | {ok, _Result} | {error, _Error}) -> _Result | no_return().
map_error(ok) ->
    ok;
map_error({ok, Result}) ->
    Result;
map_error({exception, Reason}) ->
    throw(Reason);
map_error({error, notfound}) ->
    throw(#payproc_RecurrentPaymentToolNotFound{});
map_error({error, Reason}) ->
    error(Reason).

%%

get_history(RecPaymentToolID) ->
    History = hg_machine:get_history(?NS, RecPaymentToolID),
    unmarshal_history(map_history_error(History)).

get_history(RecPaymentToolID, AfterID, Limit) ->
    History = hg_machine:get_history(?NS, RecPaymentToolID, AfterID, Limit),
    unmarshal_history(map_history_error(History)).

get_state(RecPaymentToolID) ->
    collapse_history(get_history(RecPaymentToolID)).

collapse_history(History) ->
    lists:foldl(
        fun({_ID, _, Events}, St0) ->
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
map_start_error({error, exists}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

-include("domain.hrl").
-include("recurrent_payment_tools.hrl").

%% hg_machine callbacks

-type create_params() :: dmsl_payment_processing_thrift:'RecurrentPaymentToolParams'().

-spec namespace() -> hg_machine:ns().
namespace() ->
    ?NS.

-spec init(binary(), hg_machine:machine()) -> hg_machine:result().
init(EncodedParams, #{id := RecPaymentToolID}) ->
    Params = unmarshal_recurrent_paytool_params(EncodedParams),
    PaymentTool = get_payment_tool(Params#payproc_RecurrentPaymentToolParams.payment_resource),
    Revision = Params#payproc_RecurrentPaymentToolParams.domain_revision,
    CreatedAt = hg_datetime:format_now(),
    {Party, Shop} = get_party_shop(Params),
    RecPaymentTool = create_rec_payment_tool(RecPaymentToolID, CreatedAt, Party, Params, Revision),
    VS = collect_varset(Party, Shop, #{payment_tool => PaymentTool}),
    RiskScore = validate_risk_score(inspect(RecPaymentTool, VS)),
    VS1 = VS#{risk_score => RiskScore},
    PaymentInstitutionRef = get_payment_institution_ref(Shop, Party),
    PaymentInstitution = hg_payment_institution:compute_payment_institution(PaymentInstitutionRef, VS, Revision),
    try
        check_risk_score(RiskScore),
        NonFailRatedRoutes = gather_routes(PaymentInstitution, VS1, Revision),
        {ChosenRoute, ChoiceContext} = hg_routing:choose_route(NonFailRatedRoutes),
        ChosenPaymentRoute = hg_routing:to_payment_route(ChosenRoute),
        _ = logger:log(info, "Routing decision made", hg_routing:get_logger_metadata(ChoiceContext, Revision)),
        RecPaymentTool2 = set_minimal_payment_cost(RecPaymentTool, ChosenPaymentRoute, VS, Revision),
        {ok, {Changes, Action}} = start_session(),
        StartChanges = [
            ?recurrent_payment_tool_has_created(RecPaymentTool2),
            ?recurrent_payment_tool_risk_score_changed(RiskScore),
            ?recurrent_payment_tool_route_changed(ChosenPaymentRoute)
        ],
        handle_result(#{
            changes => StartChanges ++ Changes,
            action => Action
        })
    catch
        throw:risk_score_is_too_high = Error ->
            error(handle_route_error(Error, RecPaymentTool));
        throw:{no_route_found, {unknown, _}} = Error ->
            error(handle_route_error(Error, RecPaymentTool, VS1))
    end.

gather_routes(PaymentInstitution, VS, Revision) ->
    Predestination = recurrent_paytool,
    case
        hg_routing:gather_routes(
            Predestination,
            PaymentInstitution,
            VS,
            Revision
        )
    of
        {ok, {[], RejectedRoutes}} ->
            throw({no_route_found, {unknown, RejectedRoutes}});
        {ok, {Routes, _RejectContext}} ->
            Routes;
        {error, {misconfiguration, _Reason}} ->
            throw({no_route_found, misconfiguration})
    end.

%% TODO uncomment after inspect will implement
% check_risk_score(fatal) ->
%     throw(risk_score_is_too_high);
check_risk_score(_) ->
    ok.

get_party_shop(Params) ->
    #payproc_RecurrentPaymentToolParams{
        party_id = PartyID,
        party_revision = ParamsPartyRevision,
        shop_id = ShopID
    } = Params,
    PartyRevision = ensure_party_revision_defined(PartyID, ParamsPartyRevision),
    Party = hg_party:checkout(PartyID, {revision, PartyRevision}),
    Shop = hg_party:get_shop(ShopID, Party),
    {Party, Shop}.

get_payment_institution_ref(Shop, Party) ->
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Contract#domain_Contract.payment_institution.

get_merchant_recurrent_paytools_terms(#domain_Shop{contract_id = ContractID}, Party, Timestamp, Revision) ->
    Ctx = hg_context:load(),
    #domain_Party{id = PartyId, revision = PartyRevision} = Party,
    ok = assert_contract_active(hg_party:get_contract(ContractID, Party)),
    {ok, #domain_TermSet{recurrent_paytools = Terms}} = party_client_thrift:compute_contract_terms(
        PartyId,
        ContractID,
        Timestamp,
        {revision, PartyRevision},
        Revision,
        #payproc_Varset{},
        hg_context:get_party_client(Ctx),
        hg_context:get_party_client_context(Ctx)
    ),
    Terms.

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    % FIXME no such exception on the service interface
    throw(#payproc_InvalidContractStatus{status = Status}).

collect_varset(
    #domain_Party{id = PartyID},
    #domain_Shop{
        id = ShopID,
        category = Category,
        account = #domain_ShopAccount{currency = Currency}
    },
    VS
) ->
    VS#{
        party_id => PartyID,
        shop_id => ShopID,
        category => Category,
        currency => Currency
    }.

-spec collect_rec_payment_tool_varset(rec_payment_tool()) -> hg_varset:varset().
collect_rec_payment_tool_varset(RecPaymentTool) ->
    #payproc_RecurrentPaymentTool{
        party_id = PartyID,
        party_revision = PreservedPartyRevision,
        shop_id = ShopID,
        payment_resource = PaymentResource
    } = RecPaymentTool,
    PartyRevision = ensure_party_revision_defined(PartyID, PreservedPartyRevision),
    #domain_DisposablePaymentResource{
        payment_tool = PaymentTool
    } = PaymentResource,
    Party = hg_party:checkout(PartyID, {revision, PartyRevision}),
    Shop = hg_party:get_shop(ShopID, Party),
    collect_varset(Party, Shop, #{payment_tool => PaymentTool}).

inspect(_RecPaymentTool, _VS) ->
    % FIXME please senpai
    high.

validate_risk_score(RiskScore) when RiskScore == low; RiskScore == high ->
    RiskScore.

handle_route_error(risk_score_is_too_high = Reason, RecPaymentTool) ->
    _ = logger:log(info, "No route found, reason = ~p", [Reason], logger:get_process_metadata()),
    {misconfiguration, {'No route found for a recurrent payment tool', RecPaymentTool}}.
handle_route_error({no_route_found, {Reason, RejectedRoutes}}, RecPaymentTool, Varset) ->
    LogFun = fun(Msg, Param) ->
        _ = logger:log(
            error,
            Msg,
            [Reason, Param],
            logger:get_process_metadata()
        )
    end,
    _ = LogFun("No route found, reason = ~p, varset: ~p", Varset),
    _ = LogFun("No route found, reason = ~p, rejected routes: ~p", RejectedRoutes),
    {misconfiguration, {'No route found for a recurrent payment tool', RecPaymentTool}}.

start_session() ->
    Events = [?session_ev(?session_started())],
    Action = hg_machine_action:instant(),
    {ok, {Events, Action}}.

-spec process_repair(hg_machine:signal(), hg_machine:machine()) -> no_return().
process_repair(_, _) ->
    erlang:error({not_implemented, repair}).

-spec process_signal(hg_machine:signal(), hg_machine:machine()) -> hg_machine:result().
process_signal(Signal, #{history := History}) ->
    Result = handle_signal(Signal, collapse_history(unmarshal_history(History))),
    handle_result(Result).

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
    case get_session_timeout_behaviour(get_session(St)) of
        {callback, Payload} ->
            ProxyContext = construct_proxy_context(St),
            Route = get_route(St),
            {ok, CallbackResult} = hg_proxy_provider:handle_recurrent_token_callback(Payload, ProxyContext, Route),
            {_Response, Result} = handle_callback_result(CallbackResult, Action, get_session(St)),
            finish_processing(Result, St);
        {operation_failure, Failure} ->
            Result = handle_callback_timeout_failure(unmarshal(failure, Failure), Action),
            finish_processing(Result, St)
    end.

get_route(#st{route = Route}) ->
    Route.

%%

construct_proxy_context(St) ->
    #prxprv_RecurrentTokenContext{
        session = construct_session(St),
        token_info = construct_token_info(St),
        options = hg_proxy_provider:collect_proxy_options(get_route(St))
    }.

construct_session(St) ->
    #prxprv_RecurrentTokenSession{
        state = maps:get(proxy_state, get_session(St), undefined)
    }.

construct_token_info(St) ->
    #prxprv_RecurrentTokenInfo{
        payment_tool = construct_proxy_payment_tool(St),
        trx = get_session_trx(get_session(St)),
        shop = construct_proxy_shop(get_shop(St), get_domain_revision(St))
    }.

construct_proxy_shop(DomainShop, DomainRevision) ->
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        location = Location,
        category = ShopCategoryRef
    } = DomainShop,
    ShopCategory = hg_domain:get(DomainRevision, {category, ShopCategoryRef}),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails,
        location = Location
    }.

get_shop(St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    ShopID = RecPaymentTool#payproc_RecurrentPaymentTool.shop_id,
    Party = get_party(St),
    hg_party:get_shop(ShopID, Party).

get_party(St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    #payproc_RecurrentPaymentTool{
        party_id = PartyID,
        party_revision = PartyRevision
    } = RecPaymentTool,
    Revision = ensure_party_revision_defined(PartyID, PartyRevision),
    Party = hg_party:checkout(PartyID, {revision, Revision}),
    Party.

get_domain_revision(St) ->
    RecPaymentTool = get_rec_payment_tool(St),
    DomainRevison = RecPaymentTool#payproc_RecurrentPaymentTool.domain_revision,
    ensure_domain_revision_defined(DomainRevison).

get_session_trx(#{trx := Trx}) ->
    Trx;
get_session_trx(_) ->
    undefined.

get_session_timeout_behaviour(#{timeout_behaviour := TimeoutBehaviour}) ->
    TimeoutBehaviour.

get_rec_payment_tool(#st{rec_payment_tool = RecPaymentTool}) ->
    RecPaymentTool.

construct_proxy_payment_tool(St) ->
    RecPaymentTool =
        #payproc_RecurrentPaymentTool{
            id = ID,
            created_at = CreatedAt,
            payment_resource = PaymentResource,
            domain_revision = DomainRevison
        } = get_rec_payment_tool(St),
    VS = collect_rec_payment_tool_varset(RecPaymentTool),
    #prxprv_RecurrentPaymentTool{
        id = ID,
        created_at = CreatedAt,
        payment_resource = PaymentResource,
        minimal_payment_cost = construct_proxy_cash(get_route(St), VS, DomainRevison)
    }.

construct_proxy_cash(#domain_PaymentRoute{provider = ProviderRef}, VS, DomainRevison) ->
    ProviderTerms = get_rec_paytools_terms(ProviderRef, VS, DomainRevison),
    #domain_Cash{
        amount = Amount,
        currency = CurrencyRef
    } = get_minimal_payment_cost(ProviderTerms),
    #prxprv_Cash{
        amount = Amount,
        currency = hg_domain:get(DomainRevison, {currency, CurrencyRef})
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
    Changes2 = hg_proxy_provider:update_proxy_state(ProxyState, Session),
    {Changes3, Action} = hg_proxy_provider:handle_proxy_intent(Intent, Action0),
    Changes = Changes1 ++ Changes2 ++ Changes3,
    case Intent of
        #prxprv_RecurrentTokenFinishIntent{status = {'success', #prxprv_RecurrentTokenSuccess{token = Token}}} ->
            make_proxy_result(Changes, Action, Token);
        _ ->
            make_proxy_result(Changes, Action)
    end.

-spec handle_callback_result(proxy_callback_result(), action(), session()) ->
    {callback_response(), {[rec_payment_tool_change()], action(), token()}}.
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

handle_callback_timeout_failure(Failure, Action) ->
    Changes = [?session_finished(?session_failed(Failure))],
    make_proxy_result(Changes, Action).

wrap_session_events(SessionEvents) ->
    [?session_ev(Ev) || Ev <- SessionEvents].

%%

-spec finish_processing({[rec_payment_tool_change()], action(), token()}, st()) -> call_result().
finish_processing({Changes, Action, Token}, St) ->
    St1 = apply_changes(Changes, St),
    case get_session(St1) of
        #{status := finished, result := ?session_succeeded()} ->
            #{
                changes => Changes ++ [?recurrent_payment_tool_has_acquired(Token)],
                action => Action
            };
        #{status := finished, result := ?session_failed(Failure)} ->
            #{
                changes => Changes ++ [?recurrent_payment_tool_has_failed(Failure)],
                action => Action
            };
        #{} ->
            #{
                changes => Changes,
                action => Action
            }
    end.

apply_changes(Changes, St) ->
    lists:foldl(fun apply_change/2, St, Changes).

apply_change(Event, undefined) ->
    apply_change(Event, #st{});
apply_change(?recurrent_payment_tool_has_created(RecPaymentTool), St) ->
    St#st{
        rec_payment_tool = RecPaymentTool
    };
apply_change(?recurrent_payment_tool_risk_score_changed(RiskScore), St) ->
    St#st{
        risk_score = RiskScore
    };
apply_change(?recurrent_payment_tool_route_changed(Route), St = #st{rec_payment_tool = RecPaymentTool}) ->
    St#st{
        rec_payment_tool = RecPaymentTool#payproc_RecurrentPaymentTool{
            route = Route
        },
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
merge_session_change(?session_suspended(Tag, undefined), Session) ->
    Session#{status := suspended, tag => Tag};
merge_session_change(?session_suspended(Tag, TimeoutBehaviour), Session) ->
    Session#{status := suspended, tag => Tag, timeout_behaviour := TimeoutBehaviour};
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
        trx => undefined,
        timeout_behaviour => {operation_failure, ?operation_timeout()}
    }.

-type call() :: abandon.
-type call_result() :: #{
    changes => [rec_payment_tool_change()],
    action => action(),
    response => ok | term()
}.

-spec process_call(call(), hg_machine:machine()) -> {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try
        handle_result(handle_call(Call, St))
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

-spec handle_call(call(), st()) -> call_result().
handle_call(abandon, St) ->
    ok = assert_rec_payment_tool_status(acquired, St),
    Changes = [?recurrent_payment_tool_has_abandoned()],
    St1 = apply_changes(Changes, St),
    #{
        response => get_rec_payment_tool(St1),
        changes => Changes
    };
handle_call({callback, Callback}, St) ->
    dispatch_callback(Callback, St).

-spec dispatch_callback(callback(), st()) -> call_result().
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

-spec process_callback(tag(), callback()) ->
    {ok, callback_response()} | {error, invalid_callback | notfound | failed} | no_return().
process_callback(Tag, Callback) ->
    case hg_machine:call(?NS, {tag, Tag}, {callback, Callback}) of
        {ok, _CallbackResponse} = Result ->
            Result;
        {exception, invalid_callback} ->
            {error, invalid_callback};
        {error, _} = Error ->
            Error
    end.

-spec handle_result(call_result()) -> {hg_machine:response(), hg_machine:result()} | hg_machine:result().
handle_result(Params) ->
    Result = handle_result_changes(Params, handle_result_action(Params, #{})),
    case Params of
        #{response := Response} ->
            {{ok, Response}, Result};
        #{} ->
            Result
    end.

handle_result_changes(#{changes := Changes = [_ | _]}, Acc) ->
    Acc#{events => [marshal_event_payload(Changes)]};
handle_result_changes(#{}, Acc) ->
    Acc.

handle_result_action(#{action := Action}, Acc) ->
    Acc#{action => Action};
handle_result_action(#{}, Acc) ->
    Acc.

%%

ensure_party_accessible(#payproc_RecurrentPaymentToolParams{party_id = PartyID, party_revision = Revision0}) ->
    _ = hg_invoice_utils:assert_party_accessible(PartyID),
    Revision = ensure_party_revision_defined(PartyID, Revision0),
    hg_party:checkout(PartyID, {revision, Revision}).

ensure_shop_exists(#payproc_RecurrentPaymentToolParams{shop_id = ShopID}, Party) ->
    hg_invoice_utils:assert_shop_exists(hg_party:get_shop(ShopID, Party)).

validate_payment_tool(PaymentTool, {value, PMs}) ->
    _ =
        hg_payment_tool:has_any_payment_method(PaymentTool, PMs) orelse
            throw(#payproc_InvalidPaymentMethod{}),
    PaymentTool;
validate_payment_tool(_PaymentTool, Ambiguous) ->
    error({misconfiguration, {'Could not reduce selector to a value', Ambiguous}}).

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

-spec assert_operation_permitted(shop(), party(), domain_revision()) -> merchant_terms().
assert_operation_permitted(Shop, Party, DomainRevison) ->
    CreatedAt = hg_datetime:format_now(),
    case get_merchant_recurrent_paytools_terms(Shop, Party, CreatedAt, DomainRevison) of
        undefined ->
            throw(#payproc_OperationNotPermitted{});
        Terms ->
            Terms
    end.

get_rec_payment_tool_status(RecPaymentTool) ->
    RecPaymentTool#payproc_RecurrentPaymentTool.status.

%%

create_rec_payment_tool(RecPaymentToolID, CreatedAt, Party, Params, Revision) ->
    #payproc_RecurrentPaymentTool{
        id = RecPaymentToolID,
        shop_id = Params#payproc_RecurrentPaymentToolParams.shop_id,
        party_id = Party#domain_Party.id,
        party_revision = Party#domain_Party.revision,
        domain_revision = Revision,
        status = ?recurrent_payment_tool_created(),
        created_at = CreatedAt,
        payment_resource = Params#payproc_RecurrentPaymentToolParams.payment_resource,
        rec_token = undefined,
        route = undefined
    }.

set_minimal_payment_cost(RecPaymentTool, #domain_PaymentRoute{provider = ProviderRef}, VS, Revision) ->
    ProviderTerms = get_rec_paytools_terms(ProviderRef, VS, Revision),
    RecPaymentTool#payproc_RecurrentPaymentTool{
        minimal_payment_cost = get_minimal_payment_cost(ProviderTerms)
    }.

get_rec_paytools_terms(ProviderRef, VS, Revision) ->
    Ctx = hg_context:load(),
    {ok, #domain_Provider{terms = Terms}} = party_client_thrift:compute_provider(
        ProviderRef,
        Revision,
        hg_varset:prepare_varset(VS),
        hg_context:get_party_client(Ctx),
        hg_context:get_party_client_context(Ctx)
    ),
    Terms#domain_ProvisionTermSet.recurrent_paytools.

get_minimal_payment_cost(#domain_RecurrentPaytoolsProvisionTerms{cash_value = Cash}) ->
    case Cash of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', Ambiguous}})
    end.

get_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.

-spec ensure_party_revision_defined(dmsl_domain_thrift:'PartyID'(), hg_party:party_revision() | undefined) ->
    hg_party:party_revision().
ensure_party_revision_defined(PartyID, undefined) ->
    hg_party:get_party_revision(PartyID);
ensure_party_revision_defined(_PartyID, Revision) ->
    Revision.

-spec ensure_domain_revision_defined(dmsl_domain_thrift:'DataRevision'() | undefined) ->
    dmsl_domain_thrift:'DataRevision'().
ensure_domain_revision_defined(undefined) ->
    hg_domain:head();
ensure_domain_revision_defined(Revision) ->
    Revision.

%%
%% Marshalling
%%

-spec marshal_recurrent_paytool_params(create_params()) -> binary().
marshal_recurrent_paytool_params(Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'RecurrentPaymentToolParams'}},
    hg_proto_utils:serialize(Type, Params).

-spec marshal_event_payload([rec_payment_tool_change()]) -> hg_machine:event_payload().
marshal_event_payload(Changes) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'RecurrentPaymentToolEventData'}},
    Bin = hg_proto_utils:serialize(Type, #payproc_RecurrentPaymentToolEventData{changes = Changes}),
    #{
        format_version => 1,
        data => {bin, Bin}
    }.

%%
%% Unmarshalling
%%

-spec unmarshal_recurrent_paytool_params(binary()) -> create_params().
unmarshal_recurrent_paytool_params(Binary) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'RecurrentPaymentToolParams'}},
    hg_proto_utils:deserialize(Type, Binary).

-spec unmarshal_history([hg_machine:event()]) -> [hg_machine:event([rec_payment_tool_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) -> hg_machine:event([rec_payment_tool_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) -> [rec_payment_tool_change()].
unmarshal_event_payload(#{format_version := 1, data := {bin, Bin}}) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'RecurrentPaymentToolEventData'}},
    #payproc_RecurrentPaymentToolEventData{changes = Changes} = hg_proto_utils:deserialize(Type, Bin),
    Changes;
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, changes}, Changes).

%%

unmarshal({list, changes}, Changes) when is_list(Changes) ->
    lists:flatten([unmarshal(change, Change) || Change <- Changes]);
%%

unmarshal(change, [
    1,
    #{
        <<"change">> := <<"created">>,
        <<"rec_payment_tool">> := RecPaymentTool,
        <<"risk_score">> := RiskScore,
        <<"route">> := Route
    }
]) ->
    [
        ?recurrent_payment_tool_has_created(unmarshal(rec_payment_tool, RecPaymentTool)),
        ?recurrent_payment_tool_risk_score_changed(unmarshal(risk_score, RiskScore)),
        ?recurrent_payment_tool_route_changed(hg_routing:unmarshal(Route))
    ];
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"created">>,
        <<"rec_payment_tool">> := RecPaymentTool
    }
]) ->
    ?recurrent_payment_tool_has_created(unmarshal(rec_payment_tool, RecPaymentTool));
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"risk_score_changed">>,
        <<"risk_score">> := RiskScore
    }
]) ->
    ?recurrent_payment_tool_risk_score_changed(unmarshal(risk_score, RiskScore));
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"route_changed">>,
        <<"route">> := Route
    }
]) ->
    ?recurrent_payment_tool_route_changed(hg_routing:unmarshal(Route));
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"acquired">>,
        <<"token">> := Token
    }
]) ->
    ?recurrent_payment_tool_has_acquired(unmarshal(str, Token));
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"abandoned">>
    }
]) ->
    ?recurrent_payment_tool_has_abandoned();
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"failed">>,
        <<"failure">> := Failure
    }
]) ->
    ?recurrent_payment_tool_has_failed(unmarshal(failure, Failure));
unmarshal(change, [
    1,
    #{
        <<"change">> := <<"session_change">>,
        <<"payload">> := Payload
    }
]) ->
    ?session_ev(unmarshal(session_change, Payload));
%%

unmarshal(
    rec_payment_tool,
    #{
        <<"id">> := ID,
        <<"shop_id">> := ShopID,
        <<"party_id">> := PartyID,
        <<"domain_revision">> := Revision,
        <<"status">> := Status,
        <<"created_at">> := CreatedAt,
        <<"payment_resource">> := PaymentResource,
        <<"rec_token">> := RecToken,
        <<"route">> := Route
    } = MarshalledRecPaymentTool
) ->
    PartyRevision = maps:get(<<"party_revision">>, MarshalledRecPaymentTool, undefined),
    MinimalPaymentCost = maps:get(<<"minimal_payment_cost">>, MarshalledRecPaymentTool, undefined),
    #payproc_RecurrentPaymentTool{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        party_id = unmarshal(str, PartyID),
        party_revision = maybe_unmarshal_party_revision(PartyRevision),
        domain_revision = unmarshal(int, Revision),
        status = unmarshal(status, Status),
        created_at = unmarshal(str, CreatedAt),
        payment_resource = unmarshal(disposable_payment_resource, PaymentResource),
        rec_token = unmarshal(str, RecToken),
        route = hg_routing:unmarshal(Route),
        minimal_payment_cost = maybe_unmarshal_cash(MinimalPaymentCost)
    };
unmarshal(risk_score, <<"low">>) ->
    low;
unmarshal(risk_score, <<"high">>) ->
    high;
unmarshal(risk_score, <<"fatal">>) ->
    fatal;
unmarshal(sub_failure, undefined) ->
    undefined;
unmarshal(sub_failure, #{<<"code">> := Code} = SubFailure) ->
    #domain_SubFailure{
        code = unmarshal(str, Code),
        sub = unmarshal(sub_failure, maps:get(<<"sub">>, SubFailure, undefined))
    };
unmarshal(failure, [1, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, [<<"failure">>, #{<<"code">> := Code} = Failure]]) ->
    {failure, #domain_Failure{
        code = unmarshal(str, Code),
        reason = unmarshal(str, maps:get(<<"reason">>, Failure, undefined)),
        sub = unmarshal(sub_failure, maps:get(<<"sub">>, Failure, undefined))
    }};
unmarshal(failure, <<"operation_timeout">>) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {failure, #domain_Failure{
        code = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};
%% Session change

unmarshal(session_change, <<"started">>) ->
    ?session_started();
unmarshal(session_change, [<<"finished">>, Result]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, <<"suspended">>) ->
    ?session_suspended();
unmarshal(session_change, [<<"suspended">>, Tag, TimeoutBehaviour]) ->
    ?session_suspended(Tag, unmarshal(timeout_behaviour, TimeoutBehaviour));
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

unmarshal(
    trx,
    #{
        <<"id">> := ID,
        <<"extra">> := Extra
    } = TRX
) ->
    Timestamp = maps:get(<<"timestamp">>, TRX, undefined),
    #domain_TransactionInfo{
        id = unmarshal(str, ID),
        timestamp = unmarshal(str, Timestamp),
        extra = unmarshal(map_str, Extra)
    };
unmarshal(interaction, #{<<"redirect">> := [<<"get_request">>, URI]}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, #{
    <<"redirect">> := [
        <<"post_request">>,
        #{
            <<"uri">> := URI,
            <<"form">> := Form
        }
    ]
}) ->
    {redirect,
        {post_request, #'BrowserPostRequest'{
            uri = unmarshal(str, URI),
            form = unmarshal(map_str, Form)
        }}};
unmarshal(timeout_behaviour, #{<<"callback">> := Callback}) ->
    {callback, unmarshal(str, Callback)};
unmarshal(timeout_behaviour, #{<<"operation_failure">> := Failure}) ->
    {operation_failure, unmarshal(failure, Failure)};
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
        ip_address = unmarshal(str, IpAddress),
        fingerprint = unmarshal(str, Fingerprint)
    };
%%

unmarshal(_, Other) ->
    Other.

maybe_unmarshal_cash(undefined) ->
    undefined;
maybe_unmarshal_cash(#{<<"amount">> := Amount, <<"currency_ref">> := Code}) ->
    #domain_Cash{
        amount = unmarshal(int, Amount),
        currency = #domain_CurrencyRef{
            symbolic_code = unmarshal(str, Code)
        }
    }.

maybe_unmarshal_party_revision(undefined) ->
    undefined;
maybe_unmarshal_party_revision(PartyRevision) ->
    unmarshal(int, PartyRevision).

%%
%% Event sink
%%

publish_rec_payment_tool_events(Events) ->
    [publish_rec_payment_tool_event(Event) || Event <- Events].

publish_rec_payment_tool_event({ID, _Ns, SourceID, {EventID, Dt, Payload}}) ->
    publish_rec_payment_tool_event(ID, SourceID, {EventID, Dt, Payload}).

publish_rec_payment_tool_event(EventID, MachineID, {ID, Dt, Ev}) ->
    Payload = unmarshal_event_payload(Ev),
    #payproc_RecurrentPaymentToolEvent{
        id = EventID,
        source = MachineID,
        created_at = Dt,
        payload = Payload,
        sequence = ID
    }.
