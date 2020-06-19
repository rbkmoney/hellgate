%%% Invoice payment submachine
%%%
%%% TODO
%%%  - make proper submachine interface
%%%     - `init` should provide `next` or `done` to the caller
%%%  - handle idempotent callbacks uniformly
%%%     - get rid of matches against session status
%%%  - tag machine with the provider trx
%%%     - distinguish between trx tags and callback tags
%%%     - tag namespaces
%%%  - think about safe clamping of timers returned by some proxy
%%%  - why don't user interaction events imprint anything on the state?
%%%  - adjustments look and behave very much like claims over payments
%%%  - payment status transition are caused by the fact that some session
%%%    finishes, which could have happened in the past, not just now

-module(hg_invoice_payment).
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("damsel/include/dmsl_msgpack_thrift.hrl").

-include_lib("fault_detector_proto/include/fd_proto_fault_detector_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).
-export([get_refunds/1]).
-export([get_chargebacks/1]).
-export([get_chargeback_state/2]).
-export([get_refund/2]).
-export([get_route/1]).
-export([get_adjustments/1]).
-export([get_adjustment/2]).

-export([get_final_cashflow/1]).
-export([get_sessions/1]).

-export([get_party_revision/1]).
-export([get_remaining_payment_balance/1]).
-export([get_activity/1]).
-export([get_tags/1]).

-export([construct_payment_info/2]).
-export([set_repair_scenario/2]).

%% Business logic

-export([capture/5]).
-export([cancel/2]).
-export([refund/3]).

-export([manual_refund/3]).

-export([create_adjustment/4]).
-export([capture_adjustment/3]).
-export([cancel_adjustment/3]).

-export([create_chargeback/3]).
-export([cancel_chargeback/3]).
-export([reject_chargeback/3]).
-export([accept_chargeback/3]).
-export([reopen_chargeback/3]).

%% Machine like

-export([init/3]).

-export([process_signal/3]).
-export([process_call/3]).

-export([merge_change/3]).
-export([collapse_changes/2]).

-export([get_log_params/2]).

%% Marshalling

-export([unmarshal/1]).

%%

-export_type([st/0]).
-export_type([activity/0]).
-export_type([machine_result/0]).
-export_type([opts/0]).
-export_type([payment/0]).

-type activity()            :: payment_activity()
                             | refund_activity()
                             | adjustment_activity()
                             | chargeback_activity()
                             | idle.

-type payment_activity()    :: {payment, payment_step()}.

-type refund_activity()     :: {refund_new      , refund_id()}
                             | {refund_session  , refund_id()}
                             | {refund_failure  , refund_id()}
                             | {refund_accounter, refund_id()}
                             .

-type adjustment_activity() :: {adjustment_new    , adjustment_id()}
                             | {adjustment_pending, adjustment_id()}.

-type chargeback_activity() :: {chargeback, chargeback_id(), chargeback_activity_type()}.

-type chargeback_activity_type() :: hg_invoice_payment_chargeback:activity().

-type payment_step()        :: new
                             | risk_scoring
                             | routing
                             | cash_flow_building
                             | processing_session
                             | processing_accounter
                             | processing_capture
                             | processing_failure
                             | updating_accounter
                             | flow_waiting
                             | finalizing_session
                             | finalizing_accounter
                             .

-record(st, {
    activity               :: activity(),
    payment                :: undefined | payment(),
    risk_score             :: undefined | risk_score(),
    route                  :: undefined | route(),
    cash_flow              :: undefined | cash_flow(),
    partial_cash_flow      :: undefined | cash_flow(),
    final_cash_flow        :: undefined | cash_flow(),
    trx                    :: undefined | trx_info(),
    target                 :: undefined | target(),
    sessions       = #{}   :: #{target_type() => [session()]},
    retry_attempts = #{}   :: #{target_type() => non_neg_integer()},
    refunds        = #{}   :: #{refund_id() => refund_state()},
    chargebacks    = #{}   :: #{chargeback_id() => chargeback_state()},
    adjustments    = []    :: [adjustment()],
    recurrent_token        :: undefined | recurrent_token(),
    opts                   :: undefined | opts(),
    repair_scenario        :: undefined | hg_invoice_repair:scenario(),
    capture_params         :: undefined | capture_params(),
    failure                :: undefined | failure(),
    timings                :: undefined | hg_timings:t()
}).

-record(refund_st, {
    refund            :: undefined | domain_refund(),
    cash_flow         :: undefined | cash_flow(),
    sessions     = [] :: [session()],
    transaction_info  :: undefined | trx_info(),
    failure           :: undefined | failure()
}).

-type chargeback_state() :: hg_invoice_payment_chargeback:state().

-type refund_state() :: #refund_st{}.
-type st() :: #st{}.

-type cash()                :: dmsl_domain_thrift:'Cash'().
-type cart()                :: dmsl_domain_thrift:'InvoiceCart'().
-type party()               :: dmsl_domain_thrift:'Party'().
-type payer()               :: dmsl_domain_thrift:'Payer'().
-type invoice()             :: dmsl_domain_thrift:'Invoice'().
-type invoice_id()          :: dmsl_domain_thrift:'InvoiceID'().
-type payment()             :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()          :: dmsl_domain_thrift:'InvoicePaymentID'().
-type payment_status()      :: dmsl_domain_thrift:'InvoicePaymentStatus'().
-type domain_refund()       :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type payment_refund()      :: dmsl_payment_processing_thrift:'InvoicePaymentRefund'().
-type refund_id()           :: dmsl_domain_thrift:'InvoicePaymentRefundID'().
-type refund_params()       :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type payment_chargeback()  :: dmsl_payment_processing_thrift:'InvoicePaymentChargeback'().
-type chargeback()          :: dmsl_domain_thrift:'InvoicePaymentChargeback'().
-type chargeback_id()       :: dmsl_domain_thrift:'InvoicePaymentChargebackID'().
-type adjustment()          :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()       :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params()   :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type adjustment_state()    :: dmsl_domain_thrift:'InvoicePaymentAdjustmentState'().
-type adjustment_status_change() :: dmsl_domain_thrift:'InvoicePaymentAdjustmentStatusChange'().
-type target()              :: dmsl_domain_thrift:'TargetInvoicePaymentStatus'().
-type target_type()         :: 'processed' | 'captured' | 'cancelled' | 'refunded'.
-type risk_score()          :: dmsl_domain_thrift:'RiskScore'().
-type route()               :: dmsl_domain_thrift:'PaymentRoute'().
-type cash_flow()           :: dmsl_domain_thrift:'FinalCashFlow'().
-type trx_info()            :: dmsl_domain_thrift:'TransactionInfo'().
-type session_result()      :: dmsl_payment_processing_thrift:'SessionResult'().
-type proxy_state()         :: dmsl_proxy_provider_thrift:'ProxyState'().
-type tag()                 :: dmsl_proxy_provider_thrift:'CallbackTag'().
-type callback()            :: dmsl_proxy_provider_thrift:'Callback'().
-type callback_response()   :: dmsl_proxy_provider_thrift:'CallbackResponse'().
-type timeout_behaviour()   :: dmsl_timeout_behaviour_thrift:'TimeoutBehaviour'().
-type make_recurrent()      :: true | false.
-type recurrent_token()     :: dmsl_domain_thrift:'Token'().
-type retry_strategy()      :: hg_retry:strategy().
-type capture_params()      :: dmsl_payment_processing_thrift:'InvoicePaymentCaptureParams'().
-type payment_session()     :: dmsl_payment_processing_thrift:'InvoicePaymentSession'().
-type failure()             :: dmsl_domain_thrift:'OperationFailure'().

-type session_status()      :: active | suspended | finished.

-type session() :: #{
    target            := target(),
    status            := session_status(),
    trx               := trx_info(),
    tags              := [tag()],
    timeout_behaviour := timeout_behaviour(),
    result            => session_result(),
    proxy_state       => proxy_state(),
    timings           => hg_timings:t()
}.

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

%%

-include("domain.hrl").
-include("payment_events.hrl").

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().

%%

-spec get_party_revision(st()) -> {hg_party:party_revision(), hg_datetime:timestamp()}.

get_party_revision(#st{activity = {payment, _}} = St) ->
    #domain_InvoicePayment{party_revision = Revision, created_at = Timestamp} = get_payment(St),
    {Revision, Timestamp};
get_party_revision(#st{activity = {chargeback, ID, _Type}} = St) ->
    CB = hg_invoice_payment_chargeback:get(get_chargeback_state(ID, St)),
    #domain_InvoicePaymentChargeback{party_revision = Revision, created_at = Timestamp} = CB,
    {Revision, Timestamp};
get_party_revision(#st{activity = {_, ID} = Activity} = St)
    when Activity =:= {refund_new      , ID} orelse
         Activity =:= {refund_failure  , ID} orelse
         Activity =:= {refund_session  , ID} orelse
         Activity =:= {refund_accounter, ID} ->
    #domain_InvoicePaymentRefund{party_revision = Revision, created_at = Timestamp} = get_refund(ID, St),
    {Revision, Timestamp};
get_party_revision(#st{activity = {adjustment_new, ID}} = St) ->
    #domain_InvoicePaymentAdjustment{party_revision = Revision, created_at = Timestamp} = get_adjustment(ID, St),
    {Revision, Timestamp};
get_party_revision(#st{activity = Activity}) ->
    erlang:error({no_revision_for_activity, Activity}).

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

-spec get_route(st()) -> route().

get_route(#st{route = Route}) ->
    Route.

-spec get_adjustments(st()) -> [adjustment()].

get_adjustments(#st{adjustments = As}) ->
    As.

-spec get_adjustment(adjustment_id(), st()) -> adjustment() | no_return().

get_adjustment(ID, St) ->
    case try_get_adjustment(ID, St) of
        Adjustment = #domain_InvoicePaymentAdjustment{} ->
            Adjustment;
        undefined ->
            throw(#payproc_InvoicePaymentAdjustmentNotFound{})
    end.

-spec get_chargeback_state(chargeback_id(), st()) -> chargeback_state() | no_return().

get_chargeback_state(ID, St) ->
    case try_get_chargeback_state(ID, St) of
        undefined ->
            throw(#payproc_InvoicePaymentChargebackNotFound{});
        ChargebackState ->
            ChargebackState
    end.

-spec get_chargebacks(st()) -> [payment_chargeback()].

get_chargebacks(#st{chargebacks = CBs}) ->
    [build_payment_chargeback(CB) || {_ID, CB} <- lists:sort(maps:to_list(CBs))].

build_payment_chargeback(ChargebackState) ->
    #payproc_InvoicePaymentChargeback{
       chargeback = hg_invoice_payment_chargeback:get(ChargebackState),
       cash_flow = hg_invoice_payment_chargeback:get_cash_flow(ChargebackState)
    }.

-spec get_sessions(st()) -> [payment_session()].

get_sessions(#st{sessions = S}) ->
    [
        #payproc_InvoicePaymentSession{
            target_status = TS,
            transaction_info = TR
        }
        || #{target := TS, trx := TR} <- lists:flatten(maps:values(S))
    ].

-spec get_refunds(st()) -> [payment_refund()].

get_refunds(#st{refunds = Rs, payment = Payment}) ->
    RefundList = lists:map(
        fun (#refund_st{refund = R, sessions = S, cash_flow = C}) ->
            #payproc_InvoicePaymentRefund{
                refund = enrich_refund_with_cash(R, Payment),
                sessions = lists:map(fun convert_refund_sessions/1, S),
                cash_flow = C
            }
        end,
        maps:values(Rs)
    ),
    lists:sort(
        fun(
            #payproc_InvoicePaymentRefund{refund = X},
            #payproc_InvoicePaymentRefund{refund = Y}
        ) ->
            Xid = X#domain_InvoicePaymentRefund.id,
            Yid = Y#domain_InvoicePaymentRefund.id,
            Xid =< Yid
        end,
        RefundList
    ).

-spec get_refunds_count(st()) -> non_neg_integer().

get_refunds_count(#st{refunds = Refunds}) ->
    maps:size(Refunds).

convert_refund_sessions(#{trx := TR}) ->
    #payproc_InvoiceRefundSession{
        transaction_info = TR
    }.

-spec get_refund(refund_id(), st()) -> domain_refund() | no_return().

get_refund(ID, St = #st{payment = Payment}) ->
    case try_get_refund_state(ID, St) of
        #refund_st{refund = Refund} ->
            enrich_refund_with_cash(Refund, Payment);
        undefined ->
            throw(#payproc_InvoicePaymentRefundNotFound{})
    end.

%%

-spec get_activity(st()) -> activity().

get_activity(#st{activity = Activity}) ->
    Activity.

-spec get_tags(st()) -> [tag()].

get_tags(#st{sessions = Sessions, refunds = Refunds}) ->
    lists:usort(lists:flatten(
        [get_session_tags(S)                     || S <- lists:flatten(maps:values(Sessions))] ++
        [get_session_tags(get_refund_session(R)) || R <- maps:values(Refunds) ]
    )).

-spec get_opts(st()) -> opts().

get_opts(#st{opts = Opts}) ->
    Opts.

-spec get_chargeback_opts(st()) -> hg_invoice_payment_chargeback:opts().

get_chargeback_opts(#st{opts = Opts} = St) ->
    maps:merge(Opts, #{payment_state => St}).

%%

-type event()  :: dmsl_payment_processing_thrift:'InvoicePaymentChangePayload'().
-type action() :: hg_machine_action:t().
-type events() :: [event()].
-type result() :: {events(), action()}.
-type machine_result() :: {next | done, result()}.

-spec init(payment_id(), _, opts()) ->
    {st(), result()}.

init(PaymentID, PaymentParams, Opts) ->
    scoper:scope(
        payment,
        #{
            id => PaymentID
        },
        fun() -> init_(PaymentID, PaymentParams, Opts) end
    ).

-spec init_(payment_id(), _, opts()) ->
    {st(), result()}.

init_(PaymentID, Params, Opts) ->
    Revision = hg_domain:head(),
    Party = get_party(Opts),
    Shop = get_shop(Opts),
    Invoice = get_invoice(Opts),
    Cost = get_invoice_cost(Invoice),
    {ok, Payer, VS0} = construct_payer(get_payer_params(Params), Shop),
    Flow = get_flow_params(Params),
    MakeRecurrent = get_make_recurrent_params(Params),
    ExternalID = get_external_id(Params),
    CreatedAt = hg_datetime:format_now(),
    MerchantTerms = get_merchant_terms(Opts, Revision, CreatedAt),
    VS1 = collect_validation_varset(Party, Shop, VS0),
    Context = get_context_params(Params),
    Deadline = get_processing_deadline(Params),
    Payment = construct_payment(
        PaymentID, CreatedAt, Cost, Payer, Flow, MerchantTerms, Party, Shop,
        VS1, Revision, MakeRecurrent, Context, ExternalID, Deadline
    ),
    Events = [?payment_started(Payment)],
    {collapse_changes(Events, undefined), {Events, hg_machine_action:instant()}}.

get_merchant_payments_terms(Opts, Revision, Timestamp) ->
    TermSet = get_merchant_terms(Opts, Revision, Timestamp),
    TermSet#domain_TermSet.payments.

get_merchant_terms(Opts, Revision, Timestamp) ->
    Invoice = get_invoice(Opts),
    Party = get_party(Opts),
    Shop = hg_party:get_shop(get_invoice_shop_id(Invoice), Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_contract_active(Contract),
    pm_party:get_terms(Contract, Timestamp, Revision).

get_provider_payments_terms(Route, Revision) ->
    hg_routing:get_payments_terms(Route, Revision).

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    throw(#payproc_InvalidContractStatus{status = Status}).

get_payer_params(#payproc_InvoicePaymentParams{payer = PayerParams}) ->
    PayerParams.

get_flow_params(#payproc_InvoicePaymentParams{flow = FlowParams}) ->
    FlowParams.

get_make_recurrent_params(#payproc_InvoicePaymentParams{make_recurrent = undefined}) ->
    false;
get_make_recurrent_params(#payproc_InvoicePaymentParams{make_recurrent = MakeRecurrent}) ->
    MakeRecurrent.

get_context_params(#payproc_InvoicePaymentParams{context = Context}) ->
    Context.

get_external_id(#payproc_InvoicePaymentParams{external_id = ExternalID}) ->
    ExternalID.

get_processing_deadline(#payproc_InvoicePaymentParams{processing_deadline = Deadline}) ->
    Deadline.

construct_payer({payment_resource, #payproc_PaymentResourcePayerParams{
    resource = Resource,
    contact_info = ContactInfo
}}, _) ->
    {ok, ?payment_resource_payer(Resource, ContactInfo), #{}};
construct_payer({recurrent, #payproc_RecurrentPayerParams{
    recurrent_parent = Parent,
    contact_info = ContactInfo
}}, _) ->
    ?recurrent_parent(InvoiceID, PaymentID) = Parent,
    ParentPayment = try get_payment_state(InvoiceID, PaymentID)
    catch
        throw:#payproc_InvoiceNotFound{} ->
            throw_invalid_recurrent_parent(<<"Parent invoice not found">>);
        throw:#payproc_InvoicePaymentNotFound{} ->
            throw_invalid_recurrent_parent(<<"Parent payment not found">>)
    end,
    #domain_InvoicePayment{payer = ParentPayer} = get_payment(ParentPayment),
    ParentPaymentTool = get_payer_payment_tool(ParentPayer),
    {ok, ?recurrent_payer(ParentPaymentTool, Parent, ContactInfo), #{parent_payment => ParentPayment}};
construct_payer({customer, #payproc_CustomerPayerParams{customer_id = CustomerID}}, Shop) ->
    Customer = get_customer(CustomerID),
    ok = validate_customer_shop(Customer, Shop),
    ActiveBinding = get_active_binding(Customer),
    % by keynfawkes
    % TODO Should we bake recurrent token right in too?
    %      Expect to have some issues related to access control while trying
    %      to fetch this token during deeper payment flow stages
    % by antibi0tic
    % we dont need it for refund, so I think - no
    Payer = ?customer_payer(
        CustomerID,
        ActiveBinding#payproc_CustomerBinding.id,
        ActiveBinding#payproc_CustomerBinding.rec_payment_tool_id,
        get_resource_payment_tool(ActiveBinding#payproc_CustomerBinding.payment_resource),
        get_customer_contact_info(Customer)
    ),
    {ok, Payer, #{}}.

validate_customer_shop(#payproc_Customer{shop_id = ShopID}, #domain_Shop{id = ShopID}) ->
    ok;
validate_customer_shop(_, _) ->
    throw_invalid_request(<<"Invalid customer">>).

get_active_binding(#payproc_Customer{bindings = Bindings, active_binding_id = BindingID}) ->
    case lists:keysearch(BindingID, #payproc_CustomerBinding.id, Bindings) of
        {value, ActiveBinding} ->
            ActiveBinding;
        false ->
            throw_invalid_request(<<"Specified customer is not ready">>)
    end.

get_customer_contact_info(#payproc_Customer{contact_info = ContactInfo}) ->
    ContactInfo.

construct_payment(
    PaymentID,
    CreatedAt,
    Cost,
    Payer,
    FlowParams,
    Terms,
    Party,
    Shop,
    VS0,
    Revision,
    MakeRecurrent,
    Context,
    ExternalID,
    Deadline
) ->
    #domain_TermSet{payments = PaymentTerms, recurrent_paytools = RecurrentTerms} = Terms,
    PaymentTool = get_payer_payment_tool(Payer),
    VS1 = validate_payment_tool(
        PaymentTool,
        PaymentTerms#domain_PaymentsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS2 = validate_payment_cost(
        Cost,
        PaymentTerms#domain_PaymentsServiceTerms.cash_limit,
        VS1,
        Revision
    ),
    Flow = construct_payment_flow(
        FlowParams,
        CreatedAt,
        PaymentTerms#domain_PaymentsServiceTerms.holds,
        VS2,
        Revision
    ),
    RecurrentValidationVarset = #{
        payer => Payer,
        shop => Shop,
        party => Party,
        varset => VS2,
        revision => Revision,
        created_at => CreatedAt,
        recurrent_terms => RecurrentTerms,
        payment_tool => PaymentTool,
        parent_payment => maps:get(parent_payment, VS2, undefined)
    },
    ok = validate_recurrent_intention(RecurrentValidationVarset, MakeRecurrent),
    #domain_InvoicePayment{
        id                  = PaymentID,
        created_at          = CreatedAt,
        owner_id            = Party#domain_Party.id,
        shop_id             = Shop#domain_Shop.id,
        domain_revision     = Revision,
        party_revision      = Party#domain_Party.revision,
        status              = ?pending(),
        cost                = Cost,
        payer               = Payer,
        flow                = Flow,
        make_recurrent      = MakeRecurrent,
        context             = Context,
        external_id         = ExternalID,
        processing_deadline = Deadline
    }.

construct_payment_flow({instant, _}, _CreatedAt, _Terms, _VS, _Revision) ->
    ?invoice_payment_flow_instant();
construct_payment_flow({hold, Params}, CreatedAt, Terms, VS, Revision) ->
    OnHoldExpiration = Params#payproc_InvoicePaymentParamsFlowHold.on_hold_expiration,
    ?hold_lifetime(Seconds) = validate_hold_lifetime(Terms, VS, Revision),
    HeldUntil = hg_datetime:format_ts(hg_datetime:parse_ts(CreatedAt) + Seconds),
    ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil).

reconstruct_payment_flow(?invoice_payment_flow_instant(), _CreatedAt, VS) ->
    VS#{flow => instant};
reconstruct_payment_flow(?invoice_payment_flow_hold(_OnHoldExpiration, HeldUntil), CreatedAt, VS) ->
    Seconds = hg_datetime:parse_ts(HeldUntil) - hg_datetime:parse_ts(CreatedAt),
    VS#{flow => {hold, ?hold_lifetime(Seconds)}}.

-spec get_predefined_route(payer()) -> {ok, route()} | undefined.
get_predefined_route(?payment_resource_payer()) ->
    undefined;
get_predefined_route(?recurrent_payer() = Payer) ->
    get_predefined_recurrent_route(Payer);
get_predefined_route(?customer_payer() = Payer) ->
    get_predefined_customer_route(Payer).

-spec get_predefined_customer_route(payer()) -> {ok, route()} | undefined.
get_predefined_customer_route(?customer_payer(_, _, RecPaymentToolID, _, _) = Payer) ->
    case get_rec_payment_tool(RecPaymentToolID) of
        {ok, #payproc_RecurrentPaymentTool{
            route = Route
        }} when Route =/= undefined ->
            {ok, Route};
        _ ->
            % TODO more elegant error
            error({'Can\'t get route for customer payer', Payer})
    end.

-spec get_predefined_recurrent_route(payer()) -> {ok, route()}.
get_predefined_recurrent_route(?recurrent_payer(_, ?recurrent_parent(InvoiceID, PaymentID), _)) ->
    PreviousPayment = get_payment_state(InvoiceID, PaymentID),
    {ok, get_route(PreviousPayment)}.

validate_hold_lifetime(
    #domain_PaymentHoldsServiceTerms{
        payment_methods = PMs,
        lifetime = LifetimeSelector
    },
    VS,
    Revision
) ->
    PaymentTool = genlib_map:get(payment_tool, VS),
    _ = validate_payment_tool(PaymentTool, PMs, VS, Revision),
    reduce_selector(hold_lifetime, LifetimeSelector, VS, Revision);
validate_hold_lifetime(undefined, _VS, _Revision) ->
    throw_invalid_request(<<"Holds are not available">>).

-spec validate_recurrent_intention(map(), make_recurrent()) ->
    ok | no_return().
validate_recurrent_intention(#{payer := ?recurrent_payer()} = VS, MakeRecurrent) ->
    ok = validate_recurrent_terms(VS),
    ok = validate_recurrent_payer(VS, MakeRecurrent),
    ok = validate_recurrent_parent(VS);
validate_recurrent_intention(VS, true = MakeRecurrent) ->
    ok = validate_recurrent_terms(VS),
    ok = validate_recurrent_payer(VS, MakeRecurrent);
validate_recurrent_intention(_VS, false = _MakeRecurrent) ->
    ok.

-spec validate_recurrent_terms(map()) -> ok | no_return().
validate_recurrent_terms(#{recurrent_terms := undefined}) ->
    throw(#payproc_OperationNotPermitted{});
validate_recurrent_terms(VS) ->
    #{
        recurrent_terms := Terms,
        varset := Varset,
        revision := Revision,
        payment_tool := PaymentTool
    } = VS,
    #domain_RecurrentPaytoolsServiceTerms{payment_methods = PaymentMethodSelector} = Terms,
    PMs = reduce_selector(recurrent_payment_methods, PaymentMethodSelector, Varset, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    ok.

-spec validate_recurrent_parent(map()) -> ok | no_return().
validate_recurrent_parent(#{parent_payment := ParentPayment} = VS) ->
    ok = validate_recurrent_token_present(ParentPayment),
    ok = validate_recurrent_parent_shop(VS, ParentPayment),
    ok = validate_recurrent_parent_status(ParentPayment).

-spec validate_recurrent_token_present(st()) -> ok | no_return().
validate_recurrent_token_present(PaymentState) ->
    case get_recurrent_token(PaymentState) of
        Token when Token =/= undefined ->
            ok;
        undefined ->
            throw_invalid_recurrent_parent(<<"Parent payment has no recurrent token">>)
    end.

-spec validate_recurrent_parent_shop(map(), st()) -> ok | no_return().
validate_recurrent_parent_shop(#{shop := Shop}, PaymentState) ->
    PaymentShopID = get_payment_shop_id(get_payment(PaymentState)),
    case Shop of
        #domain_Shop{id = ShopID} when ShopID =:= PaymentShopID ->
            ok;
        _Other ->
            throw_invalid_recurrent_parent(<<"Parent payment refer to another shop">>)
    end.

-spec validate_recurrent_parent_status(st()) -> ok | no_return().
validate_recurrent_parent_status(PaymentState) ->
    case get_payment(PaymentState) of
        #domain_InvoicePayment{status = {failed, _}} ->
            throw_invalid_recurrent_parent(<<"Invalid parent payment status">>);
        _Other ->
            ok
    end.

-spec validate_recurrent_payer(map(), make_recurrent()) -> ok | no_return().
validate_recurrent_payer(#{payer := ?recurrent_payer()}, _MakeRecurrent) ->
    ok;
validate_recurrent_payer(#{payer := ?payment_resource_payer()}, true) ->
    ok;
validate_recurrent_payer(#{payer := _Other}, true) ->
    throw_invalid_request(<<"Invalid payer">>).

validate_payment_tool(PaymentTool, PaymentMethodSelector, VS, Revision) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    VS#{payment_tool => PaymentTool}.

validate_payment_cost(Cost, CashLimitSelector, VS, Revision) ->
    ok = validate_cash(Cost, CashLimitSelector, VS, Revision),
    VS#{cost => Cost}.

validate_refund_cash(Cash, CashLimitSelector, VS, Revision) ->
    ok = validate_cash(Cash, CashLimitSelector, VS, Revision),
    VS.

validate_cash(Cash, CashLimitSelector, VS, Revision) ->
    Limit = reduce_selector(cash_limit, CashLimitSelector, VS, Revision),
    ok = validate_limit(Cash, Limit).

validate_limit(Cash, CashRange) ->
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            ok;
        {exceeds, lower} ->
            throw_invalid_request(<<"Invalid amount, less than allowed minumum">>);
        {exceeds, upper} ->
            throw_invalid_request(<<"Invalid amount, more than allowed maximum">>)
    end.

choose_route(PaymentInstitution, VS, Revision, St) ->
    Payer = get_payment_payer(St),
    case get_predefined_route(Payer) of
        {ok, _Route} = Result ->
            Result;
        undefined ->
            Payment        = get_payment(St),
            Predestination = choose_routing_predestination(Payment),
            {Routes, RejectContext} = hg_routing:gather_routes(
                Predestination,
                PaymentInstitution,
                VS,
                Revision
            ),
            FailRatedRoutes = hg_routing:gather_fail_rates(Routes),
            case hg_routing:choose_route(FailRatedRoutes, RejectContext, VS) of
                {ok, Route, ChoiceMeta} ->
                    _ = log_route_choice_meta(ChoiceMeta),
                    _ = log_misconfigurations(RejectContext),
                    {ok, Route};
                {error, {no_route_found, {RejectReason, RejectContext1}}} = Error ->
                    _ = log_reject_context(RejectReason, RejectContext1),
                    Error
            end
    end.

-spec choose_routing_predestination(payment()) -> hg_routing:route_predestination().
choose_routing_predestination(#domain_InvoicePayment{make_recurrent = true}) ->
    recurrent_payment;
choose_routing_predestination(#domain_InvoicePayment{payer = ?payment_resource_payer()}) ->
    payment.

% Other payers has predefined routes

log_route_choice_meta(ChoiceMeta) ->
    _ = logger:log(info, "Routing decision made", hg_routing:get_logger_metadata(ChoiceMeta)).

log_misconfigurations(RejectContext) ->
    RejectedProviders = maps:get(rejected_providers, RejectContext),
    RejectedRoutes    = maps:get(rejected_routes, RejectContext),
    Rejects           = RejectedProviders ++ RejectedRoutes,
    _ = lists:foreach(fun maybe_log_misconfiguration/1, Rejects),
    ok.

maybe_log_misconfiguration({PRef, {'Misconfiguration', Reason}}) ->
    Text = "The provider with ref ~p has been misconfigured: ~p",
    _ = logger:warning(Text, [PRef, Reason]);
maybe_log_misconfiguration({PRef, TRef, {'Misconfiguration', Reason}}) ->
    Text = "The route with provider ref ~p and terminal ref ~p has been misconfigured: ~p",
    _ = logger:warning(Text, [PRef, TRef, Reason]);
maybe_log_misconfiguration(_NotMisconfiguration) ->
    ok.

log_reject_context(risk_score_is_too_high = RejectReason, RejectContext) ->
    log_reject_context(info, RejectReason, RejectContext);
log_reject_context(RejectReason, RejectContext) ->
    log_reject_context(warning, RejectReason, RejectContext).

log_reject_context(Level, RejectReason, RejectContext) ->
    _ = logger:log(
        Level,
        "No route found, reason = ~p, varset: ~p",
        [RejectReason, maps:get(varset, RejectContext)],
        logger:get_process_metadata()),
    _ = logger:log(
        Level,
        "No route found, reason = ~p, rejected providers: ~p",
        [RejectReason, maps:get(rejected_providers, RejectContext)],
        logger:get_process_metadata()),
    _ = logger:log(
        Level,
        "No route found, reason = ~p, rejected routes: ~p",
        [RejectReason, maps:get(rejected_routes, RejectContext)],
        logger:get_process_metadata()),
    ok.

validate_refund_time(RefundCreatedAt, PaymentCreatedAt, TimeSpanSelector, VS, Revision) ->
    EligibilityTime = reduce_selector(eligibility_time, TimeSpanSelector, VS, Revision),
    RefundEndTime = hg_datetime:add_time_span(EligibilityTime, PaymentCreatedAt),
    case hg_datetime:compare(RefundCreatedAt, RefundEndTime) of
        Result when Result == earlier; Result == simultaneously ->
            VS;
        later ->
            throw(#payproc_OperationNotPermitted{})
    end.

collect_chargeback_varset(
    #domain_PaymentChargebackServiceTerms{ },
    VS,
    _Revision
) ->
    % nothing here yet
    VS;
collect_chargeback_varset(undefined, VS, _Revision) ->
    VS.

collect_refund_varset(
    #domain_PaymentRefundsServiceTerms{
        payment_methods  = PaymentMethodSelector,
        partial_refunds  = PartialRefundsServiceTerms
    },
    VS,
    Revision
) ->
    RPMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    case ordsets:is_element(hg_payment_tool:get_method(maps:get(payment_tool, VS)), RPMs) of
        true ->
            RVS = collect_partial_refund_varset(PartialRefundsServiceTerms, VS, Revision),
            VS#{refunds => RVS};
        false ->
            VS
    end;
collect_refund_varset(undefined, VS, _Revision) ->
    VS.

collect_partial_refund_varset(
    #domain_PartialRefundsServiceTerms{
        cash_limit = CashLimitSelector
    },
    VS,
    Revision
) ->
    #{partial => #{
        cash_limit => reduce_selector(cash_limit, CashLimitSelector, VS, Revision)
    }};
collect_partial_refund_varset(undefined, _, _) ->
    #{}.

collect_validation_varset(St, Opts) ->
    collect_validation_varset(get_party(Opts), get_shop(Opts), get_payment(St), #{}).

collect_validation_varset(Party, Shop, VS) ->
    #domain_Party{id = PartyID} = Party,
    #domain_Shop{
        id = ShopID,
        category = Category,
        account = #domain_ShopAccount{currency = Currency}
    } = Shop,
    VS#{
        party_id => PartyID,
        shop_id  => ShopID,
        category => Category,
        currency => Currency
    }.

collect_validation_varset(Party, Shop, Payment, VS) ->
    VS0 = collect_validation_varset(Party, Shop, VS),
    VS0#{
        cost         => get_payment_cost(Payment),
        payment_tool => get_payment_tool(Payment)
    }.

collect_routing_varset(Payment, Opts, VS0) ->
    VS1 = collect_validation_varset(get_party(Opts), get_shop(Opts), Payment, VS0),
    #domain_InvoicePayment{
        created_at      = CreatedAt,
        domain_revision = Revision,
        flow            = DomainFlow
    } = Payment,
    MerchantTerms = get_merchant_payments_terms(Opts, Revision, CreatedAt),
    VS2 = reconstruct_payment_flow(DomainFlow, CreatedAt, VS1),
    VS3 = collect_refund_varset(
        MerchantTerms#domain_PaymentsServiceTerms.refunds,
        VS2,
        Revision
    ),
    collect_chargeback_varset(
        MerchantTerms#domain_PaymentsServiceTerms.chargebacks,
        VS3,
        Revision
    ).

%%

collect_cashflow(
    #domain_PaymentsServiceTerms{fees = MerchantCashflowSelector},
    #domain_PaymentsProvisionTerms{cash_flow = ProviderCashflowSelector},
    VS,
    Revision
) ->
    MerchantCashflow = reduce_selector(merchant_payment_fees     , MerchantCashflowSelector, VS, Revision),
    ProviderCashflow = reduce_selector(provider_payment_cash_flow, ProviderCashflowSelector, VS, Revision),
    MerchantCashflow ++ ProviderCashflow.

construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision) ->
    hg_cashflow:finalize(
        Cashflow,
        collect_cash_flow_context(Payment),
        hg_accounting:collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision)
    ).

construct_final_cashflow(Cashflow, Context, AccountMap) ->
    hg_cashflow:finalize(Cashflow, Context, AccountMap).

collect_cash_flow_context(
    #domain_InvoicePayment{cost = Cost}
) ->
    #{
        operation_amount => Cost
    };
collect_cash_flow_context(
    #domain_InvoicePaymentRefund{cash = Cash}
) ->
    #{
        operation_amount => Cash
    }.

get_available_amount(AccountID, Clock) ->
    #{
        min_available_amount := AvailableAmount
    } =
        hg_accounting:get_balance(AccountID, Clock),
    AvailableAmount.

construct_payment_plan_id(St) ->
    construct_payment_plan_id(get_invoice(get_opts(St)), get_payment(St)).

construct_payment_plan_id(Invoice, Payment) ->
    hg_utils:construct_complex_id([
        get_invoice_id(Invoice),
        get_payment_id(Payment)
    ]).

reduce_selector(Name, Selector, VS, Revision) ->
    case pm_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

%%

-spec start_session(target()) ->
    events().

start_session(Target) ->
    [?session_ev(Target, ?session_started())].

start_capture(Reason, Cost, Cart) ->
    [?payment_capture_started(Reason, Cost, Cart)] ++
    start_session(?captured(Reason, Cost, Cart)).

start_partial_capture(Reason, Cost, Cart, Cashflow) ->
   [
       ?payment_capture_started(Reason, Cost, Cart),
       ?cash_flow_changed(Cashflow)
   ].

-spec capture(st(), binary(), cash() | undefined, cart() | undefined, opts()) -> {ok, result()}.

capture(St, Reason, Cost, Cart, Opts) ->
    Payment = get_payment(St),
    _ = assert_capture_cost_currency(Cost, Payment),
    _ = assert_capture_cart(Cost, Cart),
    _ = assert_activity({payment, flow_waiting}, St),
    _ = assert_payment_flow(hold, Payment),
    case check_equal_capture_cost_amount(Cost, Payment) of
        true ->
            total_capture(St, Reason, Cart);
        false ->
            partial_capture(St, Reason, Cost, Cart, Opts)
    end.

total_capture(St, Reason, Cart) ->
    Payment = get_payment(St),
    Cost = get_payment_cost(Payment),
    Changes = start_capture(Reason, Cost, Cart),
    {ok, {Changes, hg_machine_action:instant()}}.

partial_capture(St0, Reason, Cost, Cart, Opts) ->
    Payment         = get_payment(St0),
    Payment2        = Payment#domain_InvoicePayment{cost = Cost},
    St              = St0#st{payment = Payment2},
    Revision        = get_payment_revision(St),
    Timestamp       = get_payment_created_at(Payment),
    MerchantTerms   = get_merchant_payments_terms(Opts, Revision, Timestamp),
    ok              = validate_merchant_hold_terms(MerchantTerms),
    Route           = get_route(St),
    ProviderTerms   = get_provider_payments_terms(Route, Revision),
    ok              = validate_provider_holds_terms(ProviderTerms),
    FinalCashflow   = calculate_cashflow(Timestamp, Revision, St, Opts),
    Changes         = start_partial_capture(Reason, Cost, Cart, FinalCashflow),
    {ok, {Changes, hg_machine_action:instant()}}.

-spec cancel(st(), binary()) -> {ok, result()}.

cancel(St, Reason) ->
    Payment = get_payment(St),
    _ = assert_activity({payment, flow_waiting}, St),
    _ = assert_payment_flow(hold, Payment),
    Changes = start_session(?cancelled_with_reason(Reason)),
    {ok, {Changes, hg_machine_action:instant()}}.

assert_capture_cost_currency(undefined, _) ->
    ok;
assert_capture_cost_currency(?cash(_, SymCode), #domain_InvoicePayment{cost = ?cash(_, SymCode)}) ->
    ok;
assert_capture_cost_currency(?cash(_, PassedSymCode), #domain_InvoicePayment{cost = ?cash(_, SymCode)}) ->
    throw(#payproc_InconsistentCaptureCurrency{
        payment_currency = SymCode,
        passed_currency = PassedSymCode
    }).

validate_processing_deadline(#domain_InvoicePayment{processing_deadline = Deadline}, _TargetType = processed) ->
    case hg_invoice_utils:check_deadline(Deadline) of
        ok ->
            ok;
        {error, deadline_reached} ->
            {failure, payproc_errors:construct('PaymentFailure',
                {authorization_failed, {processing_deadline_reached, #payprocerr_GeneralFailure{}}}
            )}
    end;
validate_processing_deadline(_, _TargetType) ->
    ok.


assert_capture_cart(_Cost, undefined) ->
    ok;
assert_capture_cart(Cost, Cart) ->
    case Cost =:= hg_invoice_utils:get_cart_amount(Cart) of
        true ->
            ok;
        _ ->
            throw_invalid_request(<<"Capture amount does not match with the cart total amount">>)
    end.

check_equal_capture_cost_amount(undefined, _) ->
    true;
check_equal_capture_cost_amount(?cash(PassedAmount, _), #domain_InvoicePayment{cost = ?cash(Amount, _)})
    when PassedAmount =:= Amount
->
    true;
check_equal_capture_cost_amount(?cash(PassedAmount, _), #domain_InvoicePayment{cost = ?cash(Amount, _)})
    when PassedAmount < Amount
->
    false;
check_equal_capture_cost_amount(?cash(PassedAmount, _), #domain_InvoicePayment{cost = ?cash(Amount, _)}) ->
    throw(#payproc_AmountExceededCaptureBalance{
        payment_amount = Amount,
        passed_amount = PassedAmount
    }).

validate_merchant_hold_terms(#domain_PaymentsServiceTerms{holds = Terms}) when Terms /= undefined ->
    case Terms of
        %% Чтобы упростить интеграцию, по умолчанию разрешили частичные подтверждения
        #domain_PaymentHoldsServiceTerms{partial_captures = undefined} ->
            ok;
        #domain_PaymentHoldsServiceTerms{} ->
            throw(#payproc_OperationNotPermitted{})
    end;
%% Чтобы упростить интеграцию, по умолчанию разрешили частичные подтверждения
validate_merchant_hold_terms(#domain_PaymentsServiceTerms{holds = undefined}) ->
    ok.

validate_provider_holds_terms(#domain_PaymentsProvisionTerms{holds = Terms}) when Terms /= undefined ->
    case Terms of
        %% Чтобы упростить интеграцию, по умолчанию разрешили частичные подтверждения
        #domain_PaymentHoldsProvisionTerms{partial_captures = undefined} ->
            ok;
        #domain_PaymentHoldsProvisionTerms{} ->
            throw(#payproc_OperationNotPermitted{})
    end;
%% Чтобы упростить интеграцию, по умолчанию разрешили частичные подтверждения
validate_provider_holds_terms(#domain_PaymentsProvisionTerms{holds = undefined}) ->
    ok.

-spec create_chargeback(st(), opts(), hg_invoice_payment_chargeback:create_params()) ->
    {chargeback(), result()}.
create_chargeback(St, Opts, Params) ->
    _ = assert_no_pending_chargebacks(St),
    _ = validate_payment_status(captured, get_payment(St)),
    ChargebackID = get_chargeback_id(Params),
    CBOpts       = Opts#{payment_state => St},
    {Chargeback, {Changes, Action}} = hg_invoice_payment_chargeback:create(CBOpts, Params),
    {Chargeback, {[?chargeback_ev(ChargebackID, C) || C <- Changes], Action}}.

-spec cancel_chargeback(chargeback_id(), st(), hg_invoice_payment_chargeback:cancel_params()) ->
    {ok, result()}.
cancel_chargeback(ChargebackID, St, Params) ->
    ChargebackState = get_chargeback_state(ChargebackID, St),
    {ok, {Changes, Action}} = hg_invoice_payment_chargeback:cancel(ChargebackState, Params),
    {ok, {[?chargeback_ev(ChargebackID, C) || C <- Changes], Action}}.

-spec reject_chargeback(chargeback_id(), st(), hg_invoice_payment_chargeback:reject_params()) ->
    {ok, result()}.
reject_chargeback(ChargebackID, St, Params) ->
    ChargebackState = get_chargeback_state(ChargebackID, St),
    {ok, {Changes, Action}} = hg_invoice_payment_chargeback:reject(ChargebackState, St, Params),
    {ok, {[?chargeback_ev(ChargebackID, C) || C <- Changes], Action}}.

-spec accept_chargeback(chargeback_id(), st(), hg_invoice_payment_chargeback:accept_params()) ->
    {ok, result()}.
accept_chargeback(ChargebackID, St, Params) ->
    ChargebackState = get_chargeback_state(ChargebackID, St),
    {ok, {Changes, Action}} = hg_invoice_payment_chargeback:accept(ChargebackState, St, Params),
    {ok, {[?chargeback_ev(ChargebackID, C) || C <- Changes], Action}}.

-spec reopen_chargeback(chargeback_id(), st(), hg_invoice_payment_chargeback:reopen_params()) ->
    {ok, result()}.
reopen_chargeback(ChargebackID, St, Params) ->
    _ = assert_no_pending_chargebacks(St),
    ChargebackState = get_chargeback_state(ChargebackID, St),
    {ok, {Changes, Action}} = hg_invoice_payment_chargeback:reopen(ChargebackState, St, Params),
    {ok, {[?chargeback_ev(ChargebackID, C) || C <- Changes], Action}}.

get_chargeback_id(#payproc_InvoicePaymentChargebackParams{id = ID}) ->
    ID.

validate_payment_status(Status, #domain_InvoicePayment{status = {Status, _}}) ->
    ok;
validate_payment_status(_, #domain_InvoicePayment{status = Status}) ->
    throw(#payproc_InvalidPaymentStatus{status = Status}).

-spec refund(refund_params(), st(), opts()) ->
    {domain_refund(), result()}.

refund(Params, St0, Opts) ->
    St = St0#st{opts = Opts},
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    Payment = get_payment(St),
    Refund = make_refund(Params, Payment, Revision, CreatedAt, St, Opts),
    FinalCashflow = make_refund_cashflow(Refund, Payment, Revision, CreatedAt, St, Opts),
    Changes = [?refund_created(Refund, FinalCashflow)],
    Action = hg_machine_action:instant(),
    ID = Refund#domain_InvoicePaymentRefund.id,
    {Refund, {[?refund_ev(ID, C) || C <- Changes], Action}}.

-spec manual_refund(refund_params(), st(), opts()) ->
    {domain_refund(), result()}.

manual_refund(Params, St0, Opts) ->
    St = St0#st{opts = Opts},
    Revision = hg_domain:head(),
    CreatedAt = hg_datetime:format_now(),
    Payment = get_payment(St),
    Refund = make_refund(Params, Payment, Revision, CreatedAt, St, Opts),
    FinalCashflow = make_refund_cashflow(Refund, Payment, Revision, CreatedAt, St, Opts),
    TransactionInfo = Params#payproc_InvoicePaymentRefundParams.transaction_info,
    Changes = [?refund_created(Refund, FinalCashflow, TransactionInfo)],
    Action = hg_machine_action:instant(),
    ID = Refund#domain_InvoicePaymentRefund.id,
    {Refund, {[?refund_ev(ID, C) || C <- Changes], Action}}.

make_refund(Params, Payment, Revision, CreatedAt, St, Opts) ->
    _ = assert_no_pending_chargebacks(St),
    _ = assert_payment_status(captured, Payment),
    PartyRevision = get_opts_party_revision(Opts),
    _ = assert_previous_refunds_finished(St),
    Cash = define_refund_cash(Params#payproc_InvoicePaymentRefundParams.cash, St),
    _ = assert_refund_cash(Cash, St),
    Cart = Params#payproc_InvoicePaymentRefundParams.cart,
    _ = assert_refund_cart(Params#payproc_InvoicePaymentRefundParams.cash, Cart, St),
    #domain_InvoicePaymentRefund {
        id              = Params#payproc_InvoicePaymentRefundParams.id,
        created_at      = CreatedAt,
        domain_revision = Revision,
        party_revision  = PartyRevision,
        status          = ?refund_pending(),
        reason          = Params#payproc_InvoicePaymentRefundParams.reason,
        cash            = Cash,
        cart            = Cart,
        external_id     = Params#payproc_InvoicePaymentRefundParams.external_id
    }.

make_refund_cashflow(Refund, Payment, Revision, CreatedAt, St, Opts) ->
    Route = get_route(St),
    Shop = get_shop(Opts),
    MerchantTerms = get_merchant_refunds_terms(get_merchant_payments_terms(Opts, Revision, CreatedAt)),
    VS0 = collect_validation_varset(St, Opts),
    VS1 = validate_refund(MerchantTerms, Refund, Payment, VS0, Revision),
    ProviderPaymentsTerms = get_provider_payments_terms(Route, Revision),
    ProviderTerms = get_provider_refunds_terms(ProviderPaymentsTerms, Refund, Payment, VS1, Revision),
    Cashflow = collect_refund_cashflow(MerchantTerms, ProviderTerms, VS1, Revision),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Provider = get_route_provider(Route, Revision),
    AccountMap = hg_accounting:collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS1, Revision),
    construct_final_cashflow(Cashflow, collect_cash_flow_context(Refund), AccountMap).

assert_refund_cash(Cash, St) ->
    PaymentAmount = get_remaining_payment_amount(Cash, St),
    assert_remaining_payment_amount(PaymentAmount, St).

assert_remaining_payment_amount(?cash(Amount, _), _St) when Amount >= 0 ->
    ok;
assert_remaining_payment_amount(?cash(Amount, _), St) when Amount < 0 ->
    Maximum = get_remaining_payment_balance(St),
    throw(#payproc_InvoicePaymentAmountExceeded{maximum = Maximum}).

assert_previous_refunds_finished(St) ->
    PendingRefunds = lists:filter(
        fun(#payproc_InvoicePaymentRefund{refund = R}) ->
            case R#domain_InvoicePaymentRefund.status of
                 ?refund_pending() ->
                    true;
                _ ->
                    false
            end
        end,
        get_refunds(St)),
    case PendingRefunds of
        [] ->
            ok;
        [_R|_] ->
            throw(#payproc_OperationNotPermitted{})
    end.

assert_refund_cart(_RefundCash, undefined, _St) ->
    ok;
assert_refund_cart(undefined, _Cart, _St) ->
    throw_invalid_request(<<"Refund amount does not match with the cart total amount">>);
assert_refund_cart(RefundCash, Cart, St) ->
    InterimPaymentAmount = get_remaining_payment_balance(St),
    case hg_cash:sub(InterimPaymentAmount, RefundCash) =:= hg_invoice_utils:get_cart_amount(Cart) of
        true ->
            ok;
        _ ->
            throw_invalid_request(<<"Remaining payment amount not equal cart cost">>)
    end.

get_remaining_payment_amount(Cash, St) ->
    InterimPaymentAmount = get_remaining_payment_balance(St),
    hg_cash:sub(InterimPaymentAmount, Cash).

-spec get_remaining_payment_balance(st()) ->
    cash().

get_remaining_payment_balance(St) ->
    Chargebacks = [CB#payproc_InvoicePaymentChargeback.chargeback || CB <- get_chargebacks(St)],
    PaymentAmount = get_payment_cost(get_payment(St)),
    lists:foldl(
        fun
            (#payproc_InvoicePaymentRefund{refund = R}, Acc) ->
                case get_refund_status(R) of
                    ?refund_succeeded() ->
                        hg_cash:sub(Acc, get_refund_cash(R));
                    _ ->
                        Acc
                end;
            (CB = #domain_InvoicePaymentChargeback{}, Acc) ->
                case hg_invoice_payment_chargeback:get_status(CB) of
                    ?chargeback_status_accepted() ->
                        hg_cash:sub(Acc, hg_invoice_payment_chargeback:get_body(CB));
                    _ ->
                        Acc
                end
        end,
        PaymentAmount,
        get_refunds(St) ++ Chargebacks
    ).

get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = Terms}) when Terms /= undefined ->
    Terms;
get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = undefined}) ->
    throw(#payproc_OperationNotPermitted{}).

get_provider_refunds_terms(
    #domain_PaymentsProvisionTerms{refunds = Terms},
    Refund,
    Payment,
    VS,
    Revision
) when Terms /= undefined ->
    Cost = get_payment_cost(Payment),
    Cash = get_refund_cash(Refund),
    case hg_cash:sub(Cost, Cash) of
        ?cash(0, _) ->
            Terms;
        ?cash(Amount, _) when Amount > 0 ->
            get_provider_partial_refunds_terms(Terms, Refund, Payment, VS, Revision)
    end;
get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = undefined}, _Refund, Payment, _VS, _Revision) ->
    error({misconfiguration, {'No refund terms for a payment', Payment}}).

get_provider_partial_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{
        partial_refunds = #domain_PartialRefundsProvisionTerms{
            cash_limit = CashLimitSelector
        }
    } = Terms,
    Refund,
    _Payment,
    VS,
    Revision
) ->
    Cash = get_refund_cash(Refund),
    CashRange = reduce_selector(cash_limit, CashLimitSelector, VS, Revision),
    case hg_cash_range:is_inside(Cash, CashRange) of
        within ->
            Terms;
        {exceeds, _} ->
            error({misconfiguration, {'Refund amount doesnt match allowed cash range', CashRange}})
    end;
get_provider_partial_refunds_terms(
    #domain_PaymentRefundsProvisionTerms{partial_refunds = undefined},
    _Refund,
    Payment,
    _VS,
    _Revision
) ->
    error({misconfiguration, {'No partial refund terms for a payment', Payment}}).

validate_refund(Terms, Refund, Payment, VS0, Revision) ->
    Cost = get_payment_cost(Payment),
    Cash = get_refund_cash(Refund),
    case hg_cash:sub(Cost, Cash) of
        ?cash(0, _) ->
            validate_common_refund_terms(Terms, Refund, Payment, VS0, Revision);
        ?cash(Amount, _) when Amount > 0 ->
            validate_partial_refund(Terms, Refund, Payment, VS0, Revision)
    end.

validate_partial_refund(
    #domain_PaymentRefundsServiceTerms{partial_refunds = PRs} = Terms,
    Refund,
    Payment,
    VS0,
    Revision
) when PRs /= undefined ->
    VS1 = validate_common_refund_terms(Terms, Refund, Payment, VS0, Revision),
    VS2 = validate_refund_cash(
        get_refund_cash(Refund),
        PRs#domain_PartialRefundsServiceTerms.cash_limit,
        VS1,
        Revision
    ),
    VS2;
validate_partial_refund(
    #domain_PaymentRefundsServiceTerms{partial_refunds = undefined},
    _Refund,
    _Payment,
    _VS0,
    _Revision
) ->
    throw(#payproc_OperationNotPermitted{}).

validate_common_refund_terms(Terms, Refund, Payment, VS0, Revision) ->
    VS1 = validate_payment_tool(
        get_payment_tool(Payment),
        Terms#domain_PaymentRefundsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS2 = validate_refund_time(
        get_refund_created_at(Refund),
        get_payment_created_at(Payment),
        Terms#domain_PaymentRefundsServiceTerms.eligibility_time,
        VS1,
        Revision
    ),
    VS2.

collect_refund_cashflow(
    #domain_PaymentRefundsServiceTerms{fees = MerchantCashflowSelector},
    #domain_PaymentRefundsProvisionTerms{cash_flow = ProviderCashflowSelector},
    VS,
    Revision
) ->
    MerchantCashflow = reduce_selector(merchant_refund_fees     , MerchantCashflowSelector, VS, Revision),
    ProviderCashflow = reduce_selector(provider_refund_cash_flow, ProviderCashflowSelector, VS, Revision),
    MerchantCashflow ++ ProviderCashflow.

prepare_refund_cashflow(RefundSt, St) ->
    hg_accounting:hold(construct_refund_plan_id(RefundSt, St), get_refund_cashflow_plan(RefundSt)).

commit_refund_cashflow(RefundSt, St) ->
    hg_accounting:commit(construct_refund_plan_id(RefundSt, St), [get_refund_cashflow_plan(RefundSt)]).

rollback_refund_cashflow(RefundSt, St) ->
    hg_accounting:rollback(construct_refund_plan_id(RefundSt, St), [get_refund_cashflow_plan(RefundSt)]).

construct_refund_plan_id(RefundSt, St) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(get_opts(St))),
        get_payment_id(get_payment(St)),
        {refund_session, get_refund_id(get_refund(RefundSt))}
    ]).

get_refund_cashflow_plan(RefundSt) ->
    {1, get_refund_cashflow(RefundSt)}.

%%

-spec create_adjustment(hg_datetime:timestamp(), adjustment_params(), st(), opts()) ->
    {adjustment(), result()}.

create_adjustment(Timestamp, Params, St, Opts) ->
    _ = assert_no_adjustment_pending(St),
    case Params#payproc_InvoicePaymentAdjustmentParams.legacy_domain_revision of
        undefined ->
            create_adjustment_with_scenario(Timestamp, Params, St, Opts);
        DomainRevision -> % FIXME: remove this check when new control center will be deployed
            create_cash_flow_adjustment(Timestamp, Params, DomainRevision, St, Opts)
    end.

-spec create_adjustment_with_scenario(hg_datetime:timestamp(), adjustment_params(), st(), opts()) ->
    {adjustment(), result()}.

create_adjustment_with_scenario(Timestamp, Params, St, Opts) ->
    case Params#payproc_InvoicePaymentAdjustmentParams.scenario of
        undefined ->
            create_cash_flow_adjustment(Timestamp, Params, undefined, St, Opts);
        {cash_flow, #domain_InvoicePaymentAdjustmentCashFlow{domain_revision = DomainRevision}} ->
            create_cash_flow_adjustment(Timestamp, Params, DomainRevision, St, Opts);
        {status_change, Change} ->
            create_status_adjustment(Timestamp, Params, Change, St, Opts)
    end.

-spec create_cash_flow_adjustment(
    hg_datetime:timestamp(),
    adjustment_params(),
    undefined | hg_domain:revision(),
    st(),
    opts()
) ->
    {adjustment(), result()}.

create_cash_flow_adjustment(Timestamp, Params, DomainRevision, St, Opts) ->
    Payment = get_payment(St),
    _ = assert_payment_status(captured, Payment),
    NewRevision = maybe_get_domain_revision(DomainRevision),
    PartyRevision = get_opts_party_revision(Opts),
    OldCashFlow = get_final_cashflow(St),
    NewCashFlow = calculate_cashflow(Timestamp, NewRevision, St, Opts),
    AdjState = {cash_flow, #domain_InvoicePaymentAdjustmentCashFlowState{
        scenario = #domain_InvoicePaymentAdjustmentCashFlow{domain_revision = DomainRevision}
    }},
    construct_adjustment(
        Timestamp,
        Params,
        NewRevision,
        PartyRevision,
        OldCashFlow,
        NewCashFlow,
        AdjState,
        St
    ).

-spec create_status_adjustment(
    hg_datetime:timestamp(),
    adjustment_params(),
    adjustment_status_change(),
    st(),
    opts()
) ->
    {adjustment(), result()}.

create_status_adjustment(Timestamp, Params, Change, St, Opts) ->
    #domain_InvoicePaymentAdjustmentStatusChange{
        target_status = TargetStatus
    } = Change,
    #domain_InvoicePayment{
        status = Status,
        domain_revision = DomainRevision,
        party_revision  = PartyRevision
    } = get_payment(St),
    ok = assert_adjustment_payment_status(Status),
    ok = assert_no_refunds(St),
    ok = assert_adjustment_payment_statuses(TargetStatus, Status),
    OldCashFlow = get_cash_flow_for_status(Status, St),
    NewCashFlow = get_cash_flow_for_target_status(TargetStatus, St, Opts),
    AdjState = {status_change, #domain_InvoicePaymentAdjustmentStatusChangeState{
        scenario = Change
    }},
    construct_adjustment(
        Timestamp,
        Params,
        DomainRevision,
        PartyRevision,
        OldCashFlow,
        NewCashFlow,
        AdjState,
        St
    ).

-spec maybe_get_domain_revision(undefined | hg_domain:revision()) ->
    hg_domain:revision().

maybe_get_domain_revision(undefined) ->
    hg_domain:head();
maybe_get_domain_revision(DomainRevision) ->
    DomainRevision.

-spec assert_adjustment_payment_status(payment_status()) ->
    ok | no_return().

assert_adjustment_payment_status(Status) ->
    case is_adjustment_payment_status_final(Status) of
        true ->
            ok;
        false ->
            erlang:throw(#payproc_InvalidPaymentStatus{status = Status})
    end.

assert_no_refunds(St) ->
    case get_refunds_count(St) of
        0 ->
            ok;
        _ ->
            Details = <<"Cannot change status of payment with refunds.">>,
            erlang:throw(#'InvalidRequest'{errors = [Details]})
    end.

-spec assert_adjustment_payment_statuses(TargetStatus :: payment_status(), Status :: payment_status()) ->
    ok | no_return().

assert_adjustment_payment_statuses(Status, Status) ->
    erlang:throw(#payproc_InvoicePaymentAlreadyHasStatus{status = Status});
assert_adjustment_payment_statuses(TargetStatus, _Status) ->
    case is_adjustment_payment_status_final(TargetStatus) of
        true ->
            ok;
        false ->
            erlang:throw(#payproc_InvalidPaymentTargetStatus{status = TargetStatus})
    end.

-spec is_adjustment_payment_status_final(payment_status()) ->
    boolean().

is_adjustment_payment_status_final({captured, _}) ->
    true;
is_adjustment_payment_status_final({cancelled, _}) ->
    true;
is_adjustment_payment_status_final({failed, _}) ->
    true;
is_adjustment_payment_status_final(_) ->
    false.

-spec get_cash_flow_for_status(payment_status(), st()) ->
    cash_flow().

get_cash_flow_for_status({captured, _}, St) ->
    get_final_cashflow(St);
get_cash_flow_for_status({cancelled, _}, _St) ->
    [];
get_cash_flow_for_status({failed, _}, _St) ->
    [].

-spec get_cash_flow_for_target_status(payment_status(), st(), opts()) ->
    cash_flow().

get_cash_flow_for_target_status({captured, Captured}, St0, Opts) ->
    Payment0 = get_payment(St0),
    Cost = get_captured_cost(Captured, Payment0),
    Payment = Payment0#domain_InvoicePayment{
        cost = Cost
    },
    Timestamp = get_payment_created_at(Payment),
    St = St0#st{payment = Payment},
    Revision = Payment#domain_InvoicePayment.domain_revision,
    calculate_cashflow(Timestamp, Revision, St, Opts);
get_cash_flow_for_target_status({cancelled, _}, _St, _Opts) ->
    [];
get_cash_flow_for_target_status({failed, _}, _St, _Opts) ->
    [].

-spec calculate_cashflow(hg_datetime:timestamp(), hg_domain:revision(), st(), opts()) ->
    cash_flow().

calculate_cashflow(Timestamp, Revision, St, Opts) ->
    Payment = get_payment(St),
    Route = get_route(St),
    VS = collect_validation_varset(St, Opts),
    calculate_cashflow(Route, Payment, VS, Timestamp, Revision, Opts).

-spec calculate_cashflow(
    route(),
    payment(),
    map(),
    hg_datetime:timestamp(),
    hg_domain:revision(),
    opts()
) ->
    cash_flow().

calculate_cashflow(Route, Payment, VS, Timestamp, Revision, Opts) ->
    Shop = get_shop(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Provider = get_route_provider(Route, Revision),
    MerchantTerms = get_merchant_payments_terms(Opts, Revision, Timestamp),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS, Revision),
    construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision).

-spec construct_adjustment(
    Timestamp       :: hg_datetime:timestamp(),
    Params          :: adjustment_params(),
    DomainRevision  :: hg_domain:revision(),
    PartyRevision   :: hg_party:party_revision(),
    OldCashFlow     :: cash_flow(),
    NewCashFlow     :: cash_flow(),
    State           :: adjustment_state(),
    St              :: st()
) ->
    {adjustment(), result()}.

construct_adjustment(Timestamp, Params, DomainRevision, PartyRevision, OldCashFlow, NewCashFlow, State, St) ->
    ID = construct_adjustment_id(St),
    Adjustment = #domain_InvoicePaymentAdjustment{
        id                    = ID,
        status                = ?adjustment_pending(),
        created_at            = Timestamp,
        domain_revision       = DomainRevision,
        party_revision        = PartyRevision,
        reason                = Params#payproc_InvoicePaymentAdjustmentParams.reason,
        old_cash_flow_inverse = hg_cashflow:revert(OldCashFlow),
        new_cash_flow         = NewCashFlow,
        state                 = State
    },
    Event = ?adjustment_ev(ID, ?adjustment_created(Adjustment)),
    {Adjustment, {[Event], hg_machine_action:instant()}}.

construct_adjustment_id(#st{adjustments = As}) ->
    erlang:integer_to_binary(length(As) + 1).

-spec assert_activity(activity(), st()) -> ok | no_return().
assert_activity(Activity, #st{activity = Activity}) ->
    ok;
assert_activity(_Activity, St) ->
    %% TODO: Create dedicated error like "Payment is capturing already"
    #domain_InvoicePayment{status = Status} = get_payment(St),
    throw(#payproc_InvalidPaymentStatus{status = Status}).

assert_payment_status(Status, #domain_InvoicePayment{status = {Status, _}}) ->
    ok;
assert_payment_status(_, #domain_InvoicePayment{status = Status}) ->
    throw(#payproc_InvalidPaymentStatus{status = Status}).

assert_no_pending_chargebacks(PaymentState) ->
    Chargebacks = [CB#payproc_InvoicePaymentChargeback.chargeback || CB <- get_chargebacks(PaymentState)],
    case lists:any(fun hg_invoice_payment_chargeback:is_pending/1, Chargebacks) of
        true ->
            throw(#payproc_InvoicePaymentChargebackPending{});
        false ->
            ok
    end.

assert_no_adjustment_pending(#st{adjustments = As}) ->
    lists:foreach(fun assert_adjustment_finalized/1, As).

assert_adjustment_finalized(#domain_InvoicePaymentAdjustment{id = ID, status = {Status, _}}) when
    Status =:= pending;
    Status =:= processed
->
    throw(#payproc_InvoicePaymentAdjustmentPending{id = ID});
assert_adjustment_finalized(_) ->
    ok.

assert_payment_flow(hold, #domain_InvoicePayment{flow = ?invoice_payment_flow_hold(_, _)}) ->
    ok;
assert_payment_flow(_, _) ->
    throw(#payproc_OperationNotPermitted{}).

-spec capture_adjustment(adjustment_id(), st(), opts()) ->
    {ok, result()}.

capture_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, capture, St, Options).

-spec cancel_adjustment(adjustment_id(), st(), opts()) ->
    {ok, result()}.

cancel_adjustment(ID, St, Options) ->
    finalize_adjustment(ID, cancel, St, Options).

-spec finalize_adjustment(adjustment_id(), capture | cancel, st(), opts()) ->
    {ok, result()}.

finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(processed, Adjustment),
    ok = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
    Status = case Intent of
        capture ->
            ?adjustment_captured(hg_datetime:format_now());
        cancel ->
            ?adjustment_cancelled(hg_datetime:format_now())
    end,
    Event = ?adjustment_ev(ID, ?adjustment_status_changed(Status)),
    {ok, {[Event], hg_machine_action:new()}}.

prepare_adjustment_cashflow(Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    plan(PlanID, Plan).

finalize_adjustment_cashflow(Intent, Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    case Intent of
        capture ->
            commit(PlanID, Plan);
        cancel ->
            rollback(PlanID, Plan)
    end.

get_adjustment_cashflow_plan(#domain_InvoicePaymentAdjustment{
    old_cash_flow_inverse = CashflowInverse,
    new_cash_flow         = Cashflow
}) ->
    number_plan([CashflowInverse, Cashflow], 1, []).

number_plan([], _Number, Acc) ->
    lists:reverse(Acc);
number_plan([[] | Tail], Number, Acc) ->
    number_plan(Tail, Number, Acc);
number_plan([NonEmpty | Tail], Number, Acc) ->
    number_plan(Tail, Number + 1, [{Number, NonEmpty} | Acc]).

plan(_PlanID, []) ->
    ok;
plan(PlanID, Plan) ->
    _ = hg_accounting:plan(PlanID, Plan),
    ok.

commit(_PlanID, []) ->
    ok;
commit(PlanID, Plan) ->
    _ = hg_accounting:commit(PlanID, Plan),
    ok.

rollback(_PlanID, []) ->
    ok;
rollback(PlanID, Plan) ->
    _ = hg_accounting:rollback(PlanID, Plan),
    ok.

assert_adjustment_status(Status, #domain_InvoicePaymentAdjustment{status = {Status, _}}) ->
    ok;
assert_adjustment_status(_, #domain_InvoicePaymentAdjustment{status = Status}) ->
    throw(#payproc_InvalidPaymentAdjustmentStatus{status = Status}).

construct_adjustment_plan_id(Adjustment, St, Options) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(Options)),
        get_payment_id(get_payment(St)),
        {adj, get_adjustment_id(Adjustment)}
    ]).

get_adjustment_id(#domain_InvoicePaymentAdjustment{id = ID}) ->
    ID.

get_adjustment_status(#domain_InvoicePaymentAdjustment{status = Status}) ->
    Status.

get_adjustment_cashflow(#domain_InvoicePaymentAdjustment{new_cash_flow = Cashflow}) ->
    Cashflow.

-define(
    adjustment_target_status(Status),
    #domain_InvoicePaymentAdjustment{
        state = {status_change, #domain_InvoicePaymentAdjustmentStatusChangeState{
            scenario = #domain_InvoicePaymentAdjustmentStatusChange{target_status = Status}}
        }
    }
).

%%

-spec process_signal(timeout, st(), opts()) ->
    machine_result().
process_signal(timeout, St, Options) ->
    scoper:scope(
        payment,
        get_st_meta(St),
        fun() -> process_timeout(St#st{opts = Options}) end
    ).

process_timeout(St) ->
    Action = hg_machine_action:new(),
    repair_process_timeout(get_activity(St), Action, St).

-spec process_timeout(activity(), action(), st()) -> machine_result().
process_timeout({payment, risk_scoring}, Action, St) ->
    %% There are three processing_accounter steps here (scoring, routing and cash flow building)
    process_routing(Action, St);
process_timeout({payment, Step}, Action, St) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    process_session(Action, St);
process_timeout({payment, Step}, Action, St) when
    Step =:= processing_failure orelse
    Step =:= processing_accounter orelse
    Step =:= finalizing_accounter
->
    process_result(Action, St);
process_timeout({payment, updating_accounter}, Action, St) ->
    process_accounter_update(Action, St);
process_timeout({chargeback, ID, Type}, Action, St) ->
    process_chargeback(Type, ID, Action, St);
process_timeout({refund_new, ID}, Action, St) ->
    process_refund_cashflow(ID, Action, St);
process_timeout({refund_session, _ID}, Action, St) ->
    process_session(Action, St);
process_timeout({refund_failure, _ID}, Action, St) ->
    process_result(Action, St);
process_timeout({refund_accounter, _ID}, Action, St) ->
    process_result(Action, St);
process_timeout({adjustment_new, ID}, Action, St) ->
    process_adjustment_cashflow(ID, Action, St);
process_timeout({payment, flow_waiting}, Action, St) ->
    finalize_payment(Action, St).

repair_process_timeout(Activity, Action, St = #st{repair_scenario = Scenario}) ->
    case hg_invoice_repair:check_for_action(fail_pre_processing, Scenario) of
        {result, Result} ->
            Result;
        call ->
            process_timeout(Activity, Action, St)
    end.

-spec process_call({callback, tag(), callback()}, st(), opts()) ->
    {callback_response(), machine_result()}.
process_call({callback, Tag, Payload}, St, Options) ->
    scoper:scope(
        payment,
        get_st_meta(St),
        fun() -> process_callback(Tag, Payload, St#st{opts = Options}) end
    ).

-spec process_callback(tag(), callback(), st()) ->
    {callback_response(), machine_result()}.
process_callback(Tag, Payload, St) ->
    Action = hg_machine_action:new(),
    Session = get_activity_session(St),
    process_callback(Tag, Payload, Action, Session, St).

process_callback(Tag, Payload, Action, Session, St) when Session /= undefined ->
    case {get_session_status(Session), get_session_tags(Session)} of
        {suspended, [Tag | _]} ->
            handle_callback(Payload, Action, St);
        _ ->
            throw(invalid_callback)
    end;
process_callback(_Tag, _Payload, _Action, undefined, _St) ->
    throw(invalid_callback).

%%

-spec process_routing(action(), st()) -> machine_result().
process_routing(Action, St) ->
    Opts = get_opts(St),
    Revision = get_payment_revision(St),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Payment = get_payment(St),
    VS0 = collect_routing_varset(Payment, Opts, #{}),
    RiskScore = repair_inspect(Payment, PaymentInstitution, VS0, Opts, St),
    Events0 = [?risk_score_changed(RiskScore)],
    VS1 = VS0#{risk_score => RiskScore},
    case choose_route(PaymentInstitution, VS1, Revision, St) of
        {ok, Route} ->
            process_cash_flow_building(Route, VS1, Payment, Revision, Opts, Events0, Action);
        {error, {no_route_found, {Reason, _Details}}} ->
            Failure = {failure, payproc_errors:construct('PaymentFailure',
                {no_route_found, {Reason, #payprocerr_GeneralFailure{}}}
            )},
            process_failure(get_activity(St), Events0, Action, Failure, St)
    end.

process_cash_flow_building(Route, VS, Payment, Revision, Opts, Events0, Action) ->
    Timestamp = get_payment_created_at(Payment),
    FinalCashflow = calculate_cashflow(Route, Payment, VS, Timestamp, Revision, Opts),
    Invoice = get_invoice(Opts),
    _Clock = hg_accounting:hold(
        construct_payment_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Events1 = Events0 ++ [?route_changed(Route), ?cash_flow_changed(FinalCashflow)],
    {next, {Events1, hg_machine_action:set_timeout(0, Action)}}.

%%

-spec process_chargeback(chargeback_activity_type(), chargeback_id(), action(), st()) -> machine_result().
process_chargeback(Type = finalising_accounter, ID, Action0, St) ->
    ChargebackState    = get_chargeback_state(ID, St),
    ChargebackOpts     = get_chargeback_opts(St),
    ChargebackBody     = hg_invoice_payment_chargeback:get_body(ChargebackState),
    ChargebackTarget   = hg_invoice_payment_chargeback:get_target_status(ChargebackState),
    MaybeChargedback   = maybe_set_charged_back_status(ChargebackTarget, ChargebackBody, St),
    {Changes, Action1} = hg_invoice_payment_chargeback:process_timeout(Type, ChargebackState, Action0, ChargebackOpts),
    {done, {[?chargeback_ev(ID, C) || C <- Changes] ++ MaybeChargedback, Action1}};
process_chargeback(Type, ID, Action0, St) ->
    ChargebackState    = get_chargeback_state(ID, St),
    ChargebackOpts     = get_chargeback_opts(St),
    {Changes, Action1} = hg_invoice_payment_chargeback:process_timeout(Type, ChargebackState, Action0, ChargebackOpts),
    {done, {[?chargeback_ev(ID, C) || C <- Changes], Action1}}.

maybe_set_charged_back_status(?chargeback_status_accepted(), ChargebackBody, St) ->
    InterimPaymentAmount = get_remaining_payment_balance(St),
    case hg_cash:sub(InterimPaymentAmount, ChargebackBody) of
        ?cash(Amount, _) when Amount =:= 0 ->
            [?payment_status_changed(?charged_back())];
        ?cash(Amount, _) when Amount > 0 ->
            []
    end;
maybe_set_charged_back_status(_ChargebackStatus, _ChargebackBody, _St) ->
    [].

%%

-spec process_refund_cashflow(refund_id(), action(), st()) -> machine_result().
process_refund_cashflow(ID, Action, St) ->
    Opts = get_opts(St),
    Shop = get_shop(Opts),
    #{{merchant, settlement} := SettlementID} = hg_accounting:collect_merchant_account_map(Shop, #{}),
    RefundSt = try_get_refund_state(ID, St),
    Clock = prepare_refund_cashflow(RefundSt, St),
    % NOTE we assume that posting involving merchant settlement account MUST be present in the cashflow
    case get_available_amount(SettlementID, Clock) of
        % TODO we must pull this rule out of refund terms
        Available when Available >= 0 ->
            Events0 = [?session_ev(?refunded(), ?session_started())],
            Events1 = get_manual_refund_events(RefundSt),
            {next, {
                [?refund_ev(ID, C) || C <- Events0 ++ Events1],
                hg_machine_action:set_timeout(0, Action)
            }};
        Available when Available < 0 ->
            Failure = {failure, payproc_errors:construct('RefundFailure',
                {terms_violated, {insufficient_merchant_funds, #payprocerr_GeneralFailure{}}}
            )},
            process_failure(get_activity(St), [], Action, Failure, St, RefundSt)
    end.

get_manual_refund_events(#refund_st{transaction_info = undefined}) ->
    [];
get_manual_refund_events(#refund_st{transaction_info = TransactionInfo}) ->
    [
        ?session_ev(?refunded(), ?trx_bound(TransactionInfo)),
        ?session_ev(?refunded(), ?session_finished(?session_succeeded()))
    ].
%%

-spec process_adjustment_cashflow(adjustment_id(), action(), st()) -> machine_result().
process_adjustment_cashflow(ID, _Action, St) ->
    Opts = get_opts(St),
    Adjustment = get_adjustment(ID, St),
    ok = prepare_adjustment_cashflow(Adjustment, St, Opts),
    Events = [?adjustment_ev(ID, ?adjustment_status_changed(?adjustment_processed()))],
    {done, {Events, hg_machine_action:new()}}.

process_accounter_update(Action, St = #st{partial_cash_flow = FinalCashflow, capture_params = CaptureParams}) ->
    Opts = get_opts(St),
    #payproc_InvoicePaymentCaptureParams{
        reason = Reason,
        cash = Cost,
        cart = Cart
    } = CaptureParams,
    Invoice  = get_invoice(Opts),
    Payment  = get_payment(St),
    Payment2 = Payment#domain_InvoicePayment{cost = Cost},
    _Clock = hg_accounting:plan(
        construct_payment_plan_id(Invoice, Payment2),
        [
            {2, hg_cashflow:revert(get_cashflow(St))},
            {3, FinalCashflow}
        ]
    ),
    Events = start_session(?captured(Reason, Cost, Cart)),
    {next, {Events, hg_machine_action:set_timeout(0, Action)}}.

%%

-spec process_session(action(), st()) -> machine_result().
process_session(Action, St) ->
    Session = get_activity_session(St),
    process_session(Session, Action, St).

process_session(undefined, Action, St0) ->
    Target     = get_target(St0),
    TargetType = get_target_type(Target),
    case validate_processing_deadline(get_payment(St0), TargetType) of
        ok ->
            Events = start_session(Target),
            St1    = collapse_changes(Events, St0),
            Result = {start_session(get_target(St0)), hg_machine_action:set_timeout(0, Action)},
            finish_session_processing(Result, St1);
        Failure ->
            process_failure(get_activity(St0), [], Action, Failure, St0)
    end;
process_session(Session, Action, St) ->
    process_session(Session, Action, [], St).

process_session(Session, Action, Events, St) ->
    Status = get_session_status(Session),
    process_session(Status, Session, Action, Events, St).

-spec process_session(session_status(), session(), action(), events(), st()) -> machine_result().
process_session(active, Session, Action, Events, St) ->
    process_active_session(Action, Session, Events, St);
process_session(suspended, Session, Action, Events, St) ->
    process_callback_timeout(Action, Session, Events, St).

-spec process_active_session(action(), session(), events(), st()) -> machine_result().
process_active_session(Action, Session, Events, St) ->
    {ok, ProxyResult} = repair_session(St),
    Result = handle_proxy_result(ProxyResult, Action, Events, Session),
    finish_session_processing(Result, St).

repair_session(St = #st{repair_scenario = Scenario}) ->
    case hg_invoice_repair:check_for_action(fail_session, Scenario) of
        {result, Result} ->
            {ok, Result};
        call ->
            ProxyContext = construct_proxy_context(St),
            hg_proxy_provider:process_payment(ProxyContext, get_route(St))
    end.

-spec finalize_payment(action(), st()) -> machine_result().
finalize_payment(Action, St) ->
    Target = case get_payment_flow(get_payment(St)) of
        ?invoice_payment_flow_instant() ->
            ?captured(<<"Timeout">>, get_payment_cost(get_payment(St)));
        ?invoice_payment_flow_hold(OnHoldExpiration, _) ->
            case OnHoldExpiration of
                cancel ->
                    ?cancelled();
                capture ->
                    ?captured(
                        <<"Timeout">>,
                        get_payment_cost(get_payment(St))
                    )
            end
    end,
    StartEvents = case Target of
        ?captured(Reason, Cost) ->
            start_capture(Reason, Cost, undefined);
        _ ->
            start_session(Target)
    end,
    {done, {StartEvents, hg_machine_action:set_timeout(0, Action)}}.

-spec process_callback_timeout(action(), session(), events(), st()) -> machine_result().
process_callback_timeout(Action, Session, Events, St) ->
    case get_session_timeout_behaviour(Session) of
        {callback, Payload} ->
            ProxyContext = construct_proxy_context(St),
            Route        = get_route(St),
            {ok, CallbackResult} = hg_proxy_provider:handle_payment_callback(Payload, ProxyContext, Route),
            {_Response, Result} = handle_callback_result(CallbackResult, Action, get_activity_session(St)),
            finish_session_processing(Result, St);
        {operation_failure, OperationFailure} ->
            SessionEvents = [?session_finished(?session_failed(OperationFailure))],
            Result = {Events ++ wrap_session_events(SessionEvents, Session), Action},
            finish_session_processing(Result, St)
    end.

-spec handle_callback(callback(), action(), st()) ->
    {callback_response(), machine_result()}.
handle_callback(Payload, Action, St) ->
    ProxyContext = construct_proxy_context(St),
    Route        = get_route(St),
    {ok, CallbackResult} = hg_proxy_provider:handle_payment_callback(Payload, ProxyContext, Route),
    {Response, Result} = handle_callback_result(CallbackResult, Action, get_activity_session(St)),
    {Response, finish_session_processing(Result, St)}.

-spec finish_session_processing(result(), st()) -> machine_result().
finish_session_processing(Result, St) ->
    finish_session_processing(get_activity(St), Result, St).

finish_session_processing({payment, Step} = Activity, {Events, Action}, St) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    Target   = get_target(St),
    Activity = get_activity(St),
    St1 = collapse_changes(Events, St),
    case get_session(Target, St1) of
        #{status := finished, result := ?session_succeeded(), target := Target} ->
            TargetType = get_target_type(Target),
            _ = maybe_notify_fault_detector(Activity, TargetType, start, St),
            _ = maybe_notify_fault_detector(Activity, TargetType, finish, St),
            NewAction = hg_machine_action:set_timeout(0, Action),
            {next, {Events, NewAction}};
        #{status := finished, result := ?session_failed(Failure)} ->
            process_failure(Activity, Events, Action, Failure, St);
        #{} ->
            {next, {Events, Action}}
    end;

finish_session_processing({refund_session, ID} = Activity, {Events, Action}, St) ->
    Events1 = [?refund_ev(ID, Ev) || Ev <- Events],
    St1 = collapse_changes(Events1, St),
    RefundSt1 = try_get_refund_state(ID, St1),
    case get_refund_session(RefundSt1) of
        #{status := finished, result := ?session_succeeded()} ->
            NewAction = hg_machine_action:set_timeout(0, Action),
            {next, {Events1, NewAction}};
        #{status := finished, result := ?session_failed(Failure)} ->
            process_failure(Activity, Events1, Action, Failure, St1, RefundSt1);
        #{} ->
            {next, {Events1, Action}}
    end.

-spec process_result(action(), st()) -> machine_result().
process_result(Action, St) ->
    process_result(get_activity(St), Action, St).

process_result({payment, processing_accounter}, Action, St) ->
    Target = get_target(St),
    NewAction = get_action(Target, Action, St),
    {done, {[?payment_status_changed(Target)], NewAction}};

process_result({payment, processing_failure}, Action, St = #st{failure = Failure}) ->
    NewAction = hg_machine_action:set_timeout(0, Action),
    _Clocks = rollback_payment_cashflow(St),
    {done, {[?payment_status_changed(?failed(Failure))], NewAction}};

process_result({payment, finalizing_accounter}, Action, St) ->
    Target = get_target(St),
    _Clocks = case Target of
        ?captured() ->
            commit_payment_cashflow(St);
        ?cancelled() ->
            rollback_payment_cashflow(St)
    end,
    NewAction = get_action(Target, Action, St),
    {done, {[?payment_status_changed(Target)], NewAction}};

process_result({refund_failure, ID}, Action, St) ->
    RefundSt = try_get_refund_state(ID, St),
    Failure = RefundSt#refund_st.failure,
    _Clocks = rollback_refund_cashflow(RefundSt, St),
    Events = [
        ?refund_ev(ID, ?refund_status_changed(?refund_failed(Failure)))
    ],
    {done, {Events, Action}};

process_result({refund_accounter, ID}, Action, St) ->
    RefundSt = try_get_refund_state(ID, St),
    _Clocks = commit_refund_cashflow(RefundSt, St),
    Events2 = [
        ?refund_ev(ID, ?refund_status_changed(?refund_succeeded()))
    ],
    Events3 = case get_remaining_payment_amount(get_refund_cash(get_refund(RefundSt)), St) of
        ?cash(Amount, _) when Amount =:= 0 ->
            [
                ?payment_status_changed(?refunded())
            ];
        ?cash(Amount, _) when Amount > 0 ->
            []
    end,
    {done, {Events2 ++ Events3, Action}}.

process_failure(Activity, Events, Action, Failure, St) ->
    process_failure(Activity, Events, Action, Failure, St, undefined).

process_failure({payment, Step}, Events, Action, Failure, _St, _RefundSt) when
    Step =:= risk_scoring orelse
    Step =:= routing
->
    {done, {Events ++ [?payment_status_changed(?failed(Failure))], Action}};
process_failure({payment, Step} = Activity, Events, Action, Failure, St, _RefundSt) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    Target = get_target(St),
    case check_retry_possibility(Target, Failure, St) of
        {retry, Timeout} ->
            _ = logger:info("Retry session after transient failure, wait ~p", [Timeout]),
            {SessionEvents, SessionAction} = retry_session(Action, Target, Timeout),
            {next, {Events ++ SessionEvents, SessionAction}};
        fatal ->
            TargetType = get_target_type(Target),
            OperationStatus = choose_fd_operation_status_for_failure(Failure),
            _ = maybe_notify_fault_detector(Activity, TargetType, start, St),
            _ = maybe_notify_fault_detector(Activity, TargetType, OperationStatus, St),
            process_fatal_payment_failure(Target, Events, Action, Failure, St)
    end;

process_failure({refund_new, ID}, [], Action, Failure, _St, _RefundSt) ->
    {next, {[?refund_ev(ID, ?refund_rollback_started(Failure))], hg_machine_action:set_timeout(0, Action)}};
process_failure({refund_session, ID}, Events, Action, Failure, St, _RefundSt) ->
    Target = ?refunded(),
    case check_retry_possibility(Target, Failure, St) of
        {retry, Timeout} ->
            _ = logger:info("Retry session after transient failure, wait ~p", [Timeout]),
            {SessionEvents, SessionAction} = retry_session(Action, Target, Timeout),
            Events1 = [?refund_ev(ID, E) || E <- SessionEvents],
            {next, {Events ++ Events1, SessionAction}};
        fatal ->
            Events1 = [
                ?refund_ev(ID, ?refund_rollback_started(Failure))
            ],
            {next, {Events ++ Events1, hg_machine_action:set_timeout(0, Action)}}
    end.

choose_fd_operation_status_for_failure({failure, Failure}) ->
    payproc_errors:match('PaymentFailure', Failure, fun do_choose_fd_operation_status_for_failure/1);
choose_fd_operation_status_for_failure(_Failure) ->
    finish.

do_choose_fd_operation_status_for_failure({authorization_failed, {FailType, _}}) ->
    DefaultBenignFailures = [
        insufficient_funds,
        rejected_by_issuer,
        processing_deadline_reached
    ],
    FDConfig = genlib_app:env(hellgate, fault_detector, #{}),
    Config = genlib_map:get(conversion, FDConfig, #{}),
    BenignFailures = genlib_map:get(benign_failures, Config, DefaultBenignFailures),
    case lists:member(FailType, BenignFailures) of
        false -> error;
        true  -> finish
    end;
do_choose_fd_operation_status_for_failure(_Failure) ->
    finish.

maybe_notify_fault_detector({payment, processing_session}, processed, Status, St) ->
    notify_fault_detector(Status, St);
maybe_notify_fault_detector(_Activity, _TargetType, _Status, _St) ->
    ok.

notify_fault_detector(Status, St) ->
    ServiceType   = provider_conversion,
    ProviderRef   = get_route_provider(get_route(St)),
    ProviderID    = ProviderRef#domain_ProviderRef.id,
    PaymentID     = get_payment_id(get_payment(St)),
    InvoiceID     = get_invoice_id(get_invoice(get_opts(St))),
    FDConfig      = genlib_app:env(hellgate, fault_detector, #{}),
    Config        = genlib_map:get(conversion, FDConfig, #{}),
    SlidingWindow = genlib_map:get(sliding_window,       Config, 60000),
    OpTimeLimit   = genlib_map:get(operation_time_limit, Config, 1200000),
    PreAggrSize   = genlib_map:get(pre_aggregation_size, Config, 2),
    ServiceConfig = hg_fault_detector_client:build_config(SlidingWindow, OpTimeLimit, PreAggrSize),
    ServiceID     = hg_fault_detector_client:build_service_id(ServiceType, ProviderID),
    OperationID   = hg_fault_detector_client:build_operation_id(ServiceType, [InvoiceID, PaymentID]),
    fd_register(Status, ServiceID, OperationID, ServiceConfig).

fd_register(start, ServiceID, OperationID, ServiceConfig) ->
    _ = fd_maybe_init_service_and_start(ServiceID, OperationID, ServiceConfig);
fd_register(Status, ServiceID, OperationID, ServiceConfig) ->
    _ = hg_fault_detector_client:register_operation(Status, ServiceID, OperationID, ServiceConfig).

fd_maybe_init_service_and_start(ServiceID, OperationID, ServiceConfig) ->
    case hg_fault_detector_client:register_operation(start, ServiceID, OperationID, ServiceConfig) of
        {error, not_found} ->
            _ = hg_fault_detector_client:init_service(ServiceID, ServiceConfig),
            _ = hg_fault_detector_client:register_operation(start, ServiceID, OperationID, ServiceConfig);
        Result ->
            Result
    end.

process_fatal_payment_failure(?cancelled(), _Events, _Action, Failure, _St) ->
    error({invalid_cancel_failure, Failure});
process_fatal_payment_failure(?captured(), _Events, _Action, Failure, _St) ->
    error({invalid_capture_failure, Failure});
process_fatal_payment_failure(?processed(), Events, Action, Failure, _St) ->
    RollbackStarted = [?payment_rollback_started(Failure)],
    {next, {Events ++ RollbackStarted, hg_machine_action:set_timeout(0, Action)}}.

retry_session(Action, Target, Timeout) ->
    NewEvents = start_session(Target),
    NewAction = set_timer({timeout, Timeout}, Action),
    {NewEvents, NewAction}.

get_actual_retry_strategy(Target, #st{retry_attempts = Attempts}) ->
    AttemptNum = maps:get(get_target_type(Target), Attempts, 0),
    hg_retry:skip_steps(get_initial_retry_strategy(get_target_type(Target)), AttemptNum).

-spec get_initial_retry_strategy(target_type()) -> retry_strategy().
get_initial_retry_strategy(TargetType) ->
    PolicyConfig = genlib_app:env(hellgate, payment_retry_policy, #{}),
    hg_retry:new_strategy(maps:get(TargetType, PolicyConfig, no_retry)).

-spec check_retry_possibility(Target, Failure, St) -> {retry, Timeout} | fatal when
    Failure :: dmsl_domain_thrift:'OperationFailure'(),
    Target :: target(),
    St :: st(),
    Timeout :: non_neg_integer().
check_retry_possibility(Target, Failure, St) ->
    case check_failure_type(Target, Failure) of
        transient ->
            RetryStrategy = get_actual_retry_strategy(Target, St),
            case hg_retry:next_step(RetryStrategy) of
                {wait, Timeout, _NewStrategy} ->
                    {retry, Timeout};
                finish ->
                    _ = logger:debug("Retries strategy is exceed"),
                    fatal
            end;
        fatal ->
            _ = logger:debug("Failure ~p is not transient", [Failure]),
            fatal
    end.

-spec check_failure_type(target(), dmsl_domain_thrift:'OperationFailure'()) -> transient | fatal.
check_failure_type(Target, {failure, Failure}) ->
    payproc_errors:match(get_error_class(Target), Failure, fun do_check_failure_type/1);
check_failure_type(_Target, _Other) ->
    fatal.

get_error_class({Target, _}) when
    Target =:= processed;
    Target =:= captured;
    Target =:= cancelled
->
    'PaymentFailure';
get_error_class({refunded, _}) ->
    'RefundFailure';
get_error_class(Target) ->
    error({unsupported_target, Target}).

do_check_failure_type({authorization_failed, {temporarily_unavailable, _}}) ->
    transient;
do_check_failure_type(_Failure) ->
    fatal.

get_action(?processed(), Action, St) ->
    case get_payment_flow(get_payment(St)) of
        ?invoice_payment_flow_instant() ->
            hg_machine_action:set_timeout(0, Action);
        ?invoice_payment_flow_hold(_, HeldUntil) ->
            hg_machine_action:set_deadline(HeldUntil, Action)
    end;
get_action(_Target, Action, _St) ->
    Action.

handle_proxy_result(
    #prxprv_PaymentProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Events0,
    Session
) ->
    Events1 = wrap_session_events(hg_proxy_provider:bind_transaction(Trx, Session), Session),
    Events2 = update_proxy_state(ProxyState, Session),
    {Events3, Action} = handle_proxy_intent(Intent, Action0, Session),
    {lists:flatten([Events0, Events1, Events2, Events3]), Action}.

handle_callback_result(
    #prxprv_PaymentCallbackResult{result = ProxyResult, response = Response},
    Action0,
    Session
) ->
    {Response, handle_proxy_callback_result(ProxyResult, Action0, Session)}.

handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = {_Type, Intent}, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events0 = [wrap_session_event(?session_activated(), Session)],
    Events1 = wrap_session_events(hg_proxy_provider:bind_transaction(Trx, Session), Session),
    Events2 = update_proxy_state(ProxyState, Session),
    {Events3, Action} = handle_proxy_intent(Intent, hg_machine_action:unset_timer(Action0), Session),
    {lists:flatten([Events0, Events1, Events2, Events3]), Action};
handle_proxy_callback_result(
    #prxprv_PaymentCallbackProxyResult{intent = undefined, trx = Trx, next_state = ProxyState},
    Action0,
    Session
) ->
    Events1 = hg_proxy_provider:bind_transaction(Trx, Session),
    Events2 = update_proxy_state(ProxyState, Session),
    {wrap_session_events(Events1 ++ Events2, Session), Action0}.

wrap_session_events(SessionEvents, #{target := Target}) ->
    [?session_ev(Target, Ev) || Ev <- SessionEvents].

wrap_session_event(SessionEvent, #{target := Target}) ->
    ?session_ev(Target, SessionEvent).

update_proxy_state(undefined, _Session) ->
    [];
update_proxy_state(ProxyState, Session) ->
    case get_session_proxy_state(Session) of
        ProxyState ->
            % proxy state did not change, no need to publish an event
            [];
        _WasState ->
            [wrap_session_event(?proxy_st_changed(ProxyState), Session)]
    end.

handle_proxy_intent(#prxprv_FinishIntent{status = {success, Success}}, Action, Session) ->
    Events0 = [wrap_session_event(?session_finished(?session_succeeded()), Session)],
    Events1 = case Success of
        #prxprv_Success{token = undefined} ->
            Events0;
        #prxprv_Success{token = Token} ->
            [?rec_token_acquired(Token) | Events0]
    end,
    {Events1, Action};
handle_proxy_intent(#prxprv_FinishIntent{status = {failure, Failure}}, Action, Session = #{target := {captured, _}}) ->
    handle_proxy_capture_failure(Action, Failure, Session);
handle_proxy_intent(#prxprv_FinishIntent{status = {failure, Failure}}, Action, Session) ->
    Events = [wrap_session_event(?session_finished(?session_failed({failure, Failure})), Session)],
    {Events, Action};
handle_proxy_intent(#prxprv_SleepIntent{timer = Timer, user_interaction = UserInteraction}, Action0, Session) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = wrap_session_events(try_request_interaction(UserInteraction), Session),
    {Events, Action};
handle_proxy_intent(
    #prxprv_SuspendIntent{
        tag = Tag,
        timeout = Timer,
        user_interaction = UserInteraction,
        timeout_behaviour = TimeoutBehaviour
    },
    Action0,
    Session
) ->
    Action = set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended(Tag, TimeoutBehaviour) | try_request_interaction(UserInteraction)],
    {wrap_session_events(Events, Session), Action}.

handle_proxy_capture_failure(Action, Failure, Session = #{target := Target}) ->
    case check_failure_type(Target, {failure, Failure}) of
        transient ->
            Events = [wrap_session_event(?session_finished(?session_failed({failure, Failure})), Session)],
            {Events, Action};
        _ ->
            error({invalid_capture_failure, Failure})
    end.

set_timer(Timer, Action) ->
    hg_machine_action:set_timer(Timer, Action).

try_request_interaction(undefined) ->
    [];
try_request_interaction(UserInteraction) ->
    [?interaction_requested(UserInteraction)].

commit_payment_cashflow(St) ->
    hg_accounting:commit(construct_payment_plan_id(St), get_cashflow_plan(St)).

rollback_payment_cashflow(St) ->
    hg_accounting:rollback(construct_payment_plan_id(St), get_cashflow_plan(St)).

get_cashflow_plan(St = #st{partial_cash_flow = PartialCashFlow})
    when PartialCashFlow =/= undefined
->
    [
        {1, get_cashflow(St)},
        {2, hg_cashflow:revert(get_cashflow(St))},
        {3, PartialCashFlow}
    ];
get_cashflow_plan(St) ->
    [{1, get_cashflow(St)}].

-spec set_repair_scenario(hg_invoice_repair:scenario(), st()) -> st().

set_repair_scenario(Scenario, St) ->
    St#st{repair_scenario = Scenario}.

%%

-type payment_info() :: dmsl_proxy_provider_thrift:'PaymentInfo'().

-spec construct_payment_info(st(), opts()) ->
    payment_info().

construct_payment_info(St, Opts) ->
    construct_payment_info(
        get_activity(St),
        get_target(St),
        St,
        #prxprv_PaymentInfo{
            shop = construct_proxy_shop(get_shop(Opts)),
            invoice = construct_proxy_invoice(get_invoice(Opts)),
            payment = construct_proxy_payment(get_payment(St), get_trx(St))
        }
    ).

construct_proxy_context(St) ->
    #prxprv_PaymentContext{
        session      = construct_session(get_activity_session(St)),
        payment_info = construct_payment_info(St, get_opts(St)),
        options      = collect_proxy_options(St)
    }.

construct_session(Session = #{target := Target}) ->
    #prxprv_Session{
        target = Target,
        state = get_session_proxy_state(Session)
    }.

construct_payment_info(idle, _Target, _St, PaymentInfo) ->
    PaymentInfo;
construct_payment_info(
    {payment, _Step},
    ?captured(Reason, Cost),
    St,
    PaymentInfo
) when Cost =:= undefined ->
    %% Для обратной совместимости и legacy capture
    PaymentInfo#prxprv_PaymentInfo{
        capture = construct_proxy_capture(?captured(
            Reason,
            get_payment_cost(get_payment(St))
        ))
    };
construct_payment_info(
    {payment, _Step},
    Target = ?captured(),
    _St,
    PaymentInfo
) ->
    PaymentInfo#prxprv_PaymentInfo{
        capture = construct_proxy_capture(Target)
    };
construct_payment_info({payment, _Step}, _Target, _St, PaymentInfo) ->
    PaymentInfo;
construct_payment_info({refund_session, ID}, _Target, St, PaymentInfo) ->
    PaymentInfo#prxprv_PaymentInfo{
        refund = construct_proxy_refund(try_get_refund_state(ID, St))
    }.

construct_proxy_payment(
    #domain_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost,
        make_recurrent = MakeRecurrent,
        processing_deadline = Deadline
    },
    Trx
) ->
    ContactInfo = get_contact_info(Payer),
    #prxprv_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        trx = Trx,
        payment_resource = construct_payment_resource(Payer),
        cost = construct_proxy_cash(Cost),
        contact_info = ContactInfo,
        make_recurrent = MakeRecurrent,
        processing_deadline = Deadline
    }.

construct_payment_resource(?payment_resource_payer(Resource, _)) ->
    {disposable_payment_resource, Resource};
construct_payment_resource(?recurrent_payer(PaymentTool, ?recurrent_parent(InvoiceID, PaymentID), _)) ->
    PreviousPayment = get_payment_state(InvoiceID, PaymentID),
    RecToken = get_recurrent_token(PreviousPayment),
    {recurrent_payment_resource, #prxprv_RecurrentPaymentResource{
        payment_tool = PaymentTool,
        rec_token = RecToken
    }};
construct_payment_resource(?customer_payer(_, _, RecPaymentToolID, _, _) = Payer) ->
    case get_rec_payment_tool(RecPaymentToolID) of
        {ok, #payproc_RecurrentPaymentTool{
            payment_resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool
            },
            rec_token = RecToken
        }} when RecToken =/= undefined ->
            {recurrent_payment_resource, #prxprv_RecurrentPaymentResource{
                payment_tool = PaymentTool,
                rec_token = RecToken
            }};
        _ ->
            % TODO more elegant error
            error({'Can\'t get rec_token for customer payer', Payer})
    end.

get_contact_info(?payment_resource_payer(_, ContactInfo)) ->
    ContactInfo;
get_contact_info(?recurrent_payer(_, _, ContactInfo)) ->
    ContactInfo;
get_contact_info(?customer_payer(_, _, _, _, ContactInfo)) ->
    ContactInfo.

construct_proxy_invoice(
    #domain_Invoice{
        id = InvoiceID,
        created_at = CreatedAt,
        due = Due,
        details = Details,
        cost = Cost
    }
) ->
    #prxprv_Invoice{
        id = InvoiceID,
        created_at =  CreatedAt,
        due =  Due,
        details = Details,
        cost = construct_proxy_cash(Cost)
    }.

construct_proxy_shop(
    #domain_Shop{
        id = ShopID,
        details = ShopDetails,
        location = Location,
        category = ShopCategoryRef
    }
) ->
    ShopCategory = hg_domain:get(
        hg_domain:head(),
        {category, ShopCategoryRef}
    ),
    #prxprv_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails,
        location = Location
    }.

construct_proxy_cash(#domain_Cash{
    amount = Amount,
    currency = CurrencyRef
}) ->
    Revision = hg_domain:head(),
    #prxprv_Cash{
        amount = Amount,
        currency = hg_domain:get(Revision, {currency, CurrencyRef})
    }.

construct_proxy_refund(#refund_st{refund  = Refund} = St) ->
    #prxprv_InvoicePaymentRefund{
        id         = get_refund_id(Refund),
        created_at = get_refund_created_at(Refund),
        trx        = get_session_trx(get_refund_session(St)),
        cash       = construct_proxy_cash(get_refund_cash(Refund))
    }.

construct_proxy_capture(?captured(_, Cost)) ->
    #prxprv_InvoicePaymentCapture{
        cost = construct_proxy_cash(Cost)
    }.

collect_proxy_options(
    #st{
        route = #domain_PaymentRoute{provider = ProviderRef, terminal = TerminalRef}
    }
) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, ProviderRef}),
    Terminal = hg_domain:get(Revision, {terminal, TerminalRef}),
    Proxy    = Provider#domain_Provider.proxy,
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    lists:foldl(
        fun
            (undefined, M) ->
                M;
            (M1, M) ->
                maps:merge(M1, M)
        end,
        #{},
        [
            Terminal#domain_Terminal.options,
            Proxy#domain_Proxy.additional,
            ProxyDef#domain_ProxyDefinition.options
        ]
    ).

%%

get_party(#{party := Party}) ->
    Party.

get_shop(#{party := Party, invoice := Invoice}) ->
    hg_party:get_shop(get_invoice_shop_id(Invoice), Party).

get_contract(#{party := Party, invoice := Invoice}) ->
    Shop = hg_party:get_shop(get_invoice_shop_id(Invoice), Party),
    hg_party:get_contract(Shop#domain_Shop.contract_id, Party).

get_payment_institution(Opts, Revision) ->
    Contract = get_contract(Opts),
    PaymentInstitutionRef = Contract#domain_Contract.payment_institution,
    hg_domain:get(Revision, {payment_institution, PaymentInstitutionRef}).

get_opts_party_revision(#{party := Party}) ->
    Party#domain_Party.revision.

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_invoice_cost(#domain_Invoice{cost = Cost}) ->
    Cost.

get_invoice_shop_id(#domain_Invoice{shop_id = ShopID}) ->
    ShopID.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

get_payment_cost(#domain_InvoicePayment{cost = Cost}) ->
    Cost.

get_payment_flow(#domain_InvoicePayment{flow = Flow}) ->
    Flow.

get_payment_shop_id(#domain_InvoicePayment{shop_id = ShopID}) ->
    ShopID.

get_payment_tool(#domain_InvoicePayment{payer = Payer}) ->
    get_payer_payment_tool(Payer).

get_payment_created_at(#domain_InvoicePayment{created_at = CreatedAt}) ->
    CreatedAt.

get_payer_payment_tool(?payment_resource_payer(PaymentResource, _ContactInfo)) ->
    get_resource_payment_tool(PaymentResource);
get_payer_payment_tool(?customer_payer(_CustomerID, _, _, PaymentTool, _)) ->
    PaymentTool;
get_payer_payment_tool(?recurrent_payer(PaymentTool, _, _)) ->
    PaymentTool.

get_resource_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.
%%

-spec throw_invalid_request(binary()) -> no_return().

throw_invalid_request(Why) ->
    throw(#'InvalidRequest'{errors = [Why]}).


-spec throw_invalid_recurrent_parent(binary()) -> no_return().

throw_invalid_recurrent_parent(Details) ->
    throw(#payproc_InvalidRecurrentParentPayment{details = Details}).
%%

-type change_opts() :: #{
    timestamp  => hg_datetime:timestamp(),
    validation => strict
}.

-spec merge_change(change(), st() | undefined, change_opts()) -> st().

merge_change(Change, undefined, Opts) ->
    merge_change(Change, #st{activity = {payment, new}}, Opts);
merge_change(Change = ?payment_started(Payment), #st{} = St, Opts) ->
    _ = validate_transition({payment, new}, Change, St, Opts),
    St#st{
        target     = ?processed(),
        payment    = Payment,
        activity   = {payment, risk_scoring},
        timings    = hg_timings:mark(started, define_event_timestamp(Opts))
    };
merge_change(Change = ?risk_score_changed(RiskScore), #st{} = St, Opts) ->
    _ = validate_transition({payment, risk_scoring}, Change, St, Opts),
    St#st{
        risk_score = RiskScore,
        activity   = {payment, routing}
    };
merge_change(Change = ?route_changed(Route), St, Opts) ->
    _ = validate_transition({payment, routing}, Change, St, Opts),
    St#st{
        route      = Route,
        activity   = {payment, cash_flow_building}
    };
merge_change(Change = ?payment_capture_started(Params), #st{} = St, Opts) ->
    _ = validate_transition([{payment, S} || S <- [flow_waiting]], Change, St, Opts),
    St#st{
        capture_params = Params,
        activity = {payment, processing_capture}
    };
merge_change(Change = ?cash_flow_changed(Cashflow), #st{activity = Activity} = St0, Opts) ->
    _ = validate_transition([{payment, S} || S <- [
        cash_flow_building,
        processing_capture
    ]], Change, St0, Opts),
    St = St0#st{final_cash_flow = Cashflow},
    case Activity of
        {payment, cash_flow_building} ->
            St#st{
                cash_flow  = Cashflow,
                activity   = {payment, processing_session}
            };
        {payment, processing_capture} ->
            St#st{
                partial_cash_flow = Cashflow,
                activity   = {payment, updating_accounter}
            };
        _ ->
            St
    end;
merge_change(Change = ?rec_token_acquired(Token), #st{} = St, Opts) ->
    _ = validate_transition([{payment, processing_session}, {payment, finalizing_session}], Change, St, Opts),
    St#st{recurrent_token = Token};
merge_change(Change = ?payment_rollback_started(Failure), St, Opts) ->
    _ = validate_transition([{payment, processing_session}], Change, St, Opts),
    St#st{
        failure    = Failure,
        activity   = {payment, processing_failure},
        timings    = accrue_status_timing(failed, Opts, St)
    };
merge_change(Change = ?payment_status_changed({failed, _} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition([{payment, S} || S <- [
        risk_scoring,
        routing,
        processing_failure
    ]], Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = idle,
        failure    = undefined,
        timings    = accrue_status_timing(failed, Opts, St)
    };
merge_change(Change = ?payment_status_changed({cancelled, _} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition({payment, finalizing_accounter}, Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = idle,
        timings    = accrue_status_timing(cancelled, Opts, St)
    };
merge_change(Change = ?payment_status_changed({captured, Captured} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition({payment, finalizing_accounter}, Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{
            status = Status,
            cost   = get_captured_cost(Captured, Payment)
        },
        activity   = idle,
        timings    = accrue_status_timing(captured, Opts, St)
    };
merge_change(Change = ?payment_status_changed({processed, _} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition({payment, processing_accounter}, Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = {payment, flow_waiting},
        timings    = accrue_status_timing(processed, Opts, St)
    };
merge_change(Change = ?payment_status_changed({refunded, _} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition(idle, Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status}
    };
merge_change(Change = ?payment_status_changed({charged_back, _} = Status), #st{payment = Payment} = St, Opts) ->
    _ = validate_transition(idle, Change, St, Opts),
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status}
    };

merge_change(Change = ?chargeback_ev(ID, Event), St, Opts) ->
    St1 = case Event of
        ?chargeback_created(_) ->
            _ = validate_transition(idle, Change, St, Opts),
            St#st{activity = {chargeback, ID, preparing_initial_cash_flow}};
        ?chargeback_stage_changed(_) ->
            _ = validate_transition(idle, Change, St, Opts),
            St;
        ?chargeback_levy_changed(_) ->
            _ = validate_transition([idle, {chargeback, ID, updating_chargeback}], Change, St, Opts),
            St#st{activity = {chargeback, ID, updating_chargeback}};
        ?chargeback_body_changed(_) ->
            _ = validate_transition([idle, {chargeback, ID, updating_chargeback}], Change, St, Opts),
            St#st{activity = {chargeback, ID, updating_chargeback}};
        ?chargeback_cash_flow_changed(_) ->
            Valid = [{chargeback, ID, Activity} || Activity <- [preparing_initial_cash_flow, updating_cash_flow]],
            _ = validate_transition(Valid, Change, St, Opts),
            case St of
                #st{activity = {chargeback, ID, preparing_initial_cash_flow}} ->
                    St#st{activity = idle};
                #st{activity = {chargeback, ID, updating_cash_flow}} ->
                    St#st{activity = {chargeback, ID, finalising_accounter}}
            end;
        ?chargeback_target_status_changed(?chargeback_status_accepted()) ->
            _ = validate_transition([idle, {chargeback, ID, updating_chargeback}], Change, St, Opts),
            case St of
                #st{activity = idle} ->
                    St#st{activity = {chargeback, ID, finalising_accounter}};
                #st{activity = {chargeback, ID, updating_chargeback}} ->
                    St#st{activity = {chargeback, ID, updating_cash_flow}}
            end;
        ?chargeback_target_status_changed(_) ->
            _ = validate_transition([idle, {chargeback, ID, updating_chargeback}], Change, St, Opts),
            St#st{activity = {chargeback, ID, updating_cash_flow}};
        ?chargeback_status_changed(_) ->
            _ = validate_transition([idle, {chargeback, ID, finalising_accounter}], Change, St, Opts),
            St#st{activity = idle}
    end,
    ChargebackSt = merge_chargeback_change(Event, try_get_chargeback_state(ID, St1)),
    set_chargeback_state(ID, ChargebackSt, St1);

merge_change(Change = ?refund_ev(ID, Event), St, Opts) ->
    St1 = case Event of
        ?refund_created(_, _, _) ->
            _ = validate_transition(idle, Change, St, Opts),
            St#st{activity = {refund_new, ID}};
        ?session_ev(?refunded(), ?session_started()) ->
            _ = validate_transition([{refund_new, ID}, {refund_session, ID}], Change, St, Opts),
            St#st{activity = {refund_session, ID}};
        ?session_ev(?refunded(), ?session_finished(?session_succeeded())) ->
            _ = validate_transition({refund_session, ID}, Change, St, Opts),
            St#st{activity = {refund_accounter, ID}};
        ?refund_status_changed(?refund_succeeded()) ->
            _ = validate_transition([{refund_accounter, ID}], Change, St, Opts),
            St;
        ?refund_rollback_started(_) ->
            _ = validate_transition([{refund_session, ID}, {refund_new, ID}], Change, St, Opts),
            St#st{activity = {refund_failure, ID}};
        ?refund_status_changed(?refund_failed(_)) ->
            _ = validate_transition([{refund_failure, ID}], Change, St, Opts),
            St;
        _ ->
            _ = validate_transition([{refund_session, ID}], Change, St, Opts),
            St
    end,
    RefundSt = merge_refund_change(Event, try_get_refund_state(ID, St1)),
    St2 = set_refund_state(ID, RefundSt, St1),
    case get_refund_status(get_refund(RefundSt)) of
        {S, _} when S == succeeded; S == failed ->
            St2#st{activity = idle};
        _ ->
            St2
    end;
merge_change(Change = ?adjustment_ev(ID, Event), St, Opts) ->
    St1 = case Event of
        ?adjustment_created(_) ->
            _ = validate_transition(idle, Change, St, Opts),
            St#st{activity = {adjustment_new, ID}};
        ?adjustment_status_changed(?adjustment_processed()) ->
            _ = validate_transition({adjustment_new, ID}, Change, St, Opts),
            St#st{activity = {adjustment_pending, ID}};
        ?adjustment_status_changed(_) ->
            _ = validate_transition({adjustment_pending, ID}, Change, St, Opts),
            St#st{activity = idle}
    end,
    Adjustment = merge_adjustment_change(Event, try_get_adjustment(ID, St1)),
    St2 = set_adjustment(ID, Adjustment, St1),
    % TODO new cashflow imposed implicitly on the payment state? rough
    case get_adjustment_status(Adjustment) of
        ?adjustment_captured(_) ->
            apply_adjustment_effects(Adjustment, St2);
        _ ->
            St2
    end;

merge_change(
    Change = ?session_ev(Target, ?session_started()),
    #st{activity = Activity} = St,
    Opts
) ->
    _ = validate_transition([{payment, S} || S <- [
        processing_session,
        flow_waiting,
        processing_capture,
        updating_accounter,
        finalizing_session
    ]], Change, St, Opts),
    % FIXME why the hell dedicated handling
    Session = mark_session_timing_event(started, Opts, create_session(Target, get_trx(St))),
    St1 = add_session(Target, Session, St#st{target = Target}),
    St2 = save_retry_attempt(Target, St1),
    case Activity of
        {payment, processing_session} ->
            %% session retrying
            St2#st{activity = {payment, processing_session}};
        {payment, PaymentActivity} when
            PaymentActivity == flow_waiting;
            PaymentActivity == processing_capture
        ->
            %% session flow
            St2#st{
                activity = {payment, finalizing_session},
                timings  = try_accrue_waiting_timing(Opts, St2)
            };
        {payment, updating_accounter} ->
            %% session flow
            St2#st{activity = {payment, finalizing_session}};
        {payment, finalizing_session} ->
            %% session retrying
            St2#st{activity = {payment, finalizing_session}};
        _ ->
            St2
    end;

merge_change(Change = ?session_ev(Target, Event), St = #st{activity = Activity}, Opts) ->
    _ = validate_transition([{payment, S} || S <- [processing_session, finalizing_session]], Change, St, Opts),
    Session = merge_session_change(Event, get_session(Target, St), Opts),
    St1 = update_session(Target, Session, St),
    % FIXME leaky transactions
    St2 = set_trx(get_session_trx(Session), St1),
    case Session of
        #{status := finished, result := ?session_succeeded()} ->
            NextActivity = case Activity of
                {payment, processing_session} ->
                    {payment, processing_accounter};
                {payment, finalizing_session} ->
                    {payment, finalizing_accounter};
                _ ->
                    Activity
            end,
            St2#st{activity = NextActivity};
        _ ->
            St2
    end.

save_retry_attempt(Target, #st{retry_attempts = Attempts} = St) ->
    St#st{retry_attempts = maps:update_with(get_target_type(Target), fun(N) -> N + 1 end, 0, Attempts)}.

merge_chargeback_change(Change, ChargebackState) ->
    hg_invoice_payment_chargeback:merge_change(Change, ChargebackState).

merge_refund_change(?refund_created(Refund, Cashflow, TransactionInfo), undefined) ->
    #refund_st{refund = Refund, cash_flow = Cashflow, transaction_info = TransactionInfo};
merge_refund_change(?refund_status_changed(Status), RefundSt) ->
    set_refund(set_refund_status(Status, get_refund(RefundSt)), RefundSt);
merge_refund_change(?refund_rollback_started(Failure), RefundSt) ->
    RefundSt#refund_st{failure = Failure};
merge_refund_change(?session_ev(?refunded(), ?session_started()), St) ->
    add_refund_session(create_session(?refunded(), undefined), St);
merge_refund_change(?session_ev(?refunded(), Change), St) ->
    update_refund_session(merge_session_change(Change, get_refund_session(St), #{}), St).

merge_adjustment_change(?adjustment_created(Adjustment), undefined) ->
    Adjustment;
merge_adjustment_change(?adjustment_status_changed(Status), Adjustment) ->
    Adjustment#domain_InvoicePaymentAdjustment{status = Status}.

apply_adjustment_effects(Adjustment, St) ->
    apply_adjustment_effect(status, Adjustment,
        apply_adjustment_effect(cashflow, Adjustment, St)).

apply_adjustment_effect(status, ?adjustment_target_status(Status), St = #st{payment = Payment}) ->
    case Status of
        {captured, Capture} ->
            St#st{payment = Payment#domain_InvoicePayment{
                status = Status,
                cost = get_captured_cost(Capture, Payment)
            }};
        _ ->
            St#st{payment = Payment#domain_InvoicePayment{
                status = Status
            }}
    end;
apply_adjustment_effect(status, #domain_InvoicePaymentAdjustment{}, St) ->
    St;
apply_adjustment_effect(cashflow, Adjustment, St) ->
    set_cashflow(get_adjustment_cashflow(Adjustment), St).

validate_transition(Allowed, Change, St, Opts) ->
    Valid = is_transition_valid(Allowed, St),
    case Opts of
        #{} when Valid ->
            ok;
        #{validation := strict} when not Valid ->
            erlang:error({invalid_transition, Change, St, Allowed});
        #{} when not Valid ->
            logger:warning(
                "Invalid transition for change ~p in state ~p, allowed ~p",
                [Change, St, Allowed]
            )
    end.

is_transition_valid(Allowed, St) when is_list(Allowed) ->
    lists:any(fun (A) -> is_transition_valid(A, St) end, Allowed);
is_transition_valid(Allowed, #st{activity = Activity}) ->
    Activity =:= Allowed.

accrue_status_timing(Name, Opts, #st{timings = Timings}) ->
    EventTime = define_event_timestamp(Opts),
    hg_timings:mark(Name, EventTime, hg_timings:accrue(Name, started, EventTime, Timings)).

try_accrue_waiting_timing(Opts, #st{payment = Payment, timings = Timings}) ->
    case get_payment_flow(Payment) of
        ?invoice_payment_flow_instant() ->
            Timings;
        ?invoice_payment_flow_hold(_, _) ->
            hg_timings:accrue(waiting, processed, define_event_timestamp(Opts), Timings)
    end.

-spec get_cashflow(st()) -> cash_flow().

get_cashflow(#st{cash_flow = FinalCashflow}) ->
    FinalCashflow.

set_cashflow(Cashflow, St = #st{}) ->
    St#st{
        cash_flow = Cashflow,
        final_cash_flow = Cashflow
    }.

-spec get_final_cashflow(st()) -> cash_flow().

get_final_cashflow(#st{final_cash_flow = Cashflow}) ->
    Cashflow.

get_trx(#st{trx = Trx}) ->
    Trx.

set_trx(Trx, St = #st{}) ->
    St#st{trx = Trx}.

try_get_refund_state(ID, #st{refunds = Rs}) ->
    case Rs of
        #{ID := RefundSt} ->
            RefundSt;
        #{} ->
            undefined
    end.

set_chargeback_state(ID, ChargebackSt, St = #st{chargebacks = CBs}) ->
    St#st{chargebacks = CBs#{ID => ChargebackSt}}.

try_get_chargeback_state(ID, #st{chargebacks = CBs}) ->
    case CBs of
        #{ID := ChargebackSt} ->
            ChargebackSt;
        #{} ->
            undefined
    end.

set_refund_state(ID, RefundSt, St = #st{refunds = Rs}) ->
    St#st{refunds = Rs#{ID => RefundSt}}.

get_captured_cost(#domain_InvoicePaymentCaptured{cost = Cost}, _) when
    Cost /= undefined
->
    Cost;
get_captured_cost(_, #domain_InvoicePayment{cost = Cost}) ->
    Cost.

get_refund_session(#refund_st{sessions = []}) ->
    undefined;
get_refund_session(#refund_st{sessions = [Session | _]}) ->
    Session.

add_refund_session(Session, St = #refund_st{sessions = OldSessions}) ->
    St#refund_st{sessions = [Session | OldSessions]}.

update_refund_session(Session, St = #refund_st{sessions = []}) ->
    St#refund_st{sessions = [Session]};
update_refund_session(Session, St = #refund_st{sessions = OldSessions}) ->
    %% Replace recent session with updated one
    St#refund_st{sessions = [Session | tl(OldSessions)]}.

get_refund(#refund_st{refund = Refund}) ->
    Refund.

set_refund(Refund, RefundSt = #refund_st{}) ->
    RefundSt#refund_st{refund = Refund}.

get_refund_id(#domain_InvoicePaymentRefund{id = ID}) ->
    ID.

get_refund_status(#domain_InvoicePaymentRefund{status = Status}) ->
    Status.

set_refund_status(Status, Refund = #domain_InvoicePaymentRefund{}) ->
    Refund#domain_InvoicePaymentRefund{status = Status}.

get_refund_cashflow(#refund_st{cash_flow = CashFlow}) ->
    CashFlow.

define_refund_cash(undefined, St) ->
    PaymentBallance = get_remaining_payment_balance(St),
    PaymentBallance;
define_refund_cash(?cash(_, SymCode) = Cash, #st{payment = #domain_InvoicePayment{cost = ?cash(_, SymCode)}}) ->
    Cash;
define_refund_cash(?cash(_, SymCode), _St) ->
    throw(#payproc_InconsistentRefundCurrency{currency = SymCode}).

get_refund_cash(#domain_InvoicePaymentRefund{cash = Cash}) ->
    Cash.

get_refund_created_at(#domain_InvoicePaymentRefund{created_at = CreatedAt}) ->
    CreatedAt.

enrich_refund_with_cash(Refund, #domain_InvoicePayment{cost = PaymentCash}) ->
    #domain_InvoicePaymentRefund{cash = RefundCash} = Refund,
    case {RefundCash, PaymentCash} of
        {undefined, _} ->
            %% Earlier Refunds haven't got field cash and we got this value from PaymentCash.
            %% There are some refunds without cash in system that's why for compatablity we save this behaviour.
            Refund#domain_InvoicePaymentRefund{cash = PaymentCash};
        {?cash(_, SymCode), ?cash(_, SymCode)} ->
            Refund
    end.

try_get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoicePaymentAdjustment.id, As) of
        V = #domain_InvoicePaymentAdjustment{} ->
            V;
        false ->
            undefined
    end.

set_adjustment(ID, Adjustment, St = #st{adjustments = As}) ->
    St#st{adjustments = lists:keystore(ID, #domain_InvoicePaymentAdjustment.id, As, Adjustment)}.

merge_session_change(?session_finished(Result), Session, Opts) ->
    Session2 = Session#{status := finished, result => Result},
    accrue_session_timing(finished, started, Opts, Session2);
merge_session_change(?session_activated(), Session, Opts) ->
    Session2 = Session#{status := active},
    accrue_session_timing(suspended, suspended, Opts, Session2);

merge_session_change(?session_suspended(Tag, TimeoutBehaviour), Session, Opts) ->
    Session2 = set_session_tag(Tag, Session),
    Session3 = set_timeout_behaviour(TimeoutBehaviour, Session2),
    Session4 = mark_session_timing_event(suspended, Opts, Session3),
    Session4#{status := suspended};

merge_session_change(?trx_bound(Trx), Session, _Opts) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session, _Opts) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session, _Opts) ->
    Session.

set_timeout_behaviour(undefined, Session) ->
    Session#{timeout_behaviour => {operation_failure, ?operation_timeout()}};
set_timeout_behaviour(TimeoutBehaviour, Session) ->
    Session#{timeout_behaviour => TimeoutBehaviour}.

set_session_tag(undefined, Session) ->
    Session;
set_session_tag(Tag, Session) ->
    Session#{tags := [Tag | get_session_tags(Session)]}.

set_session_timings(Timings, Session) ->
    Session#{timings => Timings}.
get_session_timings(Session) ->
    maps:get(timings, Session, hg_timings:new()).

accrue_session_timing(Name, Event, Opts, Session) ->
    Timings = get_session_timings(Session),
    set_session_timings(hg_timings:accrue(Name, Event, define_event_timestamp(Opts), Timings), Session).

mark_session_timing_event(Event, Opts, Session) ->
    Timings = get_session_timings(Session),
    set_session_timings(hg_timings:mark(Event, define_event_timestamp(Opts), Timings), Session).

create_session(Target, Trx) ->
    #{
        target => Target,
        status => active,
        trx    => Trx,
        tags   => [],
        timeout_behaviour => {operation_failure, ?operation_timeout()}
    }.

get_invoice_state(InvoiceID) ->
    case hg_invoice:get(InvoiceID) of
        {ok, Invoice} ->
            Invoice;
        {error, notfound} ->
            throw(#payproc_InvoiceNotFound{})
    end.

-spec get_payment_state(invoice_id(), payment_id()) -> st() | no_return().
get_payment_state(InvoiceID, PaymentID) ->
    Invoice = get_invoice_state(InvoiceID),
    case hg_invoice:get_payment(PaymentID, Invoice) of
        {ok, Payment} ->
            Payment;
        {error, notfound} ->
            throw(#payproc_InvoicePaymentNotFound{})
    end.

get_session(Target, #st{sessions = Sessions}) ->
    case maps:get(get_target_type(Target), Sessions, []) of
        [] ->
            undefined;
        [Session | _] ->
            Session
    end.

add_session(Target, Session, St = #st{sessions = Sessions}) ->
    TargetType = get_target_type(Target),
    TargetTypeSessions = maps:get(TargetType, Sessions, []),
    St#st{sessions = Sessions#{TargetType => [Session | TargetTypeSessions]}}.

update_session(Target, Session, St = #st{sessions = Sessions}) ->
    TargetType = get_target_type(Target),
    [_ | Rest] = maps:get(TargetType, Sessions, []),
    St#st{sessions = Sessions#{TargetType => [Session | Rest]}}.

get_session_status(#{status := Status}) ->
    Status.

get_session_trx(#{trx := Trx}) ->
    Trx.

get_session_timeout_behaviour(#{timeout_behaviour := TimeoutBehaviour}) ->
    TimeoutBehaviour.

get_session_proxy_state(Session) ->
    maps:get(proxy_state, Session, undefined).

get_session_tags(#{tags := Tags}) ->
    Tags.

get_target(#st{target = Target}) ->
    Target.

get_target_type({Type, _}) when
    Type == 'processed';
    Type == 'captured';
    Type == 'cancelled';
    Type == 'refunded'
->
    Type.

get_recurrent_token(#st{recurrent_token = Token}) ->
    Token.

get_payment_revision(#st{payment = #domain_InvoicePayment{domain_revision = Revision}}) ->
    Revision.

get_payment_payer(#st{payment = #domain_InvoicePayment{payer = Payer}}) ->
    Payer.

%%

get_activity_session(St) ->
    get_activity_session(get_activity(St), St).

-spec get_activity_session(activity(), st()) -> session() | undefined.

get_activity_session({payment, _Step}, St) ->
    get_session(get_target(St), St);
get_activity_session({refund_session, ID}, St) ->
    RefundSt = try_get_refund_state(ID, St),
    get_refund_session(RefundSt).

%%

-spec collapse_changes([change()], st() | undefined) -> st() | undefined.

collapse_changes(Changes, St) ->
    collapse_changes(Changes, St, #{}).

collapse_changes(Changes, St, Opts) ->
    lists:foldl(fun (C, St1) -> merge_change(C, St1, Opts) end, St, Changes).

%%

get_rec_payment_tool(RecPaymentToolID) ->
    hg_woody_wrapper:call(recurrent_paytool, 'Get', [RecPaymentToolID]).

get_customer(CustomerID) ->
    case issue_customer_call('Get', [CustomerID, #payproc_EventRange{}]) of
        {ok, Customer} ->
            Customer;
        {exception, #payproc_CustomerNotFound{}} ->
            throw_invalid_request(<<"Customer not found">>);
        {exception, #payproc_InvalidUser{}} ->
            throw_invalid_request(<<"Invalid customer">>);
        {exception, Error} ->
            error({<<"Can't get customer">>, Error})
    end.

get_route_provider_ref(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

get_route_provider(#domain_PaymentRoute{provider = ProviderRef}) ->
    ProviderRef.

get_route_provider(Route, Revision) ->
    hg_domain:get(Revision, {provider, get_route_provider_ref(Route)}).

inspect(Payment = #domain_InvoicePayment{domain_revision = Revision}, PaymentInstitution, VS, Opts) ->
    InspectorSelector = PaymentInstitution#domain_PaymentInstitution.inspector,
    InspectorRef = reduce_selector(inspector, InspectorSelector, VS, Revision),
    Inspector = hg_domain:get(Revision, {inspector, InspectorRef}),
    RiskScore = hg_inspector:inspect(get_shop(Opts), get_invoice(Opts), Payment, Inspector),
    % FIXME: move this logic to inspector
    check_payment_type_risk(RiskScore, Payment).

repair_inspect(Payment, PaymentInstitution, VS, Opts, #st{repair_scenario = Scenario}) ->
    case hg_invoice_repair:check_for_action(skip_inspector, Scenario) of
        {result, Result} ->
            Result;
        call ->
            inspect(Payment, PaymentInstitution, VS, Opts)
    end.

check_payment_type_risk(low, #domain_InvoicePayment{make_recurrent = true}) ->
    high;
check_payment_type_risk(Score, _Payment) ->
    Score.

get_st_meta(#st{payment = #domain_InvoicePayment{id = ID}}) ->
    #{
        id => ID
    };

get_st_meta(_) ->
    #{}.

issue_customer_call(Func, Args) ->
    hg_woody_wrapper:call(customer_management, Func, Args).

%% Timings

-spec define_event_timestamp(change_opts()) -> integer().

define_event_timestamp(#{timestamp := Dt}) ->
    hg_datetime:parse(Dt, millisecond);
define_event_timestamp(#{}) ->
    erlang:system_time(millisecond).

%% Business metrics logging

-spec get_log_params(change(), st()) ->
    {ok, #{type := invoice_payment_event, params := list(), message := string()}} | undefined.

get_log_params(?payment_started(Payment), _) ->
    Params = #{
        payment => Payment,
        event_type => invoice_payment_started
    },
    make_log_params(Params);
get_log_params(?risk_score_changed(RiskScore), _) ->
    Params = #{
        risk_score => RiskScore,
        event_type => invoice_payment_risk_score_changed
    },
    make_log_params(Params);
get_log_params(?route_changed(Route), _) ->
    Params = #{
        route => Route,
        event_type => invoice_payment_route_changed
    },
    make_log_params(Params);
get_log_params(?cash_flow_changed(Cashflow), _) ->
    Params = #{
        cashflow => Cashflow,
        event_type => invoice_payment_cash_flow_changed
    },
    make_log_params(Params);
get_log_params(?payment_started(Payment, RiskScore, Route, Cashflow), _) ->
    Params = #{
        payment => Payment,
        cashflow => Cashflow,
        risk_score => RiskScore,
        route => Route,
        event_type => invoice_payment_started
    },
    make_log_params(Params);
get_log_params(?payment_status_changed(Status), State) ->
    make_log_params(
        #{
            status     => Status,
            payment    => get_payment(State),
            cashflow   => get_final_cashflow(State),
            timings    => State,
            event_type => invoice_payment_status_changed
        }
    );
get_log_params(_, _) ->
    undefined.

make_log_params(Params) ->
    LogParams = maps:fold(
        fun(K, V, Acc) ->
            make_log_params(K, V) ++ Acc
        end,
        [],
        Params
    ),
    Message = get_message(maps:get(event_type, Params)),
    {ok, #{
        type => invoice_payment_event,
        params => LogParams,
        message => Message
    }}.

make_log_params(
    payment,
    #domain_InvoicePayment{
        id = ID,
        cost = Cost,
        flow = Flow
    }
) ->
    [{id, ID}, {cost, make_log_params(cash, Cost)}, {flow, make_log_params(flow, Flow)}];
make_log_params(cash, ?cash(Amount, SymCode)) ->
    [{amount, Amount}, {currency, SymCode}];
make_log_params(flow, ?invoice_payment_flow_instant()) ->
    [{type, instant}];
make_log_params(flow, ?invoice_payment_flow_hold(OnHoldExpiration, _)) ->
    [{type, hold}, {on_hold_expiration, OnHoldExpiration}];
make_log_params(cashflow, undefined) ->
    [];
make_log_params(cashflow, CashFlow) ->
    Remainders = maps:to_list(hg_cashflow:get_partial_remainders(CashFlow)),
    Accounts = lists:map(
        fun ({Account, ?cash(Amount, SymCode)}) ->
            Remainder = [{remainder, [{amount, Amount}, {currency, SymCode}]}],
            {get_account_key(Account), Remainder}
        end,
        Remainders
    ),
    [{accounts, Accounts}];
make_log_params(timings, #st{timings = Timings, sessions = Sessions}) ->
    Params1 = maps:fold(
        fun (N, T, Acc) -> [{hg_utils:join(<<"payment">>, $., N), T} | Acc] end,
        [],
        hg_timings:to_map(Timings)
    ),
    Params2 = maps:fold(
        fun (Target, Ss, Acc) ->
            TargetTimings = hg_timings:merge([get_session_timings(S) || S <- Ss]),
            maps:fold(
                fun (N, T, Acc1) -> [{hg_utils:join($., [<<"session">>, Target, N]), T} | Acc1] end,
                Acc,
                hg_timings:to_map(TargetTimings)
            )
        end,
        Params1,
        Sessions
    ),
    [{timings, Params2}];
make_log_params(risk_score, Score) ->
    [{risk_score, Score}];
make_log_params(route, _Route) ->
    [];
make_log_params(status, {StatusTag, StatusDetails}) ->
    [{status, StatusTag}] ++ format_status_details(StatusDetails);
make_log_params(event_type, EventType) ->
    [{type, EventType}].

format_status_details(#domain_InvoicePaymentFailed{failure = Failure}) ->
    [{error, list_to_binary(format_failure(Failure))}];
format_status_details(_) ->
    [].

format_failure({operation_timeout, _}) ->
    [<<"timeout">>];
format_failure({failure, Failure}) ->
    format_domain_failure(Failure).

format_domain_failure(Failure) ->
    payproc_errors:format_raw(Failure).

get_account_key({AccountParty, AccountType}) ->
    hg_utils:join(AccountParty, $., AccountType).

get_message(invoice_payment_started) ->
    "Invoice payment is started";
get_message(invoice_payment_risk_score_changed) ->
    "Invoice payment risk score changed";
get_message(invoice_payment_route_changed) ->
    "Invoice payment route changed";
get_message(invoice_payment_cash_flow_changed) ->
    "Invoice payment cash flow changed";
get_message(invoice_payment_status_changed) ->
    "Invoice payment status is changed".

%% Unmarshalling

-include("legacy_structures.hrl").

-spec unmarshal(hg_msgpack_marshalling:value()) -> [change()].

unmarshal(Change) ->
    unmarshal(change, Change).

%% Changes

unmarshal(change, [2, #{
    <<"change">>    := <<"payment_created">>,
    <<"payment">>   := Payment
}]) ->
    [?payment_started(unmarshal(payment, Payment))];
unmarshal(change, [2, #{
    <<"change">>    := <<"status_changed">>,
    <<"status">>    := Status
}]) ->
    [?payment_status_changed(unmarshal(status, Status))];
unmarshal(change, [2, #{
    <<"change">>    := <<"risk_score_changed">>,
    <<"risk_score">>:= RiskScore
}]) ->
    [?risk_score_changed(unmarshal(risk_score, RiskScore))];
unmarshal(change, [2, #{
    <<"change">>    := <<"route_changed">>,
    <<"route">>     := Route
}]) ->
    [?route_changed(hg_routing:unmarshal(Route))];
unmarshal(change, [2, #{
    <<"change">>    := <<"cash_flow_changed">>,
    <<"cash_flow">> := Cashflow
}]) ->
    [?cash_flow_changed(hg_cashflow:unmarshal(Cashflow))];
unmarshal(change, [2, #{
    <<"change">>    := <<"session_change">>,
    <<"payload">>   := Payload,
    <<"target">>    := Target
}]) ->
    [?session_ev(unmarshal(status, Target), unmarshal(session_change, Payload))];
unmarshal(change, [2, #{
    <<"change">>    := <<"adjustment_change">>,
    <<"id">>        := AdjustmentID,
    <<"payload">>   := Payload
}]) ->
    [?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adjustment_change, Payload))];
unmarshal(change, [2, #{
    <<"change">>    := <<"refund">>,
    <<"id">>        := RefundID,
    <<"payload">>   := Payload
}]) ->
    [?refund_ev(unmarshal(str, RefundID), unmarshal(refund_change, Payload))];
unmarshal(change, [2, #{
    <<"change">>    := <<"token_acquired">>,
    <<"token">>     := Token
}]) ->
    [?rec_token_acquired(unmarshal(str, Token))];

%% deprecated v2 changes
unmarshal(change, [2, #{
    <<"change">>        := <<"started">>,
    <<"payment">>       := Payment,
    <<"risk_score">>    := RiskScore,
    <<"route">>         := Route,
    <<"cash_flow">>     := Cashflow
}]) ->
    [
        ?payment_started(unmarshal(payment, Payment)),
        ?risk_score_changed(unmarshal(risk_score, RiskScore)),
        ?route_changed(hg_routing:unmarshal(Route)),
        ?cash_flow_changed(hg_cashflow:unmarshal(Cashflow))
    ];

%% deprecated v1 changes
unmarshal(change, [1, ?legacy_payment_started(Payment, RiskScore, Route, Cashflow)]) ->
    [
        ?payment_started(unmarshal(payment, Payment)),
        ?risk_score_changed(unmarshal(risk_score, RiskScore)),
        ?route_changed(hg_routing:unmarshal([1, Route])),
        ?cash_flow_changed(hg_cashflow:unmarshal([1, Cashflow]))
    ];
unmarshal(change, [1, ?legacy_payment_status_changed(Status)]) ->
    [?payment_status_changed(unmarshal(status, Status))];
unmarshal(change, [1, ?legacy_session_ev(Target, Payload)]) ->
    [?session_ev(unmarshal(status, Target), unmarshal(session_change, [1, Payload]))];
unmarshal(change, [1, ?legacy_adjustment_ev(AdjustmentID, Payload)]) ->
    [?adjustment_ev(unmarshal(str, AdjustmentID), unmarshal(adjustment_change, [1, Payload]))];

%% Payment

unmarshal(payment, #{
    <<"id">>                := ID,
    <<"created_at">>        := CreatedAt,
    <<"domain_revision">>   := Revision,
    <<"cost">>              := Cash,
    <<"payer">>             := MarshalledPayer,
    <<"flow">>              := Flow
} = Payment) ->
    Context = maps:get(<<"context">>, Payment, undefined),
    OwnerID = maps:get(<<"owner_id">>, Payment, undefined),
    ShopID = maps:get(<<"shop_id">>, Payment, undefined),
    PartyRevision = maps:get(<<"party_revision">>, Payment, undefined),
    MakeRecurrent = maps:get(<<"make_recurrent">>, Payment, undefined),
    ExternalID = maps:get(<<"external_id">>, Payment, undefined),
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        party_revision  = unmarshal(int, PartyRevision),
        shop_id         = unmarshal(str, ShopID),
        owner_id        = unmarshal(str, OwnerID),
        cost            = hg_cash:unmarshal(Cash),
        payer           = unmarshal(payer, MarshalledPayer),
        status          = ?pending(),
        flow            = unmarshal(flow, Flow),
        make_recurrent  = unmarshal(bool, MakeRecurrent),
        context         = hg_content:unmarshal(Context),
        external_id     = unmarshal(str, ExternalID)
    };

unmarshal(payment,
    ?legacy_payment(ID, CreatedAt, Revision, Status, MarshalledPayer, Cash, Context)
) ->
    Payer = unmarshal(payer, MarshalledPayer),
    #domain_InvoicePayment{
        id              = unmarshal(str, ID),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Revision),
        status          = unmarshal(status, Status),
        cost            = hg_cash:unmarshal([1, Cash]),
        payer           = Payer,
        flow            = ?invoice_payment_flow_instant(),
        context         = hg_content:unmarshal(Context)
    };

%% Flow

unmarshal(flow, #{<<"type">> := <<"instant">>}) ->
    ?invoice_payment_flow_instant();
unmarshal(flow, #{
    <<"type">>                  := <<"hold">>,
    <<"on_hold_expiration">>    := OnHoldExpiration,
    <<"held_until">>            := HeldUntil
}) ->
    ?invoice_payment_flow_hold(
        unmarshal(on_hold_expiration, OnHoldExpiration),
        unmarshal(str, HeldUntil)
    );

%% Recerrent intention

unmarshal(recurrent_parent_payment, undefined) ->
    undefined;
unmarshal(recurrent_parent_payment, #{
    <<"invoice_id">> := InvoiceID,
    <<"payment_id">> := PaymentID
}) ->
    ?recurrent_parent(unmarshal(str, InvoiceID), unmarshal(str, PaymentID));

%% Payment status

unmarshal(status, <<"pending">>) ->
    ?pending();
unmarshal(status, <<"processed">>) ->
    ?processed();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?failed(unmarshal(failure, Failure));
unmarshal(status, [<<"captured">>, Capture]) ->
    unmarshal(capture, Capture);
unmarshal(status, [<<"cancelled">>, Reason]) ->
    ?cancelled_with_reason(unmarshal(str, Reason));
unmarshal(status, <<"refunded">>) ->
    ?refunded();

unmarshal(status, ?legacy_pending()) ->
    ?pending();
unmarshal(status, ?legacy_processed()) ->
    ?processed();
unmarshal(status, ?legacy_failed(Failure)) ->
    ?failed(unmarshal(failure, [1, Failure]));
unmarshal(status, ?legacy_captured()) ->
    ?captured();
unmarshal(status, ?legacy_cancelled()) ->
    ?cancelled();
unmarshal(status, ?legacy_captured(Reason)) ->
    ?captured_with_reason(unmarshal(str, Reason));
unmarshal(status, ?legacy_cancelled(Reason)) ->
    ?cancelled_with_reason(unmarshal(str, Reason));

unmarshal(capture, Capture) when is_map(Capture) ->
    Reason = maps:get(<<"reason">>, Capture),
    Cost = maps:get(<<"cost">>, Capture),
    ?captured(unmarshal(str, Reason), hg_cash:unmarshal(Cost), undefined);
unmarshal(capture, Reason) ->
    ?captured_with_reason(unmarshal(str, Reason));

%% Session change
unmarshal(session_change, [3, [<<"suspended">>, Tag]]) ->
    ?session_suspended(unmarshal(str, Tag), undefined);
unmarshal(session_change, [3, Change]) ->
    unmarshal(session_change, [2, Change]);

unmarshal(session_change, [2, <<"started">>]) ->
    ?session_started();
unmarshal(session_change, [2, [<<"finished">>, Result]]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [2, <<"suspended">>]) ->
    ?session_suspended(undefined, undefined);
unmarshal(session_change, [2, <<"activated">>]) ->
    ?session_activated();
unmarshal(session_change, [2, [<<"transaction_bound">>, Trx]]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [2, [<<"proxy_state_changed">>, {bin, ProxySt}]]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [2, [<<"interaction_requested">>, UserInteraction]]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(session_change, [1, ?legacy_session_started()]) ->
    ?session_started();
unmarshal(session_change, [1, ?legacy_session_finished(Result)]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [1, ?legacy_session_suspended()]) ->
    ?session_suspended(undefined, undefined);
unmarshal(session_change, [1, ?legacy_session_activated()]) ->
    ?session_activated();
unmarshal(session_change, [1, ?legacy_trx_bound(Trx)]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(session_change, [1, ?legacy_proxy_st_changed(ProxySt)]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(session_change, [1, ?legacy_interaction_requested(UserInteraction)]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

%% Session status

unmarshal(session_status, <<"succeeded">>) ->
    ?session_succeeded();
unmarshal(session_status, [<<"failed">>, Failure]) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(session_status, ?legacy_session_succeeded()) ->
    ?session_succeeded();
unmarshal(session_status, ?legacy_session_failed(Failure)) ->
    ?session_failed(unmarshal(failure, [1, Failure]));

%% Adjustment change

unmarshal(adjustment_change, [2, [<<"created">>, Adjustment]]) ->
    ?adjustment_created(unmarshal(adjustment, Adjustment));
unmarshal(adjustment_change, [2, [<<"status_changed">>, Status]]) ->
    ?adjustment_status_changed(unmarshal(adjustment_status, Status));

unmarshal(adjustment_change, [1, ?legacy_adjustment_created(Adjustment)]) ->
    ?adjustment_created(unmarshal(adjustment, Adjustment));
unmarshal(adjustment_change, [1, ?legacy_adjustment_status_changed(Status)]) ->
    ?adjustment_status_changed(unmarshal(adjustment_status, Status));

%% Refund change

unmarshal(refund_change, [2, [<<"created">>, Refund, Cashflow]]) ->
    ?refund_created(unmarshal(refund, Refund), hg_cashflow:unmarshal(Cashflow));
unmarshal(refund_change, [2, [<<"status">>, Status]]) ->
    ?refund_status_changed(unmarshal(refund_status, Status));
unmarshal(refund_change, [2, [<<"session">>, Payload]]) ->
    ?session_ev(?refunded(), unmarshal(session_change, Payload));

%% Adjustment

unmarshal(adjustment, #{
    <<"id">>                    := ID,
    <<"created_at">>            := CreatedAt,
    <<"domain_revision">>       := Revision,
    <<"reason">>                := Reason,
    <<"old_cash_flow_inverse">> := OldCashFlowInverse,
    <<"new_cash_flow">>         := NewCashFlow
} = Payment) ->
    PartyRevision = maps:get(<<"party_revision">>, Payment, undefined),
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = ?adjustment_pending(),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        party_revision        = unmarshal(int, PartyRevision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal(OldCashFlowInverse),
        new_cash_flow         = hg_cashflow:unmarshal(NewCashFlow)
    };

unmarshal(adjustment,
    ?legacy_adjustment(ID, Status, CreatedAt, Revision, Reason, NewCashFlow, OldCashFlowInverse)
) ->
    #domain_InvoicePaymentAdjustment{
        id                    = unmarshal(str, ID),
        status                = unmarshal(adjustment_status, Status),
        created_at            = unmarshal(str, CreatedAt),
        domain_revision       = unmarshal(int, Revision),
        reason                = unmarshal(str, Reason),
        old_cash_flow_inverse = hg_cashflow:unmarshal([1, OldCashFlowInverse]),
        new_cash_flow         = hg_cashflow:unmarshal([1, NewCashFlow])
    };

%% Adjustment status

unmarshal(adjustment_status, <<"pending">>) ->
    ?adjustment_pending();
unmarshal(adjustment_status, [<<"captured">>, At]) ->
    ?adjustment_captured(At);
unmarshal(adjustment_status, [<<"cancelled">>, At]) ->
    ?adjustment_cancelled(At);

unmarshal(adjustment_status, ?legacy_adjustment_pending()) ->
    ?adjustment_pending();
unmarshal(adjustment_status, ?legacy_adjustment_captured(At)) ->
    ?adjustment_captured(At);
unmarshal(adjustment_status, ?legacy_adjustment_cancelled(At)) ->
    ?adjustment_cancelled(At);

%% Refund

unmarshal(refund, #{
    <<"id">>         := ID,
    <<"created_at">> := CreatedAt,
    <<"rev">>        := Rev
} = Refund) ->
    Cash = maps:get(<<"cash">>, Refund, undefined),
    PartyRevision = maps:get(<<"party_revision">>, Refund, undefined),
    #domain_InvoicePaymentRefund{
        id              = unmarshal(str, ID),
        status          = ?refund_pending(),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Rev),
        party_revision  = unmarshal(int, PartyRevision),
        reason          = genlib_map:get(<<"reason">>, Refund),
        cash            = hg_cash:unmarshal(Cash)
    };

unmarshal(refund_status, <<"pending">>) ->
    ?refund_pending();
unmarshal(refund_status, <<"succeeded">>) ->
    ?refund_succeeded();
unmarshal(refund_status, [<<"failed">>, Failure]) ->
    ?refund_failed(unmarshal(failure, Failure));

%% Payer

unmarshal(payer, [3, #{
    <<"type">>                := <<"customer_payer">>,
    <<"customer_id">>         := CustomerID,
    <<"customer_binding_id">> := CustomerBindingID,
    <<"rec_payment_tool_id">> := RecurrentPaytoolID,
    <<"payment_tool">>        := PaymentTool,
    <<"contact_info">>        := ContactInfo
}]) ->
    ?customer_payer(
        unmarshal(str, CustomerID),
        unmarshal(str, CustomerBindingID),
        unmarshal(str, RecurrentPaytoolID),
        hg_payment_tool:unmarshal(PaymentTool),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, [2, #{
    <<"type">>           := <<"payment_resource_payer">>,
    <<"resource">>       := Resource,
    <<"contact_info">>   := ContactInfo
}]) ->
    ?payment_resource_payer(
        unmarshal(disposable_payment_resource, Resource),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, [2, #{
    <<"type">>             := <<"recurrent_payer">>,
    <<"payment_tool">>     := PaymentTool,
    <<"recurrent_parent">> := RecurrentParent,
    <<"contact_info">>     := ContactInfo
}]) ->
    ?recurrent_payer(
        hg_payment_tool:unmarshal(PaymentTool),
        unmarshal(recurrent_parent_payment, RecurrentParent),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, [2, #{
    <<"type">>                  := <<"customer_payer">>,
    <<"customer_id">>           := CustomerID,
    <<"customer_binding_id">>   := CustomerBindingID,
    <<"rec_payment_tool_id">>   := RecurrentPaytoolID,
    <<"payment_tool">>          := PaymentTool
}]) ->
    ?customer_payer(
        unmarshal(str, CustomerID),
        unmarshal(str, CustomerBindingID),
        unmarshal(str, RecurrentPaytoolID),
        hg_payment_tool:unmarshal(PaymentTool),
        get_customer_contact_info(get_customer(unmarshal(str, CustomerID)))
    );

unmarshal(payer, #{
    <<"payment_tool">>  := PaymentTool,
    <<"session_id">>    := SessionId,
    <<"client_info">>   := ClientInfo,
    <<"contact_info">>  := ContactInfo
}) ->
    Resource = #{
        <<"payment_tool">>         => PaymentTool,
        <<"payment_session_id">>   => SessionId,
        <<"client_info">>          => ClientInfo
    },
    ?payment_resource_payer(
        unmarshal(disposable_payment_resource, Resource),
        unmarshal(contact_info, ContactInfo)
    );

unmarshal(payer, ?legacy_payer(PaymentTool, SessionId, ClientInfo, ContactInfo)) ->
    ?payment_resource_payer(
        #domain_DisposablePaymentResource{
            payment_tool = hg_payment_tool:unmarshal([1, PaymentTool]),
            payment_session_id = unmarshal(str, SessionId),
            client_info = unmarshal(client_info, ClientInfo)
        },
        unmarshal(contact_info, ContactInfo)
    );

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


%% Client info

unmarshal(client_info, undefined) ->
    undefined;
unmarshal(client_info, ?legacy_client_info(IpAddress, Fingerprint)) ->
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };
unmarshal(client_info, ClientInfo) ->
    IpAddress = maps:get(<<"ip_address">>, ClientInfo, undefined),
    Fingerprint = maps:get(<<"fingerprint">>, ClientInfo, undefined),
    #domain_ClientInfo{
        ip_address      = unmarshal(str, IpAddress),
        fingerprint     = unmarshal(str, Fingerprint)
    };

%% Contract info

unmarshal(contact_info, ?legacy_contract_info(PhoneNumber, Email)) ->
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

unmarshal(contact_info, ContractInfo) ->
    PhoneNumber = maps:get(<<"phone_number">>, ContractInfo, undefined),
    Email = maps:get(<<"email">>, ContractInfo, undefined),
    #domain_ContactInfo{
        phone_number    = unmarshal(str, PhoneNumber),
        email           = unmarshal(str, Email)
    };

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

unmarshal(trx, ?legacy_trx(ID, Timestamp, Extra)) ->
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
unmarshal(interaction, #{<<"payment_terminal_receipt">> := #{
    <<"spid">>  := SPID,
    <<"due">>   := DueDate
}}) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(interaction, ?legacy_get_request(URI)) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, ?legacy_post_request(URI, Form)) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};
unmarshal(interaction, ?legacy_payment_terminal_reciept(SPID, DueDate)) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(sub_failure, undefined) ->
    undefined;
unmarshal(sub_failure, #{<<"code">> := Code} = SubFailure) ->
    #domain_SubFailure{
        code   = unmarshal(str        , Code),
        sub    = unmarshal(sub_failure, maps:get(<<"sub">>, SubFailure, undefined))
    };

unmarshal(failure, [3, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [3, [<<"failure">>, #{<<"code">> := Code} = Failure]]) ->
    {failure, #domain_Failure{
        code   = unmarshal(str        , Code),
        reason = unmarshal(str        , maps:get(<<"reason">>, Failure, undefined)),
        sub    = unmarshal(sub_failure, maps:get(<<"sub"   >>, Failure, undefined))
    }};

unmarshal(failure, [2, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [2, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {failure, #domain_Failure{
        code   = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};

unmarshal(failure, [1, ?legacy_operation_timeout()]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, ?legacy_external_failure(Code, Description)]) ->
    {failure, #domain_Failure{
        code   = unmarshal(str, Code),
        reason = unmarshal(str, Description)
    }};

unmarshal(on_hold_expiration, <<"cancel">>) ->
    cancel;
unmarshal(on_hold_expiration, <<"capture">>) ->
    capture;

unmarshal(on_hold_expiration, OnHoldExpiration) when is_atom(OnHoldExpiration) ->
    OnHoldExpiration;

unmarshal(risk_score, <<"low">>) ->
    low;
unmarshal(risk_score, <<"high">>) ->
    high;
unmarshal(risk_score, <<"fatal">>) ->
    fatal;

unmarshal(risk_score, RiskScore) when is_atom(RiskScore) ->
    RiskScore;

unmarshal(_, Other) ->
    Other.
