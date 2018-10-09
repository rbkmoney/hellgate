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
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API

%% St accessors

-export([get_payment/1]).
-export([get_refunds/1]).
-export([get_refund/2]).
-export([get_adjustments/1]).
-export([get_adjustment/2]).


-export([get_party_revision/1]).
-export([get_activity/1]).
-export([get_tags/1]).

-export([get_fail_event/1]).
-export([get_repair_event/1]).

-export([construct_payment_info/2]).

%% Business logic

-export([capture/2]).
-export([cancel/2]).
-export([refund/3]).

-export([create_adjustment/4]).
-export([capture_adjustment/3]).
-export([cancel_adjustment/3]).

%% Machine like

-export([init/3]).

-export([process_signal/3]).
-export([process_call/3]).

-export([merge_change/2]).

-export([get_log_params/2]).

%% Marshalling

-export([marshal/1]).
-export([unmarshal/1]).

%%

-type activity()      :: payment_activity() | refund_activity() | idle.
-type payment_activity()  :: {payment, payment_step()}.
-type refund_activity()   :: {refund_session | refund_accounter, refund_id()}.
-type payment_step()      ::
    new |
    risk_scoring |
    routing |
    cash_flow_building |
    processing_session |
    processing_accounter |
    flow_waiting |
    finalizing_session |
    finalizing_accounter.

-type repair_type()      :: simple.

-record(st, {
    activity               :: activity(),
    payment                :: undefined | payment(),
    risk_score             :: undefined | risk_score(),
    route                  :: undefined | route(),
    cash_flow              :: undefined | cash_flow(),
    trx                    :: undefined | trx_info(),
    target                 :: undefined | target(),
    sessions       = #{}   :: #{target() => session()},
    retry_attempts = #{}   :: #{target() => non_neg_integer()},
    refunds        = #{}   :: #{refund_id() => refund_state()},
    adjustments    = []    :: [adjustment()],
    recurrent_token        :: undefined | recurrent_token(),
    opts                   :: undefined | opts(),
    repair_type            :: undefined | repair_type()
}).

-record(refund_st, {
    refund            :: undefined | refund(),
    cash_flow         :: undefined | cash_flow(),
    session           :: undefined | session()
}).

-type refund_state() :: #refund_st{}.

-type st() :: #st{}.

-export_type([st/0]).

-type party()               :: dmsl_domain_thrift:'Party'().
-type payer()               :: dmsl_domain_thrift:'Payer'().
-type invoice()             :: dmsl_domain_thrift:'Invoice'().
-type invoice_id()          :: dmsl_domain_thrift:'InvoiceID'().
-type payment()             :: dmsl_domain_thrift:'InvoicePayment'().
-type payment_id()          :: dmsl_domain_thrift:'InvoicePaymentID'().
-type refund()              :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type refund_id()           :: dmsl_domain_thrift:'InvoicePaymentRefundID'().
-type refund_params()       :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type adjustment()          :: dmsl_domain_thrift:'InvoicePaymentAdjustment'().
-type adjustment_id()       :: dmsl_domain_thrift:'InvoicePaymentAdjustmentID'().
-type adjustment_params()   :: dmsl_payment_processing_thrift:'InvoicePaymentAdjustmentParams'().
-type target()              :: dmsl_domain_thrift:'TargetInvoicePaymentStatus'().
-type risk_score()          :: dmsl_domain_thrift:'RiskScore'().
-type route()               :: dmsl_domain_thrift:'PaymentRoute'().
-type cash_flow()           :: dmsl_domain_thrift:'FinalCashFlow'().
-type trx_info()            :: dmsl_domain_thrift:'TransactionInfo'().
-type session_result()      :: dmsl_payment_processing_thrift:'SessionResult'().
-type proxy_state()         :: dmsl_proxy_provider_thrift:'ProxyState'().
-type tag()                 :: dmsl_proxy_provider_thrift:'CallbackTag'().
-type make_recurrent()      :: true | false.
-type recurrent_token()     :: dmsl_domain_thrift:'Token'().
-type retry_strategy()      :: hg_retry:strategy().

-type session_status()      :: active | suspended | finished.
-type session() :: #{
    target      := target(),
    status      := session_status(),
    trx         := trx_info(),
    tags        := [tag()],
    result      => session_result(),
    proxy_state => proxy_state()
}.

-type opts() :: #{
    party => party(),
    invoice => invoice()
}.

-export_type([opts/0]).

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
get_party_revision(#st{activity = {_, ID} = Activity} = St) when
    Activity =:= {refund_session, ID} orelse
    Activity =:= {refund_accounter, ID} ->
        #domain_InvoicePaymentRefund{party_revision = Revision, created_at = Timestamp} = get_refund(ID, St),
        {Revision, Timestamp};
get_party_revision(#st{activity = Activity}) ->
    erlang:error({no_revision_for_activity, Activity}).

-spec get_payment(st()) -> payment().

get_payment(#st{payment = Payment}) ->
    Payment.

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

-spec get_refunds(st()) -> [refund()].

get_refunds(#st{refunds = Rs} = St) ->
    lists:keysort(
        #domain_InvoicePaymentRefund.id,
        [enrich_refund_with_cash(R#refund_st.refund, St) || R <- maps:values(Rs)]
    ).

-spec get_refund(refund_id(), st()) -> refund() | no_return().

get_refund(ID, St) ->
    case try_get_refund_state(ID, St) of
        #refund_st{refund = Refund} ->
            enrich_refund_with_cash(Refund, St);
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
        [get_session_tags(S)                     || S <- maps:values(Sessions)] ++
        [get_session_tags(get_refund_session(R)) || R <- maps:values(Refunds) ]
    )).

-spec get_fail_event(atom()) -> tuple().

get_fail_event(ErrorType) ->
    ?payment_status_changed(?failed(ErrorType)).

-spec get_repair_event(atom()) -> tuple().

get_repair_event(_) ->
    ?payment_repair(simple).

%%

-type event()  :: any(). % FIXME
-type action() :: hg_machine_action:t().
-type events() :: [event()].
-type result() :: {events(), action()}.
-type machine_result() :: {next | done, result()}.

-spec init(payment_id(), _, opts()) ->
    {payment(), result()}.

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
    CreatedAt = hg_datetime:format_now(),
    MerchantTerms = get_merchant_terms(Opts, Revision),
    VS1 = collect_validation_varset(Party, Shop, VS0),
    Payment = construct_payment(
        PaymentID, CreatedAt, Cost, Payer, Flow, MerchantTerms, Party, Shop,
        VS1, Revision, MakeRecurrent
    ),
    Events = [?payment_started(Payment)],
    {collapse_changes(Events), {Events, hg_machine_action:instant()}}.

get_merchant_payments_terms(Opts, Revision) ->
    get_merchant_payments_terms(Opts, Revision, get_invoice_created_at(get_invoice(Opts))).

get_merchant_payments_terms(Opts, Revision, Timestamp) ->
    TermSet = get_merchant_terms(Opts, Revision, Timestamp),
    TermSet#domain_TermSet.payments.

get_merchant_terms(Opts, Revision) ->
    get_merchant_terms(Opts, Revision, get_invoice_created_at(get_invoice(Opts))).

get_merchant_terms(Opts, Revision, Timestamp) ->
    Invoice = get_invoice(Opts),
    Party = get_party(Opts),
    Shop = hg_party:get_shop(get_invoice_shop_id(Invoice), Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    ok = assert_contract_active(Contract),
    hg_party:get_terms(Contract, Timestamp, Revision).

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

construct_payment(PaymentID, CreatedAt, Cost, Payer, FlowParams, Terms,
                  Party, Shop, VS0, Revision, MakeRecurrent) ->
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
        id              = PaymentID,
        created_at      = CreatedAt,
        owner_id        = Party#domain_Party.id,
        shop_id         = Shop#domain_Shop.id,
        domain_revision = Revision,
        party_revision  = Party#domain_Party.revision,
        status          = ?pending(),
        cost            = Cost,
        payer           = Payer,
        flow            = Flow,
        make_recurrent  = MakeRecurrent
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
            Payment = get_payment(St),
            Predestination = choose_routing_predestination(Payment),
            case hg_routing:choose(Predestination, PaymentInstitution, VS, Revision) of
                {ok, _Route} = Result ->
                    Result;
                {error, {no_route_found, RejectContext}} = Error ->
                    _ = log_reject_context(RejectContext),
                    Error
            end
    end.

-spec choose_routing_predestination(payment()) -> hg_routing:route_predestination().
choose_routing_predestination(#domain_InvoicePayment{make_recurrent = true}) ->
    recurrent_payment;
choose_routing_predestination(#domain_InvoicePayment{payer = ?payment_resource_payer()}) ->
    payment.
% Other payers has predefined routes

log_reject_context(RejectContext) ->
    _ = lager:warning("No route found, varset: ~p", [maps:get(varset, RejectContext)]),
    _ = lager:warning("No route found, rejected providers: ~p", [maps:get(rejected_providers, RejectContext)]),
    _ = lager:warning("No route found, rejected terminals: ~p", [maps:get(rejected_terminals, RejectContext)]),
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
    MerchantTerms = get_merchant_payments_terms(Opts, Revision),
    VS2 = reconstruct_payment_flow(DomainFlow, CreatedAt, VS1),
    collect_refund_varset(
        MerchantTerms#domain_PaymentsServiceTerms.refunds,
        VS2,
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
        collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS, Revision)
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

collect_account_map(
    Payment,
    #domain_Shop{account = MerchantAccount},
    PaymentInstitution,
    #domain_Provider{accounts = ProviderAccounts},
    VS,
    Revision
) ->
    Currency = get_currency(get_payment_cost(Payment)),
    ProviderAccount = choose_provider_account(Currency, ProviderAccounts),
    SystemAccount = hg_payment_institution:get_system_account(Currency, VS, Revision, PaymentInstitution),
    M = #{
        {merchant , settlement} => MerchantAccount#domain_ShopAccount.settlement     ,
        {merchant , guarantee } => MerchantAccount#domain_ShopAccount.guarantee      ,
        {provider , settlement} => ProviderAccount#domain_ProviderAccount.settlement ,
        {system   , settlement} => SystemAccount#domain_SystemAccount.settlement
    },
    % External account probably can be optional for some payments
    case choose_external_account(Currency, VS, Revision) of
        #domain_ExternalAccount{income = Income, outcome = Outcome} ->
            M#{
                {external, income} => Income,
                {external, outcome} => Outcome
            };
        undefined ->
            M
    end.

choose_provider_account(Currency, Accounts) ->
    case maps:find(Currency, Accounts) of
        {ok, Account} ->
            Account;
        error ->
            error({misconfiguration, {'No provider account for a given currency', Currency}})
    end.

choose_external_account(Currency, VS, Revision) ->
    Globals = hg_domain:get(Revision, {globals, #domain_GlobalsRef{}}),
    ExternalAccountSetSelector = Globals#domain_Globals.external_account_set,
    case hg_selector:reduce(ExternalAccountSetSelector, VS, Revision) of
        {value, ExternalAccountSetRef} ->
            ExternalAccountSet = hg_domain:get(Revision, {external_account_set, ExternalAccountSetRef}),
            genlib_map:get(
                Currency,
                ExternalAccountSet#domain_ExternalAccountSet.accounts
            );
        _ ->
            undefined
    end.

get_account_state(AccountType, AccountMap, Accounts) ->
    % FIXME move me closer to hg_accounting
    case AccountMap of
        #{AccountType := AccountID} ->
            #{AccountID := AccountState} = Accounts,
            AccountState;
        #{} ->
            undefined
    end.

get_available_amount(#{min_available_amount := V}) ->
    V.

construct_payment_plan_id(St) ->
    construct_payment_plan_id(get_invoice(get_opts(St)), get_payment(St)).

construct_payment_plan_id(Invoice, Payment) ->
    hg_utils:construct_complex_id([
        get_invoice_id(Invoice),
        get_payment_id(Payment)
    ]).

reduce_selector(Name, Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
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

-spec capture(st(), atom()) -> {ok, result()}.

capture(St, Reason) ->
    do_payment(St, ?captured_with_reason(hg_utils:format_reason(Reason))).

-spec cancel(st(), atom()) -> {ok, result()}.

cancel(St, Reason) ->
    do_payment(St, ?cancelled_with_reason(hg_utils:format_reason(Reason))).

do_payment(St, Target) ->
    Payment = get_payment(St),
    _ = assert_activity({payment, flow_waiting}, St),
    _ = assert_payment_flow(hold, Payment),
    {ok, {start_session(Target), hg_machine_action:instant()}}.

-spec refund(refund_params(), st(), opts()) ->
    {refund(), result()}.

refund(Params, St0, Opts) ->
    St = St0#st{opts = Opts},
    Revision = hg_domain:head(),
    Payment = get_payment(St),
    _ = assert_payment_status(captured, Payment),
    Route = get_route(St),
    Shop = get_shop(Opts),
    PartyRevision = get_opts_party_revision(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    Provider = get_route_provider(Route, Revision),
    _ = assert_previous_refunds_finished(St),
    VS0 = collect_validation_varset(St, Opts),
    Cash = define_refund_cash(Params#payproc_InvoicePaymentRefundParams.cash, Payment),
    _ = assert_refund_cash(Cash, St),
    ID = construct_refund_id(St),
    Refund = #domain_InvoicePaymentRefund{
        id              = ID,
        created_at      = hg_datetime:format_now(),
        domain_revision = Revision,
        party_revision  = PartyRevision,
        status          = ?refund_pending(),
        reason          = Params#payproc_InvoicePaymentRefundParams.reason,
        cash            = Cash
    },
    MerchantTerms = get_merchant_refunds_terms(get_merchant_payments_terms(Opts, Revision)),
    VS1 = validate_refund(MerchantTerms, Refund, Payment, VS0, Revision),
    ProviderPaymentsTerms = get_provider_payments_terms(Route, Revision),
    ProviderTerms = get_provider_refunds_terms(ProviderPaymentsTerms, Refund, Payment, VS1, Revision),
    Cashflow = collect_refund_cashflow(MerchantTerms, ProviderTerms, VS1, Revision),
    AccountMap = collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS1, Revision),
    FinalCashflow = construct_final_cashflow(Cashflow, collect_cash_flow_context(Refund), AccountMap),
    Changes = [
        ?refund_created(Refund, FinalCashflow),
        ?session_ev(?refunded(), ?session_started())
    ],
    RefundSt = collapse_refund_changes(Changes),
    AffectedAccounts = prepare_refund_cashflow(RefundSt, St),
    % NOTE we assume that posting involving merchant settlement account MUST be present in the cashflow
    case get_available_amount(get_account_state({merchant, settlement}, AccountMap, AffectedAccounts)) of
        % TODO we must pull this rule out of refund terms
        Available when Available >= 0 ->
            Action = hg_machine_action:instant(),
            {Refund, {[?refund_ev(ID, C) || C <- Changes], Action}};
        Available when Available < 0 ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt, St),
            throw(#payproc_InsufficientAccountBalance{})
    end.

construct_refund_id(St) ->
    PaymentID = get_payment_id(get_payment(St)),
    InvoiceID = get_invoice_id(get_invoice(get_opts(St))),
    SequenceID = make_refund_squence_id(PaymentID, InvoiceID),
    IntRefundID = hg_sequences:get_next(SequenceID),
    integer_to_binary(IntRefundID).

make_refund_squence_id(PaymentID, InvoiceID) ->
    <<InvoiceID/binary, <<"_">>/binary, PaymentID/binary>>.

assert_refund_cash(Cash, St) ->
    PaymentAmount = get_remaining_payment_amount(Cash, St),
    assert_remaining_payment_amount(PaymentAmount, St).

assert_remaining_payment_amount(?cash(Amount, _), _St) when Amount >= 0 ->
    ok;
assert_remaining_payment_amount(?cash(Amount, _), St) when Amount < 0 ->
    Maximum = get_payment_cost(get_payment(St)),
    throw(#payproc_InvoicePaymentAmountExceeded{maximum = Maximum}).

assert_previous_refunds_finished(St) ->
    PendingRefunds = lists:filter(
        fun
            (#domain_InvoicePaymentRefund{status = ?refund_pending()}) ->
                true;
            (#domain_InvoicePaymentRefund{}) ->
                false
        end,
        get_refunds(St)),
    case PendingRefunds of
        [] ->
            ok;
        [_R|_] ->
            throw(#payproc_OperationNotPermitted{})
    end.

get_remaining_payment_amount(RefundCash, St) ->
    PaymentAmount = get_payment_cost(get_payment(St)),
    InterimPaymentAmount = lists:foldl(
        fun(R, Acc) ->
            case get_refund_status(R) of
                {S, _} when S == succeeded ->
                    hg_cash:sub(Acc, get_refund_cash(R));
                _ ->
                    Acc
            end
        end,
        PaymentAmount,
        get_refunds(St)
    ),
    hg_cash:sub(InterimPaymentAmount, RefundCash).

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
    hg_accounting:plan(construct_refund_plan_id(RefundSt, St), get_refund_cashflow_plan(RefundSt)).

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
    Payment = get_payment(St),
    Revision = get_adjustment_revision(Params),
    _ = assert_payment_status(captured, Payment),
    _ = assert_no_adjustment_pending(St),
    Shop = get_shop(Opts),
    PaymentInstitution = get_payment_institution(Opts, Revision),
    MerchantTerms = get_merchant_payments_terms(Opts, Revision, Timestamp),
    PartyRevision = get_opts_party_revision(Opts),
    Route = get_route(St),
    Provider = get_route_provider(Route, Revision),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    VS = collect_validation_varset(St, Opts),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS, Revision),
    FinalCashflow = construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision),
    ID = construct_adjustment_id(St),
    Adjustment = #domain_InvoicePaymentAdjustment{
        id                    = ID,
        status                = ?adjustment_pending(),
        created_at            = Timestamp,
        domain_revision       = Revision,
        party_revision        = PartyRevision,
        reason                = Params#payproc_InvoicePaymentAdjustmentParams.reason,
        old_cash_flow_inverse = hg_cashflow:revert(get_cashflow(St)),
        new_cash_flow         = FinalCashflow
    },
    _AffectedAccounts = prepare_adjustment_cashflow(Adjustment, St, Opts),
    Event = ?adjustment_ev(ID, ?adjustment_created(Adjustment)),
    {Adjustment, {[Event], hg_machine_action:new()}}.

get_adjustment_revision(Params) ->
    hg_utils:select_defined(
        Params#payproc_InvoicePaymentAdjustmentParams.domain_revision,
        hg_domain:head()
    ).

construct_adjustment_id(#st{adjustments = As}) ->
    integer_to_binary(length(As) + 1).

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

assert_no_adjustment_pending(#st{adjustments = As}) ->
    lists:foreach(fun assert_adjustment_finalized/1, As).

assert_adjustment_finalized(#domain_InvoicePaymentAdjustment{id = ID, status = {pending, _}}) ->
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

finalize_adjustment(ID, Intent, St, Options) ->
    Adjustment = get_adjustment(ID, St),
    ok = assert_adjustment_status(pending, Adjustment),
    _AffectedAccounts = finalize_adjustment_cashflow(Intent, Adjustment, St, Options),
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
    hg_accounting:plan(PlanID, Plan).

finalize_adjustment_cashflow(Intent, Adjustment, St, Options) ->
    PlanID = construct_adjustment_plan_id(Adjustment, St, Options),
    Plan = get_adjustment_cashflow_plan(Adjustment),
    case Intent of
        capture ->
            hg_accounting:commit(PlanID, Plan);
        cancel ->
            hg_accounting:rollback(PlanID, Plan)
    end.

get_adjustment_cashflow_plan(#domain_InvoicePaymentAdjustment{
    old_cash_flow_inverse = CashflowInverse,
    new_cash_flow         = Cashflow
}) ->
    [
        {1, CashflowInverse},
        {2, Cashflow}
    ].

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
    process_timeout(get_activity(St), Action, St).

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
    Step =:= processing_accounter orelse
    Step =:= finalizing_accounter
->
    process_result(Action, St);
process_timeout({refund_session, _ID}, Action, St) ->
    process_session(Action, St);
process_timeout({refund_accounter, _ID}, Action, St) ->
    process_result(Action, St);
process_timeout({payment, flow_waiting}, Action, St) ->
    finalize_payment(Action, St).

-spec process_call({callback, tag(), _}, st(), opts()) ->
    {_, machine_result()}. % FIXME
process_call({callback, Tag, Payload}, St, Options) ->
    scoper:scope(
        payment,
        get_st_meta(St),
        fun() -> process_callback(Tag, Payload, St#st{opts = Options}) end
    ).

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
    RiskScore = inspect(Payment, PaymentInstitution, VS0, Opts),
    Events0 = [?risk_score_changed(RiskScore)],
    VS1 = VS0#{risk_score => RiskScore},
    case choose_route(PaymentInstitution, VS1, Revision, St) of
        {ok, Route} ->
            process_cash_flow_building(Route, VS1, Payment, PaymentInstitution, Revision, Opts, Events0, Action);
        {error, {no_route_found, _Details}} ->
            Failure = {failure, payproc_errors:construct('PaymentFailure',
                {no_route_found, #payprocerr_GeneralFailure{}}
            )},
            process_failure(get_activity(St), Events0, Action, Failure, St)
    end.

process_cash_flow_building(Route, VS, Payment, PaymentInstitution, Revision, Opts, Events0, Action) ->
    MerchantTerms = get_merchant_payments_terms(Opts, Revision),
    ProviderTerms = get_provider_payments_terms(Route, Revision),
    Provider = get_route_provider(Route, Revision),
    Cashflow = collect_cashflow(MerchantTerms, ProviderTerms, VS, Revision),
    Shop = get_shop(Opts),
    FinalCashflow = construct_final_cashflow(Payment, Shop, PaymentInstitution, Provider, Cashflow, VS, Revision),
    Invoice = get_invoice(Opts),
    _AffectedAccounts = hg_accounting:plan(
        construct_payment_plan_id(Invoice, Payment),
        {1, FinalCashflow}
    ),
    Events1 = Events0 ++ [?route_changed(Route), ?cash_flow_changed(FinalCashflow)],
    {next, {Events1, hg_machine_action:set_timeout(0, Action)}}.

%%

-spec process_session(action(), st()) -> machine_result().
process_session(Action, St) ->
    Session = get_activity_session(St),
    process_session(Session, Action, St).

process_session(undefined, Action, St0) ->
    Events = start_session(get_target(St0)),
    St1 = collapse_changes(Events, St0),
    Session = get_activity_session(St1),
    process_session(Session, Action, Events, St1);
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
    ProxyContext = construct_proxy_context(St),
    {ok, ProxyResult} = issue_process_call(ProxyContext, St),
    Result = handle_proxy_result(ProxyResult, Action, Events, Session),
    finish_session_processing(Result, St).

-spec finalize_payment(action(), st()) -> machine_result().
finalize_payment(Action, St) ->
    Target = case get_payment_flow(get_payment(St)) of
        ?invoice_payment_flow_instant() ->
            ?captured();
        ?invoice_payment_flow_hold(OnHoldExpiration, _) ->
            case OnHoldExpiration of
                cancel ->
                    ?cancelled();
                capture ->
                    ?captured()
            end
    end,
    StartEvents = start_session(Target),
    {done, {StartEvents, hg_machine_action:set_timeout(0, Action)}}.

-spec process_callback_timeout(action(), session(), events(), st()) -> machine_result().
process_callback_timeout(Action, Session, Events, St) ->
    Result = handle_proxy_callback_timeout(Action, Events, Session),
    finish_session_processing(Result, St).

handle_callback(Payload, Action, St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, CallbackResult} = issue_callback_call(Payload, ProxyContext, St),
    {Response, Result} = handle_callback_result(CallbackResult, Action, get_activity_session(St)),
    {Response, finish_session_processing(Result, St)}.

-spec finish_session_processing(result(), st()) -> machine_result().
finish_session_processing(Result, St) ->
    finish_session_processing(get_activity(St), Result, St).

finish_session_processing({payment, Step} = Activity, {Events, Action}, St) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    Target = get_target(St),
    St1 = collapse_changes(Events, St),
    case get_session(Target, St1) of
        #{status := finished, result := ?session_succeeded(), target := Target} ->
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

process_result({payment, finalizing_accounter}, Action, St) ->
    Target = get_target(St),
    _AffectedAccounts = case Target of
        ?captured() ->
            commit_payment_cashflow(St);
        ?cancelled() ->
            rollback_payment_cashflow(St)
    end,
    NewAction = get_action(Target, Action, St),
    {done, {[?payment_status_changed(Target)], NewAction}};

process_result({refund_accounter, ID}, Action, St) ->
    RefundSt = try_get_refund_state(ID, St),
    _AffectedAccounts = commit_refund_cashflow(RefundSt, St),
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
process_failure({payment, Step}, Events, Action, Failure, St, _RefundSt) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    Target = get_target(St),
    case check_retry_possibility(Target, Failure, St) of
        {retry, Timeout} ->
            _ = lager:info("Retry session after transient failure, wait ~p", [Timeout]),
            {SessionEvents, SessionAction} = retry_session(Action, Target, Timeout),
            {next, {Events ++ SessionEvents, SessionAction}};
        fatal ->
            _AffectedAccounts = rollback_payment_cashflow(St),
            {done, {Events ++ [?payment_status_changed(?failed(Failure))], Action}}
    end;
process_failure({refund_session, ID}, Events, Action, Failure, St, RefundSt) ->
    Target = ?refunded(),
    case check_retry_possibility(Target, Failure, St) of
        {retry, Timeout} ->
            _ = lager:info("Retry session after transient failure, wait ~p", [Timeout]),
            {SessionEvents, SessionAction} = retry_session(Action, Target, Timeout),
            Events1 = [?refund_ev(ID, E) || E <- SessionEvents],
            {next, {Events ++ Events1, SessionAction}};
        fatal ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt, St),
            Events1 = [
                ?refund_ev(ID, ?refund_status_changed(?refund_failed(Failure)))
            ],
            {done, {Events ++ Events1, Action}}
    end.

retry_session(Action, Target, Timeout) ->
    NewEvents = start_session(Target),
    NewAction = set_timer({timeout, Timeout}, Action),
    {NewEvents, NewAction}.

get_actual_retry_strategy(Target, #st{retry_attempts = Attempts}) ->
    AttemptNum = maps:get(Target, Attempts, 0),
    hg_retry:skip_steps(get_initial_retry_strategy(Target), AttemptNum).

-spec get_initial_retry_strategy(target()) -> retry_strategy().
get_initial_retry_strategy({TargetCode, _DomainRecord}) ->
    PolicyConfig = genlib_app:env(hellgate, payment_retry_policy, #{}),
    hg_retry:new_strategy(maps:get(TargetCode, PolicyConfig, no_retry)).

-spec check_retry_possibility(Target, Failure, St) -> {retry, Timeout} | fatal when
    Failure :: dmsl_domain_thrift:'OperationFailure'(),
    Target :: target(),
    St :: st(),
    Timeout :: non_neg_integer().
check_retry_possibility(Target, Failure, St) ->
    case check_failure_type(Failure) of
        transient ->
            RetryStrategy = get_actual_retry_strategy(Target, St),
            case hg_retry:next_step(RetryStrategy) of
                {wait, Timeout, _NewStrategy} ->
                    {retry, Timeout};
                finish ->
                    _ = lager:debug("Retries strategy is exceed"),
                    fatal
            end;
        fatal ->
            _ = lager:debug("Failure ~p is not transient", [Failure]),
            fatal
    end.

-spec check_failure_type(dmsl_domain_thrift:'OperationFailure'()) -> transient | fatal.
check_failure_type({failure, Failure}) ->
    payproc_errors:match('PaymentFailure', Failure, fun do_check_failure_type/1);
check_failure_type(_Other) ->
    fatal.

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

handle_proxy_callback_timeout(Action, Events, Session) ->
    SessionEvents = [?session_finished(?session_failed(?operation_timeout()))],
    {Events ++ wrap_session_events(SessionEvents, Session), Action}.

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
handle_proxy_intent(#prxprv_FinishIntent{status = {failure, Failure}}, Action, Session) ->
    Events = [wrap_session_event(?session_finished(?session_failed({failure, Failure})), Session)],
    {Events, Action};
handle_proxy_intent(#prxprv_SleepIntent{timer = Timer, user_interaction = UserInteraction}, Action0, Session) ->
    Action = hg_machine_action:set_timer(Timer, Action0),
    Events = wrap_session_events(try_request_interaction(UserInteraction), Session),
    {Events, Action};
handle_proxy_intent(
    #prxprv_SuspendIntent{tag = Tag, timeout = Timer, user_interaction = UserInteraction},
    Action0,
    Session
) ->
    Action = set_timer(Timer, hg_machine_action:set_tag(Tag, Action0)),
    Events = [?session_suspended(Tag) | try_request_interaction(UserInteraction)],
    {wrap_session_events(Events, Session), Action}.

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

get_cashflow_plan(St) ->
    [{1, get_cashflow(St)}].

%%

-type payment_info() :: dmsl_proxy_provider_thrift:'PaymentInfo'().

-spec construct_payment_info(st(), opts()) ->
    payment_info().

construct_payment_info(St, Opts) ->
    construct_payment_info(
        get_activity(St),
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

construct_payment_info(idle, _St, PaymentInfo) ->
    PaymentInfo;
construct_payment_info({payment, _Step}, _St, PaymentInfo) ->
    PaymentInfo;
construct_payment_info({refund_session, ID}, St, PaymentInfo) ->
    PaymentInfo#prxprv_PaymentInfo{
        refund = construct_proxy_refund(try_get_refund_state(ID, St))
    }.

construct_proxy_payment(
    #domain_InvoicePayment{
        id = ID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost,
        make_recurrent = MakeRecurrent
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
        make_recurrent = MakeRecurrent
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

construct_proxy_refund(#refund_st{
    refund  = Refund,
    session = Session
}) ->
    #prxprv_InvoicePaymentRefund{
        id         = get_refund_id(Refund),
        created_at = get_refund_created_at(Refund),
        trx        = get_session_trx(Session),
        cash       = construct_proxy_cash(get_refund_cash(Refund))
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

get_invoice_created_at(#domain_Invoice{created_at = Dt}) ->
    Dt.

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

get_currency(#domain_Cash{currency = Currency}) ->
    Currency.

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

-spec merge_change(change(), st() | undefined) -> st().

merge_change(Event, undefined) ->
    merge_change(Event, #st{activity = {payment, new}});

merge_change(?payment_started(Payment), #st{activity = {payment, new}} = St) ->
    St#st{
        target     = ?processed(),
        payment    = Payment,
        activity   = {payment, risk_scoring}
    };
merge_change(?risk_score_changed(RiskScore), #st{activity = {payment, risk_scoring}} = St) ->
    St#st{
        risk_score = RiskScore,
        activity   = {payment, routing}
    };
merge_change(?route_changed(Route), #st{activity = {payment, routing}} = St) ->
    St#st{
        route      = Route,
        activity   = {payment, cash_flow_building}
    };
merge_change(?cash_flow_changed(Cashflow), #st{activity = {payment, cash_flow_building}} = St) ->
    St#st{
        cash_flow  = Cashflow,
        activity   = {payment, processing_session}
    };
merge_change(?rec_token_acquired(Token), St) ->
    St#st{recurrent_token = Token};
merge_change(
    ?payment_status_changed({failed, _} = Status),
    #st{payment = Payment, activity = {payment, _Step}} = St
) ->
    St#st{
        repair_type = undefined,
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = idle
    };
merge_change(
    ?payment_status_changed({StatusTag, _} = Status),
    #st{payment = Payment, activity = {payment, finalizing_accounter}} = St
) when
    StatusTag =:= cancelled orelse
    StatusTag =:= captured
->
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = idle
    };
merge_change(
    ?payment_status_changed({processed, _} = Status),
    #st{payment = Payment, activity = {payment, processing_accounter}} = St
) ->
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status},
        activity   = {payment, flow_waiting}
    };
merge_change(
    ?payment_status_changed({refunded, _} = Status),
    #st{payment = Payment, activity = idle} = St
) ->
    St#st{
        payment    = Payment#domain_InvoicePayment{status = Status}
    };

merge_change(Event = ?refund_ev(ID, ?session_ev(_Target, ?session_started())),
                #st{activity = idle} = St) ->
    merge_change(Event, St#st{activity = {refund_session, ID}});
merge_change(Event = ?refund_ev(ID, ?session_ev(?refunded(), ?session_finished(?session_succeeded()))),
                #st{activity = {refund_session, ID}} = St) ->
    merge_change(Event, St#st{activity = {refund_accounter, ID}});
merge_change(?refund_ev(ID, Event), St) ->
    RefundSt = merge_refund_change(Event, try_get_refund_state(ID, St)),
    St2 = set_refund_state(ID, RefundSt, St),
    case get_refund_status(get_refund(RefundSt)) of
        {S, _} when S == succeeded; S == failed ->
            St2#st{activity = idle};
        _ ->
            St2
    end;
merge_change(?adjustment_ev(ID, Event), St) ->
    Adjustment = merge_adjustment_change(Event, try_get_adjustment(ID, St)),
    St1 = set_adjustment(ID, Adjustment, St),
    % TODO new cashflow imposed implicitly on the payment state? rough
    case get_adjustment_status(Adjustment) of
        ?adjustment_captured(_) ->
            set_cashflow(get_adjustment_cashflow(Adjustment), St1);
        _ ->
            St1
    end;
merge_change(?session_ev(Target, ?session_started()), #st{activity = {payment, Step}} = St) when
    Step =:= processing_session orelse
    Step =:= flow_waiting orelse
    Step =:= finalizing_session
->
    % FIXME why the hell dedicated handling
    St1 = set_session(Target, create_session(Target, get_trx(St)), St#st{target = Target}),
    St2 = save_retry_attempt(Target, St1),
    NextStep = case Step of
        processing_session ->
            %% session retrying
            processing_session;
        flow_waiting ->
            finalizing_session;
        finalizing_session ->
            %% session retrying
            finalizing_session
    end,
    St2#st{activity = {payment, NextStep}};
merge_change(
    ?session_ev(Target, ?session_started()) = Event,
    #st{activity = idle, payment = #domain_InvoicePayment{status = {failed, _}}} = St
) ->
    % Looks like we are in adhoc repaired machine, see HG-418 for details.
    % Lets try to guess expected activity.
    % TODO: Remove this clause as soon as machines will have been migrated.
    Activity = case Target of
        ?processed() ->
            {payment, processing_session};
        ?cancelled() ->
            {payment, finalizing_session};
        ?captured() ->
            {payment, finalizing_session}
    end,
    merge_change(Event, St#st{activity = Activity});

merge_change(Event = ?session_ev(_Target, ?session_finished(?session_succeeded())),
                #st{activity = {payment, Step}} = St) when
    Step =:= processing_session orelse
    Step =:= finalizing_session
->
    NextStep = case Step of
        processing_session ->
            processing_accounter;
        finalizing_session ->
            finalizing_accounter
    end,
    merge_change(Event, St#st{activity = {payment, NextStep}});

merge_change(?session_ev(Target, Event), St) ->
    Session = merge_session_change(Event, get_session(Target, St)),
    St1 = set_session(Target, Session, St),
    % FIXME leaky transactions
    set_trx(get_session_trx(Session), St1);

merge_change(?payment_repair(Type), St) ->
    St#st{
        repair_type = Type
    }.


save_retry_attempt(Target, #st{retry_attempts = Attempts} = St) ->
    St#st{retry_attempts = maps:update_with(Target, fun(N) -> N + 1 end, 0, Attempts)}.

collapse_refund_changes(Changes) ->
    lists:foldl(fun merge_refund_change/2, undefined, Changes).

merge_refund_change(?refund_created(Refund, Cashflow), undefined) ->
    #refund_st{refund = Refund, cash_flow = Cashflow};
merge_refund_change(?refund_status_changed(Status), RefundSt) ->
    set_refund(set_refund_status(Status, get_refund(RefundSt)), RefundSt);
merge_refund_change(?session_ev(?refunded(), ?session_started()), St) ->
    set_refund_session(create_session(?refunded(), undefined), St);
merge_refund_change(?session_ev(?refunded(), Change), St) ->
    set_refund_session(merge_session_change(Change, get_refund_session(St)), St).

merge_adjustment_change(?adjustment_created(Adjustment), undefined) ->
    Adjustment;
merge_adjustment_change(?adjustment_status_changed(Status), Adjustment) ->
    Adjustment#domain_InvoicePaymentAdjustment{status = Status}.

get_cashflow(#st{cash_flow = FinalCashflow}) ->
    FinalCashflow.

set_cashflow(Cashflow, St = #st{}) ->
    St#st{cash_flow = Cashflow}.

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

set_refund_state(ID, RefundSt, St = #st{refunds = Rs}) ->
    St#st{refunds = Rs#{ID => RefundSt}}.

get_refund_session(#refund_st{session = Session}) ->
    Session.

set_refund_session(Session, St = #refund_st{}) ->
    St#refund_st{session = Session}.

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

define_refund_cash(undefined, #domain_InvoicePayment{cost = Cost}) ->
    Cost;
define_refund_cash(?cash(_, SymCode) = Cash, #domain_InvoicePayment{cost = ?cash(_, SymCode)}) ->
    Cash;
define_refund_cash(?cash(_, SymCode), _Payment) ->
    throw(#payproc_InconsistentRefundCurrency{currency = SymCode}).

get_refund_cash(#domain_InvoicePaymentRefund{cash = Cash}) ->
    Cash.

get_refund_created_at(#domain_InvoicePaymentRefund{created_at = CreatedAt}) ->
    CreatedAt.

enrich_refund_with_cash(Refund, #st{payment = Payment}) ->
    Cash = define_refund_cash(Refund#domain_InvoicePaymentRefund.cash, Payment),
    Refund#domain_InvoicePaymentRefund{cash = Cash}.

try_get_adjustment(ID, #st{adjustments = As}) ->
    case lists:keyfind(ID, #domain_InvoicePaymentAdjustment.id, As) of
        V = #domain_InvoicePaymentAdjustment{} ->
            V;
        false ->
            undefined
    end.

set_adjustment(ID, Adjustment, St = #st{adjustments = As}) ->
    St#st{adjustments = lists:keystore(ID, #domain_InvoicePaymentAdjustment.id, As, Adjustment)}.

merge_session_change(?session_finished(Result), Session) ->
    Session#{status := finished, result => Result};
merge_session_change(?session_activated(), Session) ->
    Session#{status := active};
merge_session_change(?session_suspended(undefined), Session) ->
    Session#{status := suspended};
merge_session_change(?session_suspended(Tag), Session) ->
    Session#{status := suspended, tags := [Tag | get_session_tags(Session)]};
merge_session_change(?trx_bound(Trx), Session) ->
    Session#{trx := Trx};
merge_session_change(?proxy_st_changed(ProxyState), Session) ->
    Session#{proxy_state => ProxyState};
merge_session_change(?interaction_requested(_), Session) ->
    Session.

create_session(Target, Trx) ->
    #{
        target => Target,
        status => active,
        trx    => Trx,
        tags   => []
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
    maps:get(Target, Sessions, undefined).

set_session(Target, Session, St = #st{sessions = Sessions}) ->
    St#st{sessions = Sessions#{Target => Session}}.

get_session_status(#{status := Status}) ->
    Status.

get_session_trx(#{trx := Trx}) ->
    Trx.

get_session_proxy_state(Session) ->
    maps:get(proxy_state, Session, undefined).

get_session_tags(#{tags := Tags}) ->
    Tags.

get_target(#st{target = Target}) ->
    Target.

get_opts(#st{opts = Opts}) ->
    Opts.

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
    RefundSt#refund_st.session.

%%

collapse_changes(Changes) ->
    collapse_changes(Changes, undefined).

collapse_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

%%

get_rec_payment_tool(RecPaymentToolID) ->
    hg_woody_wrapper:call(recurrent_paytool, 'Get', [RecPaymentToolID]).

get_customer(CustomerID) ->
    case issue_customer_call('Get', [CustomerID]) of
        {ok, Customer} ->
            Customer;
        {exception, #payproc_CustomerNotFound{}} ->
            throw_invalid_request(<<"Customer not found">>);
        {exception, #payproc_InvalidUser{}} ->
            throw_invalid_request(<<"Invalid customer">>);
        {exception, Error} ->
            error({<<"Can't get customer">>, Error})
    end.

issue_process_call(ProxyContext, St) ->
    issue_proxy_call('ProcessPayment', [ProxyContext], St).

issue_callback_call(Payload, ProxyContext, St) ->
    issue_proxy_call('HandlePaymentCallback', [Payload, ProxyContext], St).

issue_proxy_call(Func, Args, St) ->
    CallOpts = get_call_options(St),
    hg_woody_wrapper:call(proxy_provider, Func, Args, CallOpts).

get_call_options(St) ->
    Revision = hg_domain:head(),
    Provider = hg_domain:get(Revision, {provider, get_route_provider_ref(get_route(St))}),
    hg_proxy:get_call_options(Provider#domain_Provider.proxy, Revision).

get_route(#st{route = Route}) ->
    Route.

get_route_provider_ref(#domain_PaymentRoute{provider = ProviderRef}) ->
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
            cashflow   => get_cashflow(State),
            event_type => invoice_payment_status_changed
        }
    );
get_log_params(_, _) ->
    undefined.

make_log_params(Params) ->
    LogParams = maps:fold(
        fun(K, V, Acc) ->
            Acc ++ make_log_params(K, V)
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
    Reminders = maps:to_list(hg_cashflow:get_partial_remainders(CashFlow)),
    Accounts = lists:map(
        fun ({Account, ?cash(Amount, SymCode)}) ->
            Remainder = [{remainder, [{amount, Amount}, {currency, SymCode}]}],
            {get_account_key(Account), Remainder}
        end,
        Reminders
    ),
    [{accounts, Accounts}];
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
    list_to_binary(lists:concat([atom_to_list(AccountParty), ".", atom_to_list(AccountType)])).

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

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(change()) ->
    hg_msgpack_marshalling:value().

marshal(Change) ->
    marshal(change, Change).

%% Changes

marshal(change, ?payment_started(Payment)) ->
    [2, #{
        <<"change">>        => <<"payment_created">>,
        <<"payment">>       => marshal(payment, Payment)
    }];
marshal(change, ?risk_score_changed(RiskScore)) ->
    [2, #{
        <<"change">>        => <<"risk_score_changed">>,
        <<"risk_score">>    => marshal(risk_score, RiskScore)
    }];
marshal(change, ?route_changed(Route)) ->
    [2, #{
        <<"change">>        => <<"route_changed">>,
        <<"route">>         => hg_routing:marshal(Route)
    }];
marshal(change, ?cash_flow_changed(Cashflow)) ->
    [2, #{
        <<"change">>        => <<"cash_flow_changed">>,
        <<"cash_flow">>     => hg_cashflow:marshal(Cashflow)
    }];
marshal(change, ?payment_status_changed(Status)) ->
    [2, #{
        <<"change">>        => <<"status_changed">>,
        <<"status">>        => marshal(status, Status)
    }];
marshal(change, ?session_ev(Target, Payload)) ->
    [2, #{
        <<"change">>        => <<"session_change">>,
        <<"target">>        => marshal(status, Target),
        <<"payload">>       => marshal(session_change, Payload)
    }];
marshal(change, ?adjustment_ev(AdjustmentID, Payload)) ->
    [2, #{
        <<"change">>        => <<"adjustment_change">>,
        <<"id">>            => marshal(str, AdjustmentID),
        <<"payload">>       => marshal(adjustment_change, Payload)
    }];
marshal(change, ?refund_ev(RefundID, Payload)) ->
    [2, #{
        <<"change">>        => <<"refund">>,
        <<"id">>            => marshal(str, RefundID),
        <<"payload">>       => marshal(refund_change, Payload)
    }];
marshal(change, ?rec_token_acquired(Token)) ->
    [2, #{
        <<"change">>        => <<"token_acquired">>,
        <<"token">>         => marshal(str, Token)
    }];

%% Payment

marshal(payment, #domain_InvoicePayment{} = Payment) ->
    genlib_map:compact(#{
        <<"id">>                => marshal(str, Payment#domain_InvoicePayment.id),
        <<"created_at">>        => marshal(str, Payment#domain_InvoicePayment.created_at),
        <<"domain_revision">>   => marshal(int, Payment#domain_InvoicePayment.domain_revision),
        <<"party_revision">>    => marshal(int, Payment#domain_InvoicePayment.party_revision),
        <<"owner_id">>          => marshal(str, Payment#domain_InvoicePayment.owner_id),
        <<"shop_id">>           => marshal(str, Payment#domain_InvoicePayment.shop_id),
        <<"cost">>              => hg_cash:marshal(Payment#domain_InvoicePayment.cost),
        <<"payer">>             => marshal(payer, Payment#domain_InvoicePayment.payer),
        <<"flow">>              => marshal(flow, Payment#domain_InvoicePayment.flow),
        <<"make_recurrent">>    => marshal(bool, Payment#domain_InvoicePayment.make_recurrent),
        <<"context">>           => hg_content:marshal(Payment#domain_InvoicePayment.context)
    });

%% Flow

marshal(flow, ?invoice_payment_flow_instant()) ->
    #{<<"type">> => <<"instant">>};
marshal(flow, ?invoice_payment_flow_hold(OnHoldExpiration, HeldUntil)) ->
    #{
        <<"type">>                  => <<"hold">>,
        <<"on_hold_expiration">>    => marshal(on_hold_expiration, OnHoldExpiration),
        <<"held_until">>            => marshal(str, HeldUntil)
    };

%% Recurrent intention

marshal(recurrent_parent_payment, undefined) ->
    undefined;
marshal(recurrent_parent_payment, ?recurrent_parent(InvoiceID, PaymentID)) ->
    #{
        <<"invoice_id">> => marshal(str, InvoiceID),
        <<"payment_id">> => marshal(str, PaymentID)
    };

%% Payment status

marshal(status, ?pending()) ->
    <<"pending">>;
marshal(status, ?processed()) ->
    <<"processed">>;
marshal(status, ?refunded()) ->
    <<"refunded">>;
marshal(status, ?failed(Failure)) ->
    [<<"failed">>, marshal(failure, Failure)];
marshal(status, ?captured_with_reason(Reason)) ->
    [<<"captured">>, marshal(str, Reason)];
marshal(status, ?cancelled_with_reason(Reason)) ->
    [<<"cancelled">>, marshal(str, Reason)];

%% Session change

marshal(session_change, ?session_started()) ->
    [3, <<"started">>];
marshal(session_change, ?session_finished(Result)) ->
    [3, [
        <<"finished">>,
        marshal(session_status, Result)
    ]];
marshal(session_change, ?session_suspended(Tag)) ->
    [3, [
        <<"suspended">>,
        marshal(str, Tag)
    ]];
marshal(session_change, ?session_activated()) ->
    [3, <<"activated">>];
marshal(session_change, ?trx_bound(Trx)) ->
    [3, [
        <<"transaction_bound">>,
        marshal(trx, Trx)
    ]];
marshal(session_change, ?proxy_st_changed(ProxySt)) ->
    [3, [
        <<"proxy_state_changed">>,
        marshal(bin, {bin, ProxySt})
    ]];
marshal(session_change, ?interaction_requested(UserInteraction)) ->
    [3, [
        <<"interaction_requested">>,
        marshal(interaction, UserInteraction)
    ]];

marshal(session_status, ?session_succeeded()) ->
    <<"succeeded">>;
marshal(session_status, ?session_failed(PayloadFailure)) ->
    [
        <<"failed">>,
        marshal(failure, PayloadFailure)
    ];

%% Adjustment change

marshal(adjustment_change, ?adjustment_created(Adjustment)) ->
    [2, [<<"created">>, marshal(adjustment, Adjustment)]];
marshal(adjustment_change, ?adjustment_status_changed(Status)) ->
    [2, [<<"status_changed">>, marshal(adjustment_status, Status)]];

%% Refund change

marshal(refund_change, ?refund_created(Refund, Cashflow)) ->
    [2, [<<"created">>, marshal(refund, Refund), hg_cashflow:marshal(Cashflow)]];
marshal(refund_change, ?refund_status_changed(Status)) ->
    [2, [<<"status">>, marshal(refund_status, Status)]];
marshal(refund_change, ?session_ev(_Target, Payload)) ->
    [2, [<<"session">>, marshal(session_change, Payload)]];

%% Adjustment

marshal(adjustment, #domain_InvoicePaymentAdjustment{} = Adjustment) ->
    #{
        <<"id">>                    => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.id),
        <<"created_at">>            => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.created_at),
        <<"domain_revision">>       => marshal(int, Adjustment#domain_InvoicePaymentAdjustment.domain_revision),
        <<"party_revision">>        => marshal(int, Adjustment#domain_InvoicePaymentAdjustment.party_revision),
        <<"reason">>                => marshal(str, Adjustment#domain_InvoicePaymentAdjustment.reason),
        % FIXME
        <<"old_cash_flow_inverse">> => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.old_cash_flow_inverse),
        <<"new_cash_flow">>         => hg_cashflow:marshal(
            Adjustment#domain_InvoicePaymentAdjustment.new_cash_flow)
    };

marshal(adjustment_status, ?adjustment_pending()) ->
    <<"pending">>;
marshal(adjustment_status, ?adjustment_captured(At)) ->
    [<<"captured">>, marshal(str, At)];
marshal(adjustment_status, ?adjustment_cancelled(At)) ->
    [<<"cancelled">>, marshal(str, At)];

%% Refund

marshal(refund, #domain_InvoicePaymentRefund{} = Refund) ->
    genlib_map:compact(#{
        <<"id">>         => marshal(str, Refund#domain_InvoicePaymentRefund.id),
        <<"created_at">> => marshal(str, Refund#domain_InvoicePaymentRefund.created_at),
        <<"rev">>        => marshal(int, Refund#domain_InvoicePaymentRefund.domain_revision),
        <<"party_revision">> => marshal(int, Refund#domain_InvoicePaymentRefund.party_revision),
        <<"reason">>     => marshal(str, Refund#domain_InvoicePaymentRefund.reason),
        <<"cash">>       => hg_cash:marshal(Refund#domain_InvoicePaymentRefund.cash)
    });

marshal(refund_status, ?refund_pending()) ->
    <<"pending">>;
marshal(refund_status, ?refund_succeeded()) ->
    <<"succeeded">>;
marshal(refund_status, ?refund_failed(Failure)) ->
    [<<"failed">>, marshal(failure, Failure)];

%%

marshal(payer, ?payment_resource_payer(Resource, ContactInfo)) ->
    [2, #{
        <<"type">>           => <<"payment_resource_payer">>,
        <<"resource">>       => marshal(disposable_payment_resource, Resource),
        <<"contact_info">>   => marshal(contact_info, ContactInfo)
    }];

marshal(payer, ?recurrent_payer(PaymentTool, Parent, ContactInfo)) ->
    [2, #{
        <<"type">>             => <<"recurrent_payer">>,
        <<"payment_tool">>     => hg_payment_tool:marshal(PaymentTool),
        <<"recurrent_parent">> => marshal(recurrent_parent_payment, Parent),
        <<"contact_info">>     => marshal(contact_info, ContactInfo)
    }];

marshal(payer, ?customer_payer(CustomerID, CustomerBindingID, RecurrentPaytoolID, PaymentTool, ContactInfo)) ->
    [3, #{
        <<"type">>                  => <<"customer_payer">>,
        <<"customer_id">>           => marshal(str, CustomerID),
        <<"customer_binding_id">>   => marshal(str, CustomerBindingID),
        <<"rec_payment_tool_id">>   => marshal(str, RecurrentPaytoolID),
        <<"payment_tool">>          => hg_payment_tool:marshal(PaymentTool),
        <<"contact_info">>          => marshal(contact_info, ContactInfo)
    }];

marshal(disposable_payment_resource, #domain_DisposablePaymentResource{} = PaymentResource) ->
    #{
        <<"payment_tool">> => hg_payment_tool:marshal(PaymentResource#domain_DisposablePaymentResource.payment_tool),
        <<"payment_session_id">> => marshal(str, PaymentResource#domain_DisposablePaymentResource.payment_session_id),
        <<"client_info">> => marshal(client_info, PaymentResource#domain_DisposablePaymentResource.client_info)
    };

marshal(client_info, undefined) ->
    undefined;
marshal(client_info, #domain_ClientInfo{} = ClientInfo) ->
    genlib_map:compact(#{
        <<"ip_address">>    => marshal(str, ClientInfo#domain_ClientInfo.ip_address),
        <<"fingerprint">>   => marshal(str, ClientInfo#domain_ClientInfo.fingerprint)
    });

marshal(contact_info, #domain_ContactInfo{} = ContactInfo) ->
    genlib_map:compact(#{
        <<"phone_number">>  => marshal(str, ContactInfo#domain_ContactInfo.phone_number),
        <<"email">>         => marshal(str, ContactInfo#domain_ContactInfo.email)
    });

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
marshal(interaction, {payment_terminal_reciept, #'PaymentTerminalReceipt'{short_payment_id = SPID, due = DueDate}}) ->
    #{<<"payment_terminal_receipt">> =>
        #{
            <<"spid">>  => marshal(str, SPID),
            <<"due">>   => marshal(str, DueDate)
        }
    };

marshal(sub_failure, undefined) ->
    undefined;
marshal(sub_failure, #domain_SubFailure{} = SubFailure) ->
    genlib_map:compact(#{
        <<"code">> => marshal(str        , SubFailure#domain_SubFailure.code),
        <<"sub" >> => marshal(sub_failure, SubFailure#domain_SubFailure.sub )
    });

marshal(failure, {operation_timeout, _}) ->
    [3, <<"operation_timeout">>];
marshal(failure, {failure, #domain_Failure{} = Failure}) ->
    [3, [<<"failure">>, genlib_map:compact(#{
        <<"code"  >> => marshal(str        , Failure#domain_Failure.code  ),
        <<"reason">> => marshal(str        , Failure#domain_Failure.reason),
        <<"sub"   >> => marshal(sub_failure, Failure#domain_Failure.sub   )
    })]];

marshal(on_hold_expiration, cancel) ->
    <<"cancel">>;
marshal(on_hold_expiration, capture) ->
    <<"capture">>;

marshal(risk_score, low) ->
    <<"low">>;
marshal(risk_score, high) ->
    <<"high">>;
marshal(risk_score, fatal) ->
    <<"fatal">>;

marshal(_, Other) ->
    Other.

%% Unmarshalling

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
        context         = hg_content:unmarshal(Context)
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
unmarshal(status, [<<"captured">>, Reason]) ->
    ?captured_with_reason(unmarshal(str, Reason));
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

%% Session change

unmarshal(session_change, [3, [<<"suspended">>, Tag]]) ->
    ?session_suspended(unmarshal(str, Tag));
unmarshal(session_change, [3, Change]) ->
    unmarshal(session_change, [2, Change]);

unmarshal(session_change, [2, <<"started">>]) ->
    ?session_started();
unmarshal(session_change, [2, [<<"finished">>, Result]]) ->
    ?session_finished(unmarshal(session_status, Result));
unmarshal(session_change, [2, <<"suspended">>]) ->
    ?session_suspended(undefined);
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
    ?session_suspended(undefined);
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
