-module(hg_payment_refund).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([init/6]).
-export([process_signal/4]).

-export([get_refunds/1]).
-export([get_refund/1]).
-export([get_refund_status/1]).
-export([get_refund_session/1]).

-export([merge_change/2]).
-export([construct_proxy_refund/1]).

%% Marshalling

-export([marshal/1]).
-export([unmarshal/1]).

-record(st, {
    refund            :: undefined | refund(),
    cash_flow         :: undefined | cash_flow(),
    session           :: undefined | hg_proxy_provider_session:st(),
    opts              :: undefined | opts(),
    payment_st        :: undefined | hg_invoice_payment:st()
}).

-type refund()            :: dmsl_domain_thrift:'InvoicePaymentRefund'().
-type refund_id()         :: dmsl_domain_thrift:'InvoicePaymentRefundID'().
-type refund_status()     :: dmsl_domain_thrift:'InvoicePaymentRefundStatus'().
-type refund_params()     :: dmsl_payment_processing_thrift:'InvoicePaymentRefundParams'().
-type party()             :: dmsl_domain_thrift:'Party'().
-type invoice()           :: dmsl_domain_thrift:'Invoice'().
-type payment()           :: dmsl_domain_thrift:'InvoicePayment'().
-type cash_flow()         :: dmsl_domain_thrift:'FinalCashFlow'().
-type prxprv_refund()     :: dmsl_proxy_provider_thrift:'InvoicePaymentRefund'().

-type st() :: #st{}.

-export_type([st/0]).
-export_type([refund/0]).
-export_type([refund_id/0]).
-export_type([refund_params/0]).

-type opts() :: #{
    party => party(),
    invoice => invoice(),
    payment => payment()
}.

-type result() :: {[_], hg_machine_action:t()}.

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentRefundChangePayload'().

-include("payment_events.hrl").

-spec get_refunds([st()]) -> [refund()].

get_refunds(Rs) ->
    lists:keysort(#domain_InvoicePaymentRefund.id, [R#st.refund || R <- Rs]).

-spec get_refund(st()) -> refund() | no_return().

get_refund(#st{refund = Refund}) ->
    Refund.

-spec init(refund_id(), refund_params(), integer(), #{any() => any()}, cash_flow(), opts()) ->
    {refund(), result()}.

init(RefundID, Params, Revision, AccountMap, FinalCashflow, Opts) ->
    CreatedAt = hg_datetime:format_now(),
    Refund = construct_refund(RefundID, CreatedAt, Revision, Params),
    Changes = [
        ?refund_created(Refund, FinalCashflow),
        ?session_ev(?refunded(), ?session_started())
    ],
    RefundSt = collapse_changes(Changes, undefined),
    AffectedAccounts = prepare_refund_cashflow(RefundSt, Opts),
    % NOTE we assume that posting involving merchant settlement account MUST be present in the cashflow
    case get_available_amount(get_account_state({merchant, settlement}, AccountMap, AffectedAccounts)) of
        % TODO we must pull this rule out of refund terms
        Available when Available >= 0 ->
            Action = hg_machine_action:instant(),
            {Refund, {[?refund_ev(RefundID, C) || C <- Changes], Action}};
        Available when Available < 0 ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt, Opts),
            throw(#payproc_InsufficientAccountBalance{})
    end.

-spec process_signal(timeout, st(), hg_invoice_payment:st(), opts()) ->
    {next | done, result()}.

process_signal(timeout, St, PaymentSt, Opts) ->
    scoper:scope(
        refund,
        fun() -> handle_timeout(St#st{opts = Opts, payment_st = PaymentSt}) end
    ).

handle_timeout(#st{session = Session} = St) ->
    Action = hg_machine_action:new(),
    case hg_proxy_provider_session:get_status(Session) of
        active ->
            process(Action, St);
        suspended ->
            process_callback_timeout(Action, St)
    end.

process_callback_timeout(Action, #st{session = Session} = St) ->
    Result = hg_proxy_provider:handle_proxy_callback_timeout(Action, Session),
    finish_processing(Result, St).

process(Action, #st{session = Session, payment_st = PaymentSt} = St) ->
    ProxyContext = construct_proxy_context(St),
    {ok, ProxyResult} = hg_proxy_provider:process_payment(ProxyContext, hg_invoice_payment:get_route(PaymentSt)),
    Result = hg_proxy_provider:handle_proxy_result(ProxyResult, Action, Session),
    finish_processing(Result, St).

construct_proxy_context(#st{session = Session, opts = Opts0, payment_st = PaymentSt}) ->
    Opts1 = maps:remove(payment, Opts0),
    PaymentInfo = hg_invoice_payment:construct_payment_info(PaymentSt, Opts1),
    hg_proxy_provider:construct_proxy_context(Session, PaymentInfo, hg_invoice_payment:get_route(PaymentSt)).

finish_processing({Events0, Action}, #st{opts = Opts} = St) ->
    St1 = collapse_changes(Events0, St),
    Session = get_refund_session(St1),
    SessionStatus = hg_proxy_provider_session:get_status(Session),
    SessionResult = hg_proxy_provider_session:get_result(Session),
    case {SessionStatus, SessionResult} of
        {finished, ?session_succeeded()} ->
            _AffectedAccounts = commit_refund_cashflow(St1, Opts),
            Events1 = [?refund_status_changed(?refund_succeeded())],
            {done, {Events0 ++ Events1, Action}};
        {finished, ?session_failed(Failure)} ->
            _AffectedAccounts = rollback_refund_cashflow(St1, Opts),
            Events1 = [?refund_status_changed(?refund_failed(Failure))],
            {done, {Events0 ++ Events1, Action}};
        {_, _} ->
            {next, {Events0, Action}}
    end.

construct_refund(ID, CreatedAt, Revision, Params) ->
    #domain_InvoicePaymentRefund{
        id              = ID,
        created_at      = CreatedAt,
        domain_revision = Revision,
        status          = ?refund_pending(),
        reason          = Params#payproc_InvoicePaymentRefundParams.reason
    }.

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

prepare_refund_cashflow(St, Opts) ->
    hg_accounting:plan(construct_refund_plan_id(St, Opts), get_refund_cashflow_plan(St)).

commit_refund_cashflow(St, Opts) ->
    hg_accounting:commit(construct_refund_plan_id(St, Opts), [get_refund_cashflow_plan(St)]).

rollback_refund_cashflow(St, Opts) ->
    hg_accounting:rollback(construct_refund_plan_id(St, Opts), [get_refund_cashflow_plan(St)]).

construct_refund_plan_id(St, Opts) ->
    hg_utils:construct_complex_id([
        get_invoice_id(get_invoice(Opts)),
        get_payment_id(get_payment(Opts)),
        {refund, get_refund_id(get_refund(St))}
    ]).

get_refund_cashflow_plan(St) ->
    {1, get_refund_cashflow(St)}.

-spec construct_proxy_refund(st()) -> prxprv_refund().

construct_proxy_refund(#st{
    refund  = #domain_InvoicePaymentRefund{id = ID, created_at = CreatedAt},
    session = Session
}) ->
    #prxprv_InvoicePaymentRefund{
        id         = ID,
        created_at = CreatedAt,
        trx        = hg_proxy_provider_session:get_trx(Session)
    }.

collapse_changes(Changes, St) ->
    lists:foldl(fun merge_change/2, St, Changes).

-spec merge_change(change(), st()) -> st().

merge_change(?refund_created(Refund, Cashflow), undefined) ->
    #st{refund = Refund, cash_flow = Cashflow};
merge_change(?refund_status_changed(Status), St) ->
    set_refund(set_refund_status(Status, get_refund(St)), St);
merge_change(?session_ev(?refunded(), ?session_started()), St) ->
    set_refund_session(hg_proxy_provider_session:create(?refunded(), undefined), St);
merge_change(?session_ev(?refunded(), Change), St) ->
    set_refund_session(hg_proxy_provider_session:merge_change(Change, get_refund_session(St)), St).

-spec get_refund_session(st()) -> hg_proxy_provider_session:st().

get_refund_session(#st{session = Session}) ->
    Session.

set_refund_session(Session, St = #st{}) ->
    St#st{session = Session}.

set_refund(Refund, St = #st{}) ->
    St#st{refund = Refund}.

get_refund_id(#domain_InvoicePaymentRefund{id = ID}) ->
    ID.

-spec get_refund_status(refund()) -> refund_status().

get_refund_status(#domain_InvoicePaymentRefund{status = Status}) ->
    Status.

set_refund_status(Status, Refund = #domain_InvoicePaymentRefund{}) ->
    Refund#domain_InvoicePaymentRefund{status = Status}.

get_refund_cashflow(#st{cash_flow = CashFlow}) ->
    CashFlow.

get_invoice(#{invoice := Invoice}) ->
    Invoice.

get_invoice_id(#domain_Invoice{id = ID}) ->
    ID.

get_payment(#{payment := Payment}) ->
    Payment.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(change()) ->
    hg_msgpack_marshalling:value().

marshal(Change) ->
    marshal(change, Change).

marshal(change, ?refund_created(Refund, Cashflow)) ->
    [2, [<<"created">>, marshal(refund, Refund), hg_cashflow:marshal(Cashflow)]];
marshal(change, ?refund_status_changed(Status)) ->
    [2, [<<"status">>, marshal(status, Status)]];
marshal(change, ?session_ev(_Target, Payload)) ->
    [2, [<<"session">>, hg_proxy_provider_session:marshal(Payload)]];

marshal(refund, #domain_InvoicePaymentRefund{} = Refund) ->
    genlib_map:compact(#{
        <<"id">>         => marshal(str, Refund#domain_InvoicePaymentRefund.id),
        <<"created_at">> => marshal(str, Refund#domain_InvoicePaymentRefund.created_at),
        <<"rev">>        => marshal(int, Refund#domain_InvoicePaymentRefund.domain_revision),
        <<"reason">>     => marshal(str, Refund#domain_InvoicePaymentRefund.reason)
    });

marshal(status, ?refund_pending()) ->
    <<"pending">>;
marshal(status, ?refund_succeeded()) ->
    <<"succeeded">>;
marshal(status, ?refund_failed(Failure)) ->
    [<<"failed">>, marshal(failure, Failure)];

marshal(failure, {operation_timeout, _}) ->
    [2, <<"operation_timeout">>];
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [2, [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })]];

marshal(_, Other) ->
    Other.

-spec unmarshal(hg_msgpack_marshalling:value()) -> change().

unmarshal(Change) ->
    unmarshal(change, Change).

unmarshal(change, [2, [<<"created">>, Refund, Cashflow]]) ->
    ?refund_created(unmarshal(refund, Refund), hg_cashflow:unmarshal(Cashflow));
unmarshal(change, [2, [<<"status">>, Status]]) ->
    ?refund_status_changed(unmarshal(status, Status));
unmarshal(change, [2, [<<"session">>, Payload]]) ->
    ?session_ev(?refunded(), hg_proxy_provider_session:unmarshal(Payload));

unmarshal(refund, #{
    <<"id">>         := ID,
    <<"created_at">> := CreatedAt,
    <<"rev">>        := Rev
} = V) ->
    #domain_InvoicePaymentRefund{
        id              = unmarshal(str, ID),
        status          = ?refund_pending(),
        created_at      = unmarshal(str, CreatedAt),
        domain_revision = unmarshal(int, Rev),
        reason          = genlib_map:get(<<"reason">>, V)
    };

unmarshal(status, <<"pending">>) ->
    ?refund_pending();
unmarshal(status, <<"succeeded">>) ->
    ?refund_succeeded();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?refund_failed(unmarshal(failure, Failure));

unmarshal(failure, [2, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [2, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(failure, [1, ?legacy_operation_timeout()]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, ?legacy_external_failure(Code, Description)]) ->
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(_, Other) ->
    Other.