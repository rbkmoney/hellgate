-module(hg_payment_refund).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([init/2]).
-export([process_signal/3]).

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
-type invoice_id()        :: dmsl_domain_thrift:'InvoiceID'().
-type payment_id()        :: dmsl_domain_thrift:'InvoicePaymentID'().
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

-type change() ::
    dmsl_payment_processing_thrift:'InvoicePaymentRefundChangePayload'().

-include("payment_events.hrl").
-include("domain.hrl").

-spec get_refunds([st()]) -> [refund()].

get_refunds(Rs) ->
    lists:keysort(#domain_InvoicePaymentRefund.id, [R#st.refund || R <- Rs]).

-spec get_refund(st()) -> refund() | no_return().

get_refund(#st{refund = Refund}) ->
    Refund.

-spec init(refund_id(), [invoice_id() | payment_id() | integer() | refund_params()]) ->
    hg_machine:result().

init(RefundID, [InvoiceRef, PaymentRef, Revision, Params]) ->
    PaymentSt = hg_invoice:get_payment_by_refs(InvoiceRef, PaymentRef),
    Payment = hg_invoice_payment:get_payment(PaymentSt),
    Route = hg_invoice_payment:get_route(PaymentSt),
    Shop = hg_invoice:get_shop_by_ref(InvoiceRef),
    Party = hg_invoice:get_party_by_ref(InvoiceRef),
    Contract = hg_invoice:get_contract_by_ref(InvoiceRef),
    PaymentInstitution = get_payment_institution(Contract, Revision),
    Provider = hg_proxy_provider:get_route_provider(Route, Revision),
    Invoice = hg_invoice:get_invoice_by_ref(InvoiceRef),
    VS0 = collect_varset(Party, Shop, Payment, #{}),
    MerchantTerms = get_merchant_refunds_terms(get_merchant_payments_terms(Invoice, Contract, Revision)),
    VS1 = validate_refund(Payment, MerchantTerms, VS0, Revision),
    ProviderTerms = get_provider_refunds_terms(hg_routing:get_payments_terms(Route, Revision), Payment),
    Cashflow = collect_refund_cashflow(MerchantTerms, ProviderTerms, VS1, Revision),
    % TODO specific cashflow context needed, with defined `refund_amount` for instance
    AccountMap = collect_account_map(Payment, Shop, PaymentInstitution, Provider, VS1, Revision),
    FinalCashflow = construct_final_cashflow(Cashflow, collect_cash_flow_context(Payment), AccountMap),
    CreatedAt = hg_datetime:format_now(),
    Refund = construct_refund(RefundID, CreatedAt, Revision, Params),
    Changes = [
        ?refund_created(Refund, FinalCashflow),
        ?session_ev(?refunded(), ?session_started())
    ],
    RefundSt = collapse_changes(Changes, undefined),
    AffectedAccounts = prepare_refund_cashflow(RefundSt, Invoice, Payment),
    % NOTE we assume that posting involving merchant settlement account MUST be present in the cashflow
    case get_available_amount(get_account_state({merchant, settlement}, AccountMap, AffectedAccounts)) of
        % TODO we must pull this rule out of refund terms
        Available when Available >= 0 ->
            #{
                events => Changes,
                action => hg_machine_action:instant()
            };
        Available when Available < 0 ->
            _AffectedAccounts = rollback_refund_cashflow(RefundSt, Invoice, Payment),
            throw(#payproc_InsufficientAccountBalance{})
    end.

-spec process_signal(hg_machine:signal(), hg_machine:history(), hg_machine:auxst()) ->
    hg_machine:result().

process_signal(timeout, History, #{invoice_id := InvoiceID, payment_id := PaymentID}) ->
    Events = [unmarshal(Event) || Event <- History],
    St = collapse_changes(Events, undefined),
    PaymentSt = hg_invoice:get_payment_by_refs(InvoiceID, PaymentID),
    Opts = #{
        invoice => hg_invoice:get_invoice_by_ref(InvoiceID),
        party => hg_invoice:get_party_by_ref(InvoiceID),
        payment => hg_invoice_payment:get_payment(PaymentSt)
    },
    scoper:scope(
        refund,
        fun() -> handle_timeout(St#st{opts = Opts, payment_st = PaymentSt}) end
    ).

handle_timeout(#st{session = Session} = St) ->
    Action = hg_machine_action:new(),
    {Result, {Changes, Action}} = case hg_proxy_provider_session:get_status(Session) of
        active ->
            process(Action, St);
        suspended ->
            process_callback_timeout(Action, St)
    end,
    #{result => Result, events => Changes, action => Action}.

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
    Invoice = get_invoice(Opts),
    Payment = get_payment(Opts),
    case {SessionStatus, SessionResult} of
        {finished, ?session_succeeded()} ->
            _AffectedAccounts = commit_refund_cashflow(St1, Invoice, Payment),
            Events1 = [?refund_status_changed(?refund_succeeded())],
            {done, {Events0 ++ Events1, Action}};
        {finished, ?session_failed(Failure)} ->
            _AffectedAccounts = rollback_refund_cashflow(St1, Invoice, Payment),
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

prepare_refund_cashflow(St, Invoice, Payment) ->
    hg_accounting:plan(construct_refund_plan_id(St, Invoice, Payment), get_refund_cashflow_plan(St)).

commit_refund_cashflow(St, Invoice, Payment) ->
    hg_accounting:commit(construct_refund_plan_id(St, Invoice, Payment), [get_refund_cashflow_plan(St)]).

rollback_refund_cashflow(St, Invoice, Payment) ->
    hg_accounting:rollback(construct_refund_plan_id(St, Invoice, Payment), [get_refund_cashflow_plan(St)]).

construct_refund_plan_id(St, Invoice, Payment) ->
    hg_utils:construct_complex_id([
        get_invoice_id(Invoice),
        get_payment_id(Payment),
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

get_invoice_created_at(#domain_Invoice{created_at = Dt}) ->
    Dt.

get_payment(#{payment := Payment}) ->
    Payment.

get_payment_id(#domain_InvoicePayment{id = ID}) ->
    ID.

get_payment_cost(#domain_InvoicePayment{cost = Cost}) ->
    Cost.

get_payment_institution(Contract, Revision) ->
    PaymentInstitutionRef = Contract#domain_Contract.payment_institution,
    hg_domain:get(Revision, {payment_institution, PaymentInstitutionRef}).

get_payment_tool(#domain_InvoicePayment{payer = Payer}) ->
    get_payer_payment_tool(Payer).

get_payer_payment_tool(?payment_resource_payer(PaymentResource, _ContactInfo)) ->
    get_resource_payment_tool(PaymentResource);
get_payer_payment_tool(?customer_payer(_CustomerID, _, _, PaymentTool, _)) ->
    PaymentTool.

get_currency(#domain_Cash{currency = Currency}) ->
    Currency.

get_resource_payment_tool(#domain_DisposablePaymentResource{payment_tool = PaymentTool}) ->
    PaymentTool.

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

collect_varset(Party, Shop, Payment, VS) ->
    VS0 = collect_varset(Party, Shop, VS),
    VS0#{
        cost         => get_payment_cost(Payment),
        payment_tool => get_payment_tool(Payment)
    }.

get_merchant_payments_terms(Invoice, Contract, Revision) ->
    ok = assert_contract_active(Contract),
    TermSet = hg_party:get_terms(
        Contract,
        get_invoice_created_at(Invoice),
        Revision
    ),
    TermSet#domain_TermSet.payments.

get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = Terms}) when Terms /= undefined ->
    Terms;
get_merchant_refunds_terms(#domain_PaymentsServiceTerms{refunds = undefined}) ->
    throw(#payproc_OperationNotPermitted{}).

get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = Terms}, _Payment) when Terms /= undefined ->
    Terms;
get_provider_refunds_terms(#domain_PaymentsProvisionTerms{refunds = undefined}, Payment) ->
    error({misconfiguration, {'No refund terms for a payment', Payment}}).

validate_refund(Payment, RefundTerms, VS0, Revision) ->
    VS1 = validate_payment_tool(
        get_payment_tool(Payment),
        RefundTerms#domain_PaymentRefundsServiceTerms.payment_methods,
        VS0,
        Revision
    ),
    VS1.

validate_payment_tool(PaymentTool, PaymentMethodSelector, VS, Revision) ->
    PMs = reduce_selector(payment_methods, PaymentMethodSelector, VS, Revision),
    _ = ordsets:is_element(hg_payment_tool:get_method(PaymentTool), PMs) orelse
        throw_invalid_request(<<"Invalid payment method">>),
    VS#{payment_tool => PaymentTool}.

collect_refund_cashflow(
    #domain_PaymentRefundsServiceTerms{fees = MerchantCashflowSelector},
    #domain_PaymentRefundsProvisionTerms{cash_flow = ProviderCashflowSelector},
    VS,
    Revision
) ->
    MerchantCashflow = reduce_selector(merchant_refund_fees     , MerchantCashflowSelector, VS, Revision),
    ProviderCashflow = reduce_selector(provider_refund_cash_flow, ProviderCashflowSelector, VS, Revision),
    MerchantCashflow ++ ProviderCashflow.

collect_account_map(
    Payment,
    #domain_Shop{account = MerchantAccount},
    #domain_PaymentInstitution{system_account_set = SystemAccountSetSelector},
    #domain_Provider{accounts = ProviderAccounts},
    VS,
    Revision
) ->
    Currency = get_currency(get_payment_cost(Payment)),
    ProviderAccount = choose_provider_account(Currency, ProviderAccounts),
    SystemAccount = choose_system_account(SystemAccountSetSelector, Currency, VS, Revision),
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
    choose_account(provider, Currency, Accounts).

choose_system_account(SystemAccountSetSelector, Currency, VS, Revision) ->
    SystemAccountSetRef = reduce_selector(system_account_set, SystemAccountSetSelector, VS, Revision),
    SystemAccountSet = hg_domain:get(Revision, {system_account_set, SystemAccountSetRef}),
    choose_account(
        system,
        Currency,
        SystemAccountSet#domain_SystemAccountSet.accounts
    ).

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

choose_account(Name, Currency, Accounts) ->
    case maps:find(Currency, Accounts) of
        {ok, Account} ->
            Account;
        error ->
            error({misconfiguration, {'No account for a given currency', {Name, Currency}}})
    end.

construct_final_cashflow(Cashflow, Context, AccountMap) ->
    hg_cashflow:finalize(Cashflow, Context, AccountMap).

collect_cash_flow_context(
    #domain_InvoicePayment{cost = Cost}
) ->
    #{
        payment_amount => Cost
    }.

reduce_selector(Name, Selector, VS, Revision) ->
    case hg_selector:reduce(Selector, VS, Revision) of
        {value, V} ->
            V;
        Ambiguous ->
            error({misconfiguration, {'Could not reduce selector to a value', {Name, Ambiguous}}})
    end.

assert_contract_active(#domain_Contract{status = {active, _}}) ->
    ok;
assert_contract_active(#domain_Contract{status = Status}) ->
    throw(#payproc_InvalidContractStatus{status = Status}).

-spec throw_invalid_request(binary()) -> no_return().

throw_invalid_request(Why) ->
    throw(#'InvalidRequest'{errors = [Why]}).

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