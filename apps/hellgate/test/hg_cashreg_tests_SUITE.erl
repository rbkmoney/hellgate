-module(hg_cashreg_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([receipt_registration_success/1]).
-export([receipt_registration_failed/1]).
-export([receipt_registration_success_suspend/1]).

%%

-behaviour(supervisor).
-export([init/1]).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.


%% tests descriptions

-type config()         :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name()     :: hg_ct_helper:group_name().
-type test_return()    :: _ | no_return().

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        receipt_registration_success,
        receipt_registration_failed,
        receipt_registration_success_suspend
    ].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    CowboySpec1 = hg_dummy_provider:get_http_cowboy_spec(),
    CowboySpec2 = hg_dummy_cashreg_provider:get_http_cowboy_spec(),
    {Apps, Ret} = hg_ct_helper:start_apps(
        [lager, woody, scoper, dmt_client, hellgate, {cowboy, CowboySpec1}, {cowboy, CowboySpec2}]
    ),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    PartyClient = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    CustomerClient = hg_client_customer:start(hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(PartyClient),
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    NewC = [
        {party_id, PartyID},
        {party_client, PartyClient},
        {customer_client, CustomerClient},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps},
        {test_sup, SupPid}
        | C
    ],
    ok = start_proxies([
        {hg_dummy_provider, 1, NewC},
        {hg_dummy_inspector, 2, NewC},
        {hg_dummy_cashreg_provider, 3, NewC}
    ]),
    NewC.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)],
    exit(cfg(test_sup, C), shutdown).

%% tests
-include("invoice_events.hrl").
-include("payment_events.hrl").

-define(invoice(ID), #domain_Invoice{id = ID}).
-define(payment(ID), #domain_InvoicePayment{id = ID}).
-define(invoice_state(Invoice), #payproc_Invoice{invoice = Invoice}).
-define(invoice_state(Invoice, Payments), #payproc_Invoice{invoice = Invoice, payments = Payments}).
-define(payment_state(Payment), #payproc_InvoicePayment{payment = Payment}).
-define(invoice_w_status(Status), #domain_Invoice{status = Status}).
-define(payment_w_status(Status), #domain_InvoicePayment{status = Status}).
-define(payment_w_status(ID, Status), #domain_InvoicePayment{id = ID, status = Status}).
-define(trx_info(ID), #domain_TransactionInfo{id = ID}).

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    init_per_testcase(C).

init_per_testcase(C) ->
    ApiClient = hg_ct_helper:create_client(cfg(root_url, C), cfg(party_id, C)),
    Client = hg_client_invoicing:start_link(ApiClient),
    ClientTpl = hg_client_invoice_templating:start_link(ApiClient),
    [{client, Client}, {client_tpl, ClientTpl} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, C) ->
    _ = case cfg(original_domain_revision, C) of
        Revision when is_integer(Revision) ->
            ok = hg_domain:reset(Revision);
        undefined ->
            ok
    end.

-spec receipt_registration_success(config()) -> test_return().

receipt_registration_success(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopCashRegister = #domain_ShopCashRegister{
        ref = ?cashreg(1),
        tax_system = osn,
        options = #{<<"adapter_state">> => <<"sleeping">>}
    },
    ShopID = hg_ct_helper:create_battle_ready_shop_with_cashreg(?cat(2), ?tmpl(2), PartyClient, ShopCashRegister),
    InvoiceParams = make_invoice_params(PartyID, ShopID),
    InvoiceID = create_invoice(InvoiceParams, Client),

    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_receipt_created(InvoiceID, PaymentID, Client),
    PaymentID = await_receipt_registered(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).

-spec receipt_registration_failed(config()) -> test_return().

receipt_registration_failed(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    ShopCashRegister = #domain_ShopCashRegister{
        ref = ?cashreg(1),
        tax_system = osn,
        options = #{<<"adapter_state">> => <<"finishing_failure">>}
    },
    ShopID = hg_ct_helper:create_battle_ready_shop_with_cashreg(?cat(2), ?tmpl(2), PartyClient, ShopCashRegister),
    InvoiceParams = make_invoice_params(PartyID, ShopID),
    InvoiceID = create_invoice(InvoiceParams, Client),

    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_receipt_created(InvoiceID, PaymentID, Client),
    PaymentID = await_receipt_failed(InvoiceID, PaymentID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled_with_reason(Reason), ?session_started()))
    ] = next_event(InvoiceID, Client),
    [
        ?payment_ev(PaymentID, ?session_ev(?cancelled_with_reason(Reason), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?failed(_)))
    ] = next_event(InvoiceID, Client).

-spec receipt_registration_success_suspend(config()) -> test_return().

receipt_registration_success_suspend(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    PartyID = cfg(party_id, C),
    Tag = hg_utils:unique_id(),
    ShopCashRegister = #domain_ShopCashRegister{
        ref = ?cashreg(1),
        tax_system = osn,
        options = #{<<"adapter_state">> => <<"suspending">>, <<"tag">> => Tag, <<"callback">> => <<"ok">>}
    },
    ShopID = hg_ct_helper:create_battle_ready_shop_with_cashreg(?cat(2), ?tmpl(2), PartyClient, ShopCashRegister),
    InvoiceParams = make_invoice_params(PartyID, ShopID),
    InvoiceID = create_invoice(InvoiceParams, Client),

    [?invoice_created(?invoice_w_status(?invoice_unpaid()))] = next_event(InvoiceID, Client),
    PaymentParams = make_payment_params(),
    PaymentID = process_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_receipt_created(InvoiceID, PaymentID, Client),
        timer:sleep(1000),
    _ = assert_success_post_request({hg_dummy_cashreg_provider:get_callback_url(), #{<<"tag">> => Tag}}),
    PaymentID = await_receipt_registered(InvoiceID, PaymentID, Client),
    PaymentID = await_payment_capture(InvoiceID, PaymentID, Client),
    ?invoice_state(
        ?invoice_w_status(?invoice_paid()),
        [?payment_state(?payment_w_status(PaymentID, ?captured()))]
    ) = hg_client_invoicing:get(InvoiceID, Client).
%%

next_event(InvoiceID, Client) ->
    next_event(InvoiceID, 5000, Client).

next_event(InvoiceID, Timeout, Client) ->
    case hg_client_invoicing:pull_event(InvoiceID, Timeout, Client) of
        {ok, ?invoice_ev(Changes)} ->
            case filter_changes(Changes) of
                L when length(L) > 0 ->
                    L;
                [] ->
                    next_event(InvoiceID, Timeout, Client)
            end;
        Result ->
            Result
    end.

filter_changes(Changes) ->
    lists:filtermap(fun filter_change/1, Changes).

filter_change(?payment_ev(_, C)) ->
    filter_change(C);
filter_change(?refund_ev(_, C)) ->
    filter_change(C);
filter_change(?session_ev(_, ?proxy_st_changed(_))) ->
    false;
filter_change(?session_ev(_, ?session_suspended(_))) ->
    false;
filter_change(?session_ev(_, ?session_activated())) ->
    false;
filter_change(_) ->
    true.

%%

start_service_handler(Module, C, HandlerOpts) ->
    start_service_handler(Module, Module, C, HandlerOpts).

start_service_handler(Name, Module, C, HandlerOpts) ->
    IP = "127.0.0.1",
    Port = get_random_port(),
    Opts = maps:merge(HandlerOpts, #{hellgate_root_url => cfg(root_url, C)}),
    ChildSpec = hg_test_proxy:get_child_spec(Name, Module, IP, Port, Opts),
    {ok, _} = supervisor:start_child(cfg(test_sup, C), ChildSpec),
    hg_test_proxy:get_url(Module, IP, Port).

start_proxies(Proxies) ->
    setup_proxies(lists:map(
        fun
            Mapper({Module, ProxyID, Context}) ->
                Mapper({Module, ProxyID, #{}, Context});
            Mapper({Module, ProxyID, ProxyOpts, Context}) ->
                construct_proxy(ProxyID, start_service_handler(Module, Context, #{}), ProxyOpts)
        end,
        Proxies
    )).

setup_proxies(Proxies) ->
    ok = hg_domain:upsert(Proxies).

get_random_port() ->
    rand:uniform(32768) + 32767.

construct_proxy(ID, Url, Options) ->
    {proxy, #domain_ProxyObject{
        ref = ?prx(ID),
        data = #domain_ProxyDefinition{
            name              = Url,
            description       = Url,
            url               = Url,
            options           = Options
        }
    }}.

%%

make_invoice_params(PartyID, ShopID) ->
    #payproc_InvoiceParams{
        party_id = PartyID,
        shop_id  = ShopID,
        details  = #domain_InvoiceDetails{
            product = <<"Some product">>,
            cart = #domain_InvoiceCart{
                lines = [
                    #domain_InvoiceLine{
                        product = <<"Some product">>,
                        quantity = 10,
                        price = hg_ct_helper:make_cash(420000, <<"RUB">>),
                        tax = {vat, vat0},
                        metadata = #{}
                    }
                ]
            }
        },
        due      = hg_datetime:format_ts(make_due_date(10)),
        cost     = hg_ct_helper:make_cash(42000, <<"RUB">>),
        context  = hg_ct_helper:make_invoice_context()
    }.

make_payment_params() ->
    make_payment_params(instant).

make_payment_params(FlowType) ->
    {PaymentTool, Session} = hg_ct_helper:make_simple_payment_tool(),
    make_payment_params(PaymentTool, Session, FlowType).

make_payment_params(PaymentTool, Session, FlowType) ->
    Flow = case FlowType of
        instant ->
            {instant, #payproc_InvoicePaymentParamsFlowInstant{}};
        {hold, OnHoldExpiration} ->
            {hold, #payproc_InvoicePaymentParamsFlowHold{on_hold_expiration = OnHoldExpiration}}
    end,
    #payproc_InvoicePaymentParams{
        payer = {payment_resource, #payproc_PaymentResourcePayerParams{
            resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool,
                payment_session_id = Session,
                client_info = #domain_ClientInfo{}
            },
            contact_info = #domain_ContactInfo{}
        }},
        flow = Flow
    }.

make_due_date(LifetimeSeconds) ->
    genlib_time:unow() + LifetimeSeconds.

create_invoice(InvoiceParams, Client) ->
    ?invoice_state(?invoice(InvoiceID)) = hg_client_invoicing:create(InvoiceParams, Client),
    InvoiceID.

start_payment(InvoiceID, PaymentParams, Client) ->
    ?payment_state(?payment(PaymentID)) = hg_client_invoicing:start_payment(InvoiceID, PaymentParams, Client),
    [
        ?payment_ev(PaymentID, ?payment_started(?payment_w_status(?pending()))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_started()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

process_payment(InvoiceID, PaymentParams, Client) ->
    PaymentID = start_payment(InvoiceID, PaymentParams, Client),
    PaymentID = await_payment_process_finish(InvoiceID, PaymentID, Client).

await_payment_process_finish(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?trx_bound(?trx_info(_)))),
        ?payment_ev(PaymentID, ?session_ev(?processed(), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?processed()))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_receipt_created(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?receipt_ev(_, ?receipt_created(), _))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_receipt_registered(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?receipt_ev(_, ?receipt_registered(), _))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_receipt_failed(InvoiceID, PaymentID, Client) ->
    [
        ?payment_ev(PaymentID, ?receipt_ev(_, ?receipt_failed(_), _))
    ] = next_event(InvoiceID, Client),
    PaymentID.

await_payment_capture(InvoiceID, PaymentID, Client) ->
    await_payment_capture(InvoiceID, PaymentID, undefined, Client).

await_payment_capture(InvoiceID, PaymentID, Reason, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured_with_reason(Reason), ?session_started()))
    ] = next_event(InvoiceID, Client),
    await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client).

await_payment_capture_finish(InvoiceID, PaymentID, Reason, Client) ->
    [
        ?payment_ev(PaymentID, ?session_ev(?captured_with_reason(Reason), ?session_finished(?session_succeeded()))),
        ?payment_ev(PaymentID, ?payment_status_changed(?captured_with_reason(Reason))),
        ?invoice_status_changed(?invoice_paid())
    ] = next_event(InvoiceID, Client),
    PaymentID.

assert_success_post_request(Req) ->
    {ok, 200, _RespHeaders, _ClientRef} = post_request(Req).

% assert_invalid_post_request(Req) ->
%     {ok, 400, _RespHeaders, _ClientRef} = post_request(Req).

post_request({URL, Form}) ->
    Method = post,
    Headers = [],
    Body = {form, maps:to_list(Form)},
    hackney:request(Method, URL, Headers, Body).

construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>)
            ])},
            categories = {value, ?ordset([
                ?cat(1)
            ])},
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = ?partycond(<<"DEPRIVED ONE">>, {shop_is, <<"TESTSHOP">>}),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(payment_terminal, euroset)
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
                        {exclusive, ?cash(420000000, <<"RUB">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 3}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                fees = {value, [
                    ?cfpost(
                        {merchant, settlement},
                        {system, settlement},
                        ?fixed(100, <<"RUB">>)
                    )
                ]}
            }
        },
        recurrent_paytools = #domain_RecurrentPaytoolsServiceTerms{
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ?ordset([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ?ordset([
                ?cat(2),
                ?cat(3)
            ])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     1000, <<"RUB">>)},
                        {exclusive, ?cash(  4200000, <<"RUB">>)}
                    )}
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(      200, <<"USD">>)},
                        {exclusive, ?cash(   313370, <<"USD">>)}
                    )}
                }
            ]},
            fees = {decisions, [
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(45, 1000, payment_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, payment_amount)
                        )
                    ]}
                }
            ]},
            holds = #domain_PaymentHoldsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                lifetime = {decisions, [
                    #domain_HoldLifetimeDecision{
                        if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {value, #domain_HoldLifetime{seconds = 3}}
                    }
                ]}
            },
            refunds = #domain_PaymentRefundsServiceTerms{
                payment_methods = {value, ?ordset([
                    ?pmt(bank_card, visa),
                    ?pmt(bank_card, mastercard)
                ])},
                fees = {value, [
                ]}
            }
        }
    },
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        hg_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),
        hg_ct_fixture:construct_proxy(?prx(3), <<"Cashreg proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),
        hg_ct_fixture:construct_inspector(?insp(3), <<"Fatalist">>, ?prx(2), #{<<"risk_score">> => <<"fatal">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        hg_ct_fixture:construct_cashreg(?cashreg(1), <<"Test cash register">>, #domain_Proxy{
                    ref = ?prx(3),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                }),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3)
                ])},
                system_account_set = {value, ?sas(1)},
                external_account_set = {decisions, [
                    #domain_ExternalAccountSetDecision{
                        if_ = {condition, {party, #domain_PartyCondition{
                            id = <<"LGBT">>
                        }}},
                        then_ = {value, ?eas(2)}
                    },
                    #domain_ExternalAccountSetDecision{
                        if_ = {constant, true},
                        then_ = {value, ?eas(1)}
                    }
                ]},
                default_contract_template = ?tmpl(2),
                inspector = {decisions, [
                    #domain_InspectorDecision{
                        if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                        then_ = {decisions, [
                            #domain_InspectorDecision{
                                if_ = {condition, {category_is, ?cat(3)}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(        0, <<"RUB">>)},
                                    {exclusive, ?cash(   500000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(1)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash(   500000, <<"RUB">>)},
                                    {exclusive, ?cash(100000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(2)}
                            },
                            #domain_InspectorDecision{
                                if_ = {condition, {cost_in, ?cashrng(
                                    {inclusive, ?cash( 100000000, <<"RUB">>)},
                                    {exclusive, ?cash(1000000000, <<"RUB">>)}
                                )}},
                                then_ = {value, ?insp(3)}
                            }
                        ]}
                    }
                ]}
            }
        }},
        {party_prototype, #domain_PartyPrototypeObject{
            ref = #domain_PartyPrototypeRef{id = 42},
            data = #domain_PartyPrototype{
                shop = #domain_ShopPrototype{
                    shop_id = <<"TESTSHOP">>,
                    category = ?cat(1),
                    currency = ?cur(<<"RUB">>),
                    details  = #domain_ShopDetails{
                        name = <<"SUPER DEFAULT SHOP">>
                    },
                    location = {url, <<"">>}
                },
                contract = #domain_ContractPrototype{
                    contract_id = <<"TESTCONTRACT">>,
                    test_contract_template = ?tmpl(1),
                    payout_tool = #domain_PayoutToolPrototype{
                        payout_tool_id = <<"TESTPAYOUTTOOL">>,
                        payout_tool_info = {bank_account, #domain_BankAccount{
                            account = <<"">>,
                            bank_name = <<"">>,
                            bank_post_account = <<"">>,
                            bank_bik = <<"">>
                        }},
                        payout_tool_currency = ?cur(<<"RUB">>)
                    }
                }
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TestTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = DefaultTermSet
                }]
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?trm(1)
                ])},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"brovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, {payment_system_is, visa}}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, payment_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(18, 1000, payment_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, {payment_system_is, mastercard}}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, payment_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(19, 1000, payment_amount)
                                )
                            ]}
                        }
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, {payment_system_is, visa}}}},
                                then_ = {value, ?hold_lifetime(5)}
                            }
                        ]}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, payment_amount)
                            )
                        ]}
                    }
                },
                recurrent_paytool_terms = #domain_RecurrentPaytoolsProvisionTerms{
                    categories = {value, ?ordset([?cat(1)])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_value = {value, ?cash(1000, <<"RUB">>)}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                risk_coverage = high
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(2),
            data = #domain_Provider{
                name = <<"Drovider">>,
                description = <<"I'm out of ideas of what to write here">>,
                terminal = {value, [?trm(6), ?trm(7)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"drovider">>
                    }
                },
                abs_account = <<"1234567890">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, payment_amount)
                        )
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, payment_amount)
                            )
                        ]}
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                name = <<"Drominal 1">>,
                description = <<"Drominal 1">>,
                risk_coverage = low,
                terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(2)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(bank_card, visa)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash( 5000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {external, outcome},
                            ?fixed(20, <<"RUB">>),
                            <<"Assist fee">>
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(7),
            data = #domain_Terminal{
                name = <<"Terminal 7">>,
                description = <<"Terminal 7">>,
                risk_coverage = high
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(3),
            data = #domain_Provider{
                name = <<"Crovider">>,
                description = <<"Payment terminal provider">>,
                terminal = {value, [?trm(10)]},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"crovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = {value, ?ordset([
                        ?cur(<<"RUB">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, payment_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, payment_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(10),
            data = #domain_Terminal{
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }}
    ].
