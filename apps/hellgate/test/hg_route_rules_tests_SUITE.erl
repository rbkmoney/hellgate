-module(hg_route_rules_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([ruleset_ok/1]).
-export([empty_candidate_ok/1]).

-behaviour(supervisor).
-export([init/1]).

-define(dummy_party_id,        <<"dummy_party_id">>).
-define(dummy_shop_id,         <<"dummy_shop_id">>).
-define(dummy_another_shop_id, <<"dummy_another_shop_id">>).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-type config()         :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().
-type group_name()     :: hg_ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() -> [
    ruleset_ok,
    empty_candidate_ok
].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() -> [
].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    CowboySpec = hg_dummy_provider:get_http_cowboy_spec(),
    {Apps, _Ret} = hg_ct_helper:start_apps([
        woody, scoper, dmt_client, hellgate, {cowboy, CowboySpec}
    ]),
    ok = hg_domain:insert(construct_domain_fixture()),

    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    {ok, _} = supervisor:start_child(SupPid, hg_dummy_fault_detector:child_spec()),
    _ = unlink(SupPid),
    [{apps, Apps}, {test_sup, SupPid} | C].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    SupPid = cfg(test_sup, C),
    ok = supervisor:terminate_child(SupPid, hg_dummy_fault_detector),
    ok = hg_domain:cleanup().

-spec init_per_group(group_name(), config()) -> config().

init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_GroupName, C) ->
    case cfg(original_domain_revision, C) of
        Revision when is_integer(Revision) ->
            ok = hg_domain:reset(Revision);
        undefined ->
            ok
    end.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_, C) -> C.

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, _C) -> ok.

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).


-spec ruleset_ok(config()) -> test_return().

ruleset_ok(_C) ->
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(1000, <<"RUB">>),
        payment_tool    => {payment_terminal, #domain_PaymentTerminal{terminal_type = euroset}},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(1)}),

    {AcceptedRoute, RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),
    [{_, {?trm(10), _, _}}] = AcceptedRoute,
    [
        {?prv(3), ?trm(11), {'RoutingRule', undefined}},
        {?prv(2), ?trm(6),  {'PaymentsProvisionTerms', category}},
        {?prv(1), ?trm(1),  {'PaymentsProvisionTerms', payment_tool}}
    ] = RejectContext,
    ok.

-spec empty_candidate_ok(config()) -> test_return().

empty_candidate_ok(_C) ->
    BankCard = #domain_BankCard{
        token = <<"bank card token">>,
        payment_system = visa,
        bin = <<"411111">>,
        last_digits = <<"11">>
    },
    VS = #{
        category        => ?cat(1),
        currency        => ?cur(<<"RUB">>),
        cost            => ?cash(101010, <<"RUB">>),
        payment_tool    => {bank_card, BankCard},
        party_id        => <<"12345">>,
        risk_score      => low,
        flow            => instant
    },

    Revision = hg_domain:head(),
    PaymentInstitution = hg_domain:get(Revision, {payment_institution, ?pinst(2)}),
    {[], RejectContext} = hg_routing_rule:gather_routes(payment, PaymentInstitution, VS, Revision),
    [
        {?prv(1), ?trm(1),  {'RoutingRule', undefined}},
        {?prv(2), ?trm(6),  {'PaymentsProvisionTerms', category}}
    ] = RejectContext,
    ok.

%%% Domain config fixtures

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
                    if_   = ?partycond(<<"DEPRIVED ONE">>, undefined),
                    then_ = {value, ordsets:new()}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ?ordset([
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(bank_card, jcb),
                        ?pmt(payment_terminal, euroset),
                        ?pmt(digital_wallet, qiwi),
                        ?pmt(empty_cvv_bank_card, visa),
                        ?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))
                    ])}
                }
            ]},
            cash_limit = {decisions, [
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(     10, <<"RUB">>)},
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
                            ?share(45, 1000, operation_amount)
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
                        then_ = {value, #domain_HoldLifetime{seconds = 10}}
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
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {decisions, [
                        #domain_CashLimitDecision{
                            if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                            then_ = {value, ?cashrng(
                                {inclusive, ?cash(      1000, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    ]}

                }
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
                ?cat(3),
                ?cat(4),
                ?cat(5),
                ?cat(6)
            ])},
            payment_methods = {value, ?ordset([
                ?pmt(bank_card, visa),
                ?pmt(bank_card, mastercard)
            ])},
            cash_limit = {decisions, [
                % проверяем, что условие никогда не отрабатывает
                #domain_CashLimitDecision {
                    if_ = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                        definition = {empty_cvv_is, true}
                    }}}},
                    then_ = {value,
                        ?cashrng(
                            {inclusive, ?cash(0, <<"RUB">>)},
                            {inclusive, ?cash(0, <<"RUB">>)}
                        )
                    }
                },
                #domain_CashLimitDecision{
                    if_ = {condition, {currency_is, ?cur(<<"RUB">>)}},
                    then_ = {value, ?cashrng(
                        {inclusive, ?cash(       10, <<"RUB">>)},
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
                            ?share(45, 1000, operation_amount)
                        )
                    ]}
                },
                #domain_CashFlowDecision{
                    if_ = {condition, {currency_is, ?cur(<<"USD">>)}},
                    then_ = {value, [
                        ?cfpost(
                            {merchant, settlement},
                            {system, settlement},
                            ?share(65, 1000, operation_amount)
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
                ]},
                eligibility_time = {value, #'TimeSpan'{minutes = 1}},
                partial_refunds = #domain_PartialRefundsServiceTerms{
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash( 1000, <<"RUB">>)},
                        {exclusive, ?cash(40000, <<"RUB">>)}
                    )}
                }
            }
        }
    },
    Prohibitions = {delegates, [
        delegate(condition(payment_terminal, euroset), ?ruleset(4))
    ]},
    Decision1 = {delegates, [
        delegate(condition(party, <<"12345">>), ?ruleset(2)),
        delegate(condition(payment_terminal, euroset), ?ruleset(3))
    ]},

    Decision2 = {candidates, [
        candidate(condition(cost_in, {0, 500000, <<"RUB">>}), ?trm(1)),
        candidate(condition(cost_in, {0, 500000, <<"RUB">>}), ?trm(6))
    ]},
    Decision3 = {candidates, [
        candidate({constant, true}, ?trm(10)),
        candidate({constant, true}, ?trm(11))
    ]},
    Decision4 = {candidates, [
        candidate({constant, true}, ?trm(1)),
        candidate({constant, true}, ?trm(11))
    ]},
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_currency(?cur(<<"USD">>)),
        hg_ct_fixture:construct_currency(?cur(<<"EUR">>)),

        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>, test),
        hg_ct_fixture:construct_category(?cat(2), <<"Generic Store">>, live),
        hg_ct_fixture:construct_category(?cat(3), <<"Guns & Booze">>, live),
        hg_ct_fixture:construct_category(?cat(4), <<"Offliner">>, live),
        hg_ct_fixture:construct_category(?cat(5), <<"Timeouter">>, live),
        hg_ct_fixture:construct_category(?cat(6), <<"MachineFailer">>, live),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, jcb)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),
        hg_ct_fixture:construct_payment_method(?pmt(digital_wallet, qiwi)),
        hg_ct_fixture:construct_payment_method(?pmt(empty_cvv_bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))),

        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_proxy(?prx(2), <<"Inspector proxy">>),

        hg_ct_fixture:construct_inspector(?insp(1), <<"Rejector">>, ?prx(2), #{<<"risk_score">> => <<"low">>}),
        hg_ct_fixture:construct_inspector(?insp(2), <<"Skipper">>, ?prx(2), #{<<"risk_score">> => <<"high">>}),
        hg_ct_fixture:construct_inspector(?insp(3), <<"Fatalist">>, ?prx(2), #{<<"risk_score">> => <<"fatal">>}),
        hg_ct_fixture:construct_inspector(?insp(4), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}, low),
        hg_ct_fixture:construct_inspector(?insp(5), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"timeout">>}, low),
        hg_ct_fixture:construct_inspector(?insp(6), <<"Offliner">>, ?prx(2),
            #{<<"link_state">> => <<"unexpected_failure">>}),

        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(2), ?trms(2)),
        hg_ct_fixture:construct_contract_template(?tmpl(3), ?trms(3)),

        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_system_account_set(?sas(2)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(2), <<"Assist">>, ?cur(<<"RUB">>)),

        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(1), <<"Rule#1">>, Decision1),
        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(2), <<"Rule#2">>, Decision2),
        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(3), <<"Rule#3">>, Decision3),
        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(4), <<"Rule#4">>, Decision4),
        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(5), <<"ProhobitionRule#1">>, Prohibitions),
        hg_ct_fixture:construct_payment_routing_ruleset(?ruleset(6), <<"ProhobitionRule#2">>, Decision4),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3),
                    ?prv(4)
                ])},
                payment_routing = #domain_PaymentRouting{
                    policies = ?ruleset(1),
                    prohibitions = ?ruleset(5)
                },
                % TODO do we realy need this decision hell here?
                inspector = {decisions, []},
                residences = [],
                realm = test
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([
                    ?prv(1),
                    ?prv(2),
                    ?prv(3)
                ])},
                payment_routing = #domain_PaymentRouting{
                    policies = ?ruleset(2),
                    prohibitions = ?ruleset(6)
                },
                inspector = {decisions, []},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
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
                payment_institutions = ?ordset([?pinst(1), ?pinst(2)])
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
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(1),
                term_sets = []
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, ?ordset([
                    ?prvtrm(1)
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
                        ?pmt(bank_card, mastercard),
                        ?pmt(bank_card, jcb),
                        ?pmt(empty_cvv_bank_card, visa),
                        ?pmt(tokenized_bank_card, ?tkz_bank_card(visa, applepay))
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(      1000, <<"RUB">>)},
                        {exclusive, ?cash(1000000000, <<"RUB">>)}
                    )},
                    cash_flow = {decisions, [
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, visa}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(18, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, mastercard}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(19, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system_is, jcb}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        },
                        #domain_CashFlowDecision{
                            if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                definition = {payment_system, #domain_PaymentSystemCondition{
                                    payment_system_is = visa,
                                    token_provider_is = applepay
                                }}
                            }}}},
                            then_ = {value, [
                                ?cfpost(
                                    {provider, settlement},
                                    {merchant, settlement},
                                    ?share(1, 1, operation_amount)
                                ),
                                ?cfpost(
                                    {system, settlement},
                                    {provider, settlement},
                                    ?share(20, 1000, operation_amount)
                                )
                            ]}
                        }
                    ]},
                    holds = #domain_PaymentHoldsProvisionTerms{
                        lifetime = {decisions, [
                            #domain_HoldLifetimeDecision{
                                if_   = {condition, {payment_tool, {bank_card, #domain_BankCardCondition{
                                    definition = {payment_system_is, visa}
                                }}}},
                                then_ = {value, ?hold_lifetime(12)}
                            }
                        ]}
                    },
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
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
                provider_ref = ?prv(1),
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
                terminal = {value, [?prvtrm(6), ?prvtrm(7)]},
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
                        ?cat(2),
                        ?cat(4),
                        ?cat(5),
                        ?cat(6)
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
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
                        )
                    ]},
                    refunds = #domain_PaymentRefundsProvisionTerms{
                        cash_flow = {value, [
                            ?cfpost(
                                {merchant, settlement},
                                {provider, settlement},
                                ?share(1, 1, operation_amount)
                            )
                        ]},
                        partial_refunds = #domain_PartialRefundsProvisionTerms{
                            cash_limit = {value, ?cashrng(
                                {inclusive, ?cash(        10, <<"RUB">>)},
                                {exclusive, ?cash(1000000000, <<"RUB">>)}
                            )}
                        }
                    }
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(6),
            data = #domain_Terminal{
                provider_ref = ?prv(2),
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
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(16, 1000, operation_amount)
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
                provider_ref = ?prv(2),
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
                terminal = {value, [?prvtrm(10), ?prvtrm(11)]},
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
                        ?cur(<<"RUB">>),
                        ?cur(<<"EUR">>)
                    ])},
                    categories = {value, ?ordset([
                        ?cat(1)
                    ])},
                    payment_methods = {value, ?ordset([
                        ?pmt(payment_terminal, euroset),
                        ?pmt(bank_card, visa),
                        ?pmt(bank_card, mastercard),
                        ?pmt(bank_card, jcb),
                        ?pmt(digital_wallet, qiwi)
                    ])},
                    cash_limit = {value, ?cashrng(
                        {inclusive, ?cash(    1000, <<"RUB">>)},
                        {exclusive, ?cash(10000000, <<"RUB">>)}
                    )},
                    cash_flow = {value, [
                        ?cfpost(
                            {provider, settlement},
                            {merchant, settlement},
                            ?share(1, 1, operation_amount)
                        ),
                        ?cfpost(
                            {system, settlement},
                            {provider, settlement},
                            ?share(21, 1000, operation_amount)
                        )
                    ]}
                }
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(10),
            data = #domain_Terminal{
                provider_ref = ?prv(3),
                name = <<"Payment Terminal Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},
        {terminal, #domain_TerminalObject{
            ref = ?trm(11),
            data = #domain_Terminal{
                provider_ref = ?prv(3),
                name = <<"Second Payment Terminal">>,
                description = <<"Euroset">>,
                risk_coverage = low
            }
        }},

        {provider, #domain_ProviderObject{
            ref = ?prv(4),
            data = #domain_Provider{
                name = <<"Zrovider">>,
                description = <<"Non-configured provider">>,
                terminal = {value, []},
                proxy = #domain_Proxy{
                    ref = ?prx(1),
                    additional = #{
                        <<"override">> => <<"zrovider">>
                    }
                },
                abs_account = <<"0987654321">>,
                accounts = hg_ct_fixture:construct_provider_account_set([?cur(<<"RUB">>)]),
                payment_terms = #domain_PaymentsProvisionTerms{
                    currencies = undefined,
                    categories = undefined,
                    payment_methods = undefined,
                    cash_limit = undefined,
                    cash_flow = undefined
                }
            }
        }}
    ].

condition(cost_in, {Min, Max, Cur}) ->
    {condition, {cost_in, ?cashrng(
        {inclusive, ?cash(Min, Cur)},
        {exclusive, ?cash(Max, Cur)}
    )}};
condition(party, ID) ->
    {condition, {party, #domain_PartyCondition{id = ID}}};
condition(payment_terminal, Provider) ->
    {condition, {payment_tool, {payment_terminal, #domain_PaymentTerminalCondition{
        definition = {provider_is, Provider}
    }}}}.

delegate(Allowed, RuleSetRef) ->
    delegate(undefined, Allowed, RuleSetRef).
delegate(Descr, Allowed, RuleSetRef) ->
    #domain_PaymentRoutingDelegate{
        description = Descr,
        allowed = Allowed,
        ruleset = RuleSetRef
    }.

candidate(Allowed, Terminal) ->
    candidate(undefined, Allowed, Terminal).
candidate(Descr, Allowed, Terminal) ->
    #domain_PaymentRoutingCandidate{
        description = Descr,
        allowed = Allowed,
        terminal = Terminal
    }.
