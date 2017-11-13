-module(hg_invoice_template_tests_SUITE).

-include("hg_ct_domain.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("hellgate/include/domain.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_invalid_party/1]).
-export([create_invalid_shop/1]).
-export([create_invalid_party_status/1]).
-export([create_invalid_shop_status/1]).
-export([create_invalid_cost_fixed_amount/1]).
-export([create_invalid_cost_fixed_currency/1]).
-export([create_invalid_cost_range/1]).
-export([create_invoice_template/1]).
-export([get_invoice_template_anyhow/1]).
-export([update_invalid_party_status/1]).
-export([update_invalid_shop_status/1]).
-export([update_invalid_cost_fixed_amount/1]).
-export([update_invalid_cost_fixed_currency/1]).
-export([update_invalid_cost_range/1]).
-export([update_invoice_template/1]).
-export([update_with_cart/1]).
-export([delete_invalid_party_status/1]).
-export([delete_invalid_shop_status/1]).
-export([delete_invoice_template/1]).
-export([terms_retrieval/1]).

%% tests descriptions

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-define(MISSING_PARTY_ID, <<"42">>).
-define(MISSING_SHOP_ID, <<"42">>).

-define(invoice_tpl(ID), #domain_InvoiceTemplate{id = ID}).

cfg(Key, C) ->
    hg_ct_helper:cfg(Key, C).

-spec all() -> [test_case_name()].

all() ->
    [
        create_invalid_party,
        create_invalid_shop,
        create_invalid_party_status,
        create_invalid_shop_status,
        create_invalid_cost_fixed_amount,
        create_invalid_cost_fixed_currency,
        create_invalid_cost_range,
        create_invoice_template,
        get_invoice_template_anyhow,
        update_invalid_party_status,
        update_invalid_shop_status,
        update_invalid_cost_fixed_amount,
        update_invalid_cost_fixed_currency,
        update_invalid_cost_range,
        update_invoice_template,
        update_with_cart,
        delete_invalid_party_status,
        delete_invalid_shop_status,
        delete_invoice_template,
        terms_retrieval
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_client_party', '_', '_'}, x),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, scoper, dmt_client, hellgate]),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    Client = hg_client_party:start(PartyID, hg_ct_helper:create_client(RootUrl, PartyID)),
    ShopID = hg_ct_helper:create_party_and_shop(Client),
    [
        {party_id, PartyID},
        {party_client, Client},
        {shop_id, ShopID},
        {root_url, RootUrl},
        {apps, Apps}
        | C
    ].

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = hg_domain:cleanup(),
    [application:stop(App) || App <- cfg(apps, C)].

%% tests

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(_Name, C) ->
    RootUrl = cfg(root_url, C),
    PartyID = cfg(party_id, C),
    Client = hg_client_invoice_templating:start_link(hg_ct_helper:create_client(RootUrl, PartyID)),
    [{client, Client} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

-spec create_invalid_party(config()) -> _ | no_return().

create_invalid_party(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = ?MISSING_PARTY_ID,
    Params = make_invoice_tpl_create_params(PartyID, ShopID),
    {exception, #payproc_InvalidUser{}} = hg_client_invoice_templating:create(Params, Client).

-spec create_invalid_shop(config()) -> _ | no_return().

create_invalid_shop(C) ->
    Client = cfg(client, C),
    ShopID = ?MISSING_SHOP_ID,
    PartyID = cfg(party_id, C),
    Params = make_invoice_tpl_create_params(PartyID, ShopID),
    {exception, #payproc_ShopNotFound{}} = hg_client_invoice_templating:create(Params, Client).

-spec create_invalid_party_status(config()) -> _ | no_return().

create_invalid_party_status(C) ->
    PartyClient = cfg(party_client, C),

    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = create_invoice_tpl(C),
    ok = hg_client_party:activate(PartyClient),

    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = create_invoice_tpl(C),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec create_invalid_shop_status(config()) -> _ | no_return().

create_invalid_shop_status(C) ->
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),

    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = create_invoice_tpl(C),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),

    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = create_invoice_tpl(C),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec create_invalid_cost_fixed_amount(config()) -> _ | no_return().

create_invalid_cost_fixed_amount(C) ->
    Cost = make_cost(fixed, -100, <<"RUB">>),
    create_invalid_cost(Cost, amount, C).

-spec create_invalid_cost_fixed_currency(config()) -> _ | no_return().

create_invalid_cost_fixed_currency(C) ->
    Cost = make_cost(fixed, 100, <<"KEK">>),
    create_invalid_cost(Cost, currency, C).

-spec create_invalid_cost_range(config()) -> _ | no_return().

create_invalid_cost_range(C) ->
    Cost1 = make_cost(range, {exclusive, 100, <<"RUB">>}, {exclusive, 100, <<"RUB">>}),
    create_invalid_cost(Cost1, <<"Invalid cost range">>, C),

    Cost2 = make_cost(range, {inclusive, 10000, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    create_invalid_cost(Cost2, <<"Invalid cost range">>, C),

    Cost3 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"KEK">>}),
    create_invalid_cost(Cost3, <<"Invalid cost range">>, C),

    Cost4 = make_cost(range, {inclusive, 100, <<"KEK">>}, {inclusive, 10000, <<"KEK">>}),
    create_invalid_cost(Cost4, currency, C),

    Cost5 = make_cost(range, {inclusive, -100, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    create_invalid_cost(Cost5, amount, C).


-spec create_invoice_template(config()) -> _ | no_return().

create_invoice_template(C) ->
    ok = create_cost(make_cost(unlim, sale, "50%"), C),
    ok = create_cost(make_cost(fixed, 42, <<"RUB">>), C),
    ok = create_cost(make_cost(range, {inclusive, 42, <<"RUB">>}, {inclusive, 42, <<"RUB">>}), C),
    ok = create_cost(make_cost(range, {inclusive, 42, <<"RUB">>}, {inclusive, 100, <<"RUB">>}), C).

create_cost(Cost, C) ->
    Product = <<"rubberduck">>,
    Details = hg_ct_helper:make_invoice_tpl_details(Product, Cost),
    Lifetime = make_lifetime(0, 0, 2),
    #domain_InvoiceTemplate{
        product = Product,
        invoice_lifetime = Lifetime,
        details = Details
    } = create_invoice_tpl(C, Product, Lifetime, Cost),
    ok.

-spec get_invoice_template_anyhow(config()) -> _ | no_return().

get_invoice_template_anyhow(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    InvoiceTpl = ?invoice_tpl(TplID) = create_invoice_tpl(C),

    ok = hg_client_party:suspend(PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    ok = hg_client_party:activate(PartyClient),

    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient),

    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),

    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client).

-spec update_invalid_party_status(config()) -> _ | no_return().

update_invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    Diff = make_invoice_tpl_update_params(
        #{details => hg_ct_helper:make_invoice_tpl_details(<<"teddy bear">>, make_cost(fixed, 42, <<"RUB">>))}
    ),
    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    ok = hg_client_party:activate(PartyClient),

    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec update_invalid_shop_status(config()) -> _ | no_return().

update_invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    Diff = make_invoice_tpl_update_params(
        #{details => hg_ct_helper:make_invoice_tpl_details(<<"teddy bear">>, make_cost(fixed, 42, <<"RUB">>))}
    ),
    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),

    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec update_invalid_cost_fixed_amount(config()) -> _ | no_return().

update_invalid_cost_fixed_amount(C) ->
    Client = cfg(client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    Cost = make_cost(fixed, -100, <<"RUB">>),
    update_invalid_cost(Cost, amount, TplID, Client).

-spec update_invalid_cost_fixed_currency(config()) -> _ | no_return().

update_invalid_cost_fixed_currency(C) ->
    Client = cfg(client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    Cost = make_cost(fixed, 100, <<"KEK">>),
    update_invalid_cost(Cost, currency, TplID, Client).

-spec update_invalid_cost_range(config()) -> _ | no_return().

update_invalid_cost_range(C) ->
    Client = cfg(client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),

    Cost1 = make_cost(range, {exclusive, 100, <<"RUB">>}, {exclusive, 100, <<"RUB">>}),
    update_invalid_cost(Cost1, <<"Invalid cost range">>, TplID, Client),

    Cost2 = make_cost(range, {inclusive, 10000, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    update_invalid_cost(Cost2, <<"Invalid cost range">>, TplID, Client),

    Cost3 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"KEK">>}),
    update_invalid_cost(Cost3, <<"Invalid cost range">>, TplID, Client),

    Cost4 = make_cost(range, {inclusive, 100, <<"KEK">>}, {inclusive, 10000, <<"KEK">>}),
    update_invalid_cost(Cost4, currency, TplID, Client),

    Cost5 = make_cost(range, {inclusive, -100, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    update_invalid_cost(Cost5, amount, TplID, Client).

-spec update_invoice_template(config()) -> _ | no_return().

update_invoice_template(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    NewProduct = <<"teddy bear">>,
    CostUnlim = make_cost(unlim, sale, "50%"),
    NewDetails = hg_ct_helper:make_invoice_tpl_details(NewProduct, CostUnlim),
    NewLifetime = make_lifetime(10, 32, 51),
    Diff1 = make_invoice_tpl_update_params(#{
        details => NewDetails,
        product => NewProduct,
        invoice_lifetime => NewLifetime
    }),
    Tpl1 = #domain_InvoiceTemplate{
        id = TplID,
        owner_id = PartyID,
        shop_id = ShopID,
        product = NewProduct,
        details = NewDetails,
        invoice_lifetime = NewLifetime
    } = hg_client_invoice_templating:update(TplID, Diff1, Client),

    Tpl2 = update_cost(make_cost(fixed, 42, <<"RUB">>), Tpl1, Client),
    Tpl3 = update_cost(make_cost(range, {inclusive, 42, <<"RUB">>}, {inclusive, 42, <<"RUB">>}), Tpl2, Client),
    _    = update_cost(make_cost(range, {inclusive, 42, <<"RUB">>}, {inclusive, 100, <<"RUB">>}), Tpl3, Client).

update_cost(Cost, Tpl, Client) ->
    {product, #domain_InvoiceTemplateProduct{product = Product}} = Tpl#domain_InvoiceTemplate.details,
    NewDetails = hg_ct_helper:make_invoice_tpl_details(Product, Cost),
    TplNext = Tpl#domain_InvoiceTemplate{details = NewDetails},
    TplNext = hg_client_invoice_templating:update(
        Tpl#domain_InvoiceTemplate.id,
        make_invoice_tpl_update_params(#{details => NewDetails}),
        Client
    ).

-spec update_with_cart(config()) -> _ | no_return().

update_with_cart(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    NewDetails = {cart, #domain_InvoiceCart{lines = [
        #domain_InvoiceLine{
            product = <<"Awesome staff #1">>,
            quantity = 2,
            price = ?cash(1000, <<"RUB">>),
            metadata = #{}
        },
        #domain_InvoiceLine{
            product = <<"Awesome staff #2">>,
            quantity = 1,
            price = ?cash(10000, <<"RUB">>),
            metadata = #{<<"SomeKey">> => {b, true}}
        }
    ]}},
    Diff = make_invoice_tpl_update_params(#{
        details => NewDetails
    }),
    #domain_InvoiceTemplate{
        id = TplID,
        owner_id = PartyID,
        shop_id = ShopID,
        details = NewDetails
    } = hg_client_invoice_templating:update(TplID, Diff, Client),
    #domain_InvoiceTemplate{} = hg_client_invoice_templating:get(TplID, Client).

-spec delete_invalid_party_status(config()) -> _ | no_return().

delete_invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),

    ok = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    ok = hg_client_party:activate(PartyClient),

    ok = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    ok = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec delete_invalid_shop_status(config()) -> _ | no_return().

delete_invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),

    ok = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    ok = hg_client_party:activate_shop(ShopID, PartyClient),

    ok = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    ok = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec delete_invoice_template(config()) -> _ | no_return().

delete_invoice_template(C) ->
    Client = cfg(client, C),
    ?invoice_tpl(TplID) = create_invoice_tpl(C),
    ok = hg_client_invoice_templating:delete(TplID, Client),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:get(TplID, Client),
    Diff = make_invoice_tpl_update_params(#{}),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:update(TplID, Diff, Client),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:delete(TplID, Client).

-spec terms_retrieval(config()) -> _ | no_return().

terms_retrieval(C) ->
    Client = cfg(client, C),
    ?invoice_tpl(TplID1) = create_invoice_tpl(C),
    Timestamp = hg_datetime:format_now(),
    TermSet1 = hg_client_invoice_templating:compute_terms(TplID1, Timestamp, Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = undefined
    }} = TermSet1,
    ok = hg_domain:update(construct_term_set_for_cost(5000, 11000)),
    TermSet2 = hg_client_invoice_templating:compute_terms(TplID1, Timestamp, Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {value, [?pmt(bank_card, mastercard), ?pmt(bank_card, visa), ?pmt(payment_terminal, euroset)]}
    }} = TermSet2,
    Lifetime = make_lifetime(0, 0, 2),
    Cost = make_cost(unlim, sale, "1%"),
    ?invoice_tpl(TplID2) = create_invoice_tpl(C, <<"rubberduck">>, Lifetime, Cost),
    TermSet3 = hg_client_invoice_templating:compute_terms(TplID2, Timestamp, Client),
    #domain_TermSet{payments = #domain_PaymentsServiceTerms{
        payment_methods = {decisions, _}
    }} = TermSet3.

%%

create_invoice_tpl(Config) ->
    Client = cfg(client, Config),
    ShopID = cfg(shop_id, Config),
    PartyID = cfg(party_id, Config),
    Params = make_invoice_tpl_create_params(PartyID, ShopID),
    hg_client_invoice_templating:create(Params, Client).

create_invoice_tpl(Config, Product, Lifetime, Cost) ->
    Client = cfg(client, Config),
    ShopID = cfg(shop_id, Config),
    PartyID = cfg(party_id, Config),
    Details = hg_ct_helper:make_invoice_tpl_details(Product, Cost),
    Params = make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details),
    hg_client_invoice_templating:create(Params, Client).

update_invalid_cost(Cost, amount, TplID, Client) ->
    update_invalid_cost(Cost, <<"Invalid amount">>, TplID, Client);
update_invalid_cost(Cost, currency, TplID, Client) ->
    update_invalid_cost(Cost, <<"Invalid currency">>, TplID, Client);
update_invalid_cost(Cost, Error, TplID, Client) ->
    Details = hg_ct_helper:make_invoice_tpl_details(<<"RNGName">>, Cost),
    Diff = make_invoice_tpl_update_params(#{details => Details}),
    {exception, #'InvalidRequest'{
        errors = [Error]
    }} = hg_client_invoice_templating:update(TplID, Diff, Client).

create_invalid_cost(Cost, amount, Config) ->
    create_invalid_cost(Cost, <<"Invalid amount">>, Config);
create_invalid_cost(Cost, currency, Config) ->
    create_invalid_cost(Cost, <<"Invalid currency">>, Config);
create_invalid_cost(Cost, Error, Config) ->
    Product = <<"rubberduck">>,
    Lifetime = make_lifetime(0, 0, 2),
    {exception, #'InvalidRequest'{
        errors = [Error]
    }} = create_invoice_tpl(Config, Product, Lifetime, Cost).

make_invoice_tpl_create_params(PartyID, ShopID) ->
    Lifetime = make_lifetime(0, 0, 2),
    Product = <<"rubberduck">>,
    Details = hg_ct_helper:make_invoice_tpl_details(<<"rubberduck">>, make_cost(fixed, 5000, <<"RUB">>)),
    make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details).

make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details) ->
    hg_ct_helper:make_invoice_tpl_create_params(PartyID, ShopID, Lifetime, Product, Details).

make_invoice_tpl_update_params(Diff) ->
    hg_ct_helper:make_invoice_tpl_update_params(Diff).

make_lifetime(Y, M, D) ->
    hg_ct_helper:make_lifetime(Y, M, D).

make_cost(Type, P1, P2) ->
    hg_ct_helper:make_invoice_tpl_cost(Type, P1, P2).

construct_domain_fixture() ->
    [
        hg_ct_fixture:construct_currency(?cur(<<"RUB">>)),
        hg_ct_fixture:construct_category(?cat(1), <<"Test category">>),
        hg_ct_fixture:construct_proxy(?prx(1), <<"Dummy proxy">>),
        hg_ct_fixture:construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        hg_ct_fixture:construct_contract_template(?tmpl(1), ?trms(1)),
        hg_ct_fixture:construct_system_account_set(?sas(1)),
        hg_ct_fixture:construct_external_account_set(?eas(1)),

        hg_ct_fixture:construct_payment_method(?pmt(bank_card, visa)),
        hg_ct_fixture:construct_payment_method(?pmt(bank_card, mastercard)),
        hg_ct_fixture:construct_payment_method(?pmt(payment_terminal, euroset)),

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                party_prototype = #domain_PartyPrototypeRef{id = 42},
                providers = {value, ordsets:from_list([])},
                system_account_set = {value, ?sas(1)},
                external_account_set = {value, ?eas(1)},
                default_contract_template = ?tmpl(1),
                common_merchant_proxy = ?prx(1),
                inspector = {value, ?insp(1)}
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
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
                            categories = {value, ordsets:from_list([?cat(1)])}
                        }
                    }
                }]
            }
        }}
    ].

construct_term_set_for_cost(LowerBound, UpperBound) ->
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            payment_methods = {decisions, [
                #domain_PaymentMethodDecision{
                    if_   = {condition, {cost_in, ?cashrng(
                        {inclusive, ?cash(LowerBound, <<"RUB">>)},
                        {inclusive, ?cash(UpperBound, <<"RUB">>)}
                    )}},
                    then_ = {value, ordsets:from_list(
                        [
                            ?pmt(bank_card, mastercard),
                            ?pmt(bank_card, visa),
                            ?pmt(payment_terminal, euroset)
                        ]
                    )}
                },
                #domain_PaymentMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([])}
                }
            ]}
        }
    },
    {term_set_hierarchy, #domain_TermSetHierarchyObject{
        ref = ?trms(1),
        data = #domain_TermSetHierarchy{
            parent_terms = undefined,
            term_sets = [#domain_TimedTermSet{
                action_time = #'TimestampInterval'{},
                terms = TermSet
            }]
        }
    }}.
