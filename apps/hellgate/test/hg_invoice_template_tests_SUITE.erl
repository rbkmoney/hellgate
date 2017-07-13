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
-export([create_invalid_cost_range_amount/1]).
-export([create_invalid_cost_range_currency/1]).
-export([create_invoice_template/1]).
-export([get_invoice_template_anyhow/1]).
-export([update_invalid_party/1]).
-export([update_invalid_shop/1]).
-export([update_invalid_party_status/1]).
-export([update_invalid_shop_status/1]).
-export([update_invalid_cost_fixed_amount/1]).
-export([update_invalid_cost_fixed_currency/1]).
-export([update_invalid_cost_range_amount/1]).
-export([update_invalid_cost_range_currency/1]).
-export([update_invoice_template/1]).
-export([delete_invalid_party_status/1]).
-export([delete_invalid_shop_status/1]).
-export([delete_invoice_template/1]).

%% tests descriptions

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-define(MISSING_PARTY_ID, <<"42">>).
-define(MISSING_SHOP_ID, 42).

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
        create_invalid_cost_range_amount,
        create_invalid_cost_range_currency,
        create_invoice_template,
        get_invoice_template_anyhow,
        update_invalid_party,
        update_invalid_shop,
        update_invalid_party_status,
        update_invalid_shop_status,
        update_invalid_cost_fixed_amount,
        update_invalid_cost_fixed_currency,
        update_invalid_cost_range_amount,
        update_invalid_cost_range_currency,
        update_invoice_template,
        delete_invalid_party_status,
        delete_invalid_shop_status,
        delete_invoice_template
    ].

%% starting/stopping

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'hg_client_party', '_', '_'}, x),
    {Apps, Ret} = hg_ct_helper:start_apps([lager, woody, dmt_client, hellgate]),
    ok = hg_domain:insert(construct_domain_fixture()),
    RootUrl = maps:get(hellgate_root_url, Ret),
    PartyID = hg_utils:unique_id(),
    Client = hg_client_party:start(make_userinfo(PartyID), PartyID, hg_client_api:new(RootUrl)),
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
    PartyID = cfg(party_id, C),
    Client = hg_client_invoice_templating:start_link(make_userinfo(PartyID), hg_client_api:new(cfg(root_url, C))),
    [{client, Client} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().

end_per_testcase(_Name, _C) ->
    ok.

-spec create_invalid_party(config()) -> _ | no_return().

create_invalid_party(C) ->
    Client = cfg(client, C),
    ShopID = cfg(shop_id, C),
    PartyID = ?MISSING_PARTY_ID,
    Product = <<"rubberduck">>,
    Params = make_invoice_tpl_create_params(PartyID, ShopID, Product),
    {exception, #payproc_InvalidUser{}} = hg_client_invoice_templating:create(Params, Client).

-spec create_invalid_shop(config()) -> _ | no_return().

create_invalid_shop(C) ->
    Client = cfg(client, C),
    ShopID = ?MISSING_SHOP_ID,
    PartyID = cfg(party_id, C),
    Product = <<"rubberduck">>,
    Params = make_invoice_tpl_create_params(PartyID, ShopID, Product),
    {exception, #payproc_ShopNotFound{}} = hg_client_invoice_templating:create(Params, Client).

-spec create_invalid_party_status(config()) -> _ | no_return().

create_invalid_party_status(C) ->
    PartyClient = cfg(party_client, C),

    #payproc_ClaimResult{} = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = create_invoice_tpl(C, <<"rubberduck">>),
    #payproc_ClaimResult{} = hg_client_party:activate(PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = create_invoice_tpl(C, <<"rubberduck">>),
    #payproc_ClaimResult{} = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec create_invalid_shop_status(config()) -> _ | no_return().

create_invalid_shop_status(C) ->
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),

    #payproc_ClaimResult{} = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = create_invoice_tpl(C, <<"rubberduck">>),
    #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = create_invoice_tpl(C, <<"rubberduck">>),
    #payproc_ClaimResult{} = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec create_invalid_cost_fixed_amount(config()) -> _ | no_return().

create_invalid_cost_fixed_amount(C) ->
    Cost = make_cost(fixed, -100, <<"RUB">>),
    create_invalid_cost(Cost, amount, C).

-spec create_invalid_cost_fixed_currency(config()) -> _ | no_return().

create_invalid_cost_fixed_currency(C) ->
    Cost = make_cost(fixed, 100, <<"KEK">>),
    create_invalid_cost(Cost, currency, C).

-spec create_invalid_cost_range_amount(config()) -> _ | no_return().

create_invalid_cost_range_amount(C) ->
    Cost1 = make_cost(range, {inclusive, -100, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    create_invalid_cost(Cost1, amount, C),

    Cost2 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, -100, <<"RUB">>}),
    create_invalid_cost(Cost2, amount, C),

    Cost3 = make_cost(range, {inclusive, 10000, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    create_invalid_cost(Cost3, <<"Invalid cost range">>, C).

-spec create_invalid_cost_range_currency(config()) -> _ | no_return().

create_invalid_cost_range_currency(C) ->
    Cost1 = make_cost(range, {inclusive, 100, <<"KEK">>}, {inclusive, 10000, <<"RUB">>}),
    create_invalid_cost(Cost1, currency, C),

    Cost2 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"KEK">>}),
    create_invalid_cost(Cost2, currency, C).

-spec create_invoice_template(config()) -> _ | no_return().

create_invoice_template(C) ->
    Client = cfg(client, C),
    Product = <<"rubberduck">>,
    Details = make_invoice_details(Product),
    Lifetime = make_lifetime(0, 0, 2),
    Cost = make_cost(fixed, 10000, <<"RUB">>),
    TplID = create_invoice_tpl(C, Product, Lifetime, Cost),
    #domain_InvoiceTemplate{
       id = TplID,
       details = Details,
       invoice_lifetime = Lifetime,
       cost = Cost
    } = hg_client_invoice_templating:get(TplID, Client).

-spec get_invoice_template_anyhow(config()) -> _ | no_return().

get_invoice_template_anyhow(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),

    #payproc_ClaimResult{} = hg_client_party:suspend(PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:activate(PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient),

    #payproc_ClaimResult{} = hg_client_party:suspend_shop(ShopID, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient),
    InvoiceTpl = hg_client_invoice_templating:get(TplID, Client).

-spec update_invalid_party(config()) -> _ | no_return().

update_invalid_party(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Diff = make_invoice_tpl_update_params(#{party_id => ?MISSING_PARTY_ID}),
    {exception, #payproc_InvalidUser{}} = hg_client_invoice_templating:update(TplID, Diff, Client).

-spec update_invalid_shop(config()) -> _ | no_return().

update_invalid_shop(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Diff = make_invoice_tpl_update_params(#{shop_id => ?MISSING_SHOP_ID}),
    {exception, #payproc_ShopNotFound{}} = hg_client_invoice_templating:update(TplID, Diff, Client).

-spec update_invalid_party_status(config()) -> _ | no_return().

update_invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Diff = make_invoice_tpl_update_params(#{details => make_invoice_details(<<"teddy bear">>)}),
    #payproc_ClaimResult{} = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    #payproc_ClaimResult{} = hg_client_party:activate(PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec update_invalid_shop_status(config()) -> _ | no_return().

update_invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Diff = make_invoice_tpl_update_params(#{details => make_invoice_details(<<"teddy bear">>)}),

    #payproc_ClaimResult{} = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:update(TplID, Diff, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec update_invalid_cost_fixed_amount(config()) -> _ | no_return().

update_invalid_cost_fixed_amount(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Cost = make_cost(fixed, -100, <<"RUB">>),
    update_invalid_cost(Cost, amount, TplID, Client).

-spec update_invalid_cost_fixed_currency(config()) -> _ | no_return().

update_invalid_cost_fixed_currency(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    Cost = make_cost(fixed, 100, <<"KEK">>),
    update_invalid_cost(Cost, currency, TplID, Client).

-spec update_invalid_cost_range_amount(config()) -> _ | no_return().

update_invalid_cost_range_amount(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),

    Cost1 = make_cost(range, {inclusive, -100, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    update_invalid_cost(Cost1, amount, TplID, Client),

    Cost2 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, -100, <<"RUB">>}),
    update_invalid_cost(Cost2, amount, TplID, Client),

    Cost3 = make_cost(range, {inclusive, 10000, <<"RUB">>}, {inclusive, 100, <<"RUB">>}),
    update_invalid_cost(Cost3, <<"Invalid cost range">>, TplID, Client).

-spec update_invalid_cost_range_currency(config()) -> _ | no_return().

update_invalid_cost_range_currency(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),

    Cost1 = make_cost(range, {inclusive, 100, <<"KEK">>}, {inclusive, 10000, <<"RUB">>}),
    update_invalid_cost(Cost1, currency, TplID, Client),

    Cost2 = make_cost(range, {inclusive, 100, <<"RUB">>}, {inclusive, 10000, <<"KEK">>}),
    update_invalid_cost(Cost2, currency, TplID, Client).

-spec update_invoice_template(config()) -> _ | no_return().

update_invoice_template(C) ->
    Client = cfg(client, C),
    PartyID = cfg(party_id, C),
    ShopID = cfg(shop_id, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    NewDetails = make_invoice_details(<<"teddy bear">>),
    NewCost = make_cost(unlim, sale, '50%'),
    NewLifetime = make_lifetime(10, 32, 51),
    Diff = make_invoice_tpl_update_params(#{
        details => NewDetails,
        cost => NewCost,
        invoice_lifetime => NewLifetime
    }),
    #domain_InvoiceTemplate{
        id = TplID,
        owner_id = PartyID,
        shop_id = ShopID,
        details = NewDetails,
        cost = NewCost,
        invoice_lifetime = NewLifetime
    } = hg_client_invoice_templating:update(TplID, Diff, Client).

-spec delete_invalid_party_status(config()) -> _ | no_return().

delete_invalid_party_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),

    #payproc_ClaimResult{} = hg_client_party:suspend(PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:activate(PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block(<<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidPartyStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock(<<"UNBLOOOCK">>, PartyClient).

-spec delete_invalid_shop_status(config()) -> _ | no_return().

delete_invalid_shop_status(C) ->
    Client = cfg(client, C),
    PartyClient = cfg(party_client, C),
    ShopID = cfg(shop_id, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),

    #payproc_ClaimResult{} = hg_client_party:suspend_shop(ShopID, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {suspension, {suspended, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:activate_shop(ShopID, PartyClient),

    #payproc_ClaimResult{} = hg_client_party:block_shop(ShopID, <<"BLOOOOCK">>, PartyClient),
    {exception, #payproc_InvalidShopStatus{
        status = {blocking, {blocked, _}}
    }} = hg_client_invoice_templating:delete(TplID, Client),
    #payproc_ClaimResult{} = hg_client_party:unblock_shop(ShopID, <<"UNBLOOOCK">>, PartyClient).

-spec delete_invoice_template(config()) -> _ | no_return().

delete_invoice_template(C) ->
    Client = cfg(client, C),
    TplID = create_invoice_tpl(C, <<"rubberduck">>),
    ok = hg_client_invoice_templating:delete(TplID, Client),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:get(TplID, Client),
    Diff = make_invoice_tpl_update_params(#{}),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:update(TplID, Diff, Client),
    {exception, #payproc_InvoiceTemplateRemoved{}} = hg_client_invoice_templating:delete(TplID, Client).

%%

create_invoice_tpl(Config, Product) ->
    Client = cfg(client, Config),
    ShopID = cfg(shop_id, Config),
    PartyID = cfg(party_id, Config),
    Product = <<"rubberduck">>,
    Params = make_invoice_tpl_create_params(PartyID, ShopID, Product),
    hg_client_invoice_templating:create(Params, Client).

create_invoice_tpl(Config, Product, Lifetime, Cost) ->
    Client = cfg(client, Config),
    ShopID = cfg(shop_id, Config),
    PartyID = cfg(party_id, Config),
    Product = <<"rubberduck">>,
    Params = make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost),
    hg_client_invoice_templating:create(Params, Client).

update_invalid_cost(Cost, amount, TplID, Client) ->
    update_invalid_cost(Cost, <<"Invalid amount">>, TplID, Client);
update_invalid_cost(Cost, currency, TplID, Client) ->
    update_invalid_cost(Cost, <<"Invalid currency">>, TplID, Client);
update_invalid_cost(Cost, Error, TplID, Client) ->
    Diff = make_invoice_tpl_update_params(#{cost => Cost}),
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

make_invoice_tpl_create_params(PartyID, ShopID, Product) ->
    hg_ct_helper:make_invoice_tpl_create_params(PartyID, ShopID, Product).

make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost) ->
    hg_ct_helper:make_invoice_tpl_create_params(PartyID, ShopID, Product, Lifetime, Cost).

make_invoice_tpl_update_params(Diff) ->
    hg_ct_helper:make_invoice_tpl_update_params(Diff).

construct_domain_fixture() ->
    hg_invoice_tests_SUITE:construct_domain_fixture().

make_userinfo(PartyID) ->
    hg_ct_helper:make_userinfo(PartyID).

make_lifetime(Y, M, D) ->
    hg_ct_helper:make_lifetime(Y, M, D).

make_cost(Type, P1, P2) ->
    hg_ct_helper:make_invoice_tpl_cost(Type, P1, P2).

make_invoice_details(Product) ->
    hg_ct_helper:make_invoice_details(Product).
