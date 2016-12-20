-module(hg_inspector).

-export([inspect/4]).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_inspector_thrift.hrl").

-type shop() :: dmsl_domain_thrift:'Shop'().
-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type inspector() :: dmsl_domain_thrift:'Inspector'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().

-spec inspect(shop(), payment(), inspector(), hg_domain:revision()) -> risk_score() | no_return().
inspect(
    Shop,
    Payment,
    #domain_Inspector{proxy = Proxy},
    Revision
) ->
    PaymentInfo = get_payment_info(Shop, Payment, Revision),
    CallOpts = get_call_options(Proxy, Revision),
    Result = issue_call('InspectPayment', [PaymentInfo], CallOpts),
    case Result of
        RiskScore when is_atom(RiskScore) ->
            RiskScore;
        {error, Error} ->
            error(Error)
    end.

get_payment_info(
    #domain_Shop{
        id = ShopID,
        category = CategoryRef,
        details = ShopDetails
    },
    #domain_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    Revision
) ->
    ShopCategory = hg_domain:get(
        Revision,
        {category, CategoryRef}
    ),
    ProxyShop = #proxy_inspector_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails
    },
    ProxyPayment = #proxy_inspector_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    #proxy_inspector_PaymentInfo{
        shop = ProxyShop,
        payment = ProxyPayment
    }.

get_call_options(Proxy, Revision) ->
    ProxyDef = hg_domain:get(Revision, {proxy, Proxy#domain_Proxy.ref}),
    #{url => ProxyDef#domain_ProxyDefinition.url}.

issue_call(Func, Args, CallOpts) ->
    hg_woody_wrapper:call('InspectorProxy', Func, Args, CallOpts).
