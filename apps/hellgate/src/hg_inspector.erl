-module(hg_inspector).

-export([inspect/5]).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_inspector_thrift.hrl").

-type shop() :: dmsl_domain_thrift:'Shop'().
-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type inspector() :: dmsl_domain_thrift:'Inspector'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().

-spec inspect(shop(), invoice(), payment(), inspector(), hg_domain:revision()) -> risk_score() | no_return().
inspect(
    Shop,
    Invoice,
    Payment,
    #domain_Inspector{proxy = #domain_Proxy{
        ref = ProxyRef,
        additional = ProxyAdditional
    }},
    Revision
) ->
    ProxyDef = get_proxy_def(ProxyRef, Revision),
    Context = #proxy_inspector_Context{
        payment = get_payment_info(Shop, Invoice, Payment, Revision),
        options = maps:merge(ProxyDef#domain_ProxyDefinition.options, ProxyAdditional)
    },
    Result = issue_call('InspectPayment', [Context], get_call_options(ProxyDef)),
    case Result of
        {ok, RiskScore} when is_atom(RiskScore) ->
            RiskScore;
        {exception, Error} ->
            error(Error)
    end.

get_payment_info(
    #domain_Shop{
        id = ShopID,
        category = CategoryRef,
        details = ShopDetails
    },
    #domain_Invoice{
        id = InvoiceID,
        created_at = InvoiceCreatedAt,
        due = InvoiceDue
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
    ProxyInvoice = #proxy_inspector_Invoice{
        id = InvoiceID,
        created_at = InvoiceCreatedAt,
        due = InvoiceDue
    },
    ProxyPayment = #proxy_inspector_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    #proxy_inspector_PaymentInfo{
        shop = ProxyShop,
        invoice = ProxyInvoice,
        payment = ProxyPayment
    }.

get_call_options(ProxyDef) ->
    #{url => ProxyDef#domain_ProxyDefinition.url}.

issue_call(Func, Args, CallOpts) ->
    hg_woody_wrapper:call('InspectorProxy', Func, Args, CallOpts).

get_proxy_def(Ref, Revision) ->
    hg_domain:get(Revision, {proxy, Ref}).
