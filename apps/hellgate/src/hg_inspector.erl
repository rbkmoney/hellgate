-module(hg_inspector).

-export([inspect/4]).

-export([compare_risk_score/2]).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_proxy_inspector_thrift.hrl").

-type shop() :: dmsl_domain_thrift:'Shop'().
-type invoice() :: dmsl_domain_thrift:'Invoice'().
-type payment() :: dmsl_domain_thrift:'InvoicePayment'().
-type inspector() :: dmsl_domain_thrift:'Inspector'().
-type risk_score() :: dmsl_domain_thrift:'RiskScore'().
-type risk_magnitude() :: integer().

-spec inspect(shop(), invoice(), payment(), inspector()) -> risk_score() | no_return().
inspect(
    Shop,
    Invoice,
    #domain_InvoicePayment{
        domain_revision = Revision
    } = Payment,
    #domain_Inspector{fallback_risk_score = FallBackRiskScore, proxy = Proxy = #domain_Proxy{
        ref = ProxyRef,
        additional = ProxyAdditional
    }}
) ->
    ProxyDef = get_proxy_def(ProxyRef, Revision),
    Context = #proxy_inspector_Context{
        payment = get_payment_info(Shop, Invoice, Payment),
        options = maps:merge(ProxyDef#domain_ProxyDefinition.options, ProxyAdditional)
    },
    Result = issue_call(FallBackRiskScore, 'InspectPayment', [Context], hg_proxy:get_call_options(Proxy, Revision)),
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
        details = ShopDetails,
        location = Location
    },
    #domain_Invoice{
        owner_id = PartyID,
        id = InvoiceID,
        created_at = InvoiceCreatedAt,
        due = InvoiceDue,
        details = InvoiceDetails
    },
    #domain_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        domain_revision = Revision,
        payer = Payer,
        cost = Cost
    }
) ->
    Party = #proxy_inspector_Party{
        party_id = PartyID
    },
    ShopCategory = hg_domain:get(
        Revision,
        {category, CategoryRef}
    ),
    ProxyShop = #proxy_inspector_Shop{
        id = ShopID,
        category = ShopCategory,
        details = ShopDetails,
        location = Location
    },
    ProxyInvoice = #proxy_inspector_Invoice{
        id = InvoiceID,
        created_at = InvoiceCreatedAt,
        due = InvoiceDue,
        details = InvoiceDetails
    },
    ProxyPayment = #proxy_inspector_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost
    },
    #proxy_inspector_PaymentInfo{
        party = Party,
        shop = ProxyShop,
        invoice = ProxyInvoice,
        payment = ProxyPayment
    }.

issue_call(undefined, Func, Args, CallOpts) ->
    hg_woody_wrapper:call(proxy_inspector, Func, Args, CallOpts);
issue_call(Default, Func, Args, CallOpts) ->
    try hg_woody_wrapper:call(proxy_inspector, Func, Args, CallOpts) of
        {ok, _} = RiskScore ->
            RiskScore;
        {exception, Error} ->
            lager:warning("Fail to get RiskScore with error ~p", [Error]),
            {ok, Default}
    catch
        Class:Reason ->
            lager:warning("Fail to get RiskScore with error ~p:~p", [Class, Reason]),
            {ok, Default}
    end.

get_proxy_def(Ref, Revision) ->
    hg_domain:get(Revision, {proxy, Ref}).

%%

-spec compare_risk_score(risk_score(), risk_score()) -> risk_magnitude().
compare_risk_score(RS1, RS2) ->
    get_risk_magnitude(RS1) - get_risk_magnitude(RS2).

get_risk_magnitude(RiskScore) ->
    {enum, Info} = dmsl_domain_thrift:enum_info('RiskScore'),
    {RiskScore, Magnitude} = lists:keyfind(RiskScore, 1, Info),
    Magnitude.
