-module(hg_inspector).

-export([inspect/4]).

-export([compare_risk_score/2]).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proxy_inspector_thrift.hrl").

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
    #domain_Inspector{
        fallback_risk_score = FallBackRiskScore,
        proxy =
            Proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = ProxyAdditional
            }
    }
) ->
    DeadLine = woody_deadline:from_timeout(genlib_app:env(hellgate, inspect_timeout, infinity)),
    ProxyDef = get_proxy_def(ProxyRef, Revision),
    Context = #proxy_inspector_Context{
        payment = get_payment_info(Shop, Invoice, Payment),
        options = maps:merge(ProxyDef#domain_ProxyDefinition.options, ProxyAdditional)
    },
    Result = issue_call(
        'InspectPayment',
        {Context},
        hg_proxy:get_call_options(
            Proxy,
            Revision
        ),
        FallBackRiskScore,
        DeadLine
    ),
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
        details = InvoiceDetails,
        trusted_client = IsTrusted
    },
    #domain_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        domain_revision = Revision,
        payer = Payer,
        cost = Cost,
        make_recurrent = MakeRecurrent
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
        details = InvoiceDetails,
        trusted_client = IsTrusted
    },
    ProxyPayment = #proxy_inspector_InvoicePayment{
        id = PaymentID,
        created_at = CreatedAt,
        payer = Payer,
        cost = Cost,
        make_recurrent = MakeRecurrent
    },
    #proxy_inspector_PaymentInfo{
        party = Party,
        shop = ProxyShop,
        invoice = ProxyInvoice,
        payment = ProxyPayment
    }.

issue_call(Func, Args, CallOpts, undefined, _DeadLine) ->
    % Do not set custom deadline without fallback risk score
    hg_woody_wrapper:call(proxy_inspector, Func, Args, CallOpts);
issue_call(Func, Args, CallOpts, Default, DeadLine) ->
    try hg_woody_wrapper:call(proxy_inspector, Func, Args, CallOpts, DeadLine) of
        {ok, _} = RiskScore ->
            RiskScore;
        {exception, Error} ->
            _ = logger:error("Fail to get RiskScore with error ~p", [Error]),
            {ok, Default}
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason when
            Class =:= resource_unavailable orelse
                Class =:= result_unknown
        ->
            _ = logger:warning("Fail to get RiskScore with error ~p:~p", [error, Reason]),
            {ok, Default};
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            _ = logger:error("Fail to get RiskScore with error ~p:~p", [error, Reason]),
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
