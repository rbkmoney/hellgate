%%% Invoice template machine

-module(hg_invoice_template).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-define(NS, <<"invoice_template">>).

%% Woody handler called by hg_woody_wrapper
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

%% Machine callbacks
-behaviour(hg_machine).

-export([namespace/0]).

-export([init/2]).
-export([process_signal/2]).
-export([process_call/2]).

%% Event provider callbacks

-behaviour(hg_event_provider).

-export([publish_event/2]).

%% API

-export([get/1]).

-type tpl_id() :: dmsl_domain_thrift:'InvoiceTemplateID'().
-type tpl()    :: dmsl_domain_thrift:'InvoiceTemplate'().

%%

-spec get(tpl_id()) -> tpl().

get(TplId) ->
    get_invoice_template(TplId).

get_invoice_template(ID) ->
    History = get_history(ID),
    _ = assert_invoice_template_not_deleted(lists:last(History)),
    collapse_history(History).

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(invoice_templating,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_('Create', [UserInfo, Params], _Opts) ->
    TplID = hg_utils:unique_id(),
    ok    = assume_user_identity(UserInfo),
    _     = set_meta(TplID),
    Party = get_party(Params#payproc_InvoiceTemplateCreateParams.party_id),
    Shop  = get_shop(Params#payproc_InvoiceTemplateCreateParams.shop_id, Party),
    ok    = validate_create_params(Params, Shop),
    ok    = start(TplID, Params),
    get_invoice_template(TplID);

handle_function_('Get', [UserInfo, TplID], _Opts) ->
    ok  = assume_user_identity(UserInfo),
    _   = set_meta(TplID),
    Tpl = get_invoice_template(TplID),
    _   = hg_invoice_utils:assert_party_accessible(Tpl#domain_InvoiceTemplate.owner_id),
    Tpl;

handle_function_('Update', [UserInfo, TplID, Params], _Opts) ->
    ok    = assume_user_identity(UserInfo),
    _     = set_meta(TplID),
    Tpl   = get_invoice_template(TplID),
    Party = get_party(Tpl#domain_InvoiceTemplate.owner_id),
    Shop  = get_shop(Tpl#domain_InvoiceTemplate.shop_id, Party),
    ok = validate_update_params(Params, Shop),
    call(TplID, {update, Params});

handle_function_('Delete', [UserInfo, TplID], _Opts) ->
    ok    = assume_user_identity(UserInfo),
    Tpl   = get_invoice_template(TplID),
    Party = get_party(Tpl#domain_InvoiceTemplate.owner_id),
    _     = get_shop(Tpl#domain_InvoiceTemplate.shop_id, Party),
    _     = set_meta(TplID),
    call(TplID, delete);

handle_function_('ComputeTerms', [UserInfo, TplID, Timestamp], _Opts) ->
    ok    = assume_user_identity(UserInfo),
    _     = set_meta(TplID),
    Tpl   = get_invoice_template(TplID),
    ShopID = Tpl#domain_InvoiceTemplate.shop_id,
    PartyID = Tpl#domain_InvoiceTemplate.owner_id,
    ShopTerms = hg_invoice_utils:compute_shop_terms(UserInfo, PartyID, ShopID, Timestamp),
    case Tpl#domain_InvoiceTemplate.details of
        {product, #domain_InvoiceTemplateProduct{price = {fixed, Cash}}} ->
            Revision = hg_domain:head(),
            hg_party:reduce_terms(ShopTerms, #{cost => Cash}, Revision);
        _ ->
            ShopTerms
    end.

assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

get_party(PartyID) ->
    _     = hg_invoice_utils:assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    _     = hg_invoice_utils:assert_party_operable(Party),
    Party.

get_shop(ShopID, Party) ->
    Shop = hg_invoice_utils:assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _    = hg_invoice_utils:assert_shop_operable(Shop),
    Shop.

set_meta(ID) ->
    scoper:add_meta(#{invoice_template_id => ID}).

validate_create_params(#payproc_InvoiceTemplateCreateParams{details = Details}, Shop) ->
    ok = validate_details(Details, Shop).

validate_update_params(#payproc_InvoiceTemplateUpdateParams{details = undefined}, _) ->
    ok;
validate_update_params(#payproc_InvoiceTemplateUpdateParams{details = Details}, Shop) ->
    ok = validate_details(Details, Shop).

validate_details({cart, #domain_InvoiceCart{}}, _) ->
    ok;
validate_details({product, #domain_InvoiceTemplateProduct{price = Price}}, Shop) ->
    validate_price(Price, Shop).

validate_price({fixed, Cash}, Shop) ->
    hg_invoice_utils:validate_cost(Cash, Shop);
validate_price({range, Range = #domain_CashRange{
    lower = {_, LowerCost},
    upper = {_, UpperCost}
}}, Shop) ->
    ok = hg_invoice_utils:validate_cash_range(Range),
    ok = hg_invoice_utils:validate_cost(LowerCost, Shop),
    ok = hg_invoice_utils:validate_cost(UpperCost, Shop);
validate_price({unlim, _}, _Shop) ->
    ok.

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, ID, Args)).

get_history(TplID) ->
    unmarshal(map_history_error(hg_machine:get_history(?NS, TplID))).

map_error({ok, CallResult}) ->
    case CallResult of
        {ok, Result} ->
            Result;
        {exception, Reason} ->
            throw(Reason)
    end;
map_error({error, notfound}) ->
    throw(#payproc_InvoiceTemplateNotFound{});
map_error({error, Reason}) ->
    error(Reason).

map_start_error({ok, _}) ->
    ok;
map_start_error({error, Reason}) ->
    error(Reason).

map_history_error({ok, Result}) ->
    Result;
map_history_error({error, notfound}) ->
    throw(#payproc_InvoiceTemplateNotFound{}).

%% Machine

-type create_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateCreateParams'().
-type update_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateUpdateParams'().
-type call()          :: {update, update_params()} | delete.

-define(ev(Body),
    {invoice_template_changes, Body}
).

-define(tpl_created(InvoiceTpl),
    {invoice_template_created,
        #payproc_InvoiceTemplateCreated{invoice_template = InvoiceTpl}}
).

-define(tpl_updated(Diff),
    {invoice_template_updated,
        #payproc_InvoiceTemplateUpdated{diff = Diff}}
).

-define(tpl_deleted(),
    {invoice_template_deleted,
        #payproc_InvoiceTemplateDeleted{}}
).

assert_invoice_template_not_deleted({_, _, [?tpl_deleted()]}) ->
    throw(#payproc_InvoiceTemplateRemoved{});
assert_invoice_template_not_deleted(_) ->
    ok.

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(create_params(), hg_machine:machine()) ->
    hg_machine:result().

init(Params, #{id := ID}) ->
    Tpl = create_invoice_template(ID, Params),
    #{events => [marshal([?tpl_created(Tpl)])]}.

create_invoice_template(ID, P) ->
    #domain_InvoiceTemplate{
        id               = ID,
        owner_id         = P#payproc_InvoiceTemplateCreateParams.party_id,
        shop_id          = P#payproc_InvoiceTemplateCreateParams.shop_id,
        invoice_lifetime = P#payproc_InvoiceTemplateCreateParams.invoice_lifetime,
        product          = P#payproc_InvoiceTemplateCreateParams.product,
        description      = P#payproc_InvoiceTemplateCreateParams.description,
        details          = P#payproc_InvoiceTemplateCreateParams.details,
        context          = P#payproc_InvoiceTemplateCreateParams.context
    }.

-spec process_signal(hg_machine:signal(), hg_machine:machine()) ->
    hg_machine:result().

process_signal(timeout, _Machine) ->
    #{};

process_signal({repair, _}, _Machine) ->
    #{}.

-spec process_call(call(), hg_machine:machine()) ->
    {hg_machine:response(), hg_machine:result()}.

process_call(Call, #{history := History}) ->
    Tpl = collapse_history(unmarshal(History)),
    {Response, Changes} = handle_call(Call, Tpl),
    {{ok, Response}, #{events => [marshal(Changes)]}}.

handle_call({update, Params}, Tpl) ->
    Changes = [?tpl_updated(Params)],
    {merge_changes(Changes, Tpl), Changes};
handle_call(delete, _Tpl) ->
    {ok, [?tpl_deleted()]}.

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Ev}, Tpl) -> merge_changes(Ev, Tpl) end,
        undefined,
        History
    ).

merge_changes([?tpl_created(Tpl)], _) ->
    Tpl;
merge_changes([?tpl_updated(#payproc_InvoiceTemplateUpdateParams{
    invoice_lifetime = InvoiceLifetime,
    product          = Product,
    description      = Description,
    details          = Details,
    context          = Context
})], Tpl) ->
    Diff = [
        {invoice_lifetime, InvoiceLifetime},
        {product,          Product},
        {description,      Description},
        {details,          Details},
        {context,          Context}
    ],
    lists:foldl(fun update_field/2, Tpl, Diff).

update_field({_, undefined}, Tpl) ->
    Tpl;
update_field({invoice_lifetime, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{invoice_lifetime = V};
update_field({product, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{product = V};
update_field({description, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{description = V};
update_field({details, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{details = V};
update_field({context, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{context = V}.

%% Event provider

-type msgpack_value() :: dmsl_msgpack_thrift:'Value'().

-spec publish_event(tpl_id(), msgpack_value()) ->
    hg_event_provider:public_event().

publish_event(ID, Changes) ->
    {{invoice_template_id, ID}, ?ev(unmarshal({list, change}, Changes))}.

%%

-include("legacy_structures.hrl").

marshal(Changes) when is_list(Changes) ->
    [marshal(change, Change) || Change <- Changes].

marshal(change, ?tpl_created(InvoiceTpl)) ->
    [3, #{
        <<"change">>    => <<"created">>,
        <<"tpl">>       => marshal(invoice_template, InvoiceTpl)
    }];
marshal(change, ?tpl_updated(Diff)) ->
    [3, #{
        <<"change">>    => <<"updated">>,
        <<"diff">>      => marshal(invoice_template_diff, Diff)
    }];
marshal(change, ?tpl_deleted()) ->
    [3, #{
        <<"change">>    => <<"deleted">>
    }];

marshal(invoice_template, #domain_InvoiceTemplate{} = InvoiceTpl) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, InvoiceTpl#domain_InvoiceTemplate.id),
        <<"shop_id">>       => marshal(str, InvoiceTpl#domain_InvoiceTemplate.shop_id),
        <<"owner_id">>      => marshal(str, InvoiceTpl#domain_InvoiceTemplate.owner_id),
        <<"lifetime">>      => marshal(lifetime, InvoiceTpl#domain_InvoiceTemplate.invoice_lifetime),
        <<"product">>       => marshal(str, InvoiceTpl#domain_InvoiceTemplate.product),
        <<"description">>   => marshal(str, InvoiceTpl#domain_InvoiceTemplate.description),
        <<"details">>       => marshal(details, InvoiceTpl#domain_InvoiceTemplate.details),
        <<"context">>       => hg_content:marshal(InvoiceTpl#domain_InvoiceTemplate.context)
    });

marshal(invoice_template_diff, #payproc_InvoiceTemplateUpdateParams{} = Diff) ->
    genlib_map:compact(#{
        <<"lifetime">>      => marshal(lifetime, Diff#payproc_InvoiceTemplateUpdateParams.invoice_lifetime),
        <<"product">>       => marshal(str, Diff#payproc_InvoiceTemplateUpdateParams.product),
        <<"description">>   => marshal(str, Diff#payproc_InvoiceTemplateUpdateParams.description),
        <<"details">>       => marshal(details, Diff#payproc_InvoiceTemplateUpdateParams.details),
        <<"context">>       => hg_content:marshal(Diff#payproc_InvoiceTemplateUpdateParams.context)
    });

marshal(details, {cart, #domain_InvoiceCart{} = Cart}) ->
    [<<"cart">>, marshal(cart, Cart)];

marshal(details, {product, #domain_InvoiceTemplateProduct{} = Product}) ->
    [<<"template_product">>, marshal(template_product, Product)];

marshal(cart, #domain_InvoiceCart{lines = Lines}) ->
    #{<<"lines">> => [marshal(line, Line) || Line <- Lines]};

marshal(line, #domain_InvoiceLine{} = InvoiceLine) ->
    #{
        <<"product">>       => marshal(str, InvoiceLine#domain_InvoiceLine.product),
        <<"quantity">>      => marshal(int, InvoiceLine#domain_InvoiceLine.quantity),
        <<"price">>         => hg_cash:marshal(InvoiceLine#domain_InvoiceLine.price),
        <<"metadata">>      => marshal(metadata, InvoiceLine#domain_InvoiceLine.metadata)
    };

marshal(template_product, #domain_InvoiceTemplateProduct{} = Product) ->
    #{
        <<"product">>       => marshal(str, Product#domain_InvoiceTemplateProduct.product),
        <<"price">>         => marshal(cost, Product#domain_InvoiceTemplateProduct.price),
        <<"metadata">>      => marshal(metadata, Product#domain_InvoiceTemplateProduct.metadata)
    };

marshal(lifetime, #domain_LifetimeInterval{years = Years, months = Months, days = Days}) ->
    genlib_map:compact(#{
        <<"years">>         => marshal(int, Years),
        <<"months">>        => marshal(int, Months),
        <<"days">>          => marshal(int, Days)
    });

marshal(cost, {fixed, Cash}) ->
    [<<"fixed">>, hg_cash:marshal(Cash)];
marshal(cost, {range, CashRange}) ->
    [<<"range">>, hg_cash_range:marshal(CashRange)];
marshal(cost, {unlim, _}) ->
    <<"unlim">>;

marshal(metadata, Metadata) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(str, K), hg_msgpack_marshalling:unmarshal(V), Acc)
        end,
        #{},
        Metadata
    );

marshal(_, Other) ->
    Other.

%%

unmarshal(Events) when is_list(Events) ->
    [unmarshal(Event) || Event <- Events];

unmarshal({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal({list, change}, Payload)}.

%% Version > 1

unmarshal({list, change}, Changes) when is_list(Changes) ->
    [unmarshal(change, Change) || Change <- Changes];

%% Version 1

unmarshal({list, change}, {bin, Bin}) when is_binary(Bin) ->
    ?ev(Changes) = binary_to_term(Bin),
    [unmarshal(change, [1, Change]) || Change <- Changes];

unmarshal(change, [Version, #{
    <<"change">>    := <<"created">>,
    <<"tpl">>       := InvoiceTpl
}]) when Version > 1 ->
    ?tpl_created(unmarshal(invoice_template, [Version, InvoiceTpl]));

unmarshal(change, [Version, #{
    <<"change">>    := <<"updated">>,
    <<"diff">>      := Diff
}]) when Version > 1 ->
    ?tpl_updated(unmarshal(invoice_template_diff, [Version, Diff]));

unmarshal(change, [Version, #{
    <<"change">>    := <<"deleted">>
}]) when Version > 1 ->
    ?tpl_deleted();

%% Legacy change unmarshalling
unmarshal(change, [1, ?legacy_invoice_tpl_created(InvoiceTpl)]) ->
    ?tpl_created(unmarshal(invoice_template, [1, InvoiceTpl]));
unmarshal(change, [1, ?legacy_invoice_tpl_updated(Diff)]) ->
    ?tpl_updated(unmarshal(invoice_template_diff, [1, Diff]));
unmarshal(change, [1, ?legacy_invoice_tpl_deleted()]) ->
    ?tpl_deleted();

%%

unmarshal(invoice_template, [3, #{
    <<"id">>         := ID,
    <<"shop_id">>    := ShopID,
    <<"owner_id">>   := OwnerID,
    <<"product">>    := Product,
    <<"lifetime">>   := Lifetime,
    <<"details">>    := Details
} = V]) ->
    #domain_InvoiceTemplate{
        id                   = unmarshal(str, ID),
        shop_id              = unmarshal(str, ShopID),
        owner_id             = unmarshal(str, OwnerID),
        invoice_lifetime     = unmarshal(lifetime, Lifetime),
        product              = unmarshal(str, Product),
        description          = unmarshal(str, genlib_map:get(<<"description">>, V)),
        details              = unmarshal(details, Details),
        context              = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };

unmarshal(invoice_template, [2, #{
    <<"id">>         := ID,
    <<"shop_id">>    := ShopID,
    <<"owner_id">>   := OwnerID,
    <<"details">>    := Details,
    <<"lifetime">>   := Lifetime,
    <<"cost">>       := MarshalledCost
} = V]) ->
    Cost = unmarshal(cost, MarshalledCost),
    Product = unmarshal(str, genlib_map:get(<<"product">>, Details)),
    Description = unmarshal(str, genlib_map:get(<<"description">>, Details)),
    #domain_InvoiceTemplate{
        id                   = unmarshal(str, ID),
        shop_id              = unmarshal(str, ShopID),
        owner_id             = unmarshal(str, OwnerID),
        invoice_lifetime     = unmarshal(lifetime, Lifetime),
        product              = Product,
        description          = Description,
        details              = construct_invoice_template_details(Product, Cost),
        context              = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };

unmarshal(invoice_template, [1, {domain_InvoiceTemplate,
    ID,
    OwnerID,
    ShopID,
    Details,
    Lifetime,
    MarshalledCost,
    Context
}]) ->
    {Product, Description} = unmarshal(details_legacy, Details),
    Cost = unmarshal(cost_legacy, MarshalledCost),
    #domain_InvoiceTemplate{
        id                   = unmarshal(str, ID),
        shop_id              = unmarshal(str, ShopID),
        owner_id             = unmarshal(str, OwnerID),
        invoice_lifetime     = unmarshal(lifetime_legacy, Lifetime),
        product              = Product,
        description          = Description,
        details              = construct_invoice_template_details(Product, Cost),
        context              = hg_content:unmarshal(Context)
    };

unmarshal(invoice_template_diff, [3, #{} = V]) ->
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime     = unmarshal(lifetime, genlib_map:get(<<"lifetime">>, V)),
        product              = unmarshal(str, genlib_map:get(<<"product">>, V)),
        description          = unmarshal(str, genlib_map:get(<<"description">>, V)),
        details              = unmarshal(details, genlib_map:get(<<"details">>, V)),
        context              = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };

unmarshal(invoice_template_diff, [2, #{} = V]) ->
    MarshalledDetails = genlib_map:get(<<"details">>, V),
    {Product, Description} = case MarshalledDetails of
        undefined ->
            {undefined, undefined};
        #{<<"product">> := P} ->
            {P, genlib_map:get(<<"description">>, MarshalledDetails)}
    end,
    Cost = unmarshal(cost, genlib_map:get(<<"cost">>, V)),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime     = unmarshal(lifetime, genlib_map:get(<<"lifetime">>, V)),
        product              = Product,
        description          = Description,
        details              = construct_invoice_template_details(Product, Cost),
        context              = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };

unmarshal(invoice_template_diff, [1, {payproc_InvoiceTemplateUpdateParams,
    MarshalledDetails,
    Lifetime,
    MarshalledCost,
    Context
}]) ->
    Cost = unmarshal(cost_legacy, MarshalledCost),
    {Product, Description} = case MarshalledDetails of
        undefined ->
            {undefined, undefined};
        {domain_InvoiceDetails, P, D, _} ->
            {P, D}
    end,
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime     = unmarshal(lifetime_legacy, Lifetime),
        product              = Product,
        description          = Description,
        details              = construct_invoice_template_details(Product, Cost),
        context              = hg_content:unmarshal(Context)
    };

unmarshal(details_legacy, {domain_InvoiceDetails, MarshalledProduct, MarshalledDescription}) ->
    {unmarshal(str, MarshalledProduct), unmarshal(str, MarshalledDescription)};
unmarshal(details_legacy, {domain_InvoiceDetails, MarshalledProduct, MarshalledDescription, _}) ->
    {unmarshal(str, MarshalledProduct), unmarshal(str, MarshalledDescription)};

unmarshal(details, [<<"cart">>, Cart]) ->
    {cart, unmarshal(cart, Cart)};

unmarshal(details, [<<"template_product">>, Product]) ->
    {product, #domain_InvoiceTemplateProduct{
        product = unmarshal(str, genlib_map:get(<<"product">>, Product)),
        price = unmarshal(cost, genlib_map:get(<<"price">>, Product)),
        metadata = unmarshal(metadata, genlib_map:get(<<"metadata">>, Product))
    }};

unmarshal(cart, #{<<"lines">> := Lines}) when is_list(Lines) ->
    #domain_InvoiceCart{lines = [unmarshal(line, Line) || Line <- Lines]};

unmarshal(line, #{
    <<"product">> := Product,
    <<"quantity">> := Quantity,
    <<"price">> := Price,
    <<"metadata">> := Metadata
}) ->
    #domain_InvoiceLine{
        product = unmarshal(str, Product),
        quantity = unmarshal(int, Quantity),
        price = hg_cash:unmarshal(Price),
        metadata = unmarshal(metadata, Metadata)
    };

unmarshal(lifetime, #{} = V) ->
    #domain_LifetimeInterval{
        years               = unmarshal(int, genlib_map:get(<<"years">>, V)),
        months              = unmarshal(int, genlib_map:get(<<"months">>, V)),
        days                = unmarshal(int, genlib_map:get(<<"days">>, V))
    };

unmarshal(lifetime_legacy, {domain_LifetimeInterval, Years, Months, Days}) ->
    #domain_LifetimeInterval{
        years               = unmarshal(int, Years),
        months              = unmarshal(int, Months),
        days                = unmarshal(int, Days)
    };

unmarshal(cost, [<<"fixed">>, Cash]) ->
    {fixed, hg_cash:unmarshal(Cash)};
unmarshal(cost, [<<"range">>, CashRange]) ->
    {range, hg_cash_range:unmarshal(CashRange)};
unmarshal(cost, <<"unlim">>) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};

unmarshal(cost_legacy, {fixed, Cash}) ->
    {fixed, hg_cash:unmarshal([1, Cash])};
unmarshal(cost_legacy, {range, CashRange}) ->
    {range, hg_cash_range:unmarshal([1, CashRange])};
unmarshal(cost_legacy, {unlim, _}) ->
    {unlim, #domain_InvoiceTemplateCostUnlimited{}};

unmarshal(metadata, Metadata) ->
    maps:fold(
        fun(K, V, Acc) ->
            maps:put(unmarshal(str, K), hg_msgpack_marshalling:marshal(V), Acc)
        end,
        #{},
        Metadata
    );

unmarshal(_, Other) ->
    Other.

construct_invoice_template_details(Product, Cost) when Product =/= undefined, Cost =/= undefined->
    {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Cost,
        metadata = #{}
    }};
construct_invoice_template_details(Product, Cost) ->
    error({unmarshal_event_error, {'Cant construct invoice template details', Product, Cost}}).

