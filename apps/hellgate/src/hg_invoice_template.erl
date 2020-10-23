%%% Invoice template machine

-module(hg_invoice_template).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

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
-type tpl() :: dmsl_domain_thrift:'InvoiceTemplate'().

%% Internal types

-type invoice_template_change() :: dmsl_payment_processing_thrift:'InvoiceTemplateChange'().

%% API

-spec get(tpl_id()) -> tpl().
get(TplId) ->
    get_invoice_template(TplId).

get_invoice_template(ID) ->
    History = get_history(ID),
    _ = assert_invoice_template_not_deleted(lists:last(History)),
    collapse_history(History).

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function(Func, Args, Opts) ->
    scoper:scope(
        invoice_templating,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) -> term() | no_return().
handle_function_('Create', {UserInfo, Params}, _Opts) ->
    TplID = hg_utils:unique_id(),
    ok = assume_user_identity(UserInfo),
    _ = set_meta(TplID),
    Party = get_party(Params#payproc_InvoiceTemplateCreateParams.party_id),
    Shop = get_shop(Params#payproc_InvoiceTemplateCreateParams.shop_id, Party),
    ok = validate_create_params(Params, Shop),
    ok = start(TplID, Params),
    get_invoice_template(TplID);
handle_function_('Get', {UserInfo, TplID}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_meta(TplID),
    Tpl = get_invoice_template(TplID),
    _ = hg_invoice_utils:assert_party_accessible(Tpl#domain_InvoiceTemplate.owner_id),
    Tpl;
handle_function_('Update' = Fun, {UserInfo, TplID, Params} = Args, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_meta(TplID),
    Tpl = get_invoice_template(TplID),
    Party = get_party(Tpl#domain_InvoiceTemplate.owner_id),
    Shop = get_shop(Tpl#domain_InvoiceTemplate.shop_id, Party),
    ok = validate_update_params(Params, Shop),
    call(TplID, Fun, Args);
handle_function_('Delete' = Fun, {UserInfo, TplID} = Args, _Opts) ->
    ok = assume_user_identity(UserInfo),
    Tpl = get_invoice_template(TplID),
    Party = get_party(Tpl#domain_InvoiceTemplate.owner_id),
    _ = get_shop(Tpl#domain_InvoiceTemplate.shop_id, Party),
    _ = set_meta(TplID),
    call(TplID, Fun, Args);
handle_function_('ComputeTerms', {UserInfo, TplID, Timestamp, PartyRevision0}, _Opts) ->
    ok = assume_user_identity(UserInfo),
    _ = set_meta(TplID),
    Tpl = get_invoice_template(TplID),
    ShopID = Tpl#domain_InvoiceTemplate.shop_id,
    PartyID = Tpl#domain_InvoiceTemplate.owner_id,
    PartyRevision1 = hg_maybe:get_defined(PartyRevision0, {timestamp, Timestamp}),
    Party = hg_party:get_party(PartyID),
    Shop = hg_party:get_shop(ShopID, Party),
    Contract = hg_party:get_contract(Shop#domain_Shop.contract_id, Party),
    Cost = case Tpl#domain_InvoiceTemplate.details of
        {product, #domain_InvoiceTemplateProduct{price = {fixed, Cash}}} ->
           Cash;
        _ ->
            undefined
    end,
    VS0 = #{
        party_id => PartyID,
        shop_id => ShopID,
        category => Shop#domain_Shop.category,
        currency => (Shop#domain_Shop.account)#domain_ShopAccount.currency,
        identification_level => hg_invoice_utils:get_identification_level(Contract, Party),
        cost => Cost
    },
    VS = hg_varset:prepare_varset(genlib_map:compact(VS0)),
    hg_invoice_utils:compute_shop_terms(UserInfo, PartyID, ShopID, Timestamp, PartyRevision1, VS).


assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

get_party(PartyID) ->
    _ = hg_invoice_utils:assert_party_accessible(PartyID),
    Party = hg_party:get_party(PartyID),
    _ = hg_invoice_utils:assert_party_operable(Party),
    Party.

get_shop(ShopID, Party) ->
    Shop = hg_invoice_utils:assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _ = hg_invoice_utils:assert_shop_operable(Shop),
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
validate_price(
    {range,
        Range = #domain_CashRange{
            lower = {_, LowerCost},
            upper = {_, UpperCost}
        }},
    Shop
) ->
    ok = hg_invoice_utils:validate_cash_range(Range),
    ok = hg_invoice_utils:validate_cost(LowerCost, Shop),
    ok = hg_invoice_utils:validate_cost(UpperCost, Shop);
validate_price({unlim, _}, _Shop) ->
    ok.

start(ID, Params) ->
    EncodedParams = marchal_invoice_template_params(Params),
    map_start_error(hg_machine:start(?NS, ID, EncodedParams)).

call(ID, Function, Args) ->
    case hg_machine:thrift_call(?NS, ID, invoice_templating, {'InvoiceTemplating', Function}, Args) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {exception, Exception} ->
            erlang:throw(Exception);
        {error, Error} ->
            map_error(Error)
    end.

get_history(TplID) ->
    unmarshal_history(map_history_error(hg_machine:get_history(?NS, TplID))).

-spec map_error(notfound | any()) -> no_return().
map_error(notfound) ->
    throw(#payproc_InvoiceTemplateNotFound{});
map_error(Reason) ->
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
-type call() :: hg_machine:thrift_call().

-define(ev(Body),
    {invoice_template_changes, Body}
).

-define(tpl_created(InvoiceTpl),
    {invoice_template_created, #payproc_InvoiceTemplateCreated{invoice_template = InvoiceTpl}}
).

-define(tpl_updated(Diff),
    {invoice_template_updated, #payproc_InvoiceTemplateUpdated{diff = Diff}}
).

-define(tpl_deleted(),
    {invoice_template_deleted, #payproc_InvoiceTemplateDeleted{}}
).

assert_invoice_template_not_deleted({_, _, [?tpl_deleted()]}) ->
    throw(#payproc_InvoiceTemplateRemoved{});
assert_invoice_template_not_deleted(_) ->
    ok.

-spec namespace() -> hg_machine:ns().
namespace() ->
    ?NS.

-spec init(binary(), hg_machine:machine()) -> hg_machine:result().
init(EncodedParams, #{id := ID}) ->
    Params = unmarchal_invoice_template_params(EncodedParams),
    Tpl = create_invoice_template(ID, Params),
    #{events => [marshal_event_payload([?tpl_created(Tpl)])]}.

create_invoice_template(ID, P) ->
    #domain_InvoiceTemplate{
        id = ID,
        owner_id = P#payproc_InvoiceTemplateCreateParams.party_id,
        shop_id = P#payproc_InvoiceTemplateCreateParams.shop_id,
        invoice_lifetime = P#payproc_InvoiceTemplateCreateParams.invoice_lifetime,
        product = P#payproc_InvoiceTemplateCreateParams.product,
        description = P#payproc_InvoiceTemplateCreateParams.description,
        details = P#payproc_InvoiceTemplateCreateParams.details,
        context = P#payproc_InvoiceTemplateCreateParams.context
    }.

-spec process_signal(hg_machine:signal(), hg_machine:machine()) -> hg_machine:result().
process_signal(timeout, _Machine) ->
    #{};
process_signal({repair, _}, _Machine) ->
    #{}.

-spec process_call(call(), hg_machine:machine()) -> {hg_machine:response(), hg_machine:result()}.
process_call(Call, #{history := History}) ->
    St = collapse_history(unmarshal_history(History)),
    try handle_call(Call, St) of
        {ok, Changes} ->
            {ok, #{events => [marshal_event_payload(Changes)]}};
        {Reply, Changes} ->
            {{ok, Reply}, #{events => [marshal_event_payload(Changes)]}}
    catch
        throw:Exception ->
            {{exception, Exception}, #{}}
    end.

handle_call({{'InvoiceTemplating', 'Update'}, {_UserInfo, _TplID, Params}}, Tpl) ->
    Changes = [?tpl_updated(Params)],
    {merge_changes(Changes, Tpl), Changes};
handle_call({{'InvoiceTemplating', 'Delete'}, {_UserInfo, _TplID}}, _Tpl) ->
    {ok, [?tpl_deleted()]}.

collapse_history(History) ->
    lists:foldl(
        fun({_ID, _, Ev}, Tpl) -> merge_changes(Ev, Tpl) end,
        undefined,
        History
    ).

merge_changes([?tpl_created(Tpl)], _) ->
    Tpl;
merge_changes(
    [
        ?tpl_updated(#payproc_InvoiceTemplateUpdateParams{
            invoice_lifetime = InvoiceLifetime,
            product = Product,
            description = Description,
            details = Details,
            context = Context
        })
    ],
    Tpl
) ->
    Diff = [
        {invoice_lifetime, InvoiceLifetime},
        {product, Product},
        {description, Description},
        {details, Details},
        {context, Context}
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

-spec publish_event(tpl_id(), hg_machine:event_payload()) -> hg_event_provider:public_event().
publish_event(ID, Payload) ->
    {{invoice_template_id, ID}, ?ev(unmarshal_event_payload(Payload))}.

%% Marshaling

-include("legacy_structures.hrl").

-spec marchal_invoice_template_params(create_params()) -> binary().
marchal_invoice_template_params(Params) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'InvoiceTemplateCreateParams'}},
    hg_proto_utils:serialize(Type, Params).

-spec marshal_event_payload([invoice_template_change()]) -> hg_machine:event_payload().
marshal_event_payload(Changes) when is_list(Changes) ->
    wrap_event_payload({invoice_template_changes, Changes}).

wrap_event_payload(Payload) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    Bin = hg_proto_utils:serialize(Type, Payload),
    #{
        format_version => 1,
        data => {bin, Bin}
    }.

%% Unmashaling

-spec unmarchal_invoice_template_params(binary()) -> create_params().
unmarchal_invoice_template_params(EncodedParams) ->
    Type = {struct, struct, {dmsl_payment_processing_thrift, 'InvoiceTemplateCreateParams'}},
    hg_proto_utils:deserialize(Type, EncodedParams).

-spec unmarshal_history([hg_machine:event()]) -> [hg_machine:event([invoice_template_change()])].
unmarshal_history(Events) ->
    [unmarshal_event(Event) || Event <- Events].

-spec unmarshal_event(hg_machine:event()) -> hg_machine:event([invoice_template_change()]).
unmarshal_event({ID, Dt, Payload}) ->
    {ID, Dt, unmarshal_event_payload(Payload)}.

-spec unmarshal_event_payload(hg_machine:event_payload()) -> [invoice_template_change()].
unmarshal_event_payload(#{format_version := 1, data := {bin, Changes}}) ->
    Type = {struct, union, {dmsl_payment_processing_thrift, 'EventPayload'}},
    {invoice_template_changes, Buf} = hg_proto_utils:deserialize(Type, Changes),
    Buf;
unmarshal_event_payload(#{format_version := undefined, data := Changes}) ->
    unmarshal({list, change}, Changes).

%% Version > 1

unmarshal({list, change}, Changes) when is_list(Changes) ->
    [unmarshal(change, Change) || Change <- Changes];
%% Version 1

unmarshal({list, change}, {bin, Bin}) when is_binary(Bin) ->
    ?ev(Changes) = binary_to_term(Bin),
    [unmarshal(change, [1, Change]) || Change <- Changes];
unmarshal(change, [
    Version,
    #{
        <<"change">> := <<"created">>,
        <<"tpl">> := InvoiceTpl
    }
]) when Version > 1 ->
    ?tpl_created(unmarshal(invoice_template, [Version, InvoiceTpl]));
unmarshal(change, [
    Version,
    #{
        <<"change">> := <<"updated">>,
        <<"diff">> := Diff
    }
]) when Version > 1 ->
    ?tpl_updated(unmarshal(invoice_template_diff, [Version, Diff]));
unmarshal(change, [
    Version,
    #{
        <<"change">> := <<"deleted">>
    }
]) when Version > 1 ->
    ?tpl_deleted();
%% Legacy change unmarshalling
unmarshal(change, [1, ?legacy_invoice_tpl_created(InvoiceTpl)]) ->
    ?tpl_created(unmarshal(invoice_template, [1, InvoiceTpl]));
unmarshal(change, [1, ?legacy_invoice_tpl_updated(Diff)]) ->
    ?tpl_updated(unmarshal(invoice_template_diff, [1, Diff]));
unmarshal(change, [1, ?legacy_invoice_tpl_deleted()]) ->
    ?tpl_deleted();
%%

unmarshal(invoice_template, [
    3,
    #{
        <<"id">> := ID,
        <<"shop_id">> := ShopID,
        <<"owner_id">> := OwnerID,
        <<"product">> := Product,
        <<"lifetime">> := Lifetime,
        <<"details">> := Details
    } = V
]) ->
    #domain_InvoiceTemplate{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        owner_id = unmarshal(str, OwnerID),
        invoice_lifetime = unmarshal(lifetime, Lifetime),
        product = unmarshal(str, Product),
        description = unmarshal(str, genlib_map:get(<<"description">>, V)),
        details = unmarshal(details, Details),
        context = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };
unmarshal(invoice_template, [
    2,
    #{
        <<"id">> := ID,
        <<"shop_id">> := ShopID,
        <<"owner_id">> := OwnerID,
        <<"details">> := Details,
        <<"lifetime">> := Lifetime,
        <<"cost">> := MarshalledCost
    } = V
]) ->
    Cost = unmarshal(cost, MarshalledCost),
    Product = unmarshal(str, genlib_map:get(<<"product">>, Details)),
    Description = unmarshal(str, genlib_map:get(<<"description">>, Details)),
    #domain_InvoiceTemplate{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        owner_id = unmarshal(str, OwnerID),
        invoice_lifetime = unmarshal(lifetime, Lifetime),
        product = Product,
        description = Description,
        details = construct_invoice_template_details(Product, Cost),
        context = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };
unmarshal(invoice_template, [
    1,
    {domain_InvoiceTemplate, ID, OwnerID, ShopID, Details, Lifetime, MarshalledCost, Context}
]) ->
    {Product, Description} = unmarshal(details_legacy, Details),
    Cost = unmarshal(cost_legacy, MarshalledCost),
    #domain_InvoiceTemplate{
        id = unmarshal(str, ID),
        shop_id = unmarshal(str, ShopID),
        owner_id = unmarshal(str, OwnerID),
        invoice_lifetime = unmarshal(lifetime_legacy, Lifetime),
        product = Product,
        description = Description,
        details = construct_invoice_template_details(Product, Cost),
        context = hg_content:unmarshal(Context)
    };
unmarshal(invoice_template_diff, [3, #{} = V]) ->
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = unmarshal(lifetime, genlib_map:get(<<"lifetime">>, V)),
        product = unmarshal(str, genlib_map:get(<<"product">>, V)),
        description = unmarshal(str, genlib_map:get(<<"description">>, V)),
        details = unmarshal(details, genlib_map:get(<<"details">>, V)),
        context = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };
unmarshal(invoice_template_diff, [2, #{} = V]) ->
    MarshalledDetails = genlib_map:get(<<"details">>, V),
    {Product, Description} =
        case MarshalledDetails of
            undefined ->
                {undefined, undefined};
            #{<<"product">> := P} ->
                {P, genlib_map:get(<<"description">>, MarshalledDetails)}
        end,
    Cost = unmarshal(cost, genlib_map:get(<<"cost">>, V)),
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = unmarshal(lifetime, genlib_map:get(<<"lifetime">>, V)),
        product = Product,
        description = Description,
        details = construct_invoice_template_details(Product, Cost),
        context = hg_content:unmarshal(genlib_map:get(<<"context">>, V))
    };
unmarshal(invoice_template_diff, [
    1,
    {payproc_InvoiceTemplateUpdateParams, MarshalledDetails, Lifetime, MarshalledCost, Context}
]) ->
    Cost = unmarshal(cost_legacy, MarshalledCost),
    {Product, Description} =
        case MarshalledDetails of
            undefined ->
                {undefined, undefined};
            {domain_InvoiceDetails, P, D, _} ->
                {P, D}
        end,
    #payproc_InvoiceTemplateUpdateParams{
        invoice_lifetime = unmarshal(lifetime_legacy, Lifetime),
        product = Product,
        description = Description,
        details = construct_invoice_template_details(Product, Cost),
        context = hg_content:unmarshal(Context)
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
        years = unmarshal(int, genlib_map:get(<<"years">>, V)),
        months = unmarshal(int, genlib_map:get(<<"months">>, V)),
        days = unmarshal(int, genlib_map:get(<<"days">>, V))
    };
unmarshal(lifetime_legacy, {domain_LifetimeInterval, Years, Months, Days}) ->
    #domain_LifetimeInterval{
        years = unmarshal(int, Years),
        months = unmarshal(int, Months),
        days = unmarshal(int, Days)
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

construct_invoice_template_details(Product, Cost) when Product =/= undefined, Cost =/= undefined ->
    {product, #domain_InvoiceTemplateProduct{
        product = Product,
        price = Cost,
        metadata = #{}
    }};
construct_invoice_template_details(Product, Cost) ->
    error({unmarshal_event_error, {'Cant construct invoice template details', Product, Cost}}).
