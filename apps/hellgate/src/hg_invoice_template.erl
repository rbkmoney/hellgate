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

-export_type([tpl_id/0]).
-export_type([tpl/0]).


%%

-spec get(tpl_id()) -> tpl().

get(TplId) ->
    get_invoice_template(TplId).

get_invoice_template(ID) ->
    {History, _LastID} = get_history(ID),
    _ = assert_invoice_template_not_deleted(lists:last(History)),
    collapse_history(History).

%% Woody handler

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    hg_log_scope:scope(invoice_templating,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_('Create', [UserInfo, Params], _Opts) ->
    TplID = hg_utils:unique_id(),
    ok    = assume_user_identity(UserInfo),
    _     = set_meta(TplID),
    Party = validate_party(Params#payproc_InvoiceTemplateCreateParams.party_id),
    Shop  = validate_shop(Params#payproc_InvoiceTemplateCreateParams.shop_id, Party),
    ok    = validate_params(Params, Shop),
    ok    = start(TplID, Params),
    TplID;
handle_function_('Update', [UserInfo, TplID, Params], _Opts) ->
    ok    = assume_user_identity(UserInfo),
    _     = set_meta(TplID),
    Tpl   = get_invoice_template(TplID),
    Party = validate_party(Params#payproc_InvoiceTemplateUpdateParams.party_id, Tpl#domain_InvoiceTemplate.owner_id),
    Shop  = validate_shop(get_shop_id(Params#payproc_InvoiceTemplateUpdateParams.shop_id, Tpl), Party),
    ok = validate_params(Params, Shop),
    call(TplID, {update, Params});
handle_function_('Get', [TplID], _Opts) ->
    _     = set_meta(TplID),
    get_invoice_template(TplID);
handle_function_('Delete', [UserInfo, TplID], _Opts) ->
    ok    = assume_user_identity(UserInfo),
    Tpl   = get_invoice_template(TplID),
    Party = validate_party(undefined, Tpl#domain_InvoiceTemplate.owner_id),
    _     = validate_shop(get_shop_id(undefined, Tpl), Party),
    _     = set_meta(TplID),
    call(TplID, delete).

assume_user_identity(UserInfo) ->
    hg_woody_handler_utils:assume_user_identity(UserInfo).

validate_party(undefined, PartyID) ->
    validate_party(PartyID);
validate_party(PartyID, _) ->
    validate_party(PartyID).

validate_party(PartyID) ->
    _     = hg_invoice_utils:assert_party_accessible(PartyID),
    Party = hg_party_machine:get_party(PartyID),
    _     = hg_invoice_utils:assert_party_operable(Party),
    Party.

get_shop_id(undefined, #domain_InvoiceTemplate{shop_id = ShopID}) ->
    ShopID;
get_shop_id(ShopID, _) ->
    ShopID.

validate_shop(ShopID, Party) ->
    Shop = hg_invoice_utils:assert_shop_exists(hg_party:get_shop(ShopID, Party)),
    _    = hg_invoice_utils:assert_shop_operable(Shop),
    Shop.

set_meta(ID) ->
    hg_log_scope:set_meta(#{invoice_template_id => ID}).

validate_params(#payproc_InvoiceTemplateCreateParams{cost = Cost}, Shop) ->
    ok = validate_cost(Cost, Shop);
validate_params(#payproc_InvoiceTemplateUpdateParams{cost = undefined}, _) ->
    ok;
validate_params(#payproc_InvoiceTemplateUpdateParams{cost = Cost}, Shop) ->
    ok = validate_cost(Cost, Shop).

validate_cost({cost_fixed, Cash}, Shop) ->
    hg_invoice_utils:validate_cost(Cash, Shop);
validate_cost({cost_range, Range = #domain_CashRange{
    lower = {_, LowerCost},
    upper = {_, UpperCost}
}}, Shop) ->
    ok = hg_invoice_utils:validate_cash_range(Range),
    ok = hg_invoice_utils:validate_cost(LowerCost, Shop),
    ok = hg_invoice_utils:validate_cost(UpperCost, Shop);
validate_cost({cost_unlim, _}, _Shop) ->
    ok.

start(ID, Args) ->
    map_start_error(hg_machine:start(?NS, ID, Args)).

call(ID, Args) ->
    map_error(hg_machine:call(?NS, {id, ID}, Args)).

get_history(TplID) ->
    map_history_error(hg_machine:get_history(?NS, TplID)).

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
    throw(#payproc_InvoiceTemplateNotFound{});
map_history_error({error, Reason}) ->
    error(Reason).

%% Machine

-type ev()            :: dmsl_payment_processing_thrift:'EventPayload'().
-type create_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateCreateParams'().
-type update_params() :: dmsl_payment_processing_thrift:'InvoiceTemplateUpdateParams'().
-type call()          :: {update, update_params()} | delete.

-define(ev(Body), {invoice_template_event, Body}).

-define(tpl_created(InvoiceTpl),
    ?ev({invoice_template_created,
        #payproc_InvoiceTemplateCreated{invoice_template = InvoiceTpl}})
).

-define(tpl_updated(Diff),
    ?ev({invoice_template_updated,
        #payproc_InvoiceTemplateUpdated{diff = Diff}})
).

-define(tpl_deleted,
    ?ev({invoice_template_deleted,
        #payproc_InvoiceTemplateDeleted{}})
).

assert_invoice_template_not_deleted({_, _, ?tpl_deleted}) ->
    throw(#payproc_InvoiceTemplateRemoved{});
assert_invoice_template_not_deleted(_) ->
    ok.

-spec namespace() ->
    hg_machine:ns().

namespace() ->
    ?NS.

-spec init(tpl_id(), create_params()) ->
    hg_machine:result(ev()).

init(ID, Params) ->
    Tpl = create_invoice_template(ID, Params),
    {[?tpl_created(Tpl)], hg_machine_action:new()}.

create_invoice_template(ID, P) ->
    #domain_InvoiceTemplate{
        id               = ID,
        owner_id         = P#payproc_InvoiceTemplateCreateParams.party_id,
        shop_id          = P#payproc_InvoiceTemplateCreateParams.shop_id,
        details          = P#payproc_InvoiceTemplateCreateParams.details,
        invoice_lifetime = P#payproc_InvoiceTemplateCreateParams.invoice_lifetime,
        cost             = P#payproc_InvoiceTemplateCreateParams.cost,
        context          = P#payproc_InvoiceTemplateCreateParams.context
    }.

-spec process_signal(hg_machine:signal(), hg_machine:history(ev())) ->
    hg_machine:result(ev()).

%% There are no timers for invoice templates,
%% so no handler for such a case.
process_signal({repair, _}, _History) ->
    {[], hg_machine_action:new()}.

-spec process_call(call(), hg_machine:history(ev())) ->
    {hg_machine:response(), hg_machine:result(ev())}.

process_call(Call, History) ->
    Tpl = collapse_history(History),
    {Response, Event} = handle_call(Call, Tpl),
    {{ok, Response}, {[Event], hg_machine_action:new()}}.

handle_call({update, Params}, Tpl) ->
    Event = ?tpl_updated(Params),
    {merge_event(Event, Tpl), Event};
handle_call(delete, _Tpl) ->
    {ok, ?tpl_deleted}.

collapse_history(History) ->
    lists:foldl(
        fun ({_ID, _, Ev}, Tpl) -> merge_event(Ev, Tpl) end,
        undefined,
        History
    ).

merge_event(?tpl_created(Tpl), _) ->
    Tpl;
merge_event(?tpl_updated(#payproc_InvoiceTemplateUpdateParams{
    party_id         = PartyID,
    shop_id          = ShopID,
    details          = Details,
    invoice_lifetime = InvoiceLifetime,
    cost             = Cost,
    context          = Context
}), Tpl) ->
    Diff = [
        {party_id,         PartyID},
        {shop_id,          ShopID},
        {details,          Details},
        {invoice_lifetime, InvoiceLifetime},
        {cost,             Cost},
        {context,          Context}
    ],
    lists:foldl(fun update_field/2, Tpl, Diff).

update_field({_, undefined}, Tpl) ->
    Tpl;
update_field({party_id, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{owner_id = V};
update_field({shop_id, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{shop_id = V};
update_field({details, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{details = V};
update_field({invoice_lifetime, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{invoice_lifetime = V};
update_field({cost, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{cost = V};
update_field({context, V}, Tpl) ->
    Tpl#domain_InvoiceTemplate{context = V}.

%% Event provider

-spec publish_event(tpl_id(), ev()) ->
    {true, hg_event_provider:public_event()} | false.

publish_event(ID, Event = ?ev(_)) ->
    {true, {{invoice_template, ID}, 0, Event}};
publish_event(_ID, _Event) ->
    false.
