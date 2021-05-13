%%% Invoice utils
%%%

-module(hg_invoice_utils).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([validate_cost/2]).
-export([validate_amount/1]).
-export([validate_currency/2]).
-export([validate_cash_range/1]).
-export([assert_party_accessible/1]).
-export([assert_party_operable/1]).
-export([assert_shop_exists/1]).
-export([assert_shop_operable/1]).
-export([assert_cost_payable/2]).
-export([compute_shop_terms/5]).
-export([get_cart_amount/1]).
-export([check_deadline/1]).
-export([get_identification_level/2]).

-type amount() :: dmsl_domain_thrift:'Amount'().
-type currency() :: dmsl_domain_thrift:'CurrencyRef'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type cart() :: dmsl_domain_thrift:'InvoiceCart'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type party() :: dmsl_domain_thrift:'Party'().
-type shop() :: dmsl_domain_thrift:'Shop'().
-type contract() :: dmsl_domain_thrift:'Contract'().
-type party_id() :: dmsl_domain_thrift:'PartyID'().
-type shop_id() :: dmsl_domain_thrift:'ShopID'().
-type term_set() :: dmsl_domain_thrift:'TermSet'().
-type payment_service_terms() :: dmsl_domain_thrift:'PaymentsServiceTerms'().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().
-type party_revision_param() :: dmsl_payment_processing_thrift:'PartyRevisionParam'().
-type identification_level() :: dmsl_domain_thrift:'ContractorIdentificationLevel'().
-type varset() :: dmsl_payment_processing_thrift:'Varset'().

-spec validate_cost(cash(), shop()) -> ok.
validate_cost(#domain_Cash{currency = Currency, amount = Amount}, Shop) ->
    _ = validate_amount(Amount),
    _ = validate_currency(Currency, Shop),
    ok.

-spec validate_amount(amount()) -> ok.
validate_amount(Amount) when Amount > 0 ->
    ok;
validate_amount(_) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid amount">>]}).

-spec validate_currency(currency(), shop()) -> ok.
validate_currency(Currency, Shop = #domain_Shop{}) ->
    validate_currency_(Currency, get_shop_currency(Shop)).

-spec assert_party_accessible(party_id()) -> ok.
assert_party_accessible(PartyID) ->
    UserIdentity = hg_woody_handler_utils:get_user_identity(),
    case hg_access_control:check_user(UserIdentity, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#payproc_InvalidUser{})
    end.

-spec validate_cash_range(cash_range()) -> ok.
validate_cash_range(#domain_CashRange{
    lower = {LType, #domain_Cash{amount = LAmount, currency = Currency}},
    upper = {UType, #domain_Cash{amount = UAmount, currency = Currency}}
}) when
    LType =/= UType andalso UAmount >= LAmount orelse
        LType =:= UType andalso UAmount > LAmount orelse
        LType =:= UType andalso UType =:= inclusive andalso UAmount == LAmount
->
    ok;
validate_cash_range(_) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid cost range">>]}).

-spec assert_party_operable(party()) -> party().
assert_party_operable(#domain_Party{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_party_unblocked(Blocking),
    _ = assert_party_active(Suspension),
    V.

-spec assert_shop_operable(shop()) -> shop().
assert_shop_operable(#domain_Shop{blocking = Blocking, suspension = Suspension} = V) ->
    _ = assert_shop_unblocked(Blocking),
    _ = assert_shop_active(Suspension),
    V.

-spec assert_shop_exists(shop() | undefined) -> shop().
assert_shop_exists(#domain_Shop{} = V) ->
    V;
assert_shop_exists(undefined) ->
    throw(#payproc_ShopNotFound{}).

-spec assert_cost_payable(cash(), payment_service_terms()) -> cash().
assert_cost_payable(Cost, #domain_PaymentsServiceTerms{cash_limit = CashLimit}) ->
    case any_limit_matches(Cost, CashLimit) of
        true ->
            Cost;
        false ->
            throw(#payproc_InvoiceTermsViolated{
                reason = {invoice_unpayable, #payproc_InvoiceUnpayable{}}
            })
    end.

any_limit_matches(Cash, {value, CashRange}) ->
    hg_cash_range:is_inside(Cash, CashRange) =:= within;
any_limit_matches(Cash, {decisions, Decisions}) ->
    check_possible_limits(Cash, Decisions).

check_possible_limits(_Cash, []) ->
    false;
check_possible_limits(Cash, [#domain_CashLimitDecision{then_ = Value} | Rest]) ->
    case any_limit_matches(Cash, Value) of
        true ->
            true;
        false ->
            check_possible_limits(Cash, Rest)
    end.

-spec compute_shop_terms(party_id(), shop_id(), timestamp(), party_revision_param(), varset()) -> term_set().
compute_shop_terms(PartyID, ShopID, Timestamp, PartyRevision, Varset) ->
    {Client, Context} = get_party_client(),
    {ok, TermSet} =
        party_client_thrift:compute_shop_terms(PartyID, ShopID, Timestamp, PartyRevision, Varset, Client, Context),
    TermSet.

validate_currency_(Currency, Currency) ->
    ok;
validate_currency_(_, _) ->
    throw(#'InvalidRequest'{errors = [<<"Invalid currency">>]}).

get_shop_currency(#domain_Shop{account = #domain_ShopAccount{currency = Currency}}) ->
    Currency.

assert_party_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidPartyStatus{status = {blocking, V}}).

assert_party_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidPartyStatus{status = {suspension, V}}).

assert_shop_unblocked(V = {Status, _}) ->
    Status == unblocked orelse throw(#payproc_InvalidShopStatus{status = {blocking, V}}).

assert_shop_active(V = {Status, _}) ->
    Status == active orelse throw(#payproc_InvalidShopStatus{status = {suspension, V}}).

-spec get_cart_amount(cart()) -> cash().
get_cart_amount(#domain_InvoiceCart{lines = [FirstLine | Cart]}) ->
    lists:foldl(
        fun(Line, CashAcc) ->
            hg_cash:add(get_line_amount(Line), CashAcc)
        end,
        get_line_amount(FirstLine),
        Cart
    ).

get_line_amount(#domain_InvoiceLine{
    quantity = Quantity,
    price = #domain_Cash{amount = Amount, currency = Currency}
}) ->
    #domain_Cash{amount = Amount * Quantity, currency = Currency}.

-spec check_deadline(Deadline :: binary() | undefined) -> ok | {error, deadline_reached}.
check_deadline(undefined) ->
    ok;
check_deadline(Deadline) ->
    case hg_datetime:compare(Deadline, hg_datetime:format_now()) of
        later ->
            ok;
        _ ->
            {error, deadline_reached}
    end.

get_party_client() ->
    HgContext = hg_context:load(),
    Client = hg_context:get_party_client(HgContext),
    Context = hg_context:get_party_client_context(HgContext),
    {Client, Context}.

-spec get_identification_level(contract(), party()) -> identification_level().
get_identification_level(#domain_Contract{contractor_id = undefined, contractor = Contractor}, _) ->
    %% TODO legacy, remove after migration
    case Contractor of
        {legal_entity, _} ->
            full;
        _ ->
            none
    end;
get_identification_level(#domain_Contract{contractor_id = ContractorID}, #domain_Party{contractors = Contractors}) ->
    Contractor = maps:get(ContractorID, Contractors, undefined),
    Contractor#domain_PartyContractor.status.
