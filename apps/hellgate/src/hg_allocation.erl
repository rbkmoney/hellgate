-module(hg_allocation).

-include("domain.hrl").
-include("allocation.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

%% API
-export([calculate/4]).
-export([sub/5]).
-export([assert_allocatable/2]).

-export_type([allocation_prototype/0]).
-export_type([allocation/0]).

-type allocation_prototype() :: dmsl_domain_thrift:'AllocationPrototype'().
-type allocation() :: dmsl_domain_thrift:'Allocation'().
-type allocation_terms() :: dmsl_domain_thrift:'PaymentAllocationServiceTerms'().
-type target() :: dmsl_domain_thrift:'AllocationTransactionTarget'().
-type cash() :: dmsl_domain_thrift:'Cash'().

-type owner_id() :: dmsl_payment_processing_thrift:'PartyID'().
-type shop_id() :: dmsl_payment_processing_thrift:'ShopID'().
-type target_map() :: #{
    owner_id => owner_id(),
    shop_id => shop_id()
}.

-type sub_errors() ::
    no_transaction_to_sub |
    multiple_transactions_to_sub |
    transaction_amount_sub_too_much |
    currency_mismatch |
    sub_body_from_undefined |
    sub_undefined_from_body |
    sub_body_target_mismatch |
    sub_from_undefined_fee |
    sub_undefined_fee |
    sub_from_undefined_details |
    sub_from_undefined_cart |
    allocations_and_cost_do_not_match.

-type calculate_errors() ::
    currency_mismatch |
    allocations_and_cost_do_not_match.

-spec calculate(allocation_prototype(), owner_id(), shop_id(), cash()) ->
{ok, allocation()} | {error, calculate_errors()}.
calculate(AllocationPrototype, OwnerID, ShopID, Cost) ->
    FeeTarget = construct_target(#{
        owner_id => OwnerID,
        shop_id => ShopID
    }),
    try calculate(AllocationPrototype, FeeTarget, Cost) of
        Result ->
            {ok, Result}
    catch
        throw:Error ->
            {error, Error}
    end.

-spec sub(allocation(), allocation(), owner_id(), shop_id(), cash()) ->
    {ok, allocation()} | {error, sub_errors()}.
sub(Allocation, SubAllocation, OwnerID, ShopID, Cost) ->
    FeeTarget = construct_target(#{
        owner_id => OwnerID,
        shop_id => ShopID
    }),
    ?allocation(Transactions) = Allocation,
    ?allocation(SubTransactions) = SubAllocation,
    try sub_trxs(Transactions, SubTransactions, FeeTarget, Cost) of
        ResTransactions ->
            {ok, ?allocation(ResTransactions)}
    catch
        throw:Error ->
            {error, Error}
    end.

-spec assert_allocatable(allocation_prototype() | undefined, allocation_terms()) -> ok | {error, unallocatable}.
assert_allocatable(undefined, _AllocationTerms) ->
    ok;
assert_allocatable(_Allocation, #domain_PaymentAllocationServiceTerms{allow = Allow}) ->
    case Allow of
        {constant, true} ->
            ok;
        _ ->
            {error, unallocatable}
    end.

-spec construct_target(target_map()) -> target().
construct_target(#{owner_id := OwnerID, shop_id := ShopID}) ->
    ?allocation_trx_target_shop(OwnerID, ShopID).

-spec calculate(allocation_prototype(), target(), cash()) -> allocation().
calculate(?allocation_prototype(Transactions), FeeTarget, Cost) ->
    #domain_Allocation{
        transactions = calculate_trxs(Transactions, FeeTarget, Cost)
    }.

sub_trxs(Transactions, SubTransactions, FeeTarget, Cost) ->
    sub_trxs(Transactions, SubTransactions, FeeTarget, Cost, []).

sub_trxs(Transactions0, [], FeeTarget, Cost0, Acc0) ->
    {Cost1, Transactions1} = lists:foldl(
        fun(T, {AccCost, Acc}) ->
            case T of
                #domain_AllocationTransaction{target = FeeTarget} ->
                    {AccCost, Acc};
                #domain_AllocationTransaction{amount = Amount} = T ->
                    {sub_amount(AccCost, Amount), [T | Acc]}
            end
        end,
        {Cost0, []},
        Transactions0
    ),
    Acc1 = Transactions1 ++ Acc0,
    case Cost1 of
        ?cash(Cost, _SymCode) when Cost /= 0->
            throw(allocations_and_cost_do_not_match);
        _ ->
            ok
    end,
    genlib_list:compact(Acc1);
sub_trxs(Transactions0, [ST | SubTransactions], FeeTarget, Cost, Acc0) ->
    #domain_AllocationTransaction{
        target = Target
    } = ST,
    {Transaction, Transactions1} = take_trx(Target, Transactions0),
    {ResAmount, ResTransaction} = sub_trx(Transaction, ST),
    Acc1 = [ResTransaction | Acc0],
    sub_trxs(Transactions1, SubTransactions, FeeTarget, sub_amount(Cost, ResAmount), Acc1).

take_trx(Target, Transactions) ->
    Fun = fun(T) ->
        case T of
            #domain_AllocationTransaction{target = Target} ->
                true;
            _ ->
                false
        end
          end,
    case lists:partition(Fun, Transactions) of
        {[], Transactions} ->
            throw(no_transaction_to_sub);
        {[_Transaction1, _Transaction2 | _], _RemainingTransactions} ->
            throw(multiple_transactions_to_sub);
        {[Transaction], RemainingTransactions} ->
            {Transaction, RemainingTransactions}
    end.

sub_trx(Transaction, SubTransaction) ->
    ?allocation_trx(ID, Target, Amount, Details, Body) = Transaction,
    ?allocation_trx(_ID, Target, SubAmount, SubDetails, SubBody) = SubTransaction,
    ResAmount = sub_amount(Amount, SubAmount),
    ResBody = sub_body(Body, SubBody),
    ResDetails = sub_details(Details, SubDetails),
    case ResAmount of
        ?cash(A, _) when A == 0 ->
            {ResAmount, undefined};
        ?cash(A, _) when A < 0 ->
            throw(transaction_amount_sub_too_much);
        _ ->
            {ResAmount, #domain_AllocationTransaction{
                id = ID,
                target = Target,
                amount = ResAmount,
                body = ResBody,
                details = ResDetails
            }}
    end.

sub_amount(Amount, SubAmount) ->
    try hg_cash:sub(Amount, SubAmount) of
        Res ->
            Res
    catch
        error:badarg ->
            throw(currency_mismatch)
    end.

sub_body(undefined, undefined) ->
    undefined;
sub_body(undefined, SubBody) when SubBody /= undefined ->
    throw(sub_body_from_undefined);
sub_body(Body, undefined) when Body /= undefined ->
    throw(sub_undefined_from_body);
sub_body(
    ?allocation_trx_body_total(FeeTarget1, _, _),
    ?allocation_trx_body_total(FeeTarget2, _, _)
) when FeeTarget1 /= FeeTarget2 ->
    throw(sub_body_target_mismatch);
sub_body(Body, SubBody) ->
    ?allocation_trx_body_total(FeeTarget, Total, FeeAmount, Fee) = Body,
    ?allocation_trx_body_total(FeeTarget, SubTotal, SubFeeAmount, SubFee) = SubBody,
    ?allocation_trx_body_total(
        FeeTarget,
        sub_amount(Total, SubTotal),
        sub_amount(FeeAmount, SubFeeAmount),
        sub_fee(Fee, SubFee)
    ).

sub_fee(undefined, undefined) ->
    undefined;
sub_fee(undefined, #domain_AllocationTransactionFeeShare{}) ->
    throw(sub_from_undefined_fee);
sub_fee(#domain_AllocationTransactionFeeShare{}, undefined) ->
    throw(sub_undefined_fee);
sub_fee(Fee, _SubFee) ->
    Fee. %% TODO Do something smarter about it

sub_details(undefined, undefined) ->
    undefined;
sub_details(undefined, _SubDetails) ->
    throw(sub_from_undefined_details);
sub_details(Details, undefined) ->
    Details;
sub_details(?allocation_trx_details(Cart), ?allocation_trx_details(SubCart)) ->
    ?allocation_trx_details(sub_cart(Cart, SubCart)).

sub_cart(undefined, undefined) ->
    undefined;
sub_cart(undefined, _SubCart) ->
    throw(sub_from_undefined_cart);
sub_cart(Cart, undefined) ->
    Cart;
sub_cart(?invoice_cart(Lines), ?invoice_cart(SubLines)) ->
    ?invoice_cart(lists:subtract(Lines, SubLines)).

calculate_trxs(Transactions, FeeTarget, Cost) ->
    calculate_trxs(Transactions, FeeTarget, Cost, undefined, {1, []}).

calculate_trxs([], FeeTarget, CostLeft, FeeCost, {ID, Acc}) ->
    AggregatorCost = validate_cost(CostLeft, FeeCost),
    genlib_list:compact([construct_aggregator_transaction(FeeTarget, AggregatorCost, ID)]) ++ Acc;
calculate_trxs([Head | Transactions], FeeTarget, CostLeft, FeeCost, {ID0, Acc0}) ->
    ?allocation_trx_prototype(Target, Body, Details) = Head,
    {TransactionBody, TransactionAmount, FeeAmount} = calculate_trxs_body(Body, FeeTarget),
    Acc1 = [
        ?allocation_trx(erlang:integer_to_binary(ID0), Target, TransactionAmount, Details, TransactionBody)
        | Acc0
    ],
    ID1 = ID0 + 1,
    calculate_trxs(Transactions, FeeTarget, sub_amount(CostLeft, TransactionAmount), add_fee(FeeCost, FeeAmount), {ID1, Acc1}).

validate_cost(?cash(CostLeft, SymCode), ?cash(FeeCost, SymCode)) when FeeCost > CostLeft ->
    throw(allocations_and_cost_do_not_match);
validate_cost(CostLeft, _FeeCost) ->
    CostLeft.

add_fee(undefined, FeeAmount) ->
    FeeAmount;
add_fee(FeeCost, FeeAmount) ->
    hg_cash:add(FeeCost, FeeAmount).

calculate_trxs_body(?allocation_trx_prototype_body_amount(?cash(_, SymCode) = Amount), _FeeTarget) ->
    {undefined, Amount, ?cash(0, SymCode)};
calculate_trxs_body(?allocation_trx_prototype_body_total(Total, Fee), FeeTarget) ->
    CalculatedFee = calculate_trxs_fee(Fee, Total),
    TransformedBody = #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = Total,
        fee_amount = CalculatedFee,
        fee = get_fee_share(Fee)
    },
    {TransformedBody, sub_amount(Total, CalculatedFee), CalculatedFee}.

calculate_trxs_fee(?allocation_trx_prototype_fee_fixed(Amount), _Total) ->
    Amount;
calculate_trxs_fee(?allocation_trx_prototype_fee_share(P, Q, RoundingMethod0), ?cash(Total, SymCode)) ->
    RoundingMethod1 = get_rounding_method(RoundingMethod0),
    R = genlib_rational:new(P * Total, Q),
    Amount = ?cash(genlib_rational:round(R, RoundingMethod1), SymCode),
    Amount.

get_fee_share({fixed, _Fee}) ->
    undefined;
get_fee_share({share, Fee}) ->
    Fee.

get_rounding_method(undefined) ->
    round_half_away_from_zero;
get_rounding_method(RoundingMethod) ->
    RoundingMethod.

construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode), _ID) when Cost == 0 ->
    undefined;
construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode), _ID) when Cost < 0 ->
    throw(allocations_and_cost_do_not_match);
construct_aggregator_transaction(FeeTarget, Cost, ID) ->
    #domain_AllocationTransaction{
        id = erlang:integer_to_binary(ID),
        target = FeeTarget,
        amount = Cost
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

generic_prototype() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(15, 100)
            ),
            ?allocation_trx_details(Cart)
        )
    ]).

-spec allocation_1_test() -> _.
allocation_1_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    {ok, ?allocation([
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(25, <<"RUB">>)
        ),
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_trx_fee_share(15, 100)
            )
        ),
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_trx(
            <<"1">>,
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        )
    ])} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)).

-spec allocation_2_test() -> _.
allocation_2_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart)
        )
    ]),
    {ok, ?allocation([
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx(
            <<"1">>,
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        )
    ])} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)).

-spec allocation_3_test() -> _.
allocation_3_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_total(
                ?cash(50, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(1, 2)
            ),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_total(
                ?cash(50, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(1, 2)
            ),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_total(
                ?cash(50, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(1, 2)
            ),
            ?allocation_trx_details(Cart)
        )
    ]),
    {error, allocations_and_cost_do_not_match} =
        calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)).

-spec allocation_refund_one_transaction_1_test() -> _.
allocation_refund_one_transaction_1_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>))
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(40, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_trx_fee_share(15, 100)
            )
        ),
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(15, <<"RUB">>)
        )
    ])} = sub(
        Allocation,
        RefundAllocation,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(60, <<"RUB">>)
    ).

-spec allocation_refund_one_transaction_2_test() -> _.
allocation_refund_one_transaction_2_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_trx_details(Cart)
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(30, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_trx(
            <<"1">>,
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_trx_fee_share(15, 100)
            )
        ),
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(15, <<"RUB">>)
        )
    ])} = sub(
        Allocation,
        RefundAllocation,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(70, <<"RUB">>)
    ).

-spec allocation_refund_one_transaction_3_test() -> _.
allocation_refund_one_transaction_3_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(15, 100)
            ),
            ?allocation_trx_details(Cart)
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(30, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_trx(
            <<"1">>,
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_trx_details(Cart)
        ),
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_trx_details(Cart),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(20, <<"RUB">>)
        )
    ])} = sub(
        Allocation,
        RefundAllocation,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(70, <<"RUB">>)
    ).

-spec allocation_partial_transaction_refund_1_test() -> _.
allocation_partial_transaction_refund_1_test() ->
    Cart0 = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    Cart1 = ?invoice_cart([
        ?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>)),
        ?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))
    ]),
    AllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart1)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_trx_details(Cart0)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(15, 100)
            ),
            ?allocation_trx_details(Cart0)
        )
    ]),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(18, <<"RUB">>)),
            ?allocation_trx_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))]))
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(24, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_trx_details(Cart0),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_trx_details(Cart0),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_trx_fee_share(15, 100)
            )
        ),
        ?allocation_trx(
            <<"1">>,
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(12, <<"RUB">>),
            ?allocation_trx_details(?invoice_cart([
                ?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>))
            ]))
        ),
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(19, <<"RUB">>)
        )
    ])} = sub(
        Allocation,
        RefundAllocation,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(76, <<"RUB">>)
    ).

-spec allocation_partial_transaction_refund_1_continue_test() -> _.
allocation_partial_transaction_refund_1_continue_test() ->
    Cart0 = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    Cart1 = ?invoice_cart([
        ?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>)),
        ?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))
    ]),
    AllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_trx_details(Cart1)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_trx_details(Cart0)
        ),
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_trx_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_trx_prototype_fee_share(15, 100)
            ),
            ?allocation_trx_details(Cart0)
        )
    ]),
    RefundAllocationPrototype0 = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(18, <<"RUB">>)),
            ?allocation_trx_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))]))
        )
    ]),
    RefundAllocationPrototype1 = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(12, <<"RUB">>)),
            ?allocation_trx_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>))]))
        )
    ]),
    {ok, Allocation0} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation0} = calculate(RefundAllocationPrototype0, <<"PARTY0">>, <<"SHOP0">>, ?cash(24, <<"RUB">>)),
    {ok, Allocation1} = sub(
        Allocation0,
        RefundAllocation0,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(76, <<"RUB">>)
    ),
    {ok, RefundAllocation1} = calculate(RefundAllocationPrototype1, <<"PARTY0">>, <<"SHOP0">>, ?cash(16, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_trx(
            <<"3">>,
            ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_trx_details(Cart0),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_trx_fee_share(15, 100)
            )
        ),
        ?allocation_trx(
            <<"2">>,
            ?allocation_trx_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_trx_details(Cart0),
            ?allocation_trx_body_total(
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_trx(
            <<"4">>,
            ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(15, <<"RUB">>)
        )
    ])} = sub(
        Allocation1,
        RefundAllocation1,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(60, <<"RUB">>)
    ).

-endif.
