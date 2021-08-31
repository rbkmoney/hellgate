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
    try calculate_allocation(AllocationPrototype, FeeTarget, Cost) of
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
    #domain_Allocation{
        transactions = Transactions
    } = Allocation,
    #domain_Allocation{
        transactions = SubTransactions
    } = SubAllocation,
    try sub_transactions(Transactions, SubTransactions, FeeTarget, Cost) of
        ResTransactions ->
            {ok, #domain_Allocation{
                transactions = ResTransactions
            }}
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
    {shop, #domain_AllocationTransactionTargetShop{
        owner_id = OwnerID,
        shop_id = ShopID
    }}.

-spec calculate_allocation(allocation_prototype(), target(), cash()) -> allocation().
calculate_allocation(#domain_AllocationPrototype{transactions = Transactions}, FeeTarget, Cost) ->
    #domain_Allocation{
        transactions = calculate_allocation_transactions(Transactions, FeeTarget, Cost)
    }.

sub_transactions(Transactions, SubTransactions, FeeTarget, Cost) ->
    sub_transactions(Transactions, SubTransactions, FeeTarget, Cost, []).

sub_transactions(Transactions0, [], FeeTarget, Cost0, Acc0) ->
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
    genlib_list:compact([construct_aggregator_transaction(FeeTarget, Cost1) | Acc1]);
sub_transactions(Transactions0, [ST | SubTransactions], FeeTarget, Cost, Acc0) ->
    #domain_AllocationTransaction{
        target = Target
    } = ST,
    {Transaction, Transactions1} = take_transaction(Target, Transactions0),
    {ResAmount, ResTransaction} = sub_transaction(Transaction, ST),
    Acc1 = [ResTransaction | Acc0],
    sub_transactions(Transactions1, SubTransactions, FeeTarget, sub_amount(Cost, ResAmount), Acc1).

take_transaction(Target, Transactions) ->
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

sub_transaction(Transaction, SubTransaction) ->
    #domain_AllocationTransaction{
        id = ID,
        target = Target,
        amount = Amount,
        body = Body,
        details = Details
    } = Transaction,
    #domain_AllocationTransaction{
        id = _ID,
        target = Target,
        amount = SubAmount,
        body = SubBody,
        details = SubDetails
    } = SubTransaction,
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
    #domain_AllocationTransactionBodyTotal{fee_target = FeeTarget1},
    #domain_AllocationTransactionBodyTotal{fee_target = FeeTarget2}
) when FeeTarget1 /= FeeTarget2 ->
    throw(sub_body_target_mismatch);
sub_body(Body, SubBody) ->
    #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = Total,
        fee_amount = FeeAmount,
        fee = Fee
    } = Body,
    #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = SubTotal,
        fee_amount = SubFeeAmount,
        fee = SubFee
    } = SubBody,
    #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = sub_amount(Total, SubTotal),
        fee_amount = sub_amount(FeeAmount, SubFeeAmount),
        fee = sub_fee(Fee, SubFee)
    }.

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
sub_details(
    #domain_AllocationTransactionDetails{
        cart = Cart
    },
    #domain_AllocationTransactionDetails{
        cart = SubCart
    }
) ->
    #domain_AllocationTransactionDetails{
        cart = sub_cart(Cart, SubCart)
    }.

sub_cart(undefined, undefined) ->
    undefined;
sub_cart(undefined, _SubCart) ->
    throw(sub_from_undefined_cart);
sub_cart(Cart, undefined) ->
    Cart;
sub_cart(#domain_InvoiceCart{lines = Lines}, #domain_InvoiceCart{lines = SubLines}) ->
    #domain_InvoiceCart{
        lines = lists:subtract(Lines, SubLines)
    }.

calculate_allocation_transactions(Transactions, FeeTarget, Cost) ->
    calculate_allocation_transactions(Transactions, FeeTarget, Cost, []).

calculate_allocation_transactions([], FeeTarget, Cost, Acc) ->
    genlib_list:compact([construct_aggregator_transaction(FeeTarget, Cost) | Acc]);
calculate_allocation_transactions([Head | Transactions], FeeTarget, Cost, Acc0) ->
    #domain_AllocationTransactionPrototype{
        id = ID,
        target = Target,
        body = Body,
        details = Details
    } = Head,
    {TransformedBody, TransactionCash} = calculate_allocation_transactions_body(Body, FeeTarget),
    Acc1 = [
        #domain_AllocationTransaction{
            id = ID,
            target = Target,
            amount = TransactionCash,
            body = TransformedBody,
            details = Details
        }
        | Acc0
    ],
    calculate_allocation_transactions(Transactions, FeeTarget, sub_amount(Cost, TransactionCash), Acc1).

calculate_allocation_transactions_body({amount, Body}, _FeeTarget) ->
    #domain_AllocationTransactionPrototypeBodyAmount{
        amount = Amount
    } = Body,
    {undefined, Amount};
calculate_allocation_transactions_body({total, Body}, FeeTarget) ->
    #domain_AllocationTransactionPrototypeBodyTotal{
        total = Total,
        fee = Fee
    } = Body,
    CalculatedFee = calculate_allocation_transactions_fee(Fee, Total),
    TransformedBody = #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = Total,
        fee_amount = CalculatedFee,
        fee = get_fee_share(Fee)
    },
    {TransformedBody, sub_amount(Total, CalculatedFee)}.

calculate_allocation_transactions_fee({fixed, Fee}, _Total) ->
    #domain_AllocationTransactionPrototypeFeeFixed{
        amount = Amount
    } = Fee,
    Amount;
calculate_allocation_transactions_fee({share, Fee}, ?cash(Total, SymCode)) ->
    #domain_AllocationTransactionFeeShare{
        parts = #'Rational'{
            p = P,
            q = Q
        },
        rounding_method = RoundingMethod0
    } = Fee,
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

construct_transaction_id(
    {shop, #domain_AllocationTransactionTargetShop{
        owner_id = OwnerID,
        shop_id = ShopID
    }}
) ->
    unicode:characters_to_binary([OwnerID, <<"_">>, ShopID]).

construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode)) when Cost == 0 ->
    undefined;
construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode)) when Cost < 0 ->
    throw(allocations_and_cost_do_not_match);
construct_aggregator_transaction(FeeTarget, Cost) ->
    #domain_AllocationTransaction{
        id = construct_transaction_id(FeeTarget),
        target = FeeTarget,
        amount = Cost
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

generic_prototype() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction_prototype(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction_prototype(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_share(15, 100)
            ),
            ?allocation_transaction_details(Cart)
        )
    ]).

-spec allocation_1_test() -> _.
allocation_1_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
            ?cash(25, <<"RUB">>)
        ),
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_transaction_fee_share(15, 100)
            )
        ),
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_transaction(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        )
    ])} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)).

-spec allocation_2_test() -> _.
allocation_2_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction_prototype(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction_prototype(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart)
        )
    ]),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        )
    ])} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)).

-spec allocation_refund_one_transaction_1_test() -> _.
allocation_refund_one_transaction_1_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>))
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(40, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_transaction_fee_share(15, 100)
            )
        ),
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
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
        ?allocation_transaction_prototype(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_transaction_details(Cart)
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(30, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_transaction_fee_share(15, 100)
            )
        ),
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
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
        ?allocation_transaction_prototype(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_share(15, 100)
            ),
            ?allocation_transaction_details(Cart)
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(30, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(30, <<"RUB">>),
            ?allocation_transaction_details(Cart)
        ),
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_transaction_details(Cart),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
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
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart1)
        ),
        ?allocation_transaction_prototype(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_transaction_details(Cart0)
        ),
        ?allocation_transaction_prototype(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_share(15, 100)
            ),
            ?allocation_transaction_details(Cart0)
        )
    ]),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(18, <<"RUB">>)),
            ?allocation_transaction_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))]))
        )
    ]),
    {ok, Allocation} = calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)),
    {ok, RefundAllocation} = calculate(RefundAllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(24, <<"RUB">>)),
    {ok, ?allocation([
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_transaction_details(Cart0),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_transaction_details(Cart0),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_transaction_fee_share(15, 100)
            )
        ),
        ?allocation_transaction(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?cash(12, <<"RUB">>),
            ?allocation_transaction_details(?invoice_cart([
                ?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>))
            ]))
        ),
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
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
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(30, <<"RUB">>)),
            ?allocation_transaction_details(Cart1)
        ),
        ?allocation_transaction_prototype(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_fixed(?cash(10, <<"RUB">>))
            ),
            ?allocation_transaction_details(Cart0)
        ),
        ?allocation_transaction_prototype(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?allocation_transaction_prototype_body_total(
                ?cash(30, <<"RUB">>),
                ?allocation_transaction_prototype_fee_share(15, 100)
            ),
            ?allocation_transaction_details(Cart0)
        )
    ]),
    RefundAllocationPrototype0 = ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(18, <<"RUB">>)),
            ?allocation_transaction_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(18, <<"RUB">>))]))
        )
    ]),
    RefundAllocationPrototype1 = ?allocation_prototype([
        ?allocation_transaction_prototype(
            <<"1">>,
            ?allocation_transaction_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_transaction_prototype_body_amount(?cash(12, <<"RUB">>)),
            ?allocation_transaction_details(?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(12, <<"RUB">>))]))
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
        ?allocation_transaction(
            <<"3">>,
            ?allocation_transaction_target_shop(<<"PARTY3">>, <<"SHOP3">>),
            ?cash(25, <<"RUB">>),
            ?allocation_transaction_details(Cart0),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(5, <<"RUB">>),
                ?allocation_transaction_fee_share(15, 100)
            )
        ),
        ?allocation_transaction(
            <<"2">>,
            ?allocation_transaction_target_shop(<<"PARTY2">>, <<"SHOP2">>),
            ?cash(20, <<"RUB">>),
            ?allocation_transaction_details(Cart0),
            ?allocation_transaction_body_total(
                ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(30, <<"RUB">>),
                ?cash(10, <<"RUB">>)
            )
        ),
        ?allocation_transaction(
            <<"PARTY0_SHOP0">>,
            ?allocation_transaction_target_shop(<<"PARTY0">>, <<"SHOP0">>),
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
