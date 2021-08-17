-module(hg_allocations).

-include("domain.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

%% API
-export([construct_target/1]).
-export([calculate_allocation/3]).
-export([calculate_allocation/4]).
-export([calculate_refund_allocation/5]).
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

-spec calculate_allocation(allocation_prototype() | undefined, owner_id(), shop_id(), cash()) ->
    allocation() | undefined.
calculate_allocation(AllocationPrototype, OwnerID, ShopID, Cost) ->
    FeeTarget = construct_target(#{
        owner_id => OwnerID,
        shop_id => ShopID
    }),
    calculate_allocation(AllocationPrototype, FeeTarget, Cost).

-spec construct_target(target_map()) -> target().
construct_target(#{owner_id := OwnerID, shop_id := ShopID}) ->
    {shop, #domain_AllocationTransactionTargetShop{
        owner_id = OwnerID,
        shop_id = ShopID
    }}.

-spec calculate_refund_allocation(allocation(), allocation_prototype(), owner_id(), shop_id(), cash()) -> allocation().
calculate_refund_allocation(Allocation, AllocationPrototype, OwnerID, ShopID, Cost) ->
    #domain_Allocation{
        transactions = Transactions
    } = Allocation,
    #domain_AllocationPrototype{
        transactions = TransactionsPrototype
    } = AllocationPrototype,
    FeeTarget = construct_target(#{
        owner_id => OwnerID,
        shop_id => ShopID
    }),
    #domain_Allocation{
        transactions = calculate_refund_transactions(Transactions, TransactionsPrototype, FeeTarget, Cost)
    }.

calculate_refund_transactions(Transactions, TransactionPrototypes, FeeTarget, Cost) ->
    calculate_refund_transactions(Transactions, TransactionPrototypes, FeeTarget, Cost, []).

calculate_refund_transactions(Transactions, [], FeeTarget, Cost, Acc) ->
    Transactions0 = lists:dropwhile(
        fun
            (#domain_AllocationTransaction{target = FeeTarget}) -> true;
            (_) -> false
        end,
        Transactions
    ),
    Acc0 = [
        #domain_AllocationTransaction{
            %% TODO Get some ID
            id = <<"">>,
            target = FeeTarget,
            amount = Cost
        }
        | Acc
    ],
    Transactions0 ++ genlib_list:compact(Acc0);
calculate_refund_transactions(Transactions, [TP | TransactionPrototypes], FeeTarget, ?cash(Cost, SymCode), Acc) ->
    #domain_AllocationTransactionPrototype{
        target = Target
    } = TP,
    {[RefundTargetTransaction], RemainingTransactions} =
        lists:partition(
            fun
                (#domain_AllocationTransaction{target = Target}) -> true;
                (_) -> false
            end,
            Transactions
        ),
    {?cash(Amount, SymCode), RefundedTransaction} = calculate_refunded_transaction(RefundTargetTransaction, TP),
    Acc0 = [RefundedTransaction | Acc],
    calculate_refund_transactions(
        RemainingTransactions,
        TransactionPrototypes,
        FeeTarget,
        ?cash(Cost - Amount, SymCode),
        Acc0
    ).

calculate_refunded_transaction(Transaction, TransactionPrototype) ->
    #domain_AllocationTransaction{
        id = ID,
        target = Target,
        amount = Amount,
        body = Body,
        details = Details
    } = Transaction,
    #domain_AllocationTransactionPrototype{
        % TODO Do something with it
        id = _IDPrototype,
        target = Target,
        body = RefundBody,
        details = RefundDetails
    } = TransactionPrototype,
    _ = validate_prototype_body_currency(Amount, RefundBody),
    {RefundedAmount, RefundedBody} = calculate_refund_body(Amount, Body, RefundBody),
    case RefundedAmount of
        _ when RefundedAmount == 0 ->
            {0, undefined};
        _ when RefundedAmount < 0 ->
            %% TODO make real exception
            throw(allocation_refund_too_much);
        _ ->
            {RefundedAmount, #domain_AllocationTransaction{
                id = ID,
                target = Target,
                amount = RefundedAmount,
                body = RefundedBody,
                details = calculate_refunded_details(Details, RefundDetails)
            }}
    end.

validate_prototype_body_currency(
    ?cash(_, SymCode),
    {amount, #domain_AllocationTransactionPrototypeBodyAmount{amount = ?cash(_, OtherSymCode)}}
) when SymCode /= OtherSymCode ->
    throw(#payproc_InconsistentRefundCurrency{currency = SymCode});
validate_prototype_body_currency(
    ?cash(_, SymCode),
    {total, #domain_AllocationTransactionPrototypeBodyTotal{total = ?cash(_, OtherSymCode)}}
) when SymCode /= OtherSymCode ->
    throw(#payproc_InconsistentRefundCurrency{currency = SymCode});
validate_prototype_body_currency(_Cash, _PrototypeBody) ->
    ok.

calculate_refund_body(_Cash, undefined, {total, _}) ->
    %% TODO change to real exception
    throw(unrefundable);
calculate_refund_body(?cash(Cash, SymCode), undefined, {amount, Amount}) ->
    #domain_AllocationTransactionPrototypeBodyAmount{
        amount = ?cash(RefundAmount, SymCode)
    } = Amount,
    {?cash(Cash - RefundAmount, SymCode), undefined};
calculate_refund_body(_Cash, Body, {total, Amount}) ->
    #domain_AllocationTransactionPrototypeBodyTotal{
        total = ?cash(RefundAmount, SymCode) = Total,
        fee = Fee
    } = Amount,
    #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = ?cash(Total, SymCode),
        fee_amount = ?cash(FeeAmount, SymCode),
        fee = FeeShare
    } = Body,
    _ = validate_same_fee_type(Fee, FeeShare),
    RefundFee = calculate_allocation_transactions_fee(Fee, Total),
    {
        ?cash(Total - RefundAmount, SymCode),
        #domain_AllocationTransactionBodyTotal{
            fee_target = FeeTarget,
            total = ?cash(Total - RefundAmount, SymCode),
            fee_amount = ?cash(FeeAmount, RefundFee, SymCode),
            fee = FeeShare
        }
    }.

validate_same_fee_type({fixed, _}, undefined) ->
    ok;
validate_same_fee_type({share, _}, #domain_AllocationTransactionFeeShare{}) ->
    ok;
validate_same_fee_type(_Fee, _FeeShare) ->
    %% TODO make real exception
    throw(fee_type_mismatch).

calculate_refunded_details(undefined, _RefundDetails) ->
    undefined;
calculate_refunded_details(Details, undefined) ->
    Details;
calculate_refunded_details(
    #domain_AllocationTransactionDetails{
        cart = Cart
    },
    #domain_AllocationTransactionDetails{
        cart = RefundCart
    }
) ->
    #domain_AllocationTransactionDetails{
        cart = calculate_refunded_cart(Cart, RefundCart)
    }.

calculate_refunded_cart(undefined, _RefundCart) ->
    undefined;
calculate_refunded_cart(Cart, undefined) ->
    Cart;
calculate_refunded_cart(#domain_InvoiceCart{lines = Lines}, #domain_InvoiceCart{lines = RefundLines}) ->
    #domain_InvoiceCart{
        lines = lists:subtract(Lines, RefundLines)
    }.

-spec calculate_allocation(allocation_prototype() | undefined, target(), cash()) -> allocation() | undefined.
calculate_allocation(undefined, _FeeTarget, _Cost) ->
    undefined;
calculate_allocation(#domain_AllocationPrototype{transactions = Transactions}, FeeTarget, Cost) ->
    #domain_Allocation{
        transactions = calculate_allocation_transactions(Transactions, FeeTarget, Cost)
    }.

-spec assert_allocatable(allocation_prototype() | undefined, allocation_terms()) -> ok | no_return().
assert_allocatable(undefined, _AllocationTerms) ->
    ok;
assert_allocatable(_Allocation, #domain_PaymentAllocationServiceTerms{allow = Allow}) ->
    case Allow of
        {constant, true} ->
            ok;
        _ ->
            throw(#payproc_InvoiceTermsViolated{
                reason = {invoice_unallocatable, #payproc_InvoiceUnallocatable{}}
            })
    end.

calculate_allocation_transactions(Transactions, FeeTarget, Cost) ->
    calculate_allocation_transactions(Transactions, FeeTarget, Cost, []).

calculate_allocation_transactions([], FeeTarget, Cost, Acc) ->
    [
        #domain_AllocationTransaction{
            %% TODO Get some ID
            id = <<"">>,
            target = FeeTarget,
            amount = Cost
        }
        | Acc
    ];
calculate_allocation_transactions([Head | Transactions], FeeTarget, Cost, Acc) ->
    #domain_AllocationTransactionPrototype{
        id = ID,
        target = Target,
        body = Body,
        details = Details
    } = Head,
    {TransformedBody, TransactionCash} = calculate_allocation_transactions_body(Body, FeeTarget),
    Acc0 = [
        #domain_AllocationTransaction{
            id = ID,
            target = Target,
            amount = TransactionCash,
            body = TransformedBody,
            details = Details
        }
        | Acc
    ],
    calculate_allocation_transactions(Transactions, FeeTarget, Cost - TransactionCash, Acc0).

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
    {CalculatedFee, Fee} = calculate_allocation_transactions_fee(Fee, Total),
    TransformedBody = #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = Total,
        fee_amount = CalculatedFee,
        fee = Fee
    },
    {TransformedBody, Total - CalculatedFee}.

calculate_allocation_transactions_fee({fixed, Fee}, _Total) ->
    #domain_AllocationTransactionPrototypeFeeFixed{
        amount = Amount
    } = Fee,
    {Amount, undefined};
calculate_allocation_transactions_fee({share, Fee}, ?cash(Total, SymCode)) ->
    #domain_AllocationTransactionPrototypeFeeShare{
        parts = #'Rational'{
            p = P,
            q = Q
        },
        rounding_method = RoundingMethod
    } = Fee,
    Amount = ?cash(genlib_rational:round({P * Total, Q}, RoundingMethod), SymCode),
    FinalParts = #'Rational'{
        p = P * Total,
        q = Q
    },
    {Amount, #domain_AllocationTransactionFeeShare{
        parts = FinalParts,
        rounding_method = RoundingMethod
    }}.
