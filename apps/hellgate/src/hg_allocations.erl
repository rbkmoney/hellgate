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

calculate_refund_transactions(Transactions0, [], FeeTarget, Cost, Acc0) ->
    Transactions1 = lists:dropwhile(
        fun (T) ->
            case T of
                #domain_AllocationTransaction{target = FeeTarget} ->
                    true;
                _ ->
                    false
            end
        end,
        Transactions0
    ),
    Acc1 = [construct_aggregator_transaction(FeeTarget, Cost) | Acc0],
    Transactions1 ++ genlib_list:compact(Acc1);
calculate_refund_transactions(Transactions, [TP | TransactionPrototypes], FeeTarget, ?cash(Cost, SymCode), Acc0) ->
    #domain_AllocationTransactionPrototype{
        target = Target
    } = TP,
    {[RefundTargetTransaction], RemainingTransactions} =
        lists:partition(
            fun (T) ->
                case T of
                    #domain_AllocationTransaction{target = Target} ->
                        true;
                    _ ->
                        false
                end
            end,
            Transactions
        ),
    {?cash(Amount, SymCode), RefundedTransaction} = calculate_refunded_transaction(RefundTargetTransaction, TP),
    Acc1 = [RefundedTransaction | Acc0],
    calculate_refund_transactions(
        RemainingTransactions,
        TransactionPrototypes,
        FeeTarget,
        ?cash(Cost - Amount, SymCode),
        Acc1
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
    {?cash(RefundedAmount, SymCode), RefundedBody} = calculate_refund_body(Amount, Body, RefundBody),
    case RefundedAmount of
        _ when RefundedAmount == 0 ->
            {?cash(RefundedAmount, SymCode), undefined};
        _ when RefundedAmount < 0 ->
            %% TODO make real exception
            throw(allocation_refund_too_much);
        _ ->
            {?cash(RefundedAmount, SymCode), #domain_AllocationTransaction{
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
    {RefundFee, _} = calculate_allocation_transactions_fee(Fee, Total),
    {
        ?cash(Total - RefundAmount, SymCode),
        #domain_AllocationTransactionBodyTotal{
            fee_target = FeeTarget,
            total = ?cash(Total - RefundAmount, SymCode),
            fee_amount = ?cash(FeeAmount - RefundFee, SymCode),
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
    genlib_list:compact([construct_aggregator_transaction(FeeTarget, Cost) | Acc]);
calculate_allocation_transactions([Head | Transactions], FeeTarget, ?cash(Cost, SymCode), Acc0) ->
    #domain_AllocationTransactionPrototype{
        id = ID,
        target = Target,
        body = Body,
        details = Details
    } = Head,
    {TransformedBody, ?cash(TransactionCash, SymCode)} = calculate_allocation_transactions_body(Body, FeeTarget),
    Acc1 = [
        #domain_AllocationTransaction{
            id = ID,
            target = Target,
            amount = ?cash(TransactionCash, SymCode),
            body = TransformedBody,
            details = Details
        }
        | Acc0
    ],
    calculate_allocation_transactions(Transactions, FeeTarget, ?cash(Cost - TransactionCash, SymCode), Acc1).

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
    {?cash(CalculatedFee, SymCode), FeeShare} = calculate_allocation_transactions_fee(Fee, Total),
    TransformedBody = #domain_AllocationTransactionBodyTotal{
        fee_target = FeeTarget,
        total = Total,
        fee_amount = ?cash(CalculatedFee, SymCode),
        fee = FeeShare
    },
    ?cash(TotalAmount, SymCode) = Total,
    {TransformedBody, ?cash(TotalAmount - CalculatedFee, SymCode)}.

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
        rounding_method = RoundingMethod0
    } = Fee,
    RoundingMethod1 = get_rounding_method(RoundingMethod0),
    Amount = ?cash(genlib_rational:round({P * Total, Q}, RoundingMethod1), SymCode),
    FinalParts = #'Rational'{
        p = P * Total,
        q = Q
    },
    {Amount, #domain_AllocationTransactionFeeShare{
        parts = FinalParts,
        rounding_method = RoundingMethod1
    }}.

get_rounding_method(undefined) ->
    round_half_away_from_zero;
get_rounding_method(RoundingMethod) ->
    RoundingMethod.

construct_transaction_id({shop, #domain_AllocationTransactionTargetShop{
    owner_id = OwnerID,
    shop_id = ShopID
}}) ->
    unicode:characters_to_binary([OwnerID, <<"_">>, ShopID]).

construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode)) when Cost == 0 ->
    undefined;
construct_aggregator_transaction(_FeeTarget, ?cash(Cost, _SymCode)) when Cost < 0 ->
    throw(allocations_and_cost_do_not_match); %% TODO Real exception
construct_aggregator_transaction(FeeTarget, Cost) ->
    #domain_AllocationTransaction{
        id = construct_transaction_id(FeeTarget),
        target = FeeTarget,
        amount = Cost
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec allocation_1_test() -> _.
allocation_1_test() ->
     Cart = #domain_InvoiceCart{
        lines = [
            #domain_InvoiceLine{
                product = <<"STRING">>,
                quantity = 1,
                price = ?cash(30, <<"RUB">>),
                metadata = #{}
            }
        ]
    },
    AllocationPrototype = #domain_AllocationPrototype{
        transactions = [
            #domain_AllocationTransactionPrototype{
                id = <<"1">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY1">>,
                    shop_id = <<"SHOP1">>
                }},
                body = {amount, #domain_AllocationTransactionPrototypeBodyAmount{
                    amount = ?cash(30, <<"RUB">>)
                }},
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                }
            },
            #domain_AllocationTransactionPrototype{
                id = <<"2">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY2">>,
                    shop_id = <<"SHOP2">>
                }},
                body = {total, #domain_AllocationTransactionPrototypeBodyTotal{
                    total = ?cash(30, <<"RUB">>),
                    fee = {fixed, #domain_AllocationTransactionPrototypeFeeFixed{
                        amount = ?cash(10, <<"RUB">>)
                    }}
                }},
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                }
            },
            #domain_AllocationTransactionPrototype{
                id = <<"3">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY3">>,
                    shop_id = <<"SHOP3">>
                }},
                body = {total, #domain_AllocationTransactionPrototypeBodyTotal{
                    total = ?cash(30, <<"RUB">>),
                    fee = {share, #domain_AllocationTransactionPrototypeFeeShare{
                        parts = #'Rational'{
                            p = 15,
                            q = 100
                        }
                    }}
                }},
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                }
            }
        ]
    },
    #domain_Allocation{
        transactions = [
            #domain_AllocationTransaction{
                id = <<"PARTY0_SHOP0">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY0">>,
                    shop_id = <<"SHOP0">>
                }},
                amount = ?cash(25, <<"RUB">>)
            },
            #domain_AllocationTransaction{
                id = <<"3">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY3">>,
                    shop_id = <<"SHOP3">>
                }},
                amount = ?cash(25, <<"RUB">>),
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                },
                body = #domain_AllocationTransactionBodyTotal{
                    fee_target = {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = <<"PARTY0">>,
                        shop_id = <<"SHOP0">>
                    }},
                    total = ?cash(30, <<"RUB">>),
                    fee_amount = ?cash(5, <<"RUB">>)
                }
            },
            #domain_AllocationTransaction{
                id = <<"2">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY2">>,
                    shop_id = <<"SHOP2">>
                }},
                amount = ?cash(20, <<"RUB">>),
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                },
                body = #domain_AllocationTransactionBodyTotal{
                    fee_target = {shop, #domain_AllocationTransactionTargetShop{
                        owner_id = <<"PARTY0">>,
                        shop_id = <<"SHOP0">>
                    }},
                    total = ?cash(30, <<"RUB">>),
                    fee_amount = ?cash(10, <<"RUB">>)
                }
            },
            #domain_AllocationTransaction{
                id = <<"1">>,
                target = {shop, #domain_AllocationTransactionTargetShop{
                    owner_id = <<"PARTY1">>,
                    shop_id = <<"SHOP1">>
                }},
                amount = ?cash(30, <<"RUB">>),
                details = #domain_AllocationTransactionDetails{
                    cart = Cart
                }
            }
        ]
    } = calculate_allocation(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>)).

-endif.
