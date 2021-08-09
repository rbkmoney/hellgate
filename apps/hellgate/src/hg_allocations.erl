-module(hg_allocations).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API
-export([construct_target/1]).
-export([calculate_allocation/2]).
-export([calculate_allocation/3]).

-export_type([allocation_prototype/0]).
-export_type([allocation/0]).

-type allocation_prototype() :: dmsl_domain_thrift:'AllocationPrototype'().
-type allocation() :: dmsl_domain_thrift:'Allocation'().
-type target() :: dmsl_domain_thrift:'AllocationTransactionTarget'().

-type owner_id() :: dmsl_payment_processing_thrift:'PartyID'().
-type shop_id() :: dmsl_payment_processing_thrift:'ShopID'().
-type target_map() :: #{
    owner_id => owner_id(),
    shop_id => shop_id()
}.

-spec calculate_allocation(allocation_prototype() | undefined, owner_id(), shop_id()) -> allocation() | undefined.
calculate_allocation(AllocationPrototype, OwnerID, ShopID) ->
    FeeTarget = construct_target(#{
        owner_id => OwnerID,
        shop_id => ShopID
    }),
    calculate_allocation(AllocationPrototype, OwnerID, ShopID).

-spec construct_target(target_map()) -> target().
construct_target(#{owner_id := OwnerID, shop_id := ShopID}) ->
    {shop, #domain_AllocationTransactionTargetShop{
        owner_id = OwnerID,
        shop_id = ShopID
    }}.

-spec calculate_allocation(allocation_prototype() | undefined, target()) -> allocation() | undefined.
calculate_allocation(undefined, _FeeTarget) ->
    undefined;
calculate_allocation(#domain_AllocationPrototype{transactions = Transactions}, FeeTarget) ->
    #domain_Allocation{
        transactions = calculate_allocation_transactions(Transactions, FeeTarget)
    }.

calculate_allocation_transactions(Transactions, FeeTarget) ->
    calculate_allocation_transactions(Transactions, FeeTarget, []).

calculate_allocation_transactions([], _FeeTarget, Acc) ->
    Acc;
calculate_allocation_transactions([Head | Transactions], FeeTarget, Acc) ->
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
    calculate_allocation_transactions(Transactions, Acc0).

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
    {TransformedBody, Total}.

calculate_allocation_transactions_fee({fixed, Fee}, _Total) ->
    #domain_AllocationTransactionPrototypeFeeFixed{
        amount = Amount
    } = Fee,
    {Amount, undefined};
calculate_allocation_transactions_fee({share, Fee}, _Total) ->
    #domain_AllocationTransactionPrototypeFeeShare{
        parts =
            #'Rational'{
                p = P,
                q = Q
            } = Parts,
        rounding_method = RoundingMethod
    } = Fee,
    Amount = genlib_rational:round({P, Q}, RoundingMethod),
    {Amount, #domain_AllocationTransactionFeeShare{
        parts = Parts,
        rounding_method = RoundingMethod
    }}.
