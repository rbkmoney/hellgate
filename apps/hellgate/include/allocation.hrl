-ifndef(__hellgate_allocation__).
-define(__hellgate_allocation__, true).
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Prototypes

-define(allocation_prototype(Transactions), #domain_AllocationPrototype{
    transactions = Transactions
}).

-define(allocation_transaction_prototype(ID, Target, Body), ?allocation_transaction_prototype(ID, Target, Body, undefined)).

-define(allocation_transaction_prototype(ID, Target, Body, Details), #domain_AllocationTransactionPrototype{
    id = ID,
    target = Target,
    body = Body,
    details = Details
}).

-define(allocation_transaction_prototype_body_amount(Amount), {amount, #domain_AllocationTransactionPrototypeBodyAmount{
    amount = Amount
}}).

-define(allocation_transaction_prototype_body_total(Total, Fee), {total, #domain_AllocationTransactionPrototypeBodyTotal{
    total = Total,
    fee = Fee
}}).

-define(allocation_transaction_prototype_fee_fixed(Amount), {fixed, #domain_AllocationTransactionPrototypeFeeFixed{
    amount = Amount
}}).

-define(allocation_transaction_prototype_fee_share(P, Q), {share, ?allocation_transaction_fee_share(P, Q, undefined)}).

%% Final

-define(allocation(Transactions), #domain_Allocation{
    transactions = Transactions
}).

-define(allocation_transaction(ID, Target, Amount), ?allocation_transaction(ID, Target, Amount, undefined, undefined)).

-define(allocation_transaction(ID, Target, Amount, Details), ?allocation_transaction(ID, Target, Amount, Details, undefined)).

-define(allocation_transaction(ID, Target, Amount, Details, Body), #domain_AllocationTransaction{
    id = ID,
    target = Target,
    amount = Amount,
    details = Details,
    body = Body
}).

-define(allocation_transaction_target_shop(OwnerID, ShopID), {shop, #domain_AllocationTransactionTargetShop{
    owner_id = OwnerID,
    shop_id = ShopID
}}).

-define(allocation_transaction_details(Cart), #domain_AllocationTransactionDetails{
    cart = Cart
}).

-define(allocation_transaction_body_total(FeeTarget, Total, FeeAmount),
    ?allocation_transaction_body_total(FeeTarget, Total, FeeAmount, undefined)).

-define(allocation_transaction_body_total(FeeTarget, Total, FeeAmount, Fee), #domain_AllocationTransactionBodyTotal{
    fee_target = FeeTarget,
    total = Total,
    fee_amount = FeeAmount,
    fee = Fee
}).

-define(allocation_transaction_fee_share(P, Q), ?allocation_transaction_fee_share(P, Q, undefined)).

-define(allocation_transaction_fee_share(P, Q, RoundingMethod), #domain_AllocationTransactionFeeShare{
    parts = #'Rational'{
        p = P,
        q = Q
    },
    rounding_method = RoundingMethod
}).

-endif.
