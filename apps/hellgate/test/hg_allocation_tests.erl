-module(hg_allocation_tests).

-include("domain.hrl").
-include("allocation.hrl").

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

-spec calculate_test() -> _.
calculate_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    ?allocation(AllocationTrxs) = hg_allocation:calculate(
        AllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(90, <<"RUB">>)
    ),
    ?assertMatch(
        [
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
        ],
        lists:sort(AllocationTrxs)
    ).

-spec calculate_without_generating_agg_trx_test() -> _.
calculate_without_generating_agg_trx_test() ->
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
    ?allocation(AllocationTrxs) = hg_allocation:calculate(
        AllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(90, <<"RUB">>)
    ),
    ?assertMatch(
        [
            ?allocation_trx(
                <<"1">>,
                ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
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
                <<"3">>,
                ?allocation_trx_target_shop(<<"PARTY3">>, <<"SHOP3">>),
                ?cash(30, <<"RUB">>),
                ?allocation_trx_details(Cart)
            )
        ],
        lists:sort(AllocationTrxs)
    ).

-spec calculate_amount_exceeded_error_1_test() -> _.
calculate_amount_exceeded_error_1_test() ->
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
    ?assertThrow(
        amount_exceeded,
        hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>))
    ).

-spec calculate_amount_exceeded_error_2_test() -> _.
calculate_amount_exceeded_error_2_test() ->
    AllocationPrototype = generic_prototype(),
    ?assertThrow(
        amount_exceeded,
        hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(100, <<"RUB">>))
    ).

-spec subtract_one_transaction_1_test() -> _.
subtract_one_transaction_1_test() ->
    Cart = ?invoice_cart([?invoice_line(<<"STRING">>, 1, ?cash(30, <<"RUB">>))]),
    AllocationPrototype = generic_prototype(),
    RefundAllocationPrototype = ?allocation_prototype([
        ?allocation_trx_prototype(
            ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
            ?allocation_trx_prototype_body_amount(?cash(30, <<"RUB">>))
        )
    ]),
    Allocation = hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)),
    RefundAllocation = hg_allocation:calculate(
        RefundAllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(30, <<"RUB">>)
    ),
    {ok, ?allocation(SubbedAllocationTrxs)} = hg_allocation:sub(Allocation, RefundAllocation),
    ?assertMatch(
        [
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
        ],
        lists:sort(SubbedAllocationTrxs)
    ).

-spec subtract_one_transaction_2_test() -> _.
subtract_one_transaction_2_test() ->
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
    Allocation = hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)),
    RefundAllocation = hg_allocation:calculate(
        RefundAllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(30, <<"RUB">>)
    ),
    {ok, ?allocation(SubbedAllocationTrxs)} = hg_allocation:sub(Allocation, RefundAllocation),
    ?assertMatch(
        [
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
                ?cash(5, <<"RUB">>)
            )
        ],
        lists:sort(SubbedAllocationTrxs)
    ).

-spec subtract_one_transaction_3_test() -> _.
subtract_one_transaction_3_test() ->
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
    Allocation = hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)),
    RefundAllocation = hg_allocation:calculate(
        RefundAllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(30, <<"RUB">>)
    ),
    {ok, ?allocation(SubbedAllocationTrxs)} = hg_allocation:sub(Allocation, RefundAllocation),
    ?assertMatch(
        [
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
                ?cash(10, <<"RUB">>)
            )
        ],
        lists:sort(SubbedAllocationTrxs)
    ).

-spec subtract_partial_transaction_test() -> _.
subtract_partial_transaction_test() ->
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
    Allocation = hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)),
    RefundAllocation = hg_allocation:calculate(
        RefundAllocationPrototype,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(18, <<"RUB">>)
    ),
    {ok, ?allocation(SubbedAllocationTrxs)} = hg_allocation:sub(Allocation, RefundAllocation),
    ?assertMatch(
        [
            ?allocation_trx(
                <<"1">>,
                ?allocation_trx_target_shop(<<"PARTY1">>, <<"SHOP1">>),
                ?cash(12, <<"RUB">>),
                ?allocation_trx_details(Cart1)
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
                <<"4">>,
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(15, <<"RUB">>)
            )
        ],
        lists:sort(SubbedAllocationTrxs)
    ).

-spec consecutive_subtract_of_partial_transaction_test() -> _.
consecutive_subtract_of_partial_transaction_test() ->
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
    Allocation0 = hg_allocation:calculate(AllocationPrototype, <<"PARTY0">>, <<"SHOP0">>, ?cash(90, <<"RUB">>)),
    RefundAllocation0 = hg_allocation:calculate(
        RefundAllocationPrototype0,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(18, <<"RUB">>)
    ),
    {ok, Allocation1} = hg_allocation:sub(
        Allocation0,
        RefundAllocation0
    ),
    RefundAllocation1 = hg_allocation:calculate(
        RefundAllocationPrototype1,
        <<"PARTY0">>,
        <<"SHOP0">>,
        ?cash(12, <<"RUB">>)
    ),
    {ok, ?allocation(SubbedAllocationTrxs)} = hg_allocation:sub(Allocation1, RefundAllocation1),
    ?assertMatch(
        [
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
                <<"4">>,
                ?allocation_trx_target_shop(<<"PARTY0">>, <<"SHOP0">>),
                ?cash(15, <<"RUB">>)
            )
        ],
        lists:sort(SubbedAllocationTrxs)
    ).

-endif.
