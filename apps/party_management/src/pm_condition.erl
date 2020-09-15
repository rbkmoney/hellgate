-module(pm_condition).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-export([test/3]).

%%

-type condition() :: dmsl_domain_thrift:'Condition'().
-type varset() :: pm_selector:varset().

-spec test(condition(), varset(), pm_domain:revision()) -> true | false | undefined.
test({category_is, V1}, #{category := V2}, _) ->
    V1 =:= V2;
test({currency_is, V1}, #{currency := V2}, _) ->
    V1 =:= V2;
test({cost_in, V}, #{cost := C}, _) ->
    pm_cash_range:is_inside(C, V) =:= within;
test({payment_tool, C}, #{payment_tool := V}, Rev) ->
    pm_payment_tool:test_condition(C, V, Rev);
test({shop_location_is, V}, #{shop := S}, _) ->
    V =:= S#domain_Shop.location;
test({party, V}, #{party_id := PartyID} = VS, _) ->
    test_party(V, PartyID, VS);
test({payout_method_is, V1}, #{payout_method := V2}, _) ->
    V1 =:= V2;
test({identification_level_is, V1}, #{identification_level := V2}, _) ->
    V1 =:= V2;
test({p2p_tool, #domain_P2PToolCondition{} = C}, #{p2p_tool := #domain_P2PTool{} = V}, Rev) ->
    test_p2p_tool(C, V, Rev);
test(_, #{}, _) ->
    undefined.

test_party(#domain_PartyCondition{id = PartyID, definition = Def}, PartyID, VS) ->
    test_party_definition(Def, VS);
test_party(_, _, _) ->
    false.

test_party_definition(undefined, _) ->
    true;
test_party_definition({shop_is, ID1}, #{shop_id := ID2}) ->
    ID1 =:= ID2;
test_party_definition({wallet_is, ID1}, #{wallet_id := ID2}) ->
    ID1 =:= ID2;
test_party_definition(_, _) ->
    undefined.

test_p2p_tool(#domain_P2PToolCondition{sender_is = undefined, receiver_is = undefined}, #domain_P2PTool{}, _Rev) ->
    true;
test_p2p_tool(
    #domain_P2PToolCondition{
        sender_is = SenderIs,
        receiver_is = undefined
    },
    #domain_P2PTool{sender = Sender},
    Rev
) ->
    test({payment_tool, SenderIs}, #{payment_tool => Sender}, Rev);
test_p2p_tool(
    #domain_P2PToolCondition{
        sender_is = undefined,
        receiver_is = ReceiverIs
    },
    #domain_P2PTool{receiver = Receiver},
    Rev
) ->
    test({payment_tool, ReceiverIs}, #{payment_tool => Receiver}, Rev);
test_p2p_tool(P2PCondition, P2PTool, Rev) ->
    #domain_P2PToolCondition{
        sender_is = SenderIs,
        receiver_is = ReceiverIs
    } = P2PCondition,
    #domain_P2PTool{
        sender = Sender,
        receiver = Receiver
    } = P2PTool,
    case
        {
            test({payment_tool, SenderIs}, #{payment_tool => Sender}, Rev),
            test({payment_tool, ReceiverIs}, #{payment_tool => Receiver}, Rev)
        }
    of
        {true, true} ->
            true;
        {T1, T2} when
            T1 =:= undefined orelse
                T2 =:= undefined
        ->
            undefined;
        {_, _} ->
            false
    end.
