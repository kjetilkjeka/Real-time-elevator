-module(scheduler).
-compile(export_all).
%-export(add_order/2, get/1, remove/2, get_next_order/2]).

-record(order, {floor, direction}).
-record(schedule, {orders = [], elevator_next_floor, elevator_direction}).

-define(NUMBER_OF_FLOORS, 4).
-define(TURN_COST, ?NUMBER_OF_FLOORS).


add_order(Schedule, Order) ->
    Schedule#schedule{orders=[Order|Schedule#schedule.orders]}.

get_cheapest_order(Schedule) ->
    IncludeCostInListFunction = fun(Order) ->
					{get_cost(Schedule#schedule.elevator_next_floor, 
						  Schedule#schedule.elevator_direction,
						  Order#order.floor,
						  Order#order.direction), Order}
				end,
    CostOrderList = foreach_order(IncludeCostInListFunction, Schedule#schedule.orders),
    {_LeastCost, CheapestOrder} = lists:min(CostOrderList),
    CheapestOrder.






	
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor >= ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor < ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor =< ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor > ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor >= ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor < ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor =< ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor > ElevatorNextFloor ->
    true;
must_turn(_ElevatorNextFloor, _ElevatorDirection, _OrderFloor, _OrderDirection) ->
    erlang:error(badarg).


get_cost(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) ->
    case must_turn(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) of
	true ->
	    abs(OrderFloor - ElevatorNextFloor) + ?TURN_COST; 
	false ->
	    abs(OrderFloor - ElevatorNextFloor)
    end.


foreach_order(Function, [LastOrder]) ->
    [Function(LastOrder)];
foreach_order(Function, OrderList) ->
    [Head|Tail] = OrderList,
    [Function(Head)|foreach_order(Function, Tail)].
    

