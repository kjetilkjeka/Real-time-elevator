-module(queue).
-compile(export_all).
%-export(add_order/2, get/1, remove/2, get_next_order/2]).

-record(order, {floor, direction}).
-record(schedule, {orders = [], elevator_next_floor, elevator_direction}). % next_floor is current or next floor, not current or last, maybe rename variable to make this clearer

-define(NUMBER_OF_FLOORS, 4).
-define(TURN_COST, ?NUMBER_OF_FLOORS).



%% module interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add(Pid, OrderFloor, OrderDirection) ->
    Pid ! {add_order, OrderFloor, OrderDirection, self()}, 
    receive
	ok ->
	    ok
    end.

remove(Pid, OrderFloor, OrderDirection) ->
    Pid ! {remove_order, OrderFloor, OrderDirection, self()},
    receive
	ok ->
	    ok
    end.

get_next_direction(Pid) ->
    Pid ! {get_next_direction, self()},
    receive
	{direction, Direction} ->
	    Direction
    end.

floor_reached(Pid, Floor) ->
    Pid ! {floor_reached, Floor, self()},
    receive 
	ok ->
	    ok
    end.

floor_left(Pid, Direction) -> 
    Pid ! {floor_left, Direction, self()},
    receive
	ok ->
	    ok
    end.

make_stop(Pid) ->
    Pid ! {make_stop, self()},
    receive
	ok ->
	    ok
    end.

get_order_cost(Pid, Floor, Direction) ->
    Pid ! {cost_request, Floor, Direction, self()},
    receive
	{cost, Cost} ->
	    Cost
    end.

get_schedule(Pid) -> %% for debug only
    Pid ! {get_schedule, self()},
    receive
	X ->
	    X
    end.
%% call backs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

order_served(Floor, Direction) ->
    Listener = get(listener),
    Listener ! {order_served, Floor, Direction}.


%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Listener) ->
    spawn(fun() -> init(Listener) end). %% bad initial value hack, please fix later
    

init(Listener) ->
    put(listener, Listener),
    loop(#schedule{}).

loop(Schedule) ->
    receive
	{cost_request, Floor, Direction, Caller} ->
	    CostWithoutOrder = calculate_schedule_cost(Schedule),
	    ScheduleWithOrder = add_order_to_schedule(Schedule, #order{floor = Floor, direction = Direction}),
	    CostWithOrder = calculate_schedule_cost(ScheduleWithOrder),
	    Cost = CostWithOrder - CostWithoutOrder,
	    Caller ! {cost, Cost},
	    loop(Schedule);
	{make_stop, Caller} ->
	    NewSchedule = update_schedule_at_stop(Schedule),
	    Caller ! ok,
	    loop(NewSchedule);
	{get_schedule, Caller} -> %% for debug only
	    Caller ! Schedule,
	    loop(Schedule);
	{add_order, OrderFloor, OrderDirection, Caller} ->
	    NewSchedule = add_order_to_schedule(Schedule, #order{floor = OrderFloor, direction = OrderDirection}),
	    Caller ! ok,
	    loop(NewSchedule);
	{remove_order, OrderFloor, OrderDirection, Caller} ->
	    NewSchedule = remove_order_from_schedule(Schedule, #order{floor = OrderFloor, direction = OrderDirection}),
	    Caller ! ok,
	    loop(NewSchedule);
	{get_next_direction, Caller} ->
	    Direction = get_next_direction_from_schedule(Schedule),
	    Caller ! {direction, Direction},
	    loop(Schedule);
	{floor_reached, Floor, Caller} ->
	    NewSchedule = change_next_floor_in_schedule(Schedule, Floor),
	    Caller ! ok,
	    loop(NewSchedule);
	{floor_left, Direction, Caller} ->
	    NewSchedule = update_direction_and_increment_floor(Schedule, Direction),
	    Caller ! ok,
	    loop(NewSchedule)
    end.


%% functions for process to call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


calculate_schedule_cost(Schedule) when Schedule#schedule.orders == [] -> % correct under the assumption that order cost is linear
    0;
calculate_schedule_cost(Schedule) ->
    {OrderCost, CheapestOrder} = get_cheapest_order_from_schedule(Schedule),
    NewOrderList = lists:delete(CheapestOrder, Schedule#schedule.orders),
    OrderDirection = CheapestOrder#order.direction,
    NewDirection = case OrderDirection of
		       up -> up;
		       down -> down;
		       command -> Schedule#schedule.elevator_direction
		   end,
    NewNextFloor = CheapestOrder#order.floor,
    NewSchedule = Schedule#schedule{orders=NewOrderList, elevator_direction=NewDirection, elevator_next_floor = NewNextFloor}, 
    OrderCost + calculate_schedule_cost(NewSchedule).
    
		

% Removes all orders at floor, should possibly be extended to be more precice
update_schedule_at_stop(Schedule) -> % and update direction, realy hard function to grasp, that's not good
    ElevatorNextFloor = Schedule#schedule.elevator_next_floor,
    SameDirection = Schedule#schedule.elevator_direction,
    OtherDirection = other_direction(SameDirection),

    %% calling something FunSchedule is probably not my brightest moment. Think it's better than Schedule which already is in Head though.
    RemoveCommandOrder = fun(FunSchedule) ->
				 remove_order_from_schedule(FunSchedule, #order{floor = ElevatorNextFloor, direction = command})
			 end,
    RemoveSameDirectionOrder = fun(FunSchedule) ->
				       remove_order_from_schedule(FunSchedule, #order{floor = ElevatorNextFloor, direction = SameDirection})
			       end,
    RemoveOtherDirectionOrder = fun(FunSchedule) ->
					remove_order_from_schedule(FunSchedule, #order{floor = ElevatorNextFloor, direction = OtherDirection})
				end,

    order_served(ElevatorNextFloor, command),
    order_served(ElevatorNextFloor, SameDirection),
    order_served(ElevatorNextFloor, OtherDirection),
    _NewSchedule = RemoveOtherDirectionOrder(RemoveCommandOrder(RemoveSameDirectionOrder(Schedule))).
    



change_next_floor_in_schedule(Schedule, ElevatorNextFloor) ->
    Schedule#schedule{elevator_next_floor = ElevatorNextFloor}.

update_direction_and_increment_floor(Schedule, up) -> % should probably update instead of increment, also shorter more precise name is better
    Schedule#schedule{elevator_next_floor = Schedule#schedule.elevator_next_floor + 1, elevator_direction = up};
update_direction_and_increment_floor(Schedule, down) ->
    Schedule#schedule{elevator_next_floor = Schedule#schedule.elevator_next_floor - 1, elevator_direction = down}.
   

add_order_to_schedule(Schedule, Order) -> % please implement guard for several similar orders, and maybe for unvalid directions ? 
    Schedule#schedule{orders=[Order|Schedule#schedule.orders]}.

remove_order_from_schedule(Schedule, Order) -> % should possibly remove all identical orders
    FilterListCondition = fun(Element) ->
				    if
					Element == Order ->
					    false;
					Element /= Order ->
					    true
				    end
			    end,
    CurrentOrderList = Schedule#schedule.orders,
    NewOrderList = lists:filter(FilterListCondition, CurrentOrderList), 
    Schedule#schedule{orders = NewOrderList}.


get_next_direction_from_schedule(Schedule) ->
    case Schedule#schedule.orders of
	[] ->
	    stop;
	_OrderList ->
	    {_LeastCost, CheapestOrder} = get_cheapest_order_from_schedule(Schedule),	    
	    direction(Schedule#schedule.elevator_next_floor, CheapestOrder#order.floor)
    end.


	
get_cheapest_order_from_schedule(Schedule) -> % maybe degrade to helper, maybe take orderlist and return order? Maybe do this in cost module?
    IncludeCostInListFunction = fun(Order) ->
					{get_cost(Schedule#schedule.elevator_next_floor, 
						  Schedule#schedule.elevator_direction,
						  Order#order.floor,
						  Order#order.direction), Order}
				end,
    CostOrderList = foreach_order(IncludeCostInListFunction, Schedule#schedule.orders),
    {_LeastCost, _CheapestOrder} = lists:min(CostOrderList).



%%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

other_direction(up) -> down;
other_direction(down) -> up.


direction(ElevatorFloor, OrderFloor) when ElevatorFloor == OrderFloor->
    open;
direction(ElevatorFloor, OrderFloor) when ElevatorFloor < OrderFloor ->
    up;
direction(ElevatorFloor, OrderFloor) when ElevatorFloor > OrderFloor ->
    down.

%must_turn(EleveatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection)
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor >= ElevatorNextFloor -> false;
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor < ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor =< ElevatorNextFloor -> false;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor > ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor >= ElevatorNextFloor -> false;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor < ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor =< ElevatorNextFloor -> false;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor > ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, up, OrderFloor, down) when OrderFloor > ElevatorNextFloor -> false;
must_turn(ElevatorNextFloor, up, OrderFloor, down) when OrderFloor =< ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, down, OrderFloor, up) when OrderFloor >= ElevatorNextFloor -> true;
must_turn(ElevatorNextFloor, down, OrderFloor, up) when OrderFloor < ElevatorNextFloor -> false.

% maybe make and move to "cost" module?
get_cost(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) -> %% should probably not be named "get" since it's not a getter
    case must_turn(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) of
	true ->
	    abs(OrderFloor - ElevatorNextFloor) + ?TURN_COST; 
	false ->
	    abs(OrderFloor - ElevatorNextFloor)
    end.

foreach_order(_Function, []) -> %% does the built in function map do this?
    [];
foreach_order(Function, [LastOrder]) ->
    [Function(LastOrder)];
foreach_order(Function, OrderList) ->
    [Head|Tail] = OrderList,
    [Function(Head)|foreach_order(Function, Tail)].
    
