-module(elev).
%-export([start/1]).
-compile(export_all).

-define(NUMBER_OF_FLOORS, 4). %should maybe be in other place?
-define(BUTTON_TYPES, [up, down, command]). %should maybe be some other place?


start(ElevatorType) ->
    connection_manager:start_auto_discovery(),
    
    OrderStorageManagerPID = spawn(fun() -> order_storage_manager() end),
    OrderStoragePID = order_storage:start(OrderStorageManagerPID),
    register(order_storage, OrderStoragePID),
    
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    FsmManagerPid = spawn(fun() -> fsm_manager_init() end),

    elev_driver:start(DriverManagerPID, ElevatorType),
    FsmPID = fsm:start(FsmManagerPid),
    register(fsm, FsmPID),
    
    spawn(fun() -> button_light_manager_init() end),
    
    QueueManagerPID = spawn(fun() -> queue_manager() end),
    QueuePID = queue:start(QueueManagerPID),
    register(queue, QueuePID).
    
    





fsm_manager_init() -> % dirty hack, plz fix
    timer:sleep(100), % wait for driver initalization
    queue:floor_reached(queue, 0), % dumb hack
    fsm_manager().
fsm_manager() ->
    receive
	{init, completed} ->
	    queue:make_stop(queue);
	{direction, request, Caller} ->
	    Direction = queue:get_next_direction(queue),
	    Caller ! {direction, response, Direction};
	{motor, up} ->
	    elev_driver:set_motor_direction(up),
	    queue:floor_left(queue, up);
	{motor, down} ->
	    elev_driver:set_motor_direction(down),
	    queue:floor_left(queue, down);
	{motor, stop} ->
	    elev_driver:set_motor_direction(stop);
	{doors, open} ->
	    queue:make_stop(queue),
	    elev_driver:set_door_open_lamp(on);
	{doors, close} ->
	    elev_driver:set_door_open_lamp(off)
    end,
    fsm_manager().

driver_manager_init() -> % more dirty tricks
    timer:sleep(100),
    driver_manager().
driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    order_storage:add_order(Floor, Direction);  % this schedule event can block floor_reached
	{floor_reached, Floor} ->
	    fsm:event_floor_reached(fsm),
	    queue:floor_reached(queue, Floor)
    end,
    driver_manager().

button_light_manager_init() ->
    timer:sleep(100),
    button_light_manager().
button_light_manager() ->
    SetLightFunction = fun(Floor, Direction) ->
			       elev_driver:set_button_lamp(Floor, Direction, order_storage:is_order(Floor, Direction)) % hard line to grasp
		       end,	 
    
    foreach_button(SetLightFunction),
    timer:sleep(200),
    button_light_manager().


order_storage_manager() ->    
    receive
	{bid_request, Floor, Direction, Caller} ->
	    Caller ! {bid_price, queue:get_order_cost(queue, Floor, Direction)};
	{handle_order, Floor, Direction, _Caller} ->
	    queue:add(queue, Floor, Direction),
	    fsm:event_new_order(fsm) % maybe queue should do this?
    end,
    
    order_storage_manager().

queue_manager() ->			    
    receive
	{order_served, Floor, Direction} ->
	    order_storage:remove_order(Floor, Direction)
    end,
    queue_manager().

		


%% Helper functions (should maybe not be helper functions in this module?)
%%%%%%%%%%%%%%%%%%%%%%%

%Fun(Floor, Direction)
foreach_button(Fun) -> % This is somewhat a mess, atleast make better names for Fun and F and shizzle. Consider to rewrite.
    TopFloorButtonTypes = lists:delete(up, ?BUTTON_TYPES),
    BottomFloorButtonTypes = lists:delete(down, ?BUTTON_TYPES),
    OtherFloorButtonTypes = ?BUTTON_TYPES,
    
    ForEachDirection = fun(F, Floor) -> %F(Direction)
			       if
				   Floor == 0 ->
				       lists:foreach(F, BottomFloorButtonTypes);
				   Floor == ?NUMBER_OF_FLOORS-1 ->
				       lists:foreach(F, TopFloorButtonTypes);
				   (Floor > 0) and (Floor =< ?NUMBER_OF_FLOORS-1) ->
				       lists:foreach(F, OtherFloorButtonTypes)
			       end
		       end,

    DoFunForEachDirection = fun(Floor) ->
				    ForEachDirection(fun(Direction) -> Fun(Floor, Direction) end, Floor)
			    end,

    foreach_floor(DoFunForEachDirection).
			  
    
    
%F(Floor)
foreach_floor(F) -> %should maybe (probably) me moved to somewhere else
    FloorIterator = fun(FloorIterator, Floor) ->
			    if 
				Floor == 0 ->
				    F(Floor);
				(Floor > 0) and (Floor =< ?NUMBER_OF_FLOORS-1) ->
				    F(Floor),
				    FloorIterator(FloorIterator, Floor-1)
			    end
		    end,
    
    FloorIterator(FloorIterator, ?NUMBER_OF_FLOORS-1),
    ok.
