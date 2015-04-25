-module(elev).
%-export([start/1]).
-compile(export_all).



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
    register(queue, QueuePID),
    
    PlausibilityCheckManager = spawn(fun() -> plausibility_check_manager() end),
    PlausibilityCheckerPid = plausibility_checks:start_travel_time_plausibility_check(PlausibilityCheckManager),
    register(plausibilityChecker, PlausibilityCheckerPid).
   
    

plausibility_check_manager() ->
    receive 
	{plausibility_check_failed, _Check} ->
	    lists:foreach(fun(Node) -> erlang:disconnect(Node) end, nodes()),
	    io:format("plausibility check failed")
    end,
    plausibility_check_manager().




fsm_manager_init() -> % dirty hack, plz fix
    timer:sleep(100), % wait for driver initalization
    queue:floor_reached(queue, 0), % dumb hack
    fsm_manager().
fsm_manager() ->
    receive
	{init, completed} ->
	    ok;
	{direction, request, Caller} ->
	    Direction = queue:get_next_direction(queue),
	    Caller ! {direction, response, Direction};
	{motor, up} ->
	    plausibility_checks:motor_started(plausibilityChecker),
	    elev_driver:set_motor_direction(up),
	    queue:floor_left(queue, up);
	{motor, down} ->
	    plausibility_checks:motor_started(plausibilityChecker),
	    elev_driver:set_motor_direction(down),
	    queue:floor_left(queue, down);
	{motor, stop} ->
	    plausibility_checks:motor_stopped(plausibilityChecker),
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
	    elev_driver:set_floor_indicator(Floor),
	    fsm:event_floor_reached(fsm),
	    queue:floor_reached(queue, Floor)
    end,
    driver_manager().

button_light_manager_init() ->
    timer:sleep(300),
    button_light_manager().
button_light_manager() ->
    SetLightFunction = fun(Floor, Direction) ->
			       ButtonState = case order_storage:is_order(Floor, Direction) of
						 true ->
						     on;
						 false ->
						     off
					     end,
			       elev_driver:set_button_lamp(Floor, Direction, ButtonState)
		       end,	 
    
    elev_driver:foreach_button(SetLightFunction), % not very nice
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
