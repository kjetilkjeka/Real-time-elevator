-module(elev).
-export([start/1]).



start(ElevatorType) ->
    connection_manager:start_auto_discovery(),
    
    OrderStorageManagerPID = spawn(fun() -> order_storage_manager_init() end),
    OrderStoragePID = order_storage:start(OrderStorageManagerPID),
    register(order_storage, OrderStoragePID),
    
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    elev_driver:start(DriverManagerPID, ElevatorType),

    FsmManagerPID = spawn(fun() -> fsm_manager_init() end),
    FsmPID = elev_fsm:start(FsmManagerPID),
    register(fsm, FsmPID),
    
    ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),
    
    ScheduleManagerPID = spawn(fun() -> schedule_manager_init() end),
    SchedulePID = schedule:start(ScheduleManagerPID),
    register(schedule, SchedulePID),
    
    PlausibilityCheckManager = spawn(fun() -> plausibility_check_manager() end),
    PlausibilityCheckerPID = plausibility_checks:start_travel_time_plausibility_check(PlausibilityCheckManager),
    register(plausibilityChecker, PlausibilityCheckerPID),
    

    OrderStorageManagerPID ! init_completed,
    FsmManagerPID ! init_completed,
    DriverManagerPID ! init_completed,
    ButtonLightManagerPID ! init_completed,
    ScheduleManagerPID ! init_completed.

   
    

plausibility_check_manager() ->
    receive 
	{plausibility_check_failed, _Check} ->
	    io:format("plausibility check failed")
    end,
    plausibility_check_manager().




fsm_manager_init() ->
    receive init_completed ->
	    ok
    end,
    fsm_manager().
fsm_manager() ->
    receive
	{init, started} ->
	    elev_driver:set_motor_direction(up);
	{init, completed} ->
	    ok;
	{direction, request, Caller} ->
	    Direction = schedule:get_next_direction(schedule),
	    Caller ! {direction, response, Direction};
	{motor, up} ->
	    plausibility_checks:motor_started(plausibilityChecker),
	    elev_driver:set_motor_direction(up),
	    schedule:floor_left(schedule, up);
	{motor, down} ->
	    plausibility_checks:motor_started(plausibilityChecker),
	    elev_driver:set_motor_direction(down),
	    schedule:floor_left(schedule, down);
	{motor, stop} ->
	    plausibility_checks:motor_stopped(plausibilityChecker),
	    elev_driver:set_motor_direction(stop);
	{doors, open} ->
	    schedule:stopped_at_floor(schedule),
	    elev_driver:set_door_open_lamp(on);
	{doors, close} ->
	    elev_driver:set_door_open_lamp(off)
    end,
    fsm_manager().

driver_manager_init() ->
    receive init_completed ->
	    ok
    end,
    driver_manager().
driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    order_storage:add_order(Floor, Direction);
	{floor_reached, Floor} ->
	    elev_driver:set_floor_indicator(Floor),
	    elev_fsm:event_floor_reached(fsm),
	    schedule:floor_reached(schedule, Floor)
    end,
    driver_manager().

button_light_manager_init() ->
    receive init_completed ->
	    ok
    end,
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
    
    elev_driver:foreach_button(SetLightFunction),
    timer:sleep(200),
    button_light_manager().

order_storage_manager_init() ->
    receive init_completed ->
	    ok
    end,
    order_storage_manager().
order_storage_manager() ->    
    receive
	{bid_request, Floor, Direction, Caller} ->
	    Caller ! {bid_price, schedule:get_order_cost(schedule, Floor, Direction)};
	{handle_order, Floor, Direction, _Caller} ->
	    schedule:add_order(schedule, Floor, Direction),
	    elev_fsm:event_new_order(fsm)
    end,
    
    order_storage_manager().

schedule_manager_init() ->
    receive init_completed ->
	    ok
    end,
    schedule_manager().
schedule_manager() ->			    
    receive
	{order_served, Floor, Direction} ->
	    order_storage:remove_order(Floor, Direction)
    end,
    schedule_manager().
