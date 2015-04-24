-module(elev_driver).
-compile(export_all).

-define(NUMBER_OF_FLOORS, 4). %should maybe be in other place?
-define(BUTTON_TYPES, [up, down, command]). %should maybe be some other place?

-define(POLL_PERIOD, 50).

%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ElevatorType) -> call_port({elev_init, ElevatorType}).
set_motor_direction(Direction) -> call_port({elev_set_motor_direction, Direction}).
set_door_open_lamp(State) -> call_port({elev_set_door_open_lamp, State}).
set_stop_lamp(State) -> call_port({elev_set_stop_lamp, State}).
set_floor_indicator(Floor) -> call_port({elev_set_floor_indicator, Floor}).
set_button_lamp(Floor, Direction, State) -> call_port({elev_set_button_lamp, Direction, Floor, State}).

%% Call backs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_order(Listener, Direction, Floor) -> Listener ! {new_order, Direction, Floor}.
floor_reached(Listener, Floor) -> Listener ! {floor_reached, Floor}.

%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Listener, ElevatorType) ->
    spawn(?MODULE, init_port, ["../driver/elev_port", Listener]),
    timer:sleep(10).
    %init(ElevatorType).
    %spawn(fun() -> floor_sensor_poller(Listener, -1) end),
    %spawn(fun() -> poll_everything() end).

stop() ->
    driver ! stop.


init_port(ExtPrg, Listener) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, Listener).

loop(Port, Listener) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive %handle dead port
		{Port, {data, Data}} ->
		    Caller ! {self(), Data}
	    end,
	    loop(Port, Listener); 
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.


floor_sensor_poller(Listener, LastFloor) ->
    ThisFloor = call_port({elev_get_floor_sensor_signal}),
    case ThisFloor /= LastFloor of
	true ->
	    floor_reached(Listener, ThisFloor);
	false ->
	    timer:sleep(?POLL_PERIOD)
    end,
    floor_sensor_poller(Listener, ThisFloor).

order_button_poller(Listener, Floor, Direction, LastState) ->
    ThisState = call_port({elev_get_button_signal, Direction, Floor}),
    case ThisState /= LastState of
	true ->
	    new_order(Listener, Direction, Floor);
	false ->
	    timer:sleep(?POLL_PERIOD)
    end,
    order_button_poller(Listener, Floor, Direction, ThisState).




%% Encoding in same format as elev_port.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_port(Msg) ->
    driver ! {call, self(), Msg},
    receive 
	{_PID, [Result]} ->
	    Result
    end.


encode({elev_init, simulator}) -> [1, 1];
encode({elev_init, elevator}) -> [1, 2];
encode({elev_set_motor_direction, stop}) -> [2, 0];
encode({elev_set_motor_direction, up}) -> [2, 1];
encode({elev_set_motor_direction, down}) -> [2, 2];
encode({elev_set_door_open_lamp, off}) -> [3, 0];
encode({elev_set_door_open_lamp, on}) -> [3, 1];
encode({elev_get_obstruction_signal}) -> [4];
encode({elev_get_stop_signal}) -> [5];
encode({elev_set_stop_lamp, off}) -> [6, 0];
encode({elev_set_stop_lamp, on}) -> [6, 1];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_set_floor_indicator, Floor}) -> [8, Floor];
encode({elev_get_button_signal, up, Floor}) -> [9, 0, Floor];
encode({elev_get_button_signal, down, Floor}) -> [9, 1, Floor];
encode({elev_get_button_signal, command, Floor}) -> [9, 2, Floor];
encode({elev_set_button_lamp, up, Floor, on}) -> [10, 0, Floor, 1];
encode({elev_set_button_lamp, up, Floor, off}) -> [10, 0, Floor, 0];
encode({elev_set_button_lamp, down, Floor, on}) -> [10, 1, Floor, 1];
encode({elev_set_button_lamp, down, Floor, off}) -> [10, 1, Floor, 0];
encode({elev_set_button_lamp, command, Floor, on}) -> [10, 2, Floor, 1];
encode({elev_set_button_lamp, command, Floor, off}) -> [10, 2, Floor, 0].

decode(X) -> X. %improve
    


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
