-module(elev_driver).
-export([start/2, stop/0]).
-export([init/1, set_motor_direction/1, set_door_open_lamp/1, set_stop_lamp/1, set_floor_indicator/1, set_button_lamp/3, foreach_button/1]).

-define(NUMBER_OF_FLOORS, 4).
-define(BUTTON_TYPES, [up, down, command]).

-define(POLL_PERIOD, 50).

%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(ElevatorType) -> call_port({elev_init, ElevatorType}).
set_motor_direction(Direction) -> call_port({elev_set_motor_direction, Direction}).
set_door_open_lamp(State) -> call_port({elev_set_door_open_lamp, State}).
set_stop_lamp(State) -> call_port({elev_set_stop_lamp, State}).
set_floor_indicator(Floor) -> call_port({elev_set_floor_indicator, Floor}).
set_button_lamp(Floor, Direction, State) -> call_port({elev_set_button_lamp, Direction, Floor, State}).

%FunctionForeachButton(Floor, Direction)
foreach_button(FunctionForeachButton) ->

    %% This function executes a function for each button on the panel.
    %% Execute ForEachDirection for every floor. Every direction on every floor equals every button.
    %% ForeachDirectionWrapper is there to make arguments match with the foreach_floor function

    TopFloorButtonTypes = lists:delete(up, ?BUTTON_TYPES),
    BottomFloorButtonTypes = lists:delete(down, ?BUTTON_TYPES),
    OtherFloorButtonTypes = ?BUTTON_TYPES,
    
    ForeachDirection = fun(FunctionForeachDirection, Floor) -> %FunctionForeachDirection(Direction)
			       if
				   Floor == 0 ->
				       lists:foreach(FunctionForeachDirection, BottomFloorButtonTypes);
				   Floor == ?NUMBER_OF_FLOORS-1 ->
				       lists:foreach(FunctionForeachDirection, TopFloorButtonTypes);
				   (Floor > 0) and (Floor =< ?NUMBER_OF_FLOORS-1) ->
				       lists:foreach(FunctionForeachDirection, OtherFloorButtonTypes)
			       end
		       end,

    ForeachDirectionWrapper = fun(Floor) -> ForeachDirection(fun(Direction) -> FunctionForeachButton(Floor, Direction) end, Floor) end,
    
    foreach_floor(ForeachDirectionWrapper).
			  


%% Call backs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_order(Listener, Direction, Floor) -> Listener ! {new_order, Direction, Floor}.
floor_reached(Listener, Floor) -> Listener ! {floor_reached, Floor}.

%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Listener, ElevatorType) ->
    spawn(fun() -> init_port("../driver/elev_port", Listener) end),
    timer:sleep(10),
    init(ElevatorType),
    spawn(fun() -> floor_sensor_poller(Listener, -1) end),
    foreach_button(fun(Floor, Direction) ->
			   spawn(fun() -> order_button_poller(Listener, Floor, Direction, 0) end)
		   end).


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
	    receive
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
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.


floor_sensor_poller(Listener, LastFloor) ->
    ThisFloor = call_port({elev_get_floor_sensor_signal}),
    case (ThisFloor /= LastFloor) and (ThisFloor /= 255) of
	true ->
	    floor_reached(Listener, ThisFloor);
	false ->
	    timer:sleep(?POLL_PERIOD)
    end,
    floor_sensor_poller(Listener, ThisFloor).

order_button_poller(Listener, Floor, Direction, LastState) ->
    ThisState = call_port({elev_get_button_signal, Direction, Floor}),
    case (ThisState /= LastState) and (ThisState == 1) of
	true ->
	    new_order(Listener, Direction, Floor);
	false ->
	    timer:sleep(?POLL_PERIOD)
    end,
    order_button_poller(Listener, Floor, Direction, ThisState).




%% Encoding and message wrapping for c port
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%    


%Function(Floor)
foreach_floor(Function) -> 
    FloorIterator = fun(FloorIterator, Floor) ->
			    if 
				Floor == 0 ->
				    Function(Floor);
				(Floor > 0) and (Floor =< ?NUMBER_OF_FLOORS-1) ->
				    Function(Floor),
				    FloorIterator(FloorIterator, Floor-1)
			    end
		    end,
    
    FloorIterator(FloorIterator, ?NUMBER_OF_FLOORS-1),
    ok.
