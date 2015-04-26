-module(fsm).
-compile(export_all).
%-export([start/0, go_direction/1, event_floor_reached/1]).

%% Module Interface
%%%%%%%%%%%%%%%%%%%%
go_direction(Pid, up) -> Pid ! up;
go_direction(Pid, down) -> Pid ! down;
go_direction(Pid, open) -> Pid ! open.

event_floor_reached(Pid) -> Pid ! floor_reached.
event_new_order(Pid) -> Pid ! new_order.

%% Call backs
%%%%%%%%%%%%%%%%%%%%%%%%
motor_up(Listener) -> Listener ! {motor, up}.
motor_down(Listener) -> Listener ! {motor, down}.
motor_stop(Listener) -> Listener ! {motor, stop}.
open_doors(Listener) -> Listener ! {doors, open}.
close_doors(Listener) -> Listener ! {doors, close}.
init_started(Listener) -> Listener ! {init, started}.
init_completed(Listener) -> Listener ! {init, completed}.


request_new_direction(Listener) -> 
    Listener ! {direction, request, self()},
    receive
	{direction, response, Direction} ->
	    Direction
    end.

%% Process Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start(Listener) ->
    spawn(fun() -> state_init(Listener) end).
    

%% States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


state_init(Listener) ->
    flush(),
    init_started(Listener),
    receive
	floor_reached ->
	    init_completed(Listener),
	    state_idle(Listener)
    end.

state_driving_up(Listener) ->
    flush(),
    motor_up(Listener),
    receive
	floor_reached ->
	    state_idle(Listener)
    end.


state_driving_down(Listener) ->
    flush(),
    motor_down(Listener),
    receive
        floor_reached ->
	    state_idle(Listener)
    end.


state_idle(Listener) ->
    flush(),
    motor_stop(Listener),
    NewDirection = request_new_direction(Listener),
    case NewDirection of
	up ->
	    state_driving_up(Listener);
	down ->
	    state_driving_down(Listener);
	open ->
	    state_open_doors(Listener);
	stop ->
	    receive
		new_order ->
		    state_idle(Listener)
	    end
    end.
    
state_open_doors(Listener) ->
    flush(),
    open_doors(Listener),
    timer:sleep(3000),
    close_doors(Listener),
    state_idle(Listener).



%% Helpers
%%%%%%%%

flush() ->
    receive _Any ->
	    flush()
    after 0 ->
	    ok
    end.
	
	
