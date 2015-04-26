-module(plausibility_checks).
-compile(export_all).

-define(MAX_TRAVEL_TIME, 5000).

%% API
%%%%%%%%%%%

motor_started(PID) ->
    PID ! motor_started.

motor_stopped(PID) -> 
    PID ! motor_stopped.



%% Call backs
%%%%%%%%%%%

plausibility_check_event(Listener, Check) ->
    Listener ! {plausibility_check_failed, Check}.


%% Process functions
%%%%%%%%%%%%%%%%%

start_travel_time_plausibility_check(Listener) ->
    spawn(fun() ->
		  put(listener, Listener),
		  travel_time_plausibility()
	  end).
		   
travel_time_plausibility() ->
    receive 
	motor_stopped ->
	    do_nothing;
	motor_started ->
	    receive
		motor_stopped ->
		    ok
	    after
		?MAX_TRAVEL_TIME ->
		    plausibility_check_event(get(listener), travel_time),
		    self() ! motor_started
	    end
    end,
    travel_time_plausibility().
	    
