-module(order_storage).
-compile(export_all).

-record(order, {floor, direction}).

%% API
%%%%%%%%%%

add_order(Pid, Floor, Direction) ->
    Pid ! {add_order, Floor, Direction, self()}.


get_order_set(Pid) -> %function for debug only
    Pid ! {get_order_set, self()},
    receive
	{order_set, OrderList} ->
	    OrderList
    end.


%% process functions
%%%%%%%%%%%%%%%


start() ->
    spawn(fun() -> loop(sets:new()) end).

loop(OrderSet) -> % should maybe make a map?
    receive
	{add_order, Floor, Direction, Caller} ->
	    NewOrderSet = add_order_to_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    loop(NewOrderSet);
	{get_order_set, Caller} ->
	    Caller ! {order_set, OrderSet},
	    loop(OrderSet)
    end.
    
		   

%% Module functions, (in lack of better name)
%%%%%%%%%%%%%%%%

add_order_to_set(OrderSet, Order) ->
    sets:add_element(Order, OrderSet).
