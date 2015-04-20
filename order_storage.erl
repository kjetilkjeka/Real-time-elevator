-module(order_storage).
-compile(export_all).

-record(order, {floor, direction}).

%% API
%%%%%%%%%%

add_order(Pid, Floor, Direction) ->
    Pid ! {add_order, Floor, Direction, self()}.

remove_order(Pid, Floor, Direction) ->
    Pid ! {remove_order, Floor, Direction, self()}.

is_order(Pid, Floor, Direction) ->
    Pid ! {is_order, Floor, Direction, self()},
    receive
	{is_order, Floor, Direction, Response} ->
	    Response
    end.

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
	{is_order, Floor, Direction, Caller} ->
	    Response = is_order_in_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    Caller ! {is_order, Floor, Direction, Response},
	    loop(OrderSet);
	{remove_order, Floor, Direction, _Caller} ->
	    NewOrderSet = remove_order_from_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    loop(NewOrderSet);
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

remove_order_from_set(OrderSet, Order) ->
    sets:del_element(Order, OrderSet).

is_order_in_set(OrderSet, Order) ->
    sets:is_element(Order, OrderSet).
