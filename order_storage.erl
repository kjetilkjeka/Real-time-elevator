-module(order_storage). %change to order_distributer or maybe scheduler?
-compile(export_all).

-record(order, {floor, direction}).

-define(PROCESS_GROUP_NAME, order_distributers).


%% API
%%%%%%%%%%

add_order(Floor, Direction) ->
    Self = self(),
    AddOrderFunction = fun(OrderDistributorPid) ->
			       OrderDistributorPid ! {add_order, Floor, Direction, Self}
		       end,
    foreach_distributer(AddOrderFunction).



remove_order(Pid, Floor, Direction) ->
    Pid ! {remove_order, Floor, Direction, self()},
    receive ok ->
	    ok
    end.


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
    spawn(fun() -> init() end).

init() ->
    join_process_group(),
    loop(sets:new()).

loop(OrderSet) -> % should maybe make a map?
    receive
	{is_order, Floor, Direction, Caller} ->
	    Response = is_order_in_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    Caller ! {is_order, Floor, Direction, Response},
	    loop(OrderSet);
	{remove_order, Floor, Direction, Caller} ->
	    NewOrderSet = remove_order_from_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    Caller ! ok, % bad protocol?
	    loop(NewOrderSet);
	{add_order, Floor, Direction, Caller} ->
	    NewOrderSet = add_order_to_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    Caller ! ok, %bad protocol?
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


%% Communication/Synchronization procedures
%%%%%%%%%%%%%%%%%%%

join_process_group() -> % need maybe better name?
    pg2:create(?PROCESS_GROUP_NAME),
    pg2:join(?PROCESS_GROUP_NAME, self()).

%F(OrderDistributor)
foreach_distributer(Function) ->
    OrderDistributers = pg2:get_members(?PROCESS_GROUP_NAME),
    lists:foreach(Function, OrderDistributers).

