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

remove_order(Floor, Direction) ->
    Self = self(),
    AddOrderFunction = fun(OrderDistributorPid) ->
			       OrderDistributorPid ! {remove_order, Floor, Direction, Self}
		       end,
    foreach_distributer(AddOrderFunction).


is_order(Floor, Direction) ->
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    ClosestDistributer ! {is_order, Floor, Direction, self()},
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


%% Callbacks
%%%%%%%%%%%

request_bid(Floor, Direction) ->
    get(listener) ! {bid_request, Floor, Direction, self()},
    receive 
	{bid_price, Price} ->
	    Price
    end.


%% process functions
%%%%%%%%%%%%%%%


start(Listener) ->
    spawn(fun() -> init(Listener) end).

init(Listener) ->
    put(listener, Listener),
    join_process_group(),
    loop(sets:new()).

loop(OrderSet) -> % should maybe make a map?
    receive
	{request_bid, Floor, Direction, Caller} ->
	    Price = request_bid(Floor, Direction),
	    Caller ! {bid_price, Price, self()},
	    loop(OrderSet);						       
	{is_order, Floor, Direction, Caller} ->
	    Response = is_order_in_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    Caller ! {is_order, Floor, Direction, Response},
	    loop(OrderSet);
	{remove_order, Floor, Direction, _Caller} ->
	    NewOrderSet = remove_order_from_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    %Caller ! ok, % bad protocol?
	    loop(NewOrderSet);
	{add_order, Floor, Direction, _Caller} ->
	    NewOrderSet = add_order_to_set(OrderSet, #order{floor=Floor, direction=Direction}),
	    %Caller ! ok, %bad protocol?
	    loop(NewOrderSet);
	{get_order_set, Caller} ->
	    Caller ! {order_set, OrderSet},
	    loop(OrderSet)
    end.
    

%% functions for scheduling order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_order(Floor, Direction) ->
    Self = self(),
    RequestBidFunction = fun(Member) ->
				 Member ! {request_bid, Floor, Direction, Self}
			 end,
    foreach_distributer(RequestBidFunction),
    AllMembers = pg2:get_members(?PROCESS_GROUP_NAME),
    Bids = receive_bids(AllMembers),
    {_LeastBid, WinningMember} = lists:min(Bids),
    WinningMember.


receive_bids([]) ->
    [];
receive_bids(MembersNotCommited) ->
    receive 
	{bid_price, Price, Handler} ->
	    [{Price, Handler}|receive_bids(lists:delete(Handler, MembersNotCommited))]
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
foreach_distributer(Function) -> % maybe foreach_member
    OrderDistributers = pg2:get_members(?PROCESS_GROUP_NAME),
    lists:foreach(Function, OrderDistributers).

