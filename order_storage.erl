-module(order_storage). %change to order_distributer or maybe scheduler?
-compile(export_all).

-record(order, {floor, direction}).

-define(PROCESS_GROUP_NAME, order_distributers).

% this should maybe be done with dict so it can map from order to handler

%% API
%%%%%%%%%%

add_order(Floor, Direction) ->
    Self = self(),
    Order = #order{floor=Floor, direction=Direction},
    AddOrderFunction = fun(OrderDistributorPid) ->
			       OrderDistributorPid ! {add_order, Order, Self}
		       end,
    foreach_distributer(AddOrderFunction).

remove_order(Floor, Direction) ->
    Self = self(),
    Order = #order{floor=Floor, direction=Direction},
    AddOrderFunction = fun(OrderDistributorPid) ->
			       OrderDistributorPid ! {remove_order, Order, Self}
		       end,
    foreach_distributer(AddOrderFunction).


is_order(Floor, Direction) ->
    Order = #order{floor=Floor, direction=Direction},
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    ClosestDistributer ! {is_order, Order, self()},
    receive
	{is_order, Order, Response} ->
	    Response
    end.

get_orders(Pid) -> %function for debug only
    Pid ! {get_orders, self()},
    receive
	{orders, Orders} ->
	    Orders
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
    loop(dict:new()).

loop(Orders) -> % OrderMap maps orders to something descriptive
    receive
	{request_bid, Floor, Direction, Caller} ->
	    Price = request_bid(Floor, Direction),
	    Caller ! {bid_price, Price, self()},
	    loop(Orders);						       
	{is_order, Order, Caller} ->
	    Response = is_in_orders(Orders, Order),
	    Caller ! {is_order, Order, Response},
	    loop(Orders);
	{remove_order, Order, _Caller} ->
	    NewOrders = remove_from_orders(Orders, Order),
	    loop(NewOrders);
	{add_order, Order, Caller} ->
	    NewOrders = add_to_orders(Orders, Order, Caller),
	    loop(NewOrders);
	{get_orders, Caller} -> % for debug only
	    Caller ! {orders, Orders},
	    loop(Orders)
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


%% Functions encapsulating what datatype Orders realy is
%%%%%%%%%%%%%%%%

add_to_orders(Orders, Order, Handler) -> dict:append(Order, Handler, Orders).
remove_from_orders(Orders, Order) -> dict:erase(Order, Orders).
is_in_orders(Orders, Order) -> dict:is_key(Order, Orders).


%% Communication/Synchronization procedures
%%%%%%%%%%%%%%%%%%%

join_process_group() -> % need maybe better name?
    pg2:create(?PROCESS_GROUP_NAME),
    pg2:join(?PROCESS_GROUP_NAME, self()).

%F(OrderDistributor)
foreach_distributer(Function) -> % maybe foreach_member
    OrderDistributers = pg2:get_members(?PROCESS_GROUP_NAME),
    lists:foreach(Function, OrderDistributers).
