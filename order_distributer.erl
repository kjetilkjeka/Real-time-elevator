-module(order_distributer).
-compile(export_all).

-record(order, {floor, direction}).

-define(PROCESS_GROUP_NAME, order_distributers).
-define(DETS_TABLE_NAME, "orders").
-define(SCHEDULING_TIMEOUT, 5000).
-define(BID_TIMEOUT_TIME, 3000).


%% API
%%%%%%%%%%

add_order(Floor, Direction) when (Direction == up) or (Direction == down) ->
    schedule_order_async(#order{floor = Floor, direction = Direction}, get_member_list(), ?SCHEDULING_TIMEOUT);
add_order(Floor, Direction) when Direction == command ->
    HandlingNode = node(),
    Self = self(),
    Order = #order{floor=Floor, direction=Direction},
    MemberList = get_member_list(),
    AddOrderFunction = fun(OrderDistributorPid) -> OrderDistributorPid ! {add_order, Order, HandlingNode, Self} end,
    lists:foreach(AddOrderFunction, MemberList).

remove_order(Floor, Direction) ->
    Self = self(),
    Order = #order{floor=Floor, direction=Direction},
    AddOrderFunction = fun(OrderDistributorPid) ->
			       OrderDistributorPid ! {remove_order, Order, node(), Self}
		       end,
    foreach_member(AddOrderFunction).


is_order(Floor, Direction) ->
    Order = #order{floor=Floor, direction=Direction},
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    ClosestDistributer ! {is_order, Order, node(), self()},
    receive
	{is_order, Order, Response} ->
	    Response
    end.

get_orders() -> %function for debug only
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    ClosestDistributer ! {get_orders, self()},
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
    after ?BID_TIMEOUT_TIME ->
	    infitinity
    end.

handle_order(Order) ->
    get(listener) ! {handle_order, Order#order.floor, Order#order.direction, self()}.

%% process functions
%%%%%%%%%%%%%%%


start(Listener) ->
    spawn(fun() -> init(Listener) end).

init(Listener) ->
    put(listener, Listener),
    join_process_group(),
    start_network_topology_change_detector(),
    OrderList = get_order_list_from_dets(),
    AddToOrders = fun(Order, Orders) -> 
			  add_to_orders(Orders, Order, node())
		  end,
    Orders = lists:foldl(AddToOrders, dict:new(), OrderList),
    HandleOrders = fun(Order) -> handle_order(Order) end,
    lists:foreach(HandleOrders, OrderList),
    loop(Orders).

loop(Orders) ->
    receive
	upgrade ->
	    ?MODULE:loop(Orders);
	{reschedule, OrdersForRescheduling} ->
	    case OrdersForRescheduling of
		all ->
		    OrderList = get_order_list(Orders),
		    FilterCommand = fun(Order) ->
					    if 
						Order#order.direction == command -> false;
						Order#order.direction == up -> true;
						Order#order.direction == down -> true
					    end
				    end,
		    OrderListWithoutCommand = lists:filter(FilterCommand, OrderList),
		    RescheduleOrder = fun(Order) -> schedule_order_async(Order, get_member_list(), ?SCHEDULING_TIMEOUT) end,
		    lists:foreach(RescheduleOrder, OrderListWithoutCommand)
	    end,
	    loop(Orders);
	{request_bid, Floor, Direction, Caller} ->
	    Price = request_bid(Floor, Direction),
	    Caller ! {bid_price, Price, self()},
	    loop(Orders);						       
	{is_order, Order, Node, Caller} ->
	    Response = is_in_orders(Orders, Order, Node),
	    Caller ! {is_order, Order, Response},
	    loop(Orders);
	{remove_order, Order, HandlerNode, _Caller} ->
	    NewOrders = remove_from_orders(Orders, Order, HandlerNode),
	    remove_from_dets(Order),
	    loop(NewOrders);
	{add_order, Order, HandlerNode, _Caller} ->
	    add_to_dets(Order),
	    NewOrders = add_to_orders(Orders, Order, HandlerNode),
	    case HandlerNode == node() of
		true ->
		    handle_order(Order);
		false ->
		    do_nothing
	    end,
	    loop(NewOrders);
	{get_orders, Caller} -> % for debug only
	    Caller ! {orders, Orders},
	    loop(Orders)
    end.


%% functions for scheduling order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_order_async(Order, MemberList, Timeout) ->
    SchedulerPID = spawn(fun() -> 
				 WinningNode = schedule_order(Order, MemberList),
				 AddOrderFunction = fun(Member) -> Member ! {add_order, Order, WinningNode, self()} end,
				 lists:foreach(AddOrderFunction, MemberList)
			 end),
    timer:kill_after(Timeout, SchedulerPID),
    SchedulerPID.


schedule_order(Order, MemberList) when (Order#order.direction == up) or (Order#order.direction == down) ->
    io:format("Order auction started on order Floor: ~w, Direction: ~w ~n", [Order#order.floor, Order#order.direction]),
    io:format("Members are ~w ~n", [MemberList]),
    RequestBidFunction = fun(Member) -> Member ! {request_bid, Order#order.floor, Order#order.direction, self()} end,
    lists:foreach(RequestBidFunction, MemberList),
    Bids = receive_bids(MemberList),
    io:format("Bids are ~w ~n", [Bids]),
    {_LeastBid, WinningMember} = lists:min(Bids),
    WinningNode = node(WinningMember),
    io:format("Winning node is ~w ~n", [WinningNode]),
    WinningNode.



receive_bids([]) ->
    [];
receive_bids(MembersNotCommited) ->
    receive 
	{bid_price, Price, Handler} ->
	    [{Price, Handler}|receive_bids(lists:delete(Handler, MembersNotCommited))]
    end.


%% Functions encapsulating what datatype Orders realy is
%%%%%%%%%%%%%%%%

add_to_orders(Orders, Order, HandlerNode) when Order#order.direction == command -> 
    case dict:is_key(Order, Orders) of
	true ->
	    CurrentHandlers = dict:fetch(Order, Orders),
	    NewHandlers = case lists:member(HandlerNode, CurrentHandlers) of
			      true ->
				  CurrentHandlers;
			      false ->
				  [HandlerNode|CurrentHandlers]
			  end,
	    dict:store(Order, NewHandlers, Orders);
	false ->
	    dict:append(Order, HandlerNode, Orders)
    end;

add_to_orders(Orders, Order, HandlerNode) when (Order#order.direction == up) or (Order#order.direction == down) ->
    dict:store(Order, [HandlerNode], Orders).
    



remove_from_orders(Orders, Order, HandlerNode) when Order#order.direction == command->
    case dict:is_key(Order, Orders) of
	true ->
	    HandlerNodes = dict:fetch(Order, Orders),
	    FilterCondition = fun(ElementInHandlerNodes) -> HandlerNode /= ElementInHandlerNodes end, 
	    NewHandlerNodes = lists:filter(FilterCondition, HandlerNodes),
	    DictWithoutOrder = dict:erase(Order, Orders),
	    dict:append_list(Order, NewHandlerNodes, DictWithoutOrder);
	
	false ->
	    Orders
    end;
remove_from_orders(Orders, Order, _HandlerNode) when (Order#order.direction == up) or (Order#order.direction == down) ->
    dict:erase(Order, Orders).


% do this with pattern matching instead
is_in_orders(Orders, Order, HandlerNode) -> 
    if 
	Order#order.direction == command ->
	    case dict:is_key(Order, Orders) of
		true ->
		    HandlerNodes = dict:fetch(Order, Orders),
		    lists:member(HandlerNode, HandlerNodes);
		false ->
		    false
	    end;
	(Order#order.direction == up) or (Order#order.direction == down) ->
	    dict:is_key(Order, Orders)
    end.

get_order_list(Orders) -> 
    dict:fetch_keys(Orders).
	
%Function(Order, Distributer)
foreach_order(Orders, Function) ->
    F = fun({Order, Distributer}) -> Function(Order, Distributer) end,
    lists:foreach(F, dict:to_list(Orders)).
     

%% Communication/Synchronization procedures
%%%%%%%%%%%%%%%%%%%

join_process_group() -> 
    pg2:create(?PROCESS_GROUP_NAME),
    pg2:join(?PROCESS_GROUP_NAME, self()).

%F(OrderDistributor)
foreach_member(Function) ->
    OrderDistributers = pg2:get_members(?PROCESS_GROUP_NAME),
    lists:foreach(Function, OrderDistributers).

get_member_list() ->
    pg2:get_members(?PROCESS_GROUP_NAME).

%% Functions interfacing the disc copy
%%%%%%%%%%%%%%%%%%%%%


add_to_dets(Order) ->
    dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    dets:insert(?DETS_TABLE_NAME, Order),
    dets:close(?DETS_TABLE_NAME).

remove_from_dets(Order) ->
    dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    dets:delete_object(?DETS_TABLE_NAME, Order),
    dets:close(?DETS_TABLE_NAME).

get_order_list_from_dets() ->
    dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    OrderList = dets:lookup(?DETS_TABLE_NAME, order),
    dets:close(?DETS_TABLE_NAME),
    OrderList.



%% Functions for detecting and managing differance in network topology
%%%%%%%%%%%%%%%%%%

start_network_topology_change_detector() ->
    spawn(fun() -> network_topology_change_detector(pg2:get_members(?PROCESS_GROUP_NAME)) end).


members_added(_MembersAdded) ->
    do_nothing.

members_dissapered(_MembersDissapered) ->
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    ClosestDistributer ! {reschedule, all}.

network_topology_change_detector(OldMembers) ->
    MembersNow = pg2:get_members(?PROCESS_GROUP_NAME),
    MembersAdded = lists:subtract(MembersNow, OldMembers),
    MembersDissapered = lists:subtract(OldMembers, MembersNow),
    
    case MembersAdded == [] of
	true ->
	    do_nothing;
	false ->
	    members_added(MembersAdded)
    end,

    case MembersDissapered == [] of
	true ->
	    do_nothing;
	false ->
	    members_dissapered(MembersDissapered)
    end,
    
    timer:sleep(10000),
    network_topology_change_detector(MembersNow).

