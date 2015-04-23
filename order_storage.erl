-module(order_storage). %change to order_distributer or maybe scheduler?, Believe distributer is better than scheduler
-compile(export_all).

-record(order, {floor, direction}).

-define(PROCESS_GROUP_NAME, order_distributers).
-define(DETS_TABLE_NAME, "orders").
-define(SCHEDULING_TIMEOUT, 5000).

% this should maybe be done with dict so it can map from order to handler

%% API
%%%%%%%%%%

add_order(Floor, Direction) when (Direction == up) or (Direction == down) ->
    schedule_order_async(#order {floor = Floor, direction = Direction}, ?SCHEDULING_TIMEOUT);
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
    foreach_distributer(AddOrderFunction).


is_order(Floor, Direction) ->
    Order = #order{floor=Floor, direction=Direction},
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME), %do check if it's on same node
    ClosestDistributer ! {is_order, Order, node(), self()},
    receive
	{is_order, Order, Response} ->
	    Response
    end.

get_orders() -> %function for debug only
    Pid = pg2:get_closest_pid(?PROCESS_GROUP_NAME),
    Pid ! {get_orders, self()},
    receive
	{orders, Orders} ->
	    Orders
    end.


%% Callbacks
%%%%%%%%%%%

request_bid(Floor, Direction) -> %this can ofcourse deadlock
    get(listener) ! {bid_request, Floor, Direction, self()},
    receive 
	{bid_price, Price} ->
	    Price
    end.

handle_order(Order) -> % the world might have seen better names than handle_order
    get(listener) ! {handle_order, Order#order.floor, Order#order.direction, self()}.

%% process functions
%%%%%%%%%%%%%%%


start(Listener) ->
    spawn(fun() -> init(Listener) end).

init(Listener) ->
    put(listener, Listener),
    join_process_group(),
    %start_topology_change_detector(),
    Orders = add_orders_from_dets(dict:new()),
    %reschedule_orders_async(Orders),
    loop(Orders).

loop(Orders) -> % OrderMap maps orders to something descriptive
    receive
	upgrade ->
	    ?MODULE:loop(Orders);
	%{reschedule, OrdersForRescheduling} -> %don't know how smart this realy is
	   % case OrdersForRescheduling of
	%	all ->
	%	    reschedule_orders_async(Orders);
	%	OrdersForRescheduling ->
	%	    reschedule_orders_async(OrdersForRescheduling)
	 %   end;		    
	{request_bid, Floor, Direction, Caller} ->
	    Price = request_bid(Floor, Direction), % this may cause deadlock if request bid fucks up
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

schedule_order_async(Order, Timeout) ->
    MemberList = get_member_list(),
    SchedulerPID = spawn(fun() -> 
				 WinningNode = schedule_order(Order#order.floor, Order#order.direction, MemberList),
				 AddOrderFunction = fun(Member) -> Member ! {add_order, Order, WinningNode, self()} end,
				 lists:foreach(AddOrderFunction, MemberList)
			 end),
    timer:kill_after(Timeout, SchedulerPID),
    SchedulerPID.


% should maybe take order record since it's called reschedule_!order!
% many io:formats here for debugging, consider removing at the end.
schedule_order(Floor, Direction, MemberList) when (Direction == up) or (Direction == down) -> % may cause deadlock if members change between calls
    io:format("Order auction started on order Floor: ~w, Direction: ~w ~n", [Floor, Direction]),
    io:format("Members are ~w ~n", [MemberList]),
    Self = self(),
    RequestBidFunction = fun(Member) -> Member ! {request_bid, Floor, Direction, Self} end,
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
    



%this is not the nicest construct of a function in the world. Pretty many ifs and cases. !!!!!!Should do this with pattern matching ofcourse!!!!
remove_from_orders(Orders, Order, HandlerNode) when Order#order.direction == command-> %pretty simuliar construct as is_in_order, possible to do more general?
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
	
%Function(Order, Distributer)
foreach_order(Orders, Function) -> % this one might be broke, is it defined how many times command orders will be done?
    F = fun({Order, Distributer}) -> Function(Order, Distributer) end,
    lists:foreach(F, dict:to_list(Orders)).
     

%% Communication/Synchronization procedures
%%%%%%%%%%%%%%%%%%%

join_process_group() -> % need maybe better name?
    pg2:create(?PROCESS_GROUP_NAME),
    pg2:join(?PROCESS_GROUP_NAME, self()).

%F(OrderDistributor)
foreach_distributer(Function) -> % maybe foreach_member
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

add_orders_from_dets(Orders) ->
    %consider removing all non order elements first
    Self = self(),
    AddOrderFunction = fun(Order, Orders) -> add_to_orders(Orders, Order, Self) end, %shadowed warning, maybe find better name?
    dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    NewOrders = dets:foldl(AddOrderFunction, Orders, ?DETS_TABLE_NAME),
    dets:close(?DETS_TABLE_NAME),
    NewOrders.



%% Functions for detecting and managing differance in network topology
%%%%%%%%%%%%%%%%%%

start_topology_change_detector() ->
    spawn(fun() -> network_topology_change_detector(pg2:get_members(?PROCESS_GROUP_NAME)) end).
		  

members_added(_MembersAdded) ->
    do_nothing.

members_dissapered(_MembersDissapered) ->
    ClosestDistributer = pg2:get_closest_pid(?PROCESS_GROUP_NAME), %do check if it's on same node
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

