-module(elev_mnesia).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([start/2, stop/1, install/1]).
-export([add_elevator/4, elevator_by_id/1, remove_elevator/1]).
-export([add_order/3, remove_order/1]).

-record(elevator_info, {elevatorID,
			  floor,
                          state,
                          available}).

-record(orders, {order_number, button_type, floor}).

start(normal, []) ->
    mnesia:wait_for_tables([elevator_info, orders], 5000),
    elev_mnesia_sup:start_link().

stop(_) -> ok.

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(elevator_info,
                        [{attributes, record_info(fields, elevator_info)},
                         {index, [#elevator_info.floor]},
                         {ram_copies, Nodes}]),
    mnesia:create_table(orders, 
			[{attributes, record_info(fields, orders)},
			 {index, [#orders.order_number]},
			 {ram_copies, Nodes}]),			
    rpc:multicall(Nodes, application, stop, [mnesia]).

add_elevator(ElevatorID, Floor, State, Available) ->
    F = fun() ->
        mnesia:write(#elevator_info{elevatorID=ElevatorID,
                                      floor=Floor,
                                      state=State,
                                      available=Available})
    end,
    mnesia:activity(transaction, F).

add_order(Order_number, Button_type, Floor) ->
    F = fun() ->
        mnesia:write(#orders{order_number=Order_number,
                                      button_type=Button_type,
                                      floor=Floor})
    end,
    mnesia:activity(transaction, F).

remove_elevator(ElevatorID) ->
	F = fun() ->
		mnesia:delete({elevator_info, ElevatorID}) end,
	mnesia:activity(transaction, F).

remove_order(Order_number) ->
	F = fun() ->
		mnesia:delete({orders, Order_number}) end,
	mnesia:activity(transaction, F).


%MULIG DET NEDENFOR IKKE TRENGS
elevator_by_id(ElevatorID) ->
	F = fun() ->
		case mnesia:read({elevator_info, ElevatorID}) of
		[#elevator_info{floor = F, state = S, available = A}] ->
			{ElevatorID, F, S, A, find_elevators(ElevatorID)};
		[] ->
			undefined
		end
	end,
	mnesia:activity(transaction, F).

%%%PRIVATE FUNCTION
find_elevators(_ElevatorID) -> undefined.

