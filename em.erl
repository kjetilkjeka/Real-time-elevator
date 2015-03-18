-module(em).
-include_lib ("mnesia/src/mnesia.hrl").
-export([start/1, stop/1, install/1, add_elevator/4, add_order/3, remove_elevator/1, remove_order/1]).
-export([elevator_by_id/1, order_by_id/1, reinit_mnesia_cluster/0, add_new_node_to_cluster/1]).
-export([reconnect_node/1, sync_mnesia/2, export_table/1, import_table/1]).

-record(elevator_info, {elevatorID,
			  floor,
                          state,
                          available}).

-record(orders, {order_number, button_type, floor}).

start(Nodes) ->
	install(Nodes).
	
stop(_) ->
	rpc:multicall(mnesia, stop, []).

install(Nodes) ->
	mnesia:create_schema(Nodes),
        rpc:multicall(Nodes, application, start, [mnesia]),
        mnesia:create_table(elevator_info,
                        [{attributes, record_info(fields, elevator_info)},
                         {index, [#elevator_info.floor]},
                         {ram_copies, Nodes}]),
	mnesia:create_table(orders,
                        [{attributes, record_info(fields, orders)},
                         {index, [#orders.floor]},
                         {ram_copies, Nodes}]),
	tv:start().

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

elevator_by_id(ElevatorID) ->
	F = fun() ->
		case mnesia:read({elevator_info, ElevatorID}) of
		[#elevator_info{floor = Fl, state = S, available = A}] ->
			{ElevatorID, Fl, S, A};
		[] ->
			undefined
		end
	end,
	mnesia:activity(transaction, F).

order_by_id(OrderID) ->
	F = fun() ->
		case mnesia:read({orders, OrderID}) of
		[#orders{button_type = B, floor = Fl}] ->
			{OrderID, B, Fl};
		[] ->
			undefined
		end
	end,
	mnesia:activity(transaction, F).

add_new_node_to_cluster(NodeName) ->
	net_adm:ping(NodeName),
	em:start(lists:append([[node()], nodes()])),
	mnesia:change_config(extra_db_nodes, [NodeName]),
	mnesia:change_table_copy_type(schema, NodeName, ram_copies),
	mnesia:add_table_copy(orders, NodeName, ram_copies),
	mnesia:add_table_copy(elevator_info, NodeName, ram_copies).

reinit_mnesia_cluster() ->
	rpc:multicall(mnesia, stop, []),
	mnesia:delete_schema(lists:append([[node()], nodes()])),
	mnesia:create_schema(lists:append([[node()], nodes()])),
	rpc:multicall(mnesia, start, []).

reconnect_node(ToNode) ->
	net_adm:ping(ToNode),
	sync_mnesia(ToNode, Table),
	rpc:call(ToNode, em, 'export_table', [orders]),
	rpc:call(ToNode, em, 'export_table', [elevator_info]),
	mnesia:delete_schema(lists:append([[node()], nodes()])),
	rpc:multicall(mnesia, stop, []),
	rpc:eval_everywhere(erlang, disconnect_node, [node()]),	
	rpc:call(ToNode, em, 'install', [lists:append([[node()], nodes()])]),
	rpc:call(ToNode, em, 'import_table', [orders]),
	rpc:call(ToNode, em, 'import_table', [elevator_info]),
	rpc:call(ToNode, em, 'add_new_node_to_cluster', [node()]).

sync_mnesia(ToNode, Table) ->
	Temp1 = ets:new(ignoreme1, [bag, public]),
	Job1 = fun(Key1) ->
		[Record1] = mnesia:dirty_read(Table, Key1),
		ets:insert(Temp1, Record1) end,
	Keys1 = mnesia:dirty_all_keys(Table),

	Temp2 = ets:new(ignoreme2, [bag, public]),
	Job2 = fun(Key2) ->
		[Record2] = rpc:call(ToNode, mnesia, 'dirty_read', [Table, Key2]),
		ets:insert(Temp2, Record2) end,
	Keys2 = rpc:call(ToNode, mnesia, 'dirty_all_keys', [Table]),

	[{Job1(Key1), Job2(Key2)} || {Key1} <- Keys1, {Key2} <- Keys2],
	
	Path = lists:concat(["./",atom_to_list(Table),".ets"]),
   	ets:tab2file([Temp1,Temp2],Path,[{extended_info,[md5sum,object_count]}]),
    	ets:delete(Temp1),
	ets:delete(Temp2).

	
%qlc:e(qlc:q([{Pattern, Type1, Tag, Id1, Id2} || {{Pattern, Type1}, Id1} <- ets:table(a), {{Tag, Type2}, Id2} <- ets:table(b), Type1 =:= Type2, lists:prefix(Pattern, Tag)])).


export_table(Table) ->
    Temp = ets:new(ignoreme,[bag,public]),
    Job  = fun(Key) ->
        [Record] = mnesia:dirty_read(Table,Key),
        ets:insert(Temp,Record) end,
    Keys = mnesia:dirty_all_keys(Table),
    [Job(Key) || Key <- Keys],
    Path = lists:concat(["./",atom_to_list(Table),".ets"]),
    ets:tab2file(Temp,Path,[{extended_info,[md5sum,object_count]}]),
    ets:delete(Temp).

import_table(Table) ->
    Path = lists:concat(["./",atom_to_list(Table),".ets"]),
    {ok,Temp} = ets:file2tab(Path,[{verify,true}]),
    {atomic,Count} = mnesia:transaction(fun() ->
        ets:foldl(fun(Record,I) -> mnesia:write(Record),I+1 end
             ,0
             ,Temp)
        end),
    ets:delete(Temp).








