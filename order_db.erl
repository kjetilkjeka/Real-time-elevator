-module(order_db).
-compile(export_all).

-record(order, {floor, direction}). % might need something about origin to handle internal orders

%% DB interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install() ->
    rpc:multicall(mnesia, start, []),
    rpc:multicall(mnesia, delete_table, [orders]), % this is ugly hack, clean mnesia every time a node connects
    mnesia:create_table(orders, [
				 {record_name, order},
				 {attributes, record_info(fields, order)},
				 {ram_copies, [node()|nodes()]},
				 {type, bag}
				]).



add_order(Floor, Direction) ->
    case is_order(Floor, Direction) of
	true ->
	    already_exists;
	false ->

	    AddOrderTransaction = fun() ->
					  mnesia:write(orders, #order{direction=Direction, floor=Floor}, write)
				  end,
	    ok = mnesia:activity(transaction, AddOrderTransaction)
    end.


remove_order(Floor, Direction) ->
    RemoveOrderTransaction = fun() ->
				     mnesia:delete_object(orders, #order{floor=Floor, direction=Direction}, write)
			     end,
    mnesia:activity(transaction, RemoveOrderTransaction).

is_order(Floor, Direction) ->
    OrderList = get_order_list(),
    lists:member(#order{floor = Floor, direction = Direction}, OrderList).



%% helper functions
%%%%%%%%%%%%%%%%%%%    

get_order_list() ->
    GetAllOrdersTransaction = fun() ->
				      mnesia:match_object(orders, #order{floor='_', direction='_'}, read)
			      end,
    mnesia:activity(transaction, GetAllOrdersTransaction).
