-module(node_interface.erl)
-compile(export_all)

-define(SEND_PORT, 5667).
-define(RECV_PORT, 5677).
-define(COOKIE, "erlang"). 
-define(SEEK_FREQUENCY, 500).

start_listening_for_connections() ->
    {ok, Socket} = gen_udp:open(?RECV_PORT, [binary, {active,true}]),
    listen_for_connections().

listen_for_connections(RecvSocket) ->
    {ok, {Adress, ?SEND_PORT, ?COOKIE}} = gen_udp:recv(RecvSocket, {255.255.255.255}, ?RECV_PORT),
    case Adress of
	


start_seeking_listeners() ->
    {ok, Socket} = gen_udp:open(?SEND_PORT, [binary, {active,true}]),
    seek_listeners(Socket).
seek_listeners(SendSocket) ->
    gen_udp:send(SendSocket, {255.255.255.255}, ?RECV_PORT, ?COOKIE),
    timer:wait(?SEEK_FREQUENCY),
    seek_listeners(SendSocket).


ping_cluster() ->
    
