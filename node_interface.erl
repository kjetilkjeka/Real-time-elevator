-module(node_interface).
-compile(export_all).

-define(SEND_PORT, 5667).
-define(RECV_PORT, 5677).
-define(COOKIE, "erlang"). 
-define(SEEK_PERIOD, 2000).

start_listening_for_connections() ->
    {ok, Socket} = gen_udp:open(?RECV_PORT, [binary, {active,false}]),
    listen_for_connections(Socket).

listen_for_connections(RecvSocket) ->
    {ok, {Adress, ?SEND_PORT, ?COOKIE}} = gen_udp:recv(RecvSocket, 0),
    io:format("Adress is found -ish~n", []).
	


start_seeking_listeners() ->
    {ok, Socket} = gen_udp:open(?SEND_PORT, [binary, {active,true}]),
    seek_listeners(Socket).
seek_listeners(SendSocket) ->
    ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECV_PORT, ?COOKIE),
    io:format("UDP shizzle sent ~n", []),
    timer:sleep(?SEEK_PERIOD),
    seek_listeners(SendSocket).

    
