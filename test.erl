-module(test).
-compile(export_all).

main() -> start().

hello() -> 
    io:format("hello~p~p~p~n", [3, 4, 3]).

start() ->
    register(tcp, spawn(?MODULE, listen, [])),
    timer:sleep(infinity).

listen() ->
    io:format("Listen started~n"),
    {ok, LSock} = gen_tcp:listen(8000, [{reuseaddr, true}]),
    listen(LSock).

listen(LSock) ->
    io:format("listen on a socket started~n"),
    {ok, Socket} = gen_tcp:accept(LSock),
    io:format("just accepted a socket~n"),
    ClientResponder = spawn(?MODULE, print, [Socket]),
    gen_tcp:controlling_process(Socket, ClientResponder),
    listen(LSock).

print(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format('~nreceived some data: '),
            io:format(Data),
            print(Socket)
    end.

