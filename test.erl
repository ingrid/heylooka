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

chatroom(Users) ->
    receive
        {useradd, User} ->
            {client, X, Y, Pid} = User,
            io:format('added user ~s at ~i ~i', [Pid, X, Y]),
            chatroom(Users ++ [User])
    end.

client(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format('~n~preceived some data: ', self()),
            io:format(Data ++ "\n"),
            client(Socket);
        {msg, {client, X, Y, Pid}=From, Data} ->
            gen_tcp:send(Socket, Data++"\n")
    end.

