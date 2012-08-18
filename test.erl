-module(test).
-compile(export_all).

main() -> start().

hello() -> 
    io:format("hello~p~p~p~n", [3, 4, 3]).

start() ->
    register(chat, spawn(?MODULE, chatroom, [[]])),
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
    ClientResponder = spawn(?MODULE, client, [Socket, 0, 0]),
    io:format("just spawned a client~n"),
    gen_tcp:controlling_process(Socket, ClientResponder),
    io:format("just set client to use socket~n"),
    chat ! {useradd, ClientResponder},
    io:format("just told chatroom to add user~n"),
    listen(LSock).

chatroom(UserPids) ->
    receive
        {useradd, UserPid} ->
            io:format('added user ~w', [UserPid]),
            chatroom(UserPids ++ [UserPid]);
        {msg, From, Data} ->
            F = fun(Pid) -> Pid ! {msg, From, Data} end,
            lists:foreach(F, UserPids),
            chatroom(UserPids)
    end.

client(Socket, X, Y) ->
    receive
        {tcp, Socket, Data} ->
            io:format('~n~p received some data: ', [self()]),
            io:format(Data ++ "\n"),
            chat ! {msg, {client, X, Y, self()}, Data},
            client(Socket, X, Y);
        {msg, {client, From_X, From_Y, From_Pid}=From, Data} ->
            io:format('~n~p relayed some data: ', [self()]),
            gen_tcp:send(Socket, Data++"\n"),
            client(Socket, X, Y)
    end.
