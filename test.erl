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
    {ok, LSock} = gen_tcp:listen(8000, [{reuseaddr, true}]),
    listen(LSock).

listen(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    ClientResponder = spawn(?MODULE, client, [Socket, 0, 0, "Unnamed"]),
    gen_tcp:controlling_process(Socket, ClientResponder),
    chat ! {useradd, ClientResponder},
    listen(LSock).

chatroom(UserPids) ->
    receive
        {useradd, UserPid} ->
            chatroom(UserPids ++ [UserPid]);
        {msg, From, Data, Radius} ->
            io:format("chat room received message~n", []),
            F = fun(Pid) -> Pid ! {msg, From, Data, Radius} end,
            lists:foreach(F, UserPids),
            chatroom(UserPids)
    end.

client(Socket, X, Y, Name) ->
    receive
        {tcp, Socket, Data} ->
            case parse:parse(Data) of
                {nick, NewName} ->
                    io:format("~w name changed to ~s!~n", [self(), NewName]),
                    client(Socket, X, Y, NewName);
                {move, Dir} ->
                    io:format("~w moved!~n", [Name]),
                    client(Socket, X+1, Y, Name);
                {event, Event} -> 
                    io:format("message sent:~s~n", [Event]),
                    chat ! {msg, {client, X, Y, self(), Name}, Event, 5},
                    client(Socket, X, Y, Name)
            end;
        {msg, {client, From_X, From_Y, From_Pid, From_Name}=From, Event, Radius} ->
            if
                abs(From_X - X) + abs(From_Y - Y) < Radius ->
                    io:format("Message in range of ~w!~n", [self()]),
                    gen_tcp:send(Socket, io_lib:format("~w ~w ~w ~s", [Radius, From_X, From_Y, Event])),
                    client(Socket, X, Y, Name);
                true ->
                    io:format("Message not in range of ~w.~n", [self()])
            end
    end.
