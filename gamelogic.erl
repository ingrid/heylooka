-module(gamelogic).
-compile(export_all).

main() ->
    register(chat, spawn(gamelogic, chatroom, [])),
    {ok, _} = gen_tcp:listen(8000, [{reuseaddr, true}]),
    {ok, Socket} = gen_tcp:connect("localhost", 8000, []),
    U = spawn(?MODULE, user, [Socket, 2, 3, "tom"]),
    chat ! {useradd, U},
    U2 = spawn(?MODULE, user, [Socket, -3, -4, "fred"]),
    chat ! {useradd, U2},
    U2 ! "/nick george",
    U ! "/move N",
    U ! "/event shout".

user(Socket) ->
    chat ! {useradd, self()},
    io:format("User ~w being initialized", [self()]),
    user(Socket, 0, 0, "No Name").

user(Socket, X, Y, Name) ->
    io:format("starting user/4 loop with ~w~n",[self()]),
    receive
        {msg, {user, From_X, From_Y, From_Pid, From_Name}=From, Event, Radius}=Tuple ->
            if
                abs(From_X - X) + abs(From_Y - Y) < Radius ->
                    io:format("Message in range of ~w!~n", [self()]),
                    gen_tcp:send(Socket, parse:encode_tuple_to_raw(Tuple)),
                    user(Socket, X, Y, Name);
                true ->
                    io:format("Message not in range of ~w.~n", [self()])
            end;
        Data ->
            case parse:irc_to_tuple(Data) of
                {nick, NewName} ->
                    io:format("~w name changed to ~s!~n", [self(), NewName]),
                    user(Socket, X, Y, NewName);
                {move, Dir} ->
                    io:format("~w moved ~s!~n", [Name, Dir]),
                    {DX, DY} = gamedata:get_direction(Dir),
                    NewX = X + DX,
                    NewY = Y + DY,
                    user(Socket, NewX, NewY, Name);
                {event, EventType} ->
                    io:format("message sent:~s~n", [EventType]),
                    {event, EventName, Radius} = gamedata:get_event(EventType),
                    chat ! {msg, {user, X, Y, self(), Name}, EventName, Radius},
                    user(Socket, X, Y, Name)
            end
    end.

chatroom() -> chatroom([]).
chatroom(UserPids) ->
    receive
        {useradd, UserPid} ->
            chatroom(UserPids ++ [UserPid]);
        {msg, From, Data, Radius} ->
            io:format("chat room received message~n", []),
            io:format("in chat room: ~w~n", [UserPids]),
            F = fun(Pid) -> 
                    io:format("chatroom sends message to ~w~n", [Pid]),
                    Pid ! {msg, From, Data, Radius} end,
            lists:foreach(F, UserPids),
            chatroom(UserPids)
    end.
