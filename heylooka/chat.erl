-module(chat).
-compile(export_all).

%% -define(CLIENT, {client, X, Y, Pid}).

chat_node(Users, Neighbors) ->
    receive
        {add_user, Username, UserPID} ->
            chat_node(Users ++ [{Username, UserPID}], Neighbors);
        {dispatch, {FromName, FromPID} = Sender, Msg} ->
            io:format("Dispatching~n"),
            FormatMsg = io_lib:format(string:join(["~s: ", Msg], ""), [FromName]),
            F = fun({_,Pid}) -> Pid ! {recv, FromPID, FormatMsg} end,
            lists:foreach(F, Users),
            N = fun(Neigh) -> Neigh ! {dispatch, Sender, Msg} end,
            lists:foreach(N, Neighbors),
            chat_node(Users, Neighbors);
        {rm_user, Username, UserPID} ->
            ok;
        {users} -> 
            io:format("~p~n", [Users]),           
            chat_node (Users, Neighbors);
        {add_nightbor, Node} ->
            chat_node(Users, Neighbors ++ [Node]);
        {event, {FromName, _FromPID}, Origin, Range} ->
            ok;
        Message ->
            io:format("~p~n", [Message]),
            io:fwrite("Panic.~n")
        end.

spawn_chat_node() ->
    ok.

set_up() ->
    register(test_neighbor, spawn(?MODULE, chat_node, [[], []])),
    register(test_chat, spawn(?MODULE, chat_node, [[], [test_neighbor]])),
    register(foo, spawn(?MODULE, user, ["Foo", 0, 0])),
    register(bar, spawn(?MODULE, user, ["Bar", 0, 0, test_neighbor])),
    test_neighbor ! {add_user, "Bar", bar},
    ok.

get_neighbors(X, Y) ->
    [].

get_node(X, Y) ->
    test_chat.

user(Username, X, Y) ->
    receive
        join ->
            Node = get_node(X, Y),
            Node ! {add_user, Username, self()},
            user(Username, X, Y, Node)
    end.

user(Username, X, Y, Node) ->
    receive
        {recv, Sender, Message} when Sender =:= self() ->
            ok,
            user(Username, X, Y, Node);
        {recv, Sender, Message} ->
            io:format("~s~s~n", [Username, Message]),
            user(Username, X, Y, Node);
        {send, Message} ->
            io:format("Sending.~n"),
            Node ! {dispatch, {Username, self()}, Message},
            user(Username, X, Y, Node);
        Message ->
            io:format("~p~n", [Message]),
            io:fwrite("Panic.~n")
    end.
    
