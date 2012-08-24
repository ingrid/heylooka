-module(chat).
-compile(export_all).

%% -define(CLIENT, {client, X, Y, Pid}).

chat_node(Users, X, Y, Neighbors) ->
    receive
        {add_user, Username, UserPID} ->
            chat_node(Users ++ [{Username, UserPID}], X, Y, Neighbors);
        {dispatch, {FromName, FromPID} = Sender, Msg} ->
            io:format("Dispatching~n"),
            FormatMsg = io_lib:format(string:join(["~s: ", Msg], ""), [FromName]),
            F = fun({_,Pid}) -> Pid ! {recv, FromPID, FormatMsg} end,
            lists:foreach(F, Users),
            N = fun(Neigh) -> Neigh ! {dispatch, Sender, Msg} end,
            lists:foreach(N, Neighbors),
            chat_node(Users, X, Y, Neighbors);
        {users} -> 
            io:format("~p~n", [Users]),           
            chat_node (Users, X, Y, Neighbors);
        {add_nightbor, Node} ->
            chat_node(Users, X, Y, Neighbors ++ [Node]);
        Message ->
            io:format("~p~n", [Message]),
            io:fwrite("Panic.~n"),
            chat_node (Users, X, Y, Neighbors)
        end.

spawn_chat_node(X, Y, Neighbors) ->
    spawn(?MODULE, chat_node, [[], X, Y, Neighbors]).

spawn_chat_node_at(I, Width) ->
    

spawn_map() ->
    list:seq(1, 25),
    

set_up() ->
    register(test_neighbor, spawn(?MODULE, chat_node, [[], 0, 0, []])),
    register(test_chat, spawn(?MODULE, chat_node, [[], 0, 0, [test_neighbor]])),
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
    
