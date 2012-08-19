-module(network).
-compile(export_all).

%main() -> test_splitter().
main() -> start().

start() ->
    register(chat, spawn(gamelogic, chatroom, [])),
    register(tcp, spawn(?MODULE, listen, [])),
    timer:sleep(infinity).

listen() ->
    {ok, LSock} = gen_tcp:listen(8000, [{reuseaddr, true}]),
    listen(LSock).

listen(LSock) ->
    spawn(?MODULE, proto_splitter, [self(), LSock]),
    receive
        cxn_created -> listen(LSock)
    end.
    % wait until premptively-spawned client is being used by a connection

proto_splitter(Listener, LSock) ->
    io:format("splitter has been pre-emptively spawned~n", []),
    {ok, Socket} = gen_tcp:accept(LSock),
    Listener ! cxn_created,
    io:format("connection made~n", []),
    User = spawn(gamelogic, user, [Socket]),
    splitter(User, "").

splitter(Node, Rest) ->
    if
        Rest =/= [] ->
            {Length, Trailing} = string:to_integer(Rest),
            if
                Length =:= error ->
                    splitter(Node, "");
                true ->
                    io:format("length: ~w, trailing: ~s", [Length, Trailing]),
                    Msg = string:sub_string(Trailing, 2, Length + 1),
                    io:format("Msg: ~s", [Msg]),
                    NewRest = string:sub_string(Rest, string:len(Rest) - string:len(Trailing) + 2 + Length),
                    io:format("newrest: ~s,", [NewRest]),
                    Node ! Msg,
                    splitter(Node, NewRest)
            end;
        true ->
            ok
    end,
    io:format("splitter ~w now receiving...", [self()]),
    receive
        {tcp, Socket, Data} ->
            io:format("received message from tcp connection: ~s~n", [Data]),
            io:format("calling splitter with: ~s~n", [Rest ++ Data]),
            splitter(Node, Rest ++ Data)
    end.


test_splitter() ->
    Splitter = spawn(?MODULE, splitter, [self(), ""]),
    Splitter ! {tcp, "dummysocket", "12 abcdefhgijkl4 nextUNPARSABLEREST"},

    receive
        Anything1 ->
            io:format("received msg: ~w~n", [Anything1])
    end,
    receive
        Anything2 ->
            io:format("received msg: ~w~n", [Anything2])
    end.
