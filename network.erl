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
    NewRest = case parse:extract_irc_from_raw(Rest) of
        {no_message, RawRest} ->
            RawRest;
        {Msg, RawRest} ->
            Node ! Msg,
            splitter(Node, RawRest)
    end,
    io:format("splitter ~w now receiving...", [self()]),
    receive
        {tcp, _, Data} ->
            io:format("received message from tcp connection: ~s~n", [Data]),
            io:format("calling splitter with: ~s~n", [Rest ++ Data]),
            splitter(Node, NewRest ++ Data)
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
