-module(board).
-compile(export_all).

main() -> start().

start() ->
    register(board, spawn(?MODULE, board_positions, [])),
    Center = spawn(?MODULE, node, [0, 0]),
    radiate_msg(Center, grow, 4),
%    radiate_msg(Center, "Hello", 2),
    Levels = 4,
    io:format("predicted nodes for ~w levels: ~w~n", [Levels, total_nodes(Levels)]).

grow_and_wait(Node, Levels) ->
    radiate_msg(Node, grow, Levels),
    wait(total_nodes(Levels)).

radiate_msg(Node, Msg, Levels) ->
    Node ! {msg, Msg, n, e, next_time, Levels},
    Node ! {msg, Msg, e, s, next_time, Levels},
    Node ! {msg, Msg, s, w, next_time, Levels},
    Node ! {msg, Msg, w, n, next_time, Levels}.

wait(Max) ->
    board ! {count, self()},
    receive
        {count, Max} ->
            ok;
        {count, Count} ->
            io:format("~w/~w~n", [Count, Max]),
            wait(Max)
    end.

total_nodes(GrowLevels) ->
    CalcLevels = GrowLevels + 2,
    (2*CalcLevels*CalcLevels) - (2*CalcLevels) + 1.

board_positions() ->
    Mapping = dict:new(),
    board_positions(Mapping).

board_positions(Mapping) ->
    receive
        {position_report, X, Y, Pid} -> 
            NewMapping = dict:store({X, Y}, Pid, Mapping),
            F = fun(Dir) -> 
                    case dict:find(get_new_pos(X, Y, Dir), NewMapping) of
                        error ->
                            ok;
                        {ok, Neighbor} ->
                            Pid ! {neighbor, Neighbor, Dir},
                            Neighbor ! {neighbor, Pid, get_opposite_direction(Dir)}
                    end
            end,
            Neighbors = [n, s, e, w],
            lists:map(F, Neighbors),
            board_positions(NewMapping);
        {count, Who} ->
            Who ! {count, dict:size(Mapping)},
            board_positions(Mapping);
        {sendall, Msg} ->
            F = fun({_, Node}) -> Node ! Msg end,
            list:map(F, dict:to_list(Mapping))
    end.

offset_node(OrigX, OrigY, Direction) ->
    {NewX, NewY} = get_new_pos(OrigX, OrigY, Direction),
    node(NewX, NewY).

node(X, Y) ->
    io:format("new node ~w created at (~w,~w)~n", [self(), X, Y]),
    board ! {position_report, X, Y, self()},
    node(X, Y, dict:new()).

%TODO needs a major refactor
%TODO distinguish Msg (informatio) and Message (the whole thing)
node(X, Y, NeighborMap) ->
    receive
        {msg, _, _, _, _, -1} ->
            node(X, Y, NeighborMap);
        {msg, grow, PrimaryDir, SecondaryDir, next_time, Levels} ->
            Neighbor = spawn(?MODULE, offset_node, [X, Y, PrimaryDir]),
            Neighbor ! {msg, grow, PrimaryDir, SecondaryDir, true, Levels - 1};
        {msg, Msg, PrimaryDir, SecondaryDir, next_time, Levels} ->
            io:format("passing on message from ~w to the ~w~n", [self(), PrimaryDir]),
            send_msg_if_exists(PrimaryDir, {msg, Msg, PrimaryDir, SecondaryDir, true, Levels - 1}, NeighborMap);
        {msg, grow, PrimaryDir, SecondaryDir, true, Levels} ->
            PrimaryNeighbor = spawn(?MODULE, offset_node, [X, Y, PrimaryDir]),
            PrimaryNeighbor ! {msg, grow, PrimaryDir, SecondaryDir, true, Levels - 1},
            SecondaryNeighbor = spawn(?MODULE, offset_node, [X, Y, SecondaryDir]),
            SecondaryNeighbor ! {msg, grow, SecondaryDir, none, false, Levels - 1};
        {msg, Msg, PrimaryDir, SecondaryDir, true, Levels} ->
            io:format("passing on message from ~w to the ~w and ~w~n", [self(), PrimaryDir, SecondaryDir]),
            send_msg_if_exists(PrimaryDir, {msg, Msg, PrimaryDir, SecondaryDir, true, Levels - 1}, NeighborMap),
            send_msg_if_exists(SecondaryDir, {msg, Msg, SecondaryDir, none, false, Levels - 1}, NeighborMap);
        {msg, grow, PrimaryDir, none, false, Levels} ->
            PrimaryNeighbor = spawn(?MODULE, offset_node, [X, Y, PrimaryDir]),
            PrimaryNeighbor ! {msg, grow, PrimaryDir, none, false, Levels - 1};
        {msg, Msg, PrimaryDir, none, false, Levels} ->
            io:format("passing on message from ~w to the ~w~n", [self(), PrimaryDir]),
            send_msg_if_exists(PrimaryDir, {msg, Msg, PrimaryDir, none, false, Levels - 1}, NeighborMap);
        {neighbor, Who, Direction} -> node(X, Y, dict:store(Direction, Who, NeighborMap))
    end,
    node(X, Y, NeighborMap).

get_new_pos(X, Y, Direction) ->
    case Direction of
        n -> {X, Y+1};
        s -> {X, Y-1};
        e -> {X+1, Y};
        w -> {X-1, Y}
    end.

get_opposite_direction(Direction) ->
    case Direction of
        n -> s;
        s -> n;
        e -> w;
        w -> e
    end.

send_msg_if_exists(Direction, Message, NeighborMap) ->
    case dict:find(Direction, NeighborMap) of
        error ->
            ok;
        {ok, Neighbor} ->
            Neighbor ! Message
    end.
