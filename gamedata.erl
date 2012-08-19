-module(gamedata).
-compile(export_all).

main() ->
    io:format("result of get_event(\"horn\"): ~w~n", [get_event("horn")]),
    io:format("result of get_direct(\"N\"): ~w~n", [get_direction("N")]).

%% mapping of names to {event, <name of event>, <event radius>}


get_event(Name) ->
    EventList = [
        {"horn", {event, "horn", 7}},
        {"shout", {event, "shout", 10}},
        {"whisper", {event, "whisper", 3}}
    ],
    EventDict = dict:from_list(EventList),
    dict:fetch(Name, EventDict).

get_direction(Name) ->
    case Name of
        "N" -> {0,1};
        "S" -> {0,-1};
        "E" -> {1,0};
        "W" -> {-1,0}
    end.
