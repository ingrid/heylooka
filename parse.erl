-module(parse).
-compile(export_all).

main() -> 
    test_parse("/nick Tom"),
    test_parse("/move N"),
    test_parse("/event Boom").

test_parse(Str) ->
    io:format("result of parse(\"~s\"): ~w~n", [Str, parse(Str)]).

parse(Data) ->
    case Data of
        "/nick " ++ Name ->
            {nick, Name};
        "/move " ++ Dir ->
            {move, Dir};
        "/event " ++ Event ->
            {event, Event};
        _ ->
            io:format("Not a command")
    end.
