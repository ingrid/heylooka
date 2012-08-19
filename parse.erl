-module(parse).
-compile(export_all).

main() -> 
    test_irc_to_tuple("/nick Tom"),
    test_irc_to_tuple("/move N"),
    test_irc_to_tuple("/event Boom").

test_irc_to_tuple(Str) ->
    io:format("result of irc_to_tuple(\"~s\"): ~w~n", [Str, irc_to_tuple(Str)]).

irc_to_tuple(Data) ->
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
