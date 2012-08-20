-module(parse).
-compile(export_all).

main() -> 
    test_irc_to_tuple("/nick Tom"),
    test_irc_to_tuple("/move N"),
    test_irc_to_tuple("/event Boom"),
    test_extract_irc_from_raw("12 abcdefhgijkl4 nextUNPARSABLEREST"),
    test_extract_irc_from_raw("4 nextUNPARSABLEREST"),
    test_extract_irc_from_raw("UNPARSABLEREST").

test_irc_to_tuple(Str) ->
    io:format("result of irc_to_tuple(\"~s\"): ~w~n", [Str, irc_to_tuple(Str)]).

test_extract_irc_from_raw(Str) ->
    io:format("result of extract_irc_from_raw(\"~s\"): ~w~n", [Str, extract_irc_from_raw(Str)]).

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

extract_irc_from_raw(Data) ->
    case Data of
        [] ->
            {no_message, ""};
        _ ->
            {Length, Trailing} = string:to_integer(Data),
            if
                Length =:= error ->
                    % if it doesn't start with a number, throw out
                    % all queued data in attempt to resync
                    {no_message, ""};
                true ->
                    Msg = string:sub_string(Trailing, 2, Length + 1),
                    Rest = string:sub_string(Data, string:len(Data) - string:len(Trailing) + 2 + Length),
                    {Msg, Rest}
            end
    end.
