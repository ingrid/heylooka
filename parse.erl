-module(parse).
-compile(export_all).

main() -> 
    test_irc_to_tuple("/nick Tom"),
    test_irc_to_tuple("/move N"),
    test_irc_to_tuple("/event Boom"),
    test_extract_irc_from_raw("12 abcdefhgijkl4 nextUNPARSABLEREST"),
    test_extract_irc_from_raw("4 nextUNPARSABLEREST"),
    test_extract_irc_from_raw("UNPARSABLEREST"),
    test_encode_tuple_to_raw(),
    ok.

test_irc_to_tuple(Str) ->
    io:format("result of irc_to_tuple(\"~s\"): ~w~n", [Str, irc_to_tuple(Str)]).

test_extract_irc_from_raw(Str) ->
    io:format("result of extract_irc_from_raw(\"~s\"): ~w~n", [Str, extract_irc_from_raw(Str)]).

test_encode_tuple_to_raw() ->
    Full = {msg, {user, 4, 7, fake, "Tom"}, "Boom!", 17},
    io:format("result of encode_irc_to_raw(~w): \"~s\"~n", [Full, encode_tuple_to_raw(Full)]).

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

encode_tuple_to_raw(Tuple) ->
    case Tuple of
        {msg, {user, From_X, From_Y, From_Pid, From_Name}, EventName, Radius} ->
            Msg = lists:flatten(io_lib:format("~w ~w ~w ~s", [Radius, From_X, From_Y, EventName])),
            FullMsg = io_lib:format("~w ~s", [string:len(Msg), Msg]),
            FullMsg
    end.
