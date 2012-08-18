-module(parse).
-compile(export_all).

main() -> parse("/nick Tom").

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
    receive
        {tcp, Socket, Data} ->
            splitter(Node, Rest ++ Data)
    end.
                
    


                                    
