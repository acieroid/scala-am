main() ->
    ME = self(),
    S = spawn(fun()-> receive {_,X} -> ME ! ok, server(X) end end),
    S ! {init, zero},
    receive ok -> ok end,
    S ! {hi, self()},
    S ! {then, self()},
    S ! bye.

server(State) ->
    receive
        {X, P} -> P ! State, % This call would throw an exception if matching P=zero.
                  server(X);
        bye    -> ?label(stop_server), ok
    end.

