% Requires "verify error-free" option

main() ->
    S = spawn(fun()->server()end),
    S ! {hi, bye}.

server() ->
    receive
        {X, P} -> P ! X, % This call will throw an exception when matching P=bye.
                  server();
        bye    -> ?label(stop_server), ok
    end.

