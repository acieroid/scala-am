-uncoverable("forkjoin_mail > 1")
perform_computation(Theta) ->
    Sint = Theta + 1,
    Sint * Sint.

forkjoin() ->
    ?label_mail("forkjoin_mail"),
    receive
        {message} ->
            perform_computation(37.2),
            done
    end.

main() ->
    A1 = spawn(fun() -> forkjoin() end),
    A2 = spawn(fun() -> forkjoin() end),    
    A3 = spawn(fun() -> forkjoin() end),
    A1 ! {message},
    A2 ! {message},
    A3 ! {message}.
