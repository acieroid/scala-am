-uncoverable("ping_mail > 1").
-uncoverable("pong_mail > 1").
ping(Count, Pong) ->
    ?label_mail("ping_mail"),
    receive
        {start} ->
            Pong ! {send_ping, self()},
            ping(Count - 1, Pong);
        {ping} ->
            Pong ! {send_ping, self()},
            ping(Count - 1, Pong);
        {send_pong} ->
            if
                Count > 0 -> self() ! {ping},
                             ping(Count, Pong);
                true -> Pong ! {stop},
                        done
            end
    end.

pong(Count) ->
    %?label_mail("pong_mail"),
    receive
        {stop} -> done;
        {send_ping, To} -> 
            To ! {send_pong},
            pong(Count + 1)
    end.

main() ->
    Pong = spawn(fun() -> pong(0) end),
    N = 5,
    Ping = spawn(fun() -> ping(N, Pong) end),
    Ping ! {start}.
