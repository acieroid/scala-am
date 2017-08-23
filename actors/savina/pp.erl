-ifdef(SOTER).
-define(N, ?any_nat()).
done() ->
    done.
-else.
-module(pp).
-export([main/0]).
-define(N, 5).
done() ->
    io:format("Benchmark terminated, exiting..."),
    init:stop().
-endif.

ping(Count, Pong) ->
    receive
        {start} ->
            Pong ! {send_ping, self()},
            ping(Count - 1, Pong);
        {ping} ->
            io:format("ping [Count: ~w]~n", [Count]),
            Pong ! {send_ping, self()},
            ping(Count - 1, Pong);
        {send_pong} ->
            io:format("send_pong [Count: ~w]~n", [Count]),
            if
                Count > 0 -> self() ! {ping},
                             ping(Count, Pong);
                true -> Pong ! {stop},
                        done()
            end
    end.

pong(Count) ->
    receive
        {stop} -> done;
        {send_ping, To} ->
            To ! {send_pong},
            pong(Count + 1)
    end.

main() ->
    Pong = spawn(fun() -> pong(0) end),
    Ping = spawn(fun() -> ping(?N, Pong) end),
    Ping ! {start}.
