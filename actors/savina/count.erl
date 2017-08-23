-ifdef(SOTER).
-soter_config(peano).
-define(N, ?any_nat()).
done() ->
    done.
-else.
-module(count).
-export([main/0]).
-define(N, 5).
done() ->
    io:format("Benchmark terminated, exiting..."),
    init:stop().
-endif.

loop_send(0, _) -> done;
loop_send(N, Target) ->
    Target ! {increment},
    loop_send(N-1, Target).

producer(Counter) ->
    receive
        {increment} ->
            loop_send(?N, Counter),
            Counter ! {retrieve, self()},
            producer(Counter);
        {result, Res} ->
            case Res == ?N of
              true -> io:format("Success!~n"), done();
              false -> error("Error!")
            end
    end.

counting(Count) ->
    receive
        {increment} ->
            counting(Count+1);
        {retrieve, To} ->
            To ! {result, Count},
            done
    end.

main() ->
    Counter = spawn(fun() -> counting(0) end),
    Producer = spawn(fun() -> producer(Counter) end),
    Producer ! {increment}.
