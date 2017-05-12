%-module(count).
%-export([main/0]).
-soter_config(peano).
-uncoverable("producer_mail > 1").
-uncoverable("counting_mail > 11").

-define(N, ?any_nat).

loop_send(0, _) -> done;
loop_send(N, Target) ->
    Target ! {increment},
    loop_send(N-1, Target).


producer(Counter) ->
    ?label_mail("producer_mail"),
    receive
        {increment} ->
            io:format("received increment~n"),
            loop_send(?N, Counter),
            Counter ! {retrieve, self()},
            producer(Counter);
        {result, Res} ->
            io:format("received result~n"),
            case Res == ?N of
              true -> done;
              false -> io:format("error~n"), ?soter_error("Error!")
            end
    end.

counting(Count) ->
    ?label_mail("counting_mail"),
    receive
        {increment} ->
            io:format("count received increment~n"),
            counting({s, Count});
        {retrieve, To} ->
            io:format("count received retrieve~n"),
            To ! {result, Count},
            done
    end.

main() ->
    Counter = spawn(fun() -> counting(zero) end),
    Producer = spawn(fun() -> producer(Counter) end),
    Producer ! {increment}.
