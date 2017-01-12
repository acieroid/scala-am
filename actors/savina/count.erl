%-module(count).
%-export([main/0]).
-uncoverable("producer_mail > 1").
-uncoverable("counting_mail > 11").

-define(N, 10).

loop_send(Cur, Target) ->
    if
        Cur > 0 ->
            Target ! {increment},
            loop_send(Cur - 1, Target);
        true ->
            done
    end.

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
            if
                Res == ?N -> done;
                true -> io:format("error~n"), ?soter_error("Error!")
            end
    end.

counting(Count) ->
    ?label_mail("counting_mail"),
    receive 
        {increment} ->
            io:format("count received increment~n"),
            counting(Count + 1);
        {retrieve, To} ->
            io:format("count received retrieve~n"),
            To ! {result, Count},
            done
    end.

main() ->
    Counter = spawn(fun() -> counting(0) end),
    Producer = spawn(fun() -> producer(Counter) end),
    Producer ! {increment}.
