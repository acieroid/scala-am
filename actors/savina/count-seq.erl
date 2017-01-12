-uncoverable("producer_mail > 1").
-uncoverable("counting_mail > 2").

producer(Counter) ->
    ?label_mail("producer_mail"),
    receive
        {increment} ->
            Counter ! {increment},
            Counter ! {retrieve, self()},
            producer(Counter);
        {result, N} ->
            if
                N == 1 -> done;
                true -> ?soter_error("Error!")
            end
    end.

counting(Count) ->
    ?label_mail("counting_mail"),
    receive 
        {increment} ->
            counting(Count + 1);
        {retrieve, To} ->
            To ! {result, Count},
            done
    end.

main() ->
    Counter = spawn(fun() -> counting(0) end),
    Producer = spawn(fun() -> producer(Counter) end),
    Producer ! {increment}.
