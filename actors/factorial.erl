%-module(factorial).
%-export([main/0]).
-uncoverable("factorial_mail > 6").
-uncoverable("customer_mail > 1").

fact() ->
    ?label_mail("factorial_mail"),
    receive
        {compute, 0, Customer} ->
            Customer ! {result, 1};
        {compute, N, Customer} ->
            C = spawn(fun() -> customer(N, Customer) end),
            self() ! {compute, N - 1, C}
    end,
    fact().

customer(N, Customer) ->
    ?label_mail("customer_mail"),
    receive
        {result, K} ->
            io:format("got result ~p on pid ~p ~n", [K, self()]),
            Customer ! {result, N*K},
            customer(N, Customer)
    end.

display() ->
    receive
        {result, N} ->
             io:format("~p~n", [N])
    end.

main() ->
    F = spawn(fun() -> fact() end),
    Disp = spawn(fun() -> display() end),
    F ! {compute, 5, Disp}.
