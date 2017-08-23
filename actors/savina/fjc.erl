-ifdef(SOTER).
-soter_config(peano).
-define(N, ?any_nat()).
-else.
-module(fjc).
-export([main/0]).
-define(N, 5).
-endif.

perform_computation(Theta) ->
    Theta.

forkjoin() ->
    receive
        {message} ->
            perform_computation(37),
            done
    end.

lp(0) -> done;
lp(N) ->
    spawn(fun() -> forkjoin() end) ! {message},
    lp(N-1).

main() ->
    lp(?N).
