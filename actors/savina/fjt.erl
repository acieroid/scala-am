-ifdef(SOTER).
-soter_config(peano).
-define(A, ?any_nat()).
-define(N, ?any_nat()).
-else.
-module(fjt).
-export([main/0]).
-define(A, 3).
-define(N, 10).
-endif.

perform_computation(Theta) ->
    Theta.

throughput(1) ->
    receive
        {message} ->
            perform_computation(37),
            done
    end;
throughput(N) ->
    receive
      {message} ->
            perform_computation(37),
            throughput(N-1)
    end.

send_all([]) -> done;
send_all([A | As]) -> A ! {message}, send_all(As).

loop_send(0, _) -> done;
loop_send(N, As) -> send_all(As), loop_send(N-1, As).

spawn_throughput(0) ->
    [];
spawn_throughput(N) ->
    [spawn(fun() -> throughput(?N) end) | spawn_throughput(N-1)].

main() ->
    As = spawn_throughput(?A),
    loop_send(?N, As).
