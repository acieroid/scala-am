%-module(fjt).
%-export([main/0]).
-soter_config(peano).
%-uncoverable("throughput_mail > 10").

-define(A, 3). %?any_nat()).
-define(N, 10). % ?any_nat()).

perform_computation(Theta) ->
io:format("computation~n"), Theta.

throughput(1) ->
    %?label_mail("throughput_mail"),
    receive
        {message} ->
            perform_computation(37),
            done
    end;
throughput(N) ->
    %?label_mail("throughput_mail"),
    receive
      {message} ->
            perform_computation(37),
            throughput(N-1)
    end.

send_all([]) -> done;
send_all([A | As]) -> A ! {message}, send_all(As);
send_all(_) -> done. % should not happen, but otherwise soter has a false positive "match fail" error

loop_send(0, _) -> done;
loop_send(N, As) -> send_all(As), loop_send(N-1, As).

spawn_throughput(0) ->
    [];
spawn_throughput(N) ->
  [spawn(fun() -> throughput(?N) end) | spawn_throughput(N-1)].

main() ->
    As = spawn_throughput(?A),
    loop_send(?N, As).
