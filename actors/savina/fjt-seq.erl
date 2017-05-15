%-module(fjt).
%-export([main/0]).
-uncoverable("throughput_mail > 3").

-define(N, 10).

perform_computation(Theta) ->
    Sint = Theta + 1,
    Sint * Sint.

throughput(9) ->
    ?label_mail("throughput_mail"),
    receive
        {message} ->
            perform_computation(37),
            done
    end;
throughput(Processed) ->
    ?label_mail("throughput_mail"),
    receive
      {message} ->
            perform_computation(37),
            throughput(Processed + 1)
    end.

loop_send(0, _, _, _) -> done;
loop_send(N, A1, A2, A3) ->
    A1 ! {message}, A2 ! {message}, A3 ! {message},
    loop_send(N-1, A1, A2, A3).

main() ->
    A1 = spawn(fun() -> throughput(0) end),
    A2 = spawn(fun() -> throughput(0) end),
    A3 = spawn(fun() -> throughput(0) end),
    A1 ! {message},
    A2 ! {message},
    A3 ! {message},
    A1 ! {message},
    A2 ! {message},
    A3 ! {message},
    A1 ! {message},
    A2 ! {message},
    A3 ! {message}.
    % loop_send(?N, A1, A2, A3).
