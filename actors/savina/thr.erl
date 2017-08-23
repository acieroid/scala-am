-ifdef(SOTER).
-soter_config(peano).
-define(N, ?any_nat()).
-define(R, ?any_nat()).
-else.
-module(thr).
-export([main/0]).
-define(N, 5).
-define(R, 5).
-endif.

threadring(Id, Next) ->
    receive
        {ping, 0} ->
            Next ! {exit, ?N},
            threadring(Id, Next);
        {ping, PingsLeft} ->
            Next ! {ping, PingsLeft-1},
            threadring(Id, Next);
        {data, Next2} -> threadring(Id, Next2);
        {exit, 1} -> io:format("done~n"), done;
        {exit, ExitsLeft} ->
            Next ! {exit, ExitsLeft-1}
    end.

ring_actors(I) ->
    case I == ?N of
        true -> [];
        false ->  [spawn(fun() -> threadring(I, none) end) | ring_actors(I+1)]
    end.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

loop_next(Ring, I) ->
    case I == ?N of
        true -> done;
        false ->
            ActorA = list_ref(I, Ring),
            ActorB = list_ref(modulo((I+1), ?N), Ring),
            ActorA ! {data, ActorB},
            loop_next(Ring, I+1)
    end.

send_to_first([]) -> done;
send_to_first([Actor | _]) -> Actor ! {ping, ?R}.

main() ->
    Ring = ring_actors(0),
    loop_next(Ring, 0),
    send_to_first(Ring).
