-ifdef(SOTER).
-soter_config(peano).
-define(NMessages, ?any_nat()).
-define(NWorkers, ?any_nat()).
-else.
-module(big).
-export([main/0]).
-define(NMessages, 5).
-define(NWorkers, 5).
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

-ifdef(SOTER).
random(X) ->
    modulo(?any_nat(), X).
-else.
random(X) ->
    rand:uniform(X)-1.
-endif.

big_actor(Id, NumPings, Sink, ExpPinger, Neighbors) ->
    receive
        {ping, SenderId} ->
            io:format("ping from ~w to ~w~n", [SenderId, Id]),
            list_ref(SenderId, Neighbors) ! {pong, Id},
            big_actor(Id, NumPings, Sink, ExpPinger, Neighbors);
        {pong, Sender} ->
            io:format("pong from ~w to ~w ~n", [Sender, Id]),
            case Sender == ExpPinger of
                true -> case NumPings == ?NMessages of
                            true ->
                                Sink ! {exit},
                                big_actor(Id, NumPings, Sink, ExpPinger, Neighbors);
                            false ->
                                Target = random(list_length(Neighbors)),
                                list_ref(Target, Neighbors) ! {ping, Id},
                                big_actor(Id, NumPings+1, Sink, Target, Neighbors)
                        end;
                false -> io:format("error:expected ~w but received ping from ~w~n", [ExpPinger, Sender])
            end;
        {exit} ->
            io:format("~w exiting~n", [Id]), done;
        {neighbors, NewNeighbors} ->
            io:format("~w got its neighbors~n", [Id]),
            big_actor(Id, NumPings, Sink, ExpPinger, NewNeighbors)
    end.

sink_actor(NumMessages, Neighbors) ->
    receive
        {exit} ->
            case (NumMessages+1) == ?NWorkers of
                true ->
                    list_foreach(fun(A) -> A ! {exit} end, Neighbors),
                    io:format("exiting sink after ~w messages~n", [NumMessages+1]),
                    done;
                false ->
                    sink_actor(NumMessages+1, Neighbors)
            end;
        {neighbors, NewNeighbors} ->
            sink_actor(NumMessages, NewNeighbors)
    end.

create_big_actors(I, Sink) ->
    case I == ?NWorkers of
        true -> [];
        false -> [spawn(fun() -> big_actor(I, 0, Sink, -1, false) end) | create_big_actors(I+1, Sink)]
    end.

main() ->
    Sink = spawn(fun() -> sink_actor(0, false) end),
    BigActors = create_big_actors(0, Sink),
    Sink ! {neighbors, BigActors},
    list_foreach(fun(A) -> A ! {neighbors, BigActors} end, BigActors),
    list_foreach(fun(A) -> A ! {pong, -1} end, BigActors).
