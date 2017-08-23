-ifdef(SOTER).
-soter_config(peano).
-define(NumRounds, ?any_nat()).
-define(NumSmokers, ?any_nat()).
-else.
-module(cig).
-export([main/0]).
-define(NumRounds, 5).
-define(NumSmokers, 5).
-endif.

-ifdef(SOTER).
minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) -> modulo(?any_nat(), X).
-else.
random(X) -> rand:uniform(X)-1.
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

notify_random_smoker(SmokerActors) ->
    list_ref(random(?NumSmokers), SmokerActors) ! {startsmoking, random(1000)+10}.

create_smoker_actors(I, Arbitrator) ->
    case I == ?NumSmokers of
        true -> [];
        false -> [spawn(fun() -> smoker(Arbitrator) end) | create_smoker_actors(I+1, Arbitrator)]
    end.

arbitrator(SmokerActors, RoundsSoFar) ->
    receive
        {createsmokers} -> arbitrator(create_smoker_actors(0, self()), 0);
        {start} ->
            notify_random_smoker(SmokerActors),
            arbitrator(SmokerActors, RoundsSoFar);
        {startedsmoking} ->
            case RoundsSoFar+1 == ?NumRounds of
                true ->
                    list_foreach(fun(S) -> S ! {exit} end, SmokerActors),
                    io:format("finished~n"),
                    done;
                false ->
                    notify_random_smoker(SmokerActors),
                    arbitrator(SmokerActors, RoundsSoFar+1)
            end
    end.

smoker(Arbitrator) ->
    receive
        {startsmoking, _} ->
            io:format("smoking~n"),
            Arbitrator ! {startedsmoking},
            smoker(Arbitrator);
        {exit} -> done
    end.

main() ->
    Arbitrator = spawn(fun() -> arbitrator(false, 0) end),
    Arbitrator ! {createsmokers},
    Arbitrator ! {start}.
