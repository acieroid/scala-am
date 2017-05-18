%-module(phil).
%-export([main/0]).
%-define(Rounds, 5).
%-define(NumForks, 5).
%-define(NumPhilosophers, ?NumForks).

-soter_config(peano).
-define(Rounds, ?any_nat()).
-define(NumForks, ?any_nat()).
-define(NumPhilosophers, ?any_nat()).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

counter(N) ->
    receive
        {add, M} -> counter(plus(N,M));
        {finish} -> io:format("finished: ~w~n", [N]), done
    end.

list_get(0, [X | _]) -> X;
list_get(N, [_ | Xs]) -> list_get(N-1, Xs).

list_set(0, Y, [_ | Xs]) -> [Y | Xs];
list_set(N, Y, [X | Xs]) -> [X | list_set(N-1, Y, Xs)].

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

lor(true, _) -> true;
lor(_, true) -> true;
lor(false, false) -> false.

arbitrator(Forks, NumExited) ->
    receive
        {hungry, P, Id} ->
            Left = Id,
            Right = modulo(Id+1, ?NumForks),
            % io:format("philosopher ~w is hungry, forks are: ~w / ~w~n", [Id, list_get(Left, Forks), list_get(Right, Forks)]),
            case lor(list_get(Left, Forks), list_get(Right, Forks)) of
                true -> P ! {denied}, arbitrator(Forks, NumExited);
                false -> P ! {eat}, arbitrator(list_set(Left, true, list_set(Right, true, Forks)), NumExited)
            end;
        {done, Id} ->
            Left = Id,
            Right = modulo(Id+1, ?NumForks),
            arbitrator(list_set(Left, false, list_set(Right, false, Forks)), NumExited);
        {exit} ->
            case (NumExited+1) == ?NumForks of
                true -> io:format("finished~n"), done;
                false -> arbitrator(Forks, NumExited+1)
            end
    end.

philosopher(Id, Arbitrator, Counter, RoundsSoFar, LocalCounter) ->
    receive
        {denied} -> % io:format("philisopher ~w denied~n", [Id]), Arbitrator ! {hungry, self(), Id},
            Arbitrator ! {hungry, self(), Id},
            philosopher(Id, Arbitrator, Counter, RoundsSoFar, LocalCounter+1);
        {eat} ->
            io:format("philisopher ~w eats~n", [Id]),
            Counter ! {add, LocalCounter},
            Arbitrator ! {done, Id},
            case (RoundsSoFar+1) == ?Rounds of
                false -> Arbitrator ! {hungry, self(), Id},
                         philosopher(Id, Arbitrator, Counter, RoundsSoFar+1, LocalCounter);
                true -> Arbitrator ! {exit},
                        done
            end;
        {start} ->
            io:format("philosopher ~w starts~n", [Id]),
            Arbitrator ! {hungry, self(), Id},
            philosopher(Id, Arbitrator, Counter, RoundsSoFar, LocalCounter)
    end.

create_forks(N) ->
    case N == ?NumForks of
        true -> [];
        false -> [false | create_forks(N+1)]
    end.

create_philosophers(N, Arbitrator, Counter) ->
    case N == ?NumPhilosophers of
        true -> done;
        false -> P = spawn(fun() -> philosopher(N, Arbitrator, Counter, 0, 0) end),
                 P ! {start},
                 create_philosophers(N+1, Arbitrator, Counter)
    end.

main() ->
    Counter = spawn(fun() -> counter(0) end),
    Arbitrator = spawn(fun() -> arbitrator(create_forks(0), 0) end),
    create_philosophers(0, Arbitrator, Counter).
