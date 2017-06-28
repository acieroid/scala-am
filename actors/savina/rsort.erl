%-module(rsort).
%-export([main/0]).
%-define(NumValues, 5).
%-define(MaxValue, 1024).
-soter_config(peano).
-define(NumValues, ?any_nat()).
-define(MaxValue, ?any_nat()).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_reverse([], Res) -> Res;
list_reverse([X | Xs], Res) -> list_reverse(Xs, [X | Res]).

list_reverse(L) -> list_reverse(L, []).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
    %rand:uniform(X)-1.
    modulo(?any_nat(), X).

int_source_loop(_, 0) -> done;
int_source_loop(Actor, N) ->
    V = random(?MaxValue),
    io:format("generate ~w~n", [V]),
    Actor ! {value, V},
    int_source_loop(Actor, N-1).

int_source() ->
    receive
        {nextactor, Actor} ->
            int_source_loop(Actor, ?NumValues),
            done
    end.

sort(Radix, NextActor, Array, ValuesSoFar) ->
    CheckValues = fun(Arr) ->
                          case ValuesSoFar+1 == ?NumValues of
                              true ->
                                  list_foreach(fun(V) -> NextActor ! {value, V} end, list_reverse(Arr)),
                                  done;
                              false ->
                                  sort(Radix, NextActor, Arr, ValuesSoFar+1)
                          end
                  end,
    receive
        {value, V} ->
            case V band Radix of
                0 -> io:format("[~w] pass along value ~w~n", [Radix, V]),
                     NextActor ! {value, V},
                     CheckValues(Array);
                _ -> io:format("[~w] keep value ~w~n", [Radix, V]),
                    CheckValues([V | Array])
            end
    end.

validation(SumSoFar, ValuesSoFar, PrevValue, ErrorValue) ->
    receive
        {value, V} ->
            io:format("validate ~w~n", [V]),
            ErrorValue2 = case (V < PrevValue) and (ErrorValue < 0) of
                              true -> V;
                              false -> ErrorValue
                          end,
            case ValuesSoFar+1 == ?NumValues of
                true ->
                    case ErrorValue >= 0 of
                        true -> io:format("error! (~w) ~n", [ErrorValue]);
                        false -> io:format("~w~n", [SumSoFar])
                    end,
                    done;
                false -> validation(SumSoFar+PrevValue, ValuesSoFar+1, V, ErrorValue2)
            end
    end.

main_loop(Radix, NextActor) ->
    case Radix > 0 of
        true ->
            SortActor = spawn(fun() -> sort(Radix, NextActor, [], 0) end),
            io:format("radix ~w, actor ~w~n", [Radix, SortActor]),
            main_loop(trunc(Radix/2), SortActor);
        false -> NextActor
    end.

main() ->
    ValidationActor = spawn(fun() -> validation(0, 0, 0, -1) end),
    SourceActor = spawn(fun() -> int_source() end),
    Radix = trunc(?MaxValue/2),
    SourceActor ! {nextactor, main_loop(Radix, ValidationActor)}.
