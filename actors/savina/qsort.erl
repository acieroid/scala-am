-ifdef(SOTER).
-soter_config(peano).
-define(T, ?any_nat()).
-define(InputSize, ?any_nat()).
-else.
-module(qsort).
-export([main/0]).
-define(T, 5).
-define(InputSize, 1500).
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

filter(_, []) -> [];
filter(F, [X | Xs]) ->
    case F(X) of
        true -> [X | filter(F, Xs)];
        false -> filter(F, Xs)
    end.
append([], Ys) -> Ys;
append([X | Xs], Ys) -> [X | append(Xs, Ys)].

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
    rand:uniform(X)-1.
%    modulo(?any_nat(), X).

qsort_seq([]) -> [];
qsort_seq([X]) -> [X];
qsort_seq(Data) ->
    DataLength = list_length(Data),
    Pivot = list_ref(trunc(DataLength/2), Data),
    LeftUnsorted = filter(fun(X) -> X < Pivot end, Data),
    LeftSorted = qsort_seq(LeftUnsorted),
    EqualElements = filter(fun(X) -> X == Pivot end, Data),
    RightUnsorted = filter(fun(X) -> X > Pivot end, Data),
    RightSorted = qsort_seq(RightUnsorted),
    Res = append(LeftSorted, append(EqualElements, RightSorted)),
    Res.

qsort_actor(Parent, PositionRelativeToParent, Result, NumFragments) ->
    receive
        {sort, Data} ->
            case list_length(Data) < ?T of
                true ->
                    Result2 = qsort_seq(Data),
                    case Parent of
                        false -> done;
                        _ -> Parent ! {result, Result2, PositionRelativeToParent},
                             done
                    end;
                false ->
                    DataLength = list_length(Data),
                    DataLengthHalf = trunc(DataLength/2),
                    Pivot = list_ref(DataLengthHalf, Data),
                    LeftUnsorted = filter(fun(X) -> X < Pivot end, Data),
                    Self = self(),
                    LeftActor = spawn(fun() -> qsort_actor(Self, left, [], 0) end),
                    EqualElements = filter(fun(X) -> X == Pivot end, Data),
                    RightUnsorted = filter(fun(X) -> X > Pivot end, Data),
                    RightActor = spawn(fun() -> qsort_actor(Self, right, [], 0) end),
                    LeftActor ! {sort, LeftUnsorted},
                    RightActor ! {sort, RightUnsorted},
                    qsort_actor(Parent, PositionRelativeToParent, EqualElements, NumFragments+1)
            end;
        {result, Data, Position} ->
            Result2 = case Data of
                          [] -> Result;
                          _ -> case Position of
                                   left -> append(Data, Result);
                                   right -> append(Result, Data);
                                   _ -> Result
                               end
                      end,
            case NumFragments+1 == 3 of
                true ->
                    case Parent of
                        false -> io:format("done~n"), done;
                        _ -> Parent ! {result, Result2, PositionRelativeToParent}
                    end;
                false ->
                    qsort_actor(Parent, PositionRelativeToParent, Result2, NumFragments+1)
            end
    end.

input(0) -> [];
input(N) -> [random(1000)|input(N-1)].

main() ->
    spawn(fun() -> qsort_actor(false, initial, [], 0) end) ! {sort, input(?InputSize)}.
