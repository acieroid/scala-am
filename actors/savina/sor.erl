-ifdef(SOTER).
-soter_config(peano).
-define(JacobiNumIter, ?any_nat()).
-define(Omega, ?any_nat()).
-define(N, ?any_nat()).
-else.
-module(sor).
-export([main/0]).
-define(JacobiNumIter, 5).
-define(Omega, 1.25).
-define(N, 3).
-endif.

-define(DataSizes, [2, 8, 10, 12, 15, 25, 30, 35, 40, 80, 100, 120, 150, 200, 250, 300, 350, 400]).


build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).


list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list2_ref(L, I, J) ->
    list_ref(J, list_ref(I, L)).

list_set([], _, _) -> [];
list_set([_ | Xs], 0, Y) -> [Y|Xs];
list_set([X | Xs], I, Y) -> [X | list_set(Xs, I-1, Y)].

list2_set([],     0, _, _) -> error(err);
list2_set([X|Xs], 0, J, Y) -> [list_set(X, J, Y)|Xs];
list2_set([X|Xs], I, J, Y) -> [X|list2_set(Xs, I-1, J, Y)].

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

divide(X, Y) ->
    case X < Y of
        true -> 0;
        false -> divide(minus(X,Y),Y)+1
    end.

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

-ifdef(SOTER).
ftimes(X, Y) -> times(X, Y).
fdivide(X, Y) -> divide(X, Y).
fplus(X, Y) -> plus(X, Y).
fminus(X, Y) -> minus(X, Y).
-else.
ftimes(X, Y) -> X*Y.
fdivide(X, Y) -> X/Y.
fplus(X, Y) -> X+Y.
fminus(X, Y) -> X-Y.
-endif.

-ifdef(SOTER).
random(X) ->
    modulo(?any_nat(), X).
-else.
random(X) ->
    rand:uniform(X)-1.
-endif.

-ifdef(SOTER).
rand_mat_el() ->
    divide(divide(random(100)+1, random(100)+1), 100000).
-else.
rand_mat_el() ->
    ftimes(0.000001, (fdivide(random(100)+1,random(100)+1))).
-endif.
random_matrix(M, N) ->
    build_list(M, fun(_) -> build_list(N, fun(_) -> rand_mat_el() end) end).

sor_runner_actor(N) ->
    DataSize = list_ref(?N, ?DataSizes),
    A = random_matrix(DataSize, DataSize),
    S = list_ref(N, ?DataSizes),
    Part = divide(S, 2),
    Boot = fun(Self, OriginalSorActors) ->
                   Loop1I = fun(Loop1I, SorActors, MyBorder, I) ->
                                    case I == S of
                                        true -> {SorActors, MyBorder};
                                        false ->
                                            Loop1J = fun(Loop1J, SorActors2, MyBorder2, C, J) ->
                                                             case J == Part of
                                                                 true -> {SorActors2, MyBorder2};
                                                                 false ->
                                                                     Pos = plus(times(I, Part+1), J),
                                                                     CNew = minus(1, C),
                                                                     SorActorsNew = list_set(SorActors2, Pos, sor_actor(Pos, list2_ref(A, I, J), CNew, S, Part+1, ?Omega, Self, false)),
                                                                     MyBorderNew = case J == Part-1 of
                                                                                       true -> list_set(MyBorder2, I, list_ref(Pos, SorActorsNew));
                                                                                       false -> MyBorder2
                                                                                   end,
                                                                     Loop1J(Loop1J, SorActorsNew, MyBorderNew, CNew, J+1)
                                                             end
                                                     end,
                                            {SorActorsNew, MyBordersNew} = Loop1J(Loop1J, SorActors, MyBorder, modulo(I, 2), 0),
                                            Loop1I(Loop1I, SorActorsNew, MyBordersNew, I+1)
                                    end
                            end,
                   Loop2I = fun(Loop2I, PartialMatrix, I) ->
                                    case I == S of
                                        true -> PartialMatrix;
                                        false ->
                                            Loop2J = fun(Loop2J, PartialMatrix2, J) ->
                                                             case J == minus(S, Part) of
                                                                 true -> PartialMatrix2;
                                                                 false ->
                                                                     Loop2J(Loop2J, list2_set(PartialMatrix, I, J, list2_ref(A, I, plus(J, Part))), J+1)
                                                             end
                                                     end,
                                            NewPartialMatrix = Loop2J(Loop2J, PartialMatrix, 0),
                                            Loop2I(Loop2I, NewPartialMatrix, I+1)
                                    end
                            end,
                   {NewSorActors, NewMyBorder} = Loop1I(Loop1I, OriginalSorActors, build_list(S, fun(_) -> false end), 0),
                   PartialMatrix = Loop2I(Loop2I, build_list(S, fun(_) -> build_list(minus(S, Part), fun(_) -> 0 end) end), 0),
                   sor_peer(S, Part, PartialMatrix, NewMyBorder, Self) ! {boot},
                   NewSorActors
           end,
    Actor = fun(Actor, SorActors, GTotal, Returned, TotalMsgRcv, ExpectingBoot) ->
                    receive
                        {boot} -> SorActors2 = Boot(self(), SorActors),
                                  Actor(Actor, SorActors2, GTotal, Returned, TotalMsgRcv, false);
                        {result, _, _, Mv, MsgRcv} ->
                            case ExpectingBoot of
                                true -> error("expecting boot");
                                false -> nothing
                            end,
                            case Returned+1 == times(S, Part)+1 of
                                true -> io:format("sor_runner done~n"), done;
                                false ->
                                    Actor(Actor, plus(GTotal, Mv), Returned+1, plus(TotalMsgRcv, MsgRcv), ExpectingBoot)
                            end;
                        {border, MBorder} ->
                            case ExpectingBoot of
                                true -> error("expecting boot");
                                false -> nothing
                            end,
                            Loop1I = fun(Loop1I, SorActors, I) ->
                                             case I == S of
                                                 true -> SorActors;
                                                 false ->
                                                     Loop1I(Loop1I, list_set(SorActors, minus(times(I+1, Part+1), 1), list_ref(I, MBorder)), I+1)
                                             end
                                     end,
                            NewSorActors = Loop1I(Loop1I, SorActors, 0),
                            Loop2I = fun(Loop2I, I) ->
                                             case I == S of
                                                 true -> true;
                                                 false ->
                                                     Loop2J = fun(Loop2J, J) ->
                                                                      case J == Part of
                                                                          true -> true;
                                                                          false ->
                                                                              Pos = plus(times(I, Part+1), J),
                                                                              list_ref(Pos, NewSorActors) ! {start, ?JacobiNumIter, NewSorActors},
                                                                              Loop2J(Loop2J, J+1)
                                                                      end
                                                              end,
                                                     Loop2J(Loop2J, 0),
                                                     Loop2I(Loop2I, I+1)
                                             end
                                     end,
                            Loop2I(Loop2I, 0),
                            Actor(Actor, NewSorActors, GTotal, Returned, TotalMsgRcv, ExpectingBoot)
                    end
            end,
    spawn(fun() -> Actor(Actor, build_list(times(S, Part+1), fun(_) -> false end), 0, 0, 0, true) end).
-ifdef(SOTER).
compute_omega(Omega) ->
    OmegaOverFour = divide(Omega, 4),
    OneMinusOmega = minus(1, Omega),
    {OmegaOverFour, OneMinusOmega}.
-else.
compute_omega(Omega) ->
    OmegaOverFour = ftimes(0.25, Omega),
    OneMinusOmega = fminus(1.0, Omega),
    {OmegaOverFour, OneMinusOmega}.
-endif.

sor_actor(Pos, Value, Color, Nx, Ny, Omega, SorSource, _) ->
    CalPos = fun(X1, Y1) -> plus(times(X1, Ny), Y1) end,
    X = divide(Pos, Ny),
    Y = modulo(Pos, Ny),
    {OmegaOverFour, OneMinusOmega} = compute_omega(Omega),
    Neighbors = case (X > 0) and (X < Nx-1) and (Y > 0) and (Y < Ny-1) of
                    true -> [CalPos(X, Y+1),CalPos(X+1,Y),CalPos(X,Y-1),CalPos(X-1,Y)];
                    false -> case ((X == 0) or (X == Nx-1)) and ((Y == 0) or (Y == Ny-1)) of
                                 true -> [case X == 0 of
                                              true -> CalPos(X+1, Y);
                                              false -> CalPos(X-1, Y)
                                          end,
                                          case Y == 0 of
                                              true -> CalPos(X, Y+1);
                                              false -> CalPos(X, Y-1)
                                          end];
                                 false -> case ((X == 0) or (X == Nx-1)) or ((Y == 0) or (Y == Ny-1)) of
                                              true ->
                                                  case (X == 0) or (X == Nx-1) of
                                                      true -> [case X == 0 of
                                                                   true -> CalPos(X+1, Y);
                                                                   false -> CalPos(X-1, Y)
                                                               end,
                                                               CalPos(X, Y+1), CalPos(X, Y-1)];
                                                      false -> [case Y == 0 of
                                                                    true -> CalPos(X, Y+1);
                                                                    false -> CalPos(X, Y-1)
                                                                end,
                                                                CalPos(X+1, Y), CalPos(X-1, Y)]
                                                  end;
                                              false -> []
                                          end
                             end
                end,
    Actor = fun(Actor, Value, Iter, MaxIter, MsgRcv, SorActors, ReceivedVals, Sum, ExpectingStart, PendingMessages) ->
                    receive
                        {start, Mi, MActors} ->
                            case Color == 1 of
                                true ->
                                    list_foreach(fun(LoopNeighIndex) ->
                                                         Receiver = list_ref(LoopNeighIndex, MActors),
                                                         case Receiver of
                                                             false -> skip;
                                                             _ -> Receiver ! {value, Value}
                                                         end
                                                 end, Neighbors);
                                false -> nothing
                            end,
                            list_foreach(fun(V) -> self() ! {sor_value, V} end, PendingMessages),
                            Actor(Actor, Value, case Color == 1 of
                                                    true -> Iter+1;
                                                    false -> Iter
                                                end, Mi,
                                  case Color == 1 of
                                      true -> MsgRcv+1;
                                      false -> MsgRcv
                                  end,
                                  MActors, ReceivedVals, Sum, false, []);
                        {sor_value, V} ->
                            case ExpectingStart of
                                true ->
                                    Actor(Actor, Value, Iter, MaxIter, MsgRcv, SorActors, ReceivedVals, Sum, ExpectingStart, [V|PendingMessages]);
                                false ->
                                    case Iter < MaxIter of
                                        true ->
                                            case ReceivedVals+1 == list_length(Neighbors) of
                                                true ->
                                                    list_foreach(fun(LoopNeighIndex) -> list_ref(LoopNeighIndex, SorActors) ! {value, Value} end, Neighbors),
                                                    case Iter+1 == MaxIter of
                                                        true -> SorSource ! {result, X, Y, Value, MsgRcv}, io:format("sor_actor done~n"), done;
                                                        false -> Actor(Actor, fplus(ftimes(OmegaOverFour, Sum), ftimes(OneMinusOmega, Value)), Iter+1,
                                                                       MaxIter, MsgRcv+1, SorActors, ReceivedVals+1, plus(Sum, V), ExpectingStart, PendingMessages)
                                                    end;
                                                false -> Actor(Actor, Value, Iter, MaxIter, MsgRcv+1, SorActors, ReceivedVals+1, plus(Sum, V), ExpectingStart, PendingMessages)
                                            end;
                                        false -> io:format("sor_actor done~n"), done
                                    end
                            end
                    end
            end,
    spawn(fun() -> Actor(Actor, Value, 0, 0, 0, false, 0, 0, true, []) end).

sor_peer(S, PartStart, MatrixPart, Border, SorSource) ->
    Boot = fun(Self) ->
                   Loop1 = fun(Loop1, SorActors, I) ->
                                   case I == S of
                                       true -> SorActors;
                                       false -> Loop1(Loop1, list_set(SorActors, times(I, minus(S, PartStart)+1), list_ref(I, Border)), I+1)
                                   end
                           end,
                   Loop2I = fun(Loop2I, SorActors, MyBorder, I) ->
                                    case I == S of
                                        true -> {SorActors, MyBorder};
                                        false ->
                                            Loop2J = fun(Loop2J, SorActors2, MyBorder2, C, J) ->
                                                             case J == minus(S, PartStart)+1 of
                                                                 true -> {SorActors2, MyBorder2};
                                                                 false ->
                                                                     Pos = plus(times(I, minus(S, PartStart)+1), J),
                                                                     CNew = minus(1, C),
                                                                     SorActorsNew = list_set(SorActors2, Pos,
                                                                                             sor_actor(Pos, list2_ref(MatrixPart, I, J-1), CNew, S,
                                                                                                       minus(S, PartStart)+1, ?Omega, Self, true)),
                                                                     MyBorderNew = case J == 1 of
                                                                                       true -> list_set(MyBorder2, I, list_ref(Pos, SorActors));
                                                                                       false -> MyBorder2
                                                                                   end,
                                                                     Loop2J(Loop2J, SorActorsNew, MyBorderNew, CNew, J+1)
                                                             end
                                                     end,
                                            {SorActorsNew, MyBorderNew} = Loop2J(Loop2J, SorActors, MyBorder, modulo(plus(I, PartStart), 2), 1),
                                            Loop2I(Loop2I, SorActorsNew, MyBorderNew, I+1)
                                    end
                            end,
                   Loop3I = fun(Loop3I, SorActors, I) ->
                                    case I == S of
                                        true -> true;
                                        false ->
                                            Loop3J = fun(Loop3J, J) ->
                                                             case J == minus(S, PartStart)+1 of
                                                                 true -> true;
                                                                 false ->
                                                                     Pos = plus(times(I, minus(S, PartStart)+1), J),
                                                                     list_ref(Pos, SorActors) ! {start, ?JacobiNumIter, SorActors},
                                                                     Loop3J(Loop3J, J+1)
                                                             end
                                                     end,
                                            Loop3J(Loop3J, 1),
                                            Loop3I(Loop3I, SorActors, I+1)
                                    end
                            end,
                   SorActorsNew = Loop1(Loop1, build_list(times(S, minus(S, PartStart)+1), fun(_) -> false end), 0),
                   {SorActorsNew2, MyBorderNew} = Loop2I(Loop2I, SorActorsNew, build_list(S, fun(_) -> false end), 0),
                   SorSource ! {border, MyBorderNew},
                   Loop3I(Loop3I, SorActorsNew2, 0)
           end,
    Actor = fun(Actor, GTotal, Returned, TotalMsgRcv, ExpectingBoot) ->
                    receive
                        {boot} -> Boot(self()),
                                  Actor(Actor, GTotal, Returned, TotalMsgRcv, false);
                        {result, _, _, Mv, MsgRcv} ->
                            case ExpectingBoot of
                                true -> error("not booted yet");
                                false -> nothing
                            end,
                            case Returned+1 == times(S, minus(S, PartStart)) of
                                true -> SorSource ! {result, -1, -1, GTotal, TotalMsgRcv}, io:format("sor_peer done~n"), done;
                                false -> Actor(Actor, plus(GTotal, Mv), Returned+1, plus(TotalMsgRcv, MsgRcv), ExpectingBoot)
                            end
                    end
            end,
    spawn(fun() -> Actor(Actor, 0, 0, 0, true) end).

main() ->
    SorRunner = sor_runner_actor(?N),
    SorRunner ! {boot}.
