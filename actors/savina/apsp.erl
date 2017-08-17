% -module(apsp).
% -export([main/0]).
% -define(NumNodes, 30).
% -define(BlockSize, 5).
% -define(W, 10).
-soter_config(peano).
-define(NumNodes, ?any_nat()).
-define(BlockSize, ?any_nat()).
-define(W, ?any_nat()).

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).

list_reverse([], Res) -> Res;
list_reverse([X | Xs], Res) -> list_reverse(Xs, [X | Res]).

list_reverse(L) -> list_reverse(L, []).

list_ref(0, []) -> error(err);
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

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

list_assoc(_, []) -> error(err);
list_assoc(Z, [[X|Y]|Xs]) ->
    case X == Z of
        true -> Y;
        false -> list_assoc(Z, Xs)
    end.

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

divide(X, Y) ->
    case X < Y of
        true -> 0;
        false -> divide(minus(X,Y),Y)+1
    end.

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).


modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
    rand:uniform(X)-1.
%     modulo(?any_nat(), X).



generate_graph() ->
    LocalData = build_list(?NumNodes, fun(_) -> build_list(?NumNodes, fun(_) -> 0 end) end),
    GenLoopI = fun(GenLoopI, I, Data) ->
                       case I == ?NumNodes of
                           true -> Data;
                           false ->
                               GenLoopJ = fun(GenLoopJ, J, Data2) ->
                                                  case J == ?NumNodes of
                                                      true -> Data2;
                                                      false ->
                                                          R = random(?W)+1,
                                                          GenLoopJ(GenLoopJ, J+1, list2_set(list2_set(Data2, I, J, R), J, I, R))
                                                  end
                                          end,
                               DataNew = GenLoopJ(GenLoopJ, I+1, Data),
                               GenLoopI(GenLoopI, I+1, DataNew)
                       end
               end,
    GenLoopI(GenLoopI, 0, LocalData).

get_block(Src, Id) ->
    LocalData = build_list(?BlockSize, fun(_) -> build_list(?BlockSize, fun(_) -> 0 end) end),
    NumBlocksPerDim = divide(?NumNodes, ?BlockSize),
    GlobalStartRow = times(divide(Id, NumBlocksPerDim), ?BlockSize),
    GlobalStartCol = times(modulo(Id, NumBlocksPerDim), ?BlockSize),
    GetLoopI = fun(GetLoopI, I, Data) ->
                       case I == ?BlockSize of
                           true -> Data;
                           false ->
                               GetLoopJ = fun(GetLoopJ, J, Data2) ->
                                                  case J == ?BlockSize of
                                                      true -> Data2;
                                                      false ->
                                                          GetLoopJ(GetLoopJ, J+1, list2_set(Data2, I, J,
                                                                                            list2_ref(Src, plus(I, GlobalStartRow), plus(J, GlobalStartCol))))
                                                  end
                                          end,
                               DataNew = GetLoopJ(GetLoopJ, 0, Data),
                               GetLoopI(GetLoopI, I+1, DataNew)
                       end
               end,
    GetLoopI(GetLoopI, 0, LocalData).

apsp_actor(Id, BlockSize, GraphSize, InitGraphData) ->
    NumBlocksInSingleDim = divide(GraphSize, BlockSize),
    NumNeighbors = times(2, minus(NumBlocksInSingleDim, 1)),
    RowOffset = times(divide(Id, NumBlocksInSingleDim), BlockSize),
    ColOffset = times(modulo(Id, NumBlocksInSingleDim), BlockSize),
    Actor = fun(Actor, Neighbors, K, NeighborDataPerIteration, ReceivedNeighbors, CurrentIterData) ->
                    receive
                        {result, _, SrcId, InitData} ->
                            io:format("[apsp] result~n"),
                            case ReceivedNeighbors of
                                true -> ok;
                                false -> error("not received neighbors yet")
                            end,
                            io:format("length(NeighborDataPerIteration) = ~w, NumNeighbors = ~w~n", [list_length(NeighborDataPerIteration), NumNeighbors]),
                            case list_length(NeighborDataPerIteration) == NumNeighbors-1 of
                                true ->
                                    K2 = K+1,
                                    CurrentIterData2 = build_list(BlockSize, fun(_) -> build_list(BlockSize, fun(_) -> 0 end) end),
                                    ElementAt = fun(Row, Col, _) ->
                                                        DestBlockId = plus(times(divide(Row, BlockSize), NumBlocksInSingleDim), divide(Col, BlockSize)),
                                                        LocalRow = modulo(Row, BlockSize),
                                                        LocalCol = modulo(Col, BlockSize),
                                                        case DestBlockId == Id of
                                                            true -> list2_ref(CurrentIterData, LocalRow, LocalCol);
                                                            false ->
                                                                BlockData = list_assoc(DestBlockId, NeighborDataPerIteration),
                                                                list2_ref(BlockData, LocalRow, LocalCol)
                                                        end
                                                end,
                                    ApspLoopI = fun(ApspLoopI, I, CurrentIterData3) ->
                                                        case I == BlockSize of
                                                            true -> CurrentIterData3;
                                                            false ->
                                                                ApspLoopJ = fun(ApspLoopJ, J, CurrentIterData4) ->
                                                                                    case J == BlockSize of
                                                                                        true -> CurrentIterData4;
                                                                                        false ->
                                                                                            Gi = plus(RowOffset, I),
                                                                                            Gj = plus(ColOffset, J),
                                                                                            El1 = ElementAt(Gi, K2, minus(K2, 1)),
                                                                                            El2 = ElementAt(K2, Gj, minus(K2, 1)),
                                                                                            NewIterData = plus(El1, El2),
                                                                                            ApspLoopJ(ApspLoopJ, J+1, list2_set(CurrentIterData3, I, J,
                                                                                                                                min(list2_ref(CurrentIterData, I, J),
                                                                                                                                    NewIterData)))
                                                                                    end
                                                                            end,
                                                                CurrentIterDataNew = ApspLoopJ(ApspLoopJ, 0, CurrentIterData3),
                                                                ApspLoopI(ApspLoopI, I+1, CurrentIterDataNew)
                                                        end
                                                end,
                                    CurrentIterDataNew = ApspLoopI(ApspLoopI, 0, CurrentIterData2),
                                    list_foreach(fun(LoopNeighbor) -> LoopNeighbor ! {result, K2, Id, CurrentIterDataNew} end, Neighbors),
                                    io:format("K2: ~w, GraphSize-1: ~w~n", [K2, GraphSize-1]),
                                    case K2 == GraphSize-1 of
                                        true -> io:format("done!~n"), done;
                                        false -> Actor(Actor, Neighbors, K2, [], ReceivedNeighbors, CurrentIterDataNew)
                                    end;
                                false -> Actor(Actor, Neighbors, K, [[SrcId|InitData]|NeighborDataPerIteration], ReceivedNeighbors, CurrentIterData)
                            end;
                        {initial} ->
                            io:format("[apsp] initial~n"),
                            list_foreach(fun(LoopNeighbor) -> LoopNeighbor ! {result, K, Id, CurrentIterData} end, Neighbors),
                            Actor(Actor, Neighbors, K, NeighborDataPerIteration, ReceivedNeighbors, CurrentIterData);
                        {neighbor, NewNeighbors} ->
                            io:format("[apsp] neighbor~n"),
                            Actor(Actor, NewNeighbors, K, NeighborDataPerIteration, true, CurrentIterData)
                    end
            end,
    spawn(fun() -> Actor(Actor, [], -1, [], false, get_block(InitGraphData, Id)) end).

main() ->
    NumBlocksInSingleDim = divide(?NumNodes, ?BlockSize),
    GraphData = generate_graph(),
    BlockActors = build_list(NumBlocksInSingleDim, fun(I) -> build_list(NumBlocksInSingleDim, fun(J) -> Id = plus(times(I, NumBlocksInSingleDim), J),
                                                                                                        apsp_actor(Id, ?BlockSize, ?NumNodes, GraphData)
                                                                                              end)
                                                   end),
    InitLoopI = fun(InitLoopI, Bi) ->
                         case Bi == NumBlocksInSingleDim of
                             true -> true;
                             false ->
                                 InitLoopJ = fun(InitLoopJ, Bj) ->
                                                     case Bj == NumBlocksInSingleDim of
                                                         true -> true;
                                                         false ->
                                                             NeighborsLoop1 = fun(NeighborsLoop1, R, Acc) ->
                                                                                      case R == NumBlocksInSingleDim of
                                                                                          true -> Acc;
                                                                                          false ->
                                                                                              case R == Bi of
                                                                                                  true -> NeighborsLoop1(NeighborsLoop1,R+1,Acc);
                                                                                                  false -> NeighborsLoop1(NeighborsLoop1,R+1,
                                                                                                                          [list2_ref(BlockActors,R,Bj)|Acc])
                                                                                              end
                                                                                      end
                                                                              end,
                                                             NeighborsLoop2 = fun(NeighborsLoop2, C, Acc) ->
                                                                                      case C == NumBlocksInSingleDim of
                                                                                          true -> Acc;
                                                                                          false ->
                                                                                              case C == Bj of
                                                                                                  true -> NeighborsLoop2(NeighborsLoop2,C+1,Acc);
                                                                                                  false -> NeighborsLoop2(NeighborsLoop2,C+1,
                                                                                                                          [list2_ref(BlockActors,Bi,C)|Acc])
                                                                                              end
                                                                                      end
                                                                              end,
                                                             Neighbors = list_reverse(NeighborsLoop2(NeighborsLoop2,0,
                                                                                                     NeighborsLoop1(NeighborsLoop1,0,[]))),
                                                             list2_ref(BlockActors, Bi, Bj) ! {neighbor, Neighbors},
                                                             InitLoopJ(InitLoopJ, Bj+1)
                                                     end
                                             end,
                                 InitLoopJ(InitLoopJ, 0),
                                 InitLoopI(InitLoopI, Bi+1)
                         end
                 end,
    InitLoopI(InitLoopI, 0),
    InitLoopI2 = fun(InitLoopI2, Bi) ->
                         case Bi == NumBlocksInSingleDim of
                             true -> true;
                             false ->
                                 InitLoopJ2 = fun(InitLoopJ2,Bj) ->
                                                       case Bj == NumBlocksInSingleDim of
                                                           true -> true;
                                                           false ->
                                                               list2_ref(BlockActors, Bi, Bj) ! {initial},
                                                               InitLoopJ2(InitLoopJ2, Bj+1)
                                                       end
                                               end,
                                 InitLoopJ2(InitLoopJ2, 0),
                                 InitLoopI2(InitLoopI2, Bi+1)
                         end
                 end,
    InitLoopI2(InitLoopI2, 0).
