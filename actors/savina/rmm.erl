%-module(rmm).
%-export([main/0]).
%-define(NumWorkers, 2).
%-define(DataLength, 5).
%-define(NumBlocks, 25).
%-define(BlockThreshold, 5).

-soter_config(peano).
-define(NumWorkers, ?any_nat()).
-define(DataLength, ?any_nat()).
-define(NumBlocks, ?any_nat()).
-define(BlockThreshold, ?any_nat()).

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list2_ref(L, I, J) ->
    list_ref(J, list_ref(I, L)).

list_set([], _, _) -> [];
list_set([_ | Xs], 0, Y) -> [Y|Xs];
list_set([X | Xs], I, Y) -> [X | list_set(Xs, I-1, Y)].

list2_set([],     0, _, _) -> error;
list2_set([X|Xs], 0, J, Y) -> [list_set(X, J, Y)|Xs];
list2_set([X|Xs], I, J, Y) -> [X|list2_set(Xs, I-1, J, Y)].

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

divide(X, Y) ->
    case X < Y of
        true -> 0;
        false -> divide(minus(X,Y),Y)+1
    end.

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

master_actor(ResultActor, Workers, NumWorkersTerminated, NumWorkSent, NumWorkCompleted) ->
    receive
        {start} ->
            io:format("[master] start~n"),
            Master = self(),
            Workers2 = build_list(?NumWorkers, fun(I) -> spawn(fun() -> worker_actor(ResultActor, Master, I) end) end),
            list_ref(0, Workers2) ! {work, 0, 0, 0, 0, 0, 0, ?NumBlocks, ?DataLength, 0},
            master_actor(ResultActor, Workers2, NumWorkersTerminated, NumWorkSent, NumWorkCompleted);
        {work, SrA, ScA, SrB, ScB, SrC, ScC, NumBlocks, Dim, Priority} ->
            io:format("[master] work~n"),
            Index = modulo(plus(SrC, ScC), ?NumWorkers),
            list_ref(Index, Workers) ! {work, SrA, ScA, SrB, ScB, SrC, ScC, NumBlocks, Dim, Priority},
            master_actor(ResultActor, Workers, NumWorkersTerminated, NumWorkSent+1, NumWorkCompleted);
        {done} ->
            io:format("[master] done~n"),
            case NumWorkCompleted+1 == NumWorkSent of
                true -> list_foreach(fun(A) -> A ! {stop} end, Workers);
                false -> nothing
            end,
            master_actor(ResultActor, Workers, NumWorkersTerminated, NumWorkSent, NumWorkCompleted+1);
        {stop} ->
            io:format("[master] stop~n"),
            case NumWorkersTerminated+1 == ?NumWorkers of
                true ->
                    ResultActor ! {stop},
                    done;
                false ->
                    master_actor(ResultActor, Workers, NumWorkersTerminated+1, NumWorkSent, NumWorkCompleted)
            end
    end.

a(I, _) -> I.
b(_, J) -> J.

result_actor(C) ->
    receive
        {add, I, J, V} ->
            io:format("[result] add~n"),
            result_actor(list2_set(C, J, I, plus(list2_ref(C, J, I), V)));
        {stop} -> done
    end.

worker_actor(ResultActor, Master, Id) ->
    receive
        {work, SrA, ScA, SrB, ScB, SrC, ScC, NumBlocks, Dim, Priority} ->
            io:format("[worker] work~n"),
            case NumBlocks > ?BlockThreshold of
                true ->
                    NewDim = divide(Dim, 2),
                    NewNumBlocks = divide(NumBlocks, 4),
                    NewPriority = Priority+1,
                    Master ! {work, SrA, ScA, SrB, ScB, SrC, ScC, NewNumBlocks, NewDim, NewPriority},
                    Master ! {work, SrA, plus(ScA,NewDim), plus(SrB, NewDim), ScB, SrC, ScC, NewNumBlocks, NewDim, NewPriority},
                    Master ! {work, SrA, ScA, SrB, plus(ScB, NewDim), SrC, plus(ScC, NewDim), NewNumBlocks, NewDim, NewPriority},
                    Master ! {work, SrA, plus(ScA, NewDim), plus(SrB, NewDim), plus(ScB, NewDim), SrC, plus(ScC, NewDim), NewNumBlocks, NewDim, NewPriority},
                    Master ! {work, plus(SrA, NewDim), ScA, SrB, ScB, plus(SrC, NewDim), ScC, NewNumBlocks, NewDim, NewPriority},
                    Master ! {work, plus(SrA, NewDim), plus(ScA, NewDim), plus(SrB, NewDim), ScB, plus(SrC, NewDim), ScC, NewNumBlocks, NewPriority},
                    Master ! {work, plus(SrA, NewDim), ScA, SrB, plus(ScB, NewDim), plus(SrC, NewDim), plus(ScC, NewDim), NewNumBlocks, NewPriority},
                    Master ! {work, plus(SrA, NewDim), plus(ScA, NewDim), plus(SrB, NewDim), plus(ScB, NewDim), plus(SrC, NewDim), plus(ScC, NewDim), NewNumBlocks, NewDim, NewPriority};
                false ->
                    EndR = plus(SrC, Dim),
                    EndC = plus(ScC, Dim),
                    LoopI = fun(LoopI, I) ->
                                    case I == EndR of
                                        true -> true;
                                        false ->
                                            LoopJ = fun(LoopJ, J) ->
                                                            case J == EndC of
                                                                true -> LoopI(LoopI, I+1);
                                                                false ->
                                                                    LoopK = fun(LoopK, K) ->
                                                                                    case K == Dim of
                                                                                        true -> LoopJ(LoopJ, J+1);
                                                                                        false ->
                                                                                            R = plus(a(I, plus(ScA, K)), b(plus(SrB, K), J)),
                                                                                            ResultActor ! {add, I, J, R},
                                                                                            LoopK(LoopK, K+1)
                                                                                    end
                                                                            end,
                                                                    LoopK(LoopK, 0)
                                                            end
                                                    end,
                                            LoopJ(LoopJ, ScC)
                                    end
                            end,
                    LoopI(LoopI, SrC)
            end,
            Master ! {done},
            worker_actor(ResultActor, Master, Id);
        {stop} ->
            io:format("[worker] stop~n"),
            Master ! {stop},
            done
    end.

main() ->
    ResultActor = spawn(fun() -> result_actor(build_list(?DataLength, fun(_) -> build_list(?DataLength, fun(_) -> 0 end) end)) end),
    Master = spawn(fun() -> master_actor(ResultActor, false, 0, 0, 0) end),
    Master ! {start}.
