-ifdef(SOTER).
-soter_config(peano).
-define(NumWorkers, ?any_nat()).
-define(Threshold, ?any_nat()).
-define(MaxNeighbors, ?any_nat()).
-define(Origin, 0).
-define(Target, 10).
-else.
-module(astar).
-export([main/0]).
-define(NumWorkers, 2).
-define(Threshold, 5).
-define(MaxNeighbors, 5).
-define(Origin, 0).
-define(Target, 10).
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_append([], Ys) -> Ys;
list_append([X | Xs], Ys) -> [X | list_append(Xs, Ys)].

list_member(_, []) -> false;
list_member(Element, [X | Xs]) ->
    case Element == X of
        true -> true;
        false -> list_member(Element, Xs)
    end.

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).

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

master_actor(Workers, NumWorkersTerminated, NumWorkSent, NumWorkCompleted) ->
    receive
        {work, Target, Node} ->
            list_ref(modulo(NumWorkSent, ?NumWorkers), Workers) ! {work, Target, Node},
            master_actor(Workers, NumWorkersTerminated, NumWorkSent+1, NumWorkCompleted);
        {received} ->
            case NumWorkCompleted+1 == NumWorkSent of
                true -> list_foreach(fun(A) -> A ! {stop} end, Workers);
                false -> nothing
            end,
            master_actor(Workers, NumWorkersTerminated, NumWorkSent, NumWorkCompleted+1);
        {done} ->
            list_foreach(fun(A) -> A ! {stop} end, Workers),
            master_actor(Workers, NumWorkersTerminated, NumWorkSent, NumWorkCompleted);
        {stop} ->
            case NumWorkersTerminated+1 == ?NumWorkers of
                true -> io:format("finished~n"), done;
                false -> master_actor(Workers, NumWorkersTerminated+1, NumWorkSent, NumWorkCompleted)
            end
    end.

neighbors_loop(I, Acc) ->
    case I == ?MaxNeighbors of
        true -> Acc;
        false -> neighbors_loop(I+1, [random(1000)|Acc])
    end.
%neighbors(0) -> [1];
%neighbors(1) -> [2, 3];
%neighbors(2) -> [3, 1];
%neighbors(3) -> [5, 6];
%neighbors(4) -> [5];
%neighbors(5) -> [4, 7, 8, 9];
%neighbors(6) -> [7];
%neighbors(7) -> [5];
%neighbors(8) -> [5];
%neighbors(9) -> [10];
%neighbors(10) -> [];
%neighbors(_) -> [].
neighbors(_) ->
    neighbors_loop(0, []).

worker_loop(_, _, [], _) -> done;
worker_loop(Master, Target, [LoopNode|LoopRest], NodesProcessed) ->
    case NodesProcessed == ?Threshold of
        true ->
            list_foreach(fun(Node) -> Master ! {work, Target, Node} end, [LoopNode|LoopRest]),
            worker_loop(Master, Target, [], NodesProcessed);
        false ->
            Neighbors = neighbors(LoopNode),
            case list_member(Target, Neighbors) of
                     true -> Master ! {done};
                     false -> worker_loop(Master, Target, list_append(LoopRest, Neighbors), NodesProcessed+1)
            end
    end.

worker_actor(Master, Id) ->
    receive
        {work, Target, Node} ->
            worker_loop(Master, Target, [Node], 0),
            Master ! {received},
            worker_actor(Master, Id);
        {stop} ->
            Master ! {stop}
    end.

master_actor_init() ->
    receive
        {start} ->
            Master = self(),
            master_actor(build_list(?NumWorkers,
                                    fun(I) ->
                                            W = spawn(fun() -> worker_actor(Master, I) end),
                                            case I == 0 of
                                                true -> W ! {work, ?Target, ?Origin};
                                                false -> nothing
                                            end,
                                            W
                                    end),
                         0, 0, 0)
    end.

main() ->
    Master = spawn(fun() -> master_actor_init() end),
    Master ! {start}.
