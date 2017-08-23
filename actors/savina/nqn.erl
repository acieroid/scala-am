-ifdef(SOTER).
-soter_config(peano).
-define(NumWorkers, ?any_nat()).
-define(Priorities, ?any_nat()).
-define(Threshold, ?any_nat()).
-define(Size, ?any_nat()).
-define(SolutionsLimit, ?any_nat()).
-else.
-module(nqn).
-export([main/0]).
-define(NumWorkers, 5).
-define(Priorities, 5).
-define(Threshold, 5).
-define(Size, 5).
-define(SolutionsLimit, 3).
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

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

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

master_actor(Workers, ResultCounter, MessageCounter, NumWorkersTerminated, NumWorkSent, NumWorkCompleted) ->
    receive
        {start} ->
            Master = self(),
            Workers2 = build_list(?NumWorkers, fun(I) -> spawn(fun() -> worker_actor(Master, I) end) end),
            list_ref(MessageCounter, Workers2) ! {work, [], 0},
            master_actor(Workers2, ResultCounter, modulo(MessageCounter+1, ?NumWorkers), NumWorkersTerminated, NumWorkSent+1, NumWorkCompleted);
        {work, Data, Depth} ->
            list_ref(MessageCounter, Workers) ! {work, Data, Depth},
            master_actor(Workers, ResultCounter, modulo(MessageCounter+1, ?NumWorkers), NumWorkersTerminated, NumWorkSent+1, NumWorkCompleted);
        {result} ->
            case ResultCounter+1 == ?SolutionsLimit of
                true -> list_foreach(fun(W) -> W ! {stop} end, Workers);
                false -> nothing
            end,
            master_actor(Workers, ResultCounter+1, MessageCounter, NumWorkersTerminated, NumWorkSent, NumWorkCompleted);
        {done} ->
            case NumWorkCompleted+1 == NumWorkSent of
                true -> list_foreach(fun(W) -> W ! {stop} end, Workers);
                false -> nothing
            end,
            master_actor(Workers, ResultCounter, MessageCounter, NumWorkersTerminated, NumWorkSent, NumWorkCompleted+1);
        {stop} ->
            case NumWorkersTerminated+1 == ?NumWorkers of
                true -> io:format("done~n"), done;
                false -> master_actor(Workers, ResultCounter, MessageCounter, NumWorkersTerminated+1, NumWorkSent, NumWorkCompleted)
            end
    end.

copy_append_i([], I) -> [I];
copy_append_i([X|Xs], I) -> [X|copy_append_i(Xs, I)].

valid_board_loop_j(N, A, I, J) ->
    case J == N of
        true -> true;
        false ->
            P = list_ref(I, A),
            Q = list_ref(J, A),
            case Q == P of
                true -> false;
                false -> case Q == minus(P, minus(J, I)) of
                             true -> false;
                             false -> case Q == plus(P, minus(J, I)) of
                                          true -> false;
                                          false -> valid_board_loop_j(N, A, I, J+1)
                                      end
                         end
            end
    end.
valid_board_loop_i(N, A, I) ->
    case I == N of
        true -> true;
        false -> case valid_board_loop_j(N, A, I, I+1) of
                     true -> valid_board_loop_i(N, A, I+1);
                     false -> false
                 end
    end.
valid_board(N, A) -> valid_board_loop_i(N, A, 0).

nqueens_seq_loop(Data, Depth, Master, I) ->
    case I == ?Size of
        true -> true;
        false ->
            Data2 = copy_append_i(Data, I),
            case valid_board(Depth+1, Data2) of
                true -> nqueens_seq(Data2, Depth+1, Master);
                false -> nothing
            end,
            nqueens_seq_loop(Data, Depth, Master, I+1)
    end.
nqueens_seq(Data, Depth, Master) ->
    case Depth == ?Size of
        true -> Master ! {result};
        false -> nqueens_seq_loop(Data, Depth, Master, 0)
    end.

worker_loop(Data, Depth, Master, I) ->
    case I == ?Size of
        true -> true;
        false ->
            Data2 = copy_append_i(Data, I),
            case valid_board(Depth+1, Data2) of
                true -> Master ! {work, Data2, Depth+1};
                false -> nothing
            end,
            worker_loop(Data, Depth, Master, I+1)
    end.
worker_actor(Master, Id) ->
    receive
        {work, Data, Depth} ->
            case Depth == ?Size of
                true -> Master ! {result};
                false ->
                    case Depth >= ?Threshold of
                        true -> nqueens_seq(Data, Depth, Master);
                        false -> worker_loop(Data, Depth, Master, 0)
                    end
                end,
            Master ! {done},
            worker_actor(Master, Id);
        {stop} ->
            Master ! {stop},
            done
    end.

main() ->
    Master = spawn(fun() -> master_actor(false, 0, 0, 0, 0, 0) end),
    Master ! {start}.
