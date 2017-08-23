-ifdef(SOTER).
-soter_config(peano).
-define(NumWorkers, ?any_nat()).
-define(Tolerance, ?any_nat()).
-define(Precision, ?any_nat()).
-define(PointOne, ?any_nat()).
-else.
-module(piprec).
-export([main/0]).
-define(NumWorkers, 10).
-define(Tolerance, 0.1).
-define(PointOne, 0.1).
-define(Precision, 5).
-endif.

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

list_ref(0, []) -> error(err);
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

mmin(X, Y) ->
    case X < Y of
        true -> X;
        false -> Y
    end.
-ifdef(SOTER).
expt(X, Y) -> X.
aabs(X) -> X.

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

-else.
expt(X, Y) -> math:pow(X, Y).
aabs(X) -> abs(X).
plus(X, Y) -> X+Y.
divide(X, Y) -> X/Y.
minus(X, Y) -> X-Y.
times(X, Y) -> X*Y.
-endif.

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.


calculate_bbp_term(_, K) ->
    EightK = times(8, K),
    Term1 = divide(4, EightK+1),
    Term2 = minus(Term1, divide(1, plus(EightK, 4))),
    Term3 = minus(Term2, divide(1, plus(EightK, 5))),
    Term4 = minus(Term3, divide(1, plus(EightK, 6))),
    Term5 = divide(Term4, expt(16, K)),
    Term5.

worker_actor(Master, Id, TermsProcessed) ->
    receive
        {stop} -> Master ! {stop},
                  done;
        {work, Scale, Term} ->
            Result = calculate_bbp_term(Scale, Term),
            Master ! {result, Result, Id},
            worker_actor(Master, Id, TermsProcessed+1)
    end.

master_actor(Workers, Result, NumWorkersTerminated, NumTermsRequested, NumTermsReceived, StopRequests) ->
    receive
        {result, R, Id} ->
            StopRequests2 = aabs(minus(R, ?Tolerance)) =< ?PointOne,
            NumTermsRequested2 = case StopRequests of
                                    true ->
                                        list_ref(Id, Workers) ! {work, ?Precision, NumTermsRequested},
                                        NumTermsRequested+1;
                                    false -> NumTermsRequested
                                end,
            case NumTermsRequested2 == NumTermsReceived of
                true -> list_foreach(fun(W) -> W ! {stop} end, Workers);
                false -> nothing
            end,
            master_actor(Workers, plus(Result, R), NumWorkersTerminated, NumTermsRequested2, NumTermsReceived+1, StopRequests2);
        {stop} ->
            case NumWorkersTerminated+1 == ?NumWorkers of
                true -> done;
                false -> master_actor(Workers, Result, NumWorkersTerminated+1, NumTermsRequested, NumTermsReceived, StopRequests)
            end;
        {start} ->
            Self = self(),
            Workers2 = build_list(?NumWorkers, fun(I) -> spawn(fun() -> worker_actor(Self, I, 0) end) end),
            Loop = fun(Loop, I) ->
                           case I == mmin(?Precision, times(10, ?NumWorkers)) of
                               true -> master_actor(Workers2, Result, NumWorkersTerminated, plus(NumTermsRequested, I), NumTermsReceived, StopRequests);
                               false ->
                                   list_ref(modulo(I, ?NumWorkers), Workers2) ! {work, ?Precision, plus(NumTermsRequested, I)},
                                   Loop(Loop, I+1)
                           end
                   end,
            Loop(Loop, 0)
    end.

main() ->
    Master = spawn(fun() -> master_actor(false, 0, 0, 0, 0, false) end),
    Master ! {start}.
