-ifdef(SOTER).
-soter_config(peano).
-define(NumWorkers, ?any_nat()).
-define(Precision, ?any_nat()).
-define(L, ?any_nat()).
-define(R, ?any_nat()).
-else.
-module(trapr).
-export([main/0]).
-define(NumWorkers, 2).
-define(Precision, 5).
-define(L, 5).
-define(R, 5).
-endif.

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).

list_ref(0, []) -> error(err);
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

-ifdef(SOTER).
expt(X, Y) -> X.
exp(X) -> X.
sin(X) -> X.
sqrt(X) -> X.
aabs(X) -> X.
-else.
expt(X, Y) -> math:pow(X, Y).
exp(X) -> expt(2.718281828459045, X).
sin(X) -> math:sin(X).
sqrt(X) -> math:sqrt(X).
aabs(X) -> abs(X).
-endif.

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

fx(X) ->
    A = sin(expt(X, 3)-1),
    B = X+1,
    C = divide(A, B),
    D = sqrt(exp(sqrt(times(2, X)))+1),
    R = times(C, D),
    R.

compute_area(L, R, H) ->
    N = divide(aabs(minus(R, L)), H),
    Loop = fun(Loop, I, Acc) ->
                   case I == N of
                       true -> Acc;
                       false ->
                           Lx = plus(times(I, H), L),
                           Rx = plus(Lx, H),
                           Ly = fx(Lx),
                           Ry = fx(Rx),
                           Loop(Loop, I+1, plus(Acc, times(divide(1, 2), times(plus(Ly, Ry), H))))
                   end
           end,
    Loop(Loop, 0, 0).

master_actor(Workers, TermsReceived, ResultArea) ->
    receive
        {result, V, _} ->
            case TermsReceived+1 == ?NumWorkers of
                true -> io:format("done, result: ~w~n", [ResultArea]), done;
                false -> master_actor(Workers, TermsReceived+1, plus(ResultArea, V))
            end;
        {work, L, R, H} ->
            Range = divide(minus(R, L), ?NumWorkers),
            Loop = fun(Loop, I) ->
                           case I == ?NumWorkers of
                               true -> master_actor(Workers, TermsReceived, ResultArea);
                               false ->
                                   Wl = plus(times(Range, I), L),
                                   Wr = plus(Wl, Range),
                                   list_ref(I, Workers) ! {work, Wl, Wr, H},
                                   Loop(Loop, I+1)
                           end
                   end,
            Loop(Loop, 0)
    end.

master_actor_init() ->
    receive
        {start} ->
            Master = self(),
            Workers = build_list(?NumWorkers, fun(I) -> spawn(fun() -> worker_actor(Master, I) end) end),
            self() ! {work, ?L, ?R, ?Precision},
            master_actor(Workers, 0, 0)
    end.

worker_actor(Master, Id) ->
    receive
        {work, L, R, H} ->
            Area = compute_area(L, R, H),
            Master ! {result, Area, Id},
            done
    end.

main() ->
    Master = spawn(fun() -> master_actor_init() end),
    Master ! {start}.
