%-module(logm).
%-export([main/0]).
%-define(NumSeries, 5).
%-define(NumComputers, ?NumSeries).
%-define(NumWorkers, ?NumSeries).
%-define(StartRate, 5).
%-define(Increment, 2).

-soter_config(peano).
-define(NumSeries, ?any_nat()).
-define(NumComputers, ?NumSeries).
-define(NumWorkers, ?NumSeries).
-define(StartRate, ?any_nat()).
-define(Increment, ?any_nat()).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).


master(Computers, Workers, NumWorkRequested, NumWorkReceived, TermsSum) ->
    receive
        {start} ->
            Computers2 = build_list(?NumComputers, fun (I) ->
                                                          Rate = plus(?StartRate, (times(I,?Increment))),
                                                          spawn(fun() -> rate_computer(Rate) end)
                                                  end),
            Workers2 = build_list(?NumWorkers, fun (I) ->
                                                       RateComputer = list_ref(modulo(I, ?NumComputers), Computers2),
                                                       StartTerm = times(I,?Increment),
                                                       Master = self(),
                                                       Actor = spawn(fun() -> series_worker(Master, RateComputer, StartTerm) end),
                                                       Actor ! {next_term},
                                                       Actor ! {get_term},
                                                       Actor
                                              end),
            master(Computers2, Workers2, ?NumWorkers, NumWorkReceived, TermsSum);
        {result, Term} ->
            case NumWorkReceived+1 == NumWorkRequested of
                true ->
                    list_foreach(fun (A) -> A ! {stop} end, Computers),
                    list_foreach(fun (A) -> A ! {stop} end, Workers),
                    % io:format("Finished with: ~w~n", [TermsSum]),
                    done;
                false ->
                    master(Computers, Workers, NumWorkRequested, NumWorkReceived+1, plus(TermsSum,Term))
            end
    end.

series_worker_wait(Master, Computer) ->
    receive
        {next_term} ->
            self() ! {next_term},
            series_worker_wait(Master, Computer);
        {result, Term} ->
            series_worker(Master, Computer, Term);
        {get_term} ->
            self() ! {get_term},
            series_worker_wait(Master, Computer);
        {stop} ->
            self() ! {stop},
            series_worker_wait(Master, Computer)
    end.

series_worker(Master, Computer, CurTerm) ->
    receive
        {next_term} ->
            Computer ! {compute, CurTerm, self()},
            series_worker_wait(Master, Computer);
        {result, Term} ->
            series_worker(Master, Computer, Term);
        {get_term} ->
            Master ! {result, CurTerm},
            series_worker(Master, Computer, CurTerm);
        {stop} ->
            done
    end.

compute_next_term(Cur, Rate) ->
    times(Rate,times(Cur,(Cur-1))).

rate_computer(Rate) ->
    receive
        {compute, Term, Sender} ->
            Sender ! {result, compute_next_term(Term, Rate)},
            rate_computer(rate);
        {stop} ->
            done
    end.

main() ->
    Master = spawn(fun() -> master(false, false, 0, 0, 0) end),
    Master ! {start}.
