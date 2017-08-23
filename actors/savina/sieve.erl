-ifdef(SOTER).
-soter_config(peano).
-define(Limit, ?any_nat()).
-define(NumMaxLocalPrimes, ?any_nat()).
-else.
-module(sieve).
-export([main/0]).
-define(Limit, 250).
-define(NumMaxLocalPrimes, 10).
-endif.

-ifdef(SOTER).
random_bool() -> ?any_bool().
-else.
random_bool() ->
    rand:uniform(2) == 1.
-endif.

locally_prime(_) ->
    random_bool().

loop(Actor, N) ->
    case N >= ?Limit  of
        true ->
            Actor ! {exit},
            io:format("done generating~n");
        false ->
            Actor ! {candidate, N},
            loop(Actor, N+2)
    end.

number_producer_actor() ->
    receive
        {primefilter, Actor} ->
            loop(Actor, 3)
    end.

prime_filter_actor(Id, Initial, Next, LocalPrimes, AvailableLocalPrimes) ->
    receive
        {candidate, Candidate} ->
            case locally_prime(Candidate) of
                true -> case Next of
                            false -> case AvailableLocalPrimes == ?NumMaxLocalPrimes of
                                         false ->
                                             prime_filter_actor(Id, Initial, Next, [Candidate|LocalPrimes], AvailableLocalPrimes+1);
                                         true ->
                                             NewNext = spawn(fun() -> prime_filter_actor(Id+1, Candidate, false, [Candidate], 1) end),
                                             prime_filter_actor(Id, Initial, NewNext, LocalPrimes, AvailableLocalPrimes)
                                     end;
                            Actor ->
                                Actor ! {candidate, Candidate},
                                prime_filter_actor(Id, Initial, Next, LocalPrimes, AvailableLocalPrimes)
                        end;
                false -> prime_filter_actor(Id, Initial, Next, LocalPrimes, AvailableLocalPrimes)
            end;
        {exit} ->
            case Next of
                false -> done;
                Actor -> Actor ! {exit}
            end,
            io:format("done prime filter ~w~n", [Id])
    end.

main() ->
    Producer = spawn(fun() -> number_producer_actor() end),
    Filter = spawn(fun() -> prime_filter_actor(1, 2, false, [2], 1) end),
    Producer ! {primefilter, Filter}.
