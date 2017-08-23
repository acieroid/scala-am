-ifdef(SOTER).
-soter_config(peano).
-define(NWorkers, ?any_nat()).
-define(NMsgsPerWorker, ?any_nat()).
-define(WritePercent, ?any_nat()).
-else.
-module(cdict).
-export([main/0]).
-define(NWorkers, 5).
-define(NMsgsPerWorker, 5).
-define(WritePercent, 50).
-endif.

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

-ifdef(SOTER).
minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) -> modulo(?any_nat(), X).
-else.
random(X) -> rand:uniform(X)-1.
-endif.

create_workers(Master, Dictionary, I) ->
    case I == ?NWorkers of
        true -> [];
        false -> [spawn(fun() -> worker(Master, Dictionary, I, 0) end) | create_workers(Master, Dictionary, I+1) ]
    end.

master(Workers, Dictionary, Terminated) ->
    receive
        {createworkers} ->
            NewWorkers = create_workers(self(), Dictionary, 0),
            list_foreach(fun(W) -> W ! {dowork} end, NewWorkers),
            master(Workers, Dictionary, Terminated);
        {endwork} ->
            case (Terminated+1) == ?NWorkers of
                true ->
                    io:format("done!~n"),
                    Dictionary ! {endwork}, done;
                false -> master(Workers, Dictionary, Terminated+1)
            end
    end.

send_work(Master, Dictionary, Id, MessageCount) ->
    AnInt = random(100),
    case AnInt < ?WritePercent of
        true -> Dictionary ! {write, self(), random(100), random(100)};
        false -> Dictionary ! {read, self(), random(100)}
    end,
    worker(Master, Dictionary, Id, MessageCount+1).

worker(Master, Dictionary, Id, MessageCount) ->
    receive
        {dowork} -> send_work(Master, Dictionary, Id, MessageCount);
        {result, _} ->
            case (MessageCount+1) == ?NMsgsPerWorker of
                false -> send_work(Master, Dictionary, Id, MessageCount);
                true -> Master ! {endwork}, done
            end
    end.

alist_get(_, [], Default) -> Default;
alist_get(Key, [{Key1, Val1} | Rest], Default) ->
    case Key == Key1 of
        true -> Val1;
        false -> alist_get(Key, Rest, Default)
    end.

dictionary(State) ->
    receive
        {write, Sender, Key, Value} ->
            Sender ! {result, Value},
            dictionary([{Key, Value} | State]);
        {read, Sender, Key} ->
            Value = alist_get(Key, State, 0),
            Sender ! {result, Value},
            dictionary(State)
    end.

main() ->
    Dictionary = spawn(fun() -> dictionary([]) end),
    Master = spawn(fun() -> master(false, Dictionary, 0) end),
    Master ! {createworkers}.
