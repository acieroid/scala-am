%-module(csll).
%-export([main/0]).
%-define(NWorkers, 5).
%-define(NMsgsPerWorker, 5).
%-define(WritePercent, 50).
%-define(SizePercent, 50).

-soter_config(peano).
-define(NWorkers, ?any_nat()).
-define(NMsgsPerWorker, ?any_nat()).
-define(WritePercent, ?any_nat()).
-define(SizePercent, ?any_nat()).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
    %rand:uniform(X)-1.
    modulo(?any_nat(), X).

create_workers(Master, SortedList, I) ->
    case I == ?NWorkers of
        true -> [];
        false -> [spawn(fun() -> worker(Master, SortedList, I, 0) end) | create_workers(Master, SortedList, I+1) ]
    end.

master(Workers, SortedList, Terminated) ->
    receive
        {createworkers} ->
            NewWorkers = create_workers(self(), SortedList, 0),
            list_foreach(fun(W) -> W ! {dowork} end, NewWorkers),
            master(Workers, SortedList, Terminated);
        {endwork} ->
            case (Terminated+1) == ?NWorkers of
                true ->
                    io:format("done!~n"),
                    SortedList ! {endwork}, done;
                false -> master(Workers, SortedList, Terminated+1)
            end
    end.

send_work(Master, Dictionary, Id, MessageCount) ->
    AnInt = random(100),
    case AnInt < ?SizePercent of
        true -> Dictionary ! {size, self()};
        false -> case AnInt < (?SizePercent + ?WritePercent) of
                     true -> Dictionary ! {write, self(), random(100)};
                     false -> Dictionary ! {contains, self(), random(100)}
                 end
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

list_member(_, []) -> false;
list_member(Element, [X | Xs]) ->
    case X == Xs of
        true -> true;
        false -> list_member(Element, Xs)
    end.

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

sortedlist(State) ->
    receive
        {size, Sender} ->
            Sender ! {result, list_length(State)},
            sortedlist(State);
        {write, Sender, Value} ->
            Sender ! {result, Value},
            sortedlist([Value | State]);
        {contains, Sender, Value} ->
            Sender ! {result, list_member(Value, State)},
            sortedlist(State)
    end.

main() ->
    SortedList = spawn(fun() -> sortedlist([]) end),
    Master = spawn(fun() -> master(false, SortedList, 0) end),
    Master ! {createworkers}.
