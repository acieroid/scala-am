-ifdef(SOTER).
-soter_config(peano).
-define(BufferSize, ?any_nat()).
-define(NumProducers, ?any_nat()).
-define(NumConsumers, ?any_nat()).
-define(NumItemsPerProducer, ?any_nat()).
-define(ConsCost, 40).
-define(ProdCost, 40).
-define(AdjustedBufferSize, ?any_nat()).
-else.
-module(pcbb).
-export([main/0]).
-define(BufferSize, 5).
-define(NumProducers, 2).
-define(NumConsumers, 2).
-define(NumItemsPerProducer, 2).
-define(ConsCost, 40).
-define(ProdCost, 40).
-define(AdjustedBufferSize, ?BufferSize-?NumProducers).
-endif.

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

-ifdef(SOTER).
random(X) -> modulo(?any_nat(), X).
-else.
random(X) -> rand:uniform(X)-1.
-endif.

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

list_empty([]) -> true;
list_empty(_) -> false.

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

first([A|_]) -> A.

rest([_|A]) -> A.

andd(true, true) -> true;
andd(_, _) -> false.

spawn_producers_loop(Manager, I) ->
    case I == ?NumProducers of
        true -> [];
        false ->
            P = spawn(fun() -> producer(I, Manager, 0, 0) end),
            P ! {produce_data},
            [P | spawn_producers_loop(Manager, I+1)]
    end.

spawn_producers(Manager) ->
    spawn_producers_loop(Manager, 0).

spawn_consumers_loop(Manager, I) ->
    case I == ?NumConsumers of
        true -> [];
        false ->
            P = spawn(fun() -> consumer(I, Manager, 0) end),
            [P | spawn_consumers_loop(Manager, I+1)]
    end.

spawn_consumers(Manager) ->
    spawn_consumers_loop(Manager, 0).

manager(Producers, Consumers, AvailableProducers, AvailableConsumers, PendingData, TerminatedProducers) ->
    receive
        {start} ->
            io:format("[manager] start~n"),
            Producers2 = spawn_producers(self()),
            Consumers2 = spawn_consumers(self()),
            manager(Producers2, Consumers2, [], Consumers, PendingData, TerminatedProducers);
        {data_item, Producer, Data} ->
            io:format("[manager] data_item~n"),
            case list_empty(AvailableConsumers) of
                true ->
                    case ?AdjustedBufferSize < list_length(PendingData) of
                        true ->
                            manager(Producers, Consumers, [Producer|AvailableProducers], AvailableConsumers, [Data|PendingData], TerminatedProducers);
                        false ->
                            Producer ! {produce_data},
                            manager(Producers, Consumers, AvailableProducers, AvailableConsumers, [Data|PendingData], TerminatedProducers)
                    end;
                false ->
                    AvailableProducers = case ?AdjustedBufferSize < list_length(PendingData) of
                                             true -> [Producer|AvailableProducers];
                                             false ->
                                                 Producer ! {produce_data},
                                                 AvailableProducers
                                         end,
                    first(AvailableConsumers) ! {data_item, Data},
                    manager(Producers, Consumers, AvailableProducers, rest(AvailableConsumers), [Data|PendingData], TerminatedProducers)
            end;
        {consumer_available , Consumer} ->
            io:format("[manager] consumer_available~n"),
            case list_empty(PendingData) of
                true -> case TerminatedProducers == ?NumProducers of
                            true ->
                                case list_length(AvailableConsumers)+1 == ?NumConsumers of
                                    true ->
                                        list_foreach(fun(A) -> A ! {exit} end, Consumers),
                                        done;
                                    false ->
                                        manager(Producers, Consumers, AvailableProducers, [Consumer|AvailableConsumers], PendingData, TerminatedProducers)
                                end;
                            false -> manager(Producers, Consumers, AvailableProducers, [Consumer|AvailableConsumers], PendingData, TerminatedProducers)
                        end;
                false ->
                    Consumer ! {data_item, first(PendingData)},
                    case list_empty(AvailableProducers) of
                        true ->
                            first(AvailableProducers) ! {produce_data},
                            manager(Producers, Consumers, rest(AvailableProducers), AvailableConsumers, rest(PendingData), TerminatedProducers);
                        false -> manager(Producers, Consumers, AvailableProducers, AvailableConsumers, rest(PendingData), TerminatedProducers)
                    end
            end;
        {exit} ->
            io:format("[manager] exit"),
            case TerminatedProducers+1 == ?NumProducers of
                true -> case list_length(AvailableConsumers) == ?NumConsumers of
                            true ->
                                list_foreach(fun(A) -> A ! {exit} end, Consumers),
                                done;
                            false ->
                                manager(Producers, Consumers, AvailableProducers, AvailableConsumers, PendingData, TerminatedProducers+1)
                        end;
                false -> manager(Producers, Consumers, AvailableProducers, AvailableConsumers, PendingData, TerminatedProducers+1)
            end
    end.

process_item(_, _) ->
    random(10).

producer(Id, Manager, ItemsProduced, ProdItem) ->
    receive
        {produce_data} ->
            io:format("[producer] << produce_data~n"),
            case ItemsProduced == ?NumItemsPerProducer of
                true ->
                    Manager ! {exit},
                    done;
                false ->
                    ProdItem2 = process_item(ProdItem, ?ProdCost),
                    Manager ! {data_item, self(), ProdItem},
                    producer(Id, Manager, ItemsProduced+1, ProdItem2)
            end
    end.

consumer(Id, Manager, ConsItem) ->
    receive
        {data_item, Data} ->
            io:format("[consumer] << data_item~n"),
            ConsItem2 = process_item(plus(ConsItem, Data), ?ConsCost),
            Manager ! {consumer_available, self()},
            consumer(Id, Manager, ConsItem2);
        {exit} ->
            io:format("[consumer] << exit"),
            done
    end.

main() ->
    M = spawn(fun() -> manager([], [], [], [], [], 0) end),
    M ! {start}.
