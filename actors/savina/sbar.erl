%-module(sbar).
%-export([main/0]).
%-define(Capacity, 5).
%-define(Haircuts, 5).

-soter_config(peano).
-define(Capacity, ?any_nat()).
-define(Haircuts, ?any_nat()).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

waiting_room(BarberActor, WaitingCustomers, BarberAsleep) ->
    receive
        {enter, Customer} ->
            case list_length(WaitingCustomers) == ?Capacity of
                true ->
                    Customer ! {full},
                    waiting_room(BarberActor, WaitingCustomers, BarberAsleep);
                false ->
                    case BarberAsleep of
                        true ->
                            self() ! {next},
                            waiting_room(BarberActor, [Customer | WaitingCustomers], true);
                        false ->
                            Customer ! {wait},
                            waiting_room(BarberActor, [Customer | WaitingCustomers], false)
                    end
            end;
        {next} ->
            case WaitingCustomers of
                [Customer | Customers] ->
                    BarberActor ! {enter, Customer, self()},
                    waiting_room(BarberActor, Customers, BarberAsleep);
                [] ->
                    BarberActor ! {wait},
                    waiting_room(BarberActor, WaitingCustomers, true)
            end;
        {exit} ->
            BarberActor ! {exit},
            done
    end.

barber() ->
    receive
        {enter, Customer, Room} ->
            Customer ! {start},
            Customer ! {done},
            Room ! {next},
            barber();
        {wait} ->
            barber();
        {exit} ->
            done
    end.

create_customers(I, FactoryActor, WaitingRoomActor, IdGen) ->
    case I == ?Haircuts of
        true -> done;
        false ->
            C = spawn(fun() -> customer(FactoryActor, plus(IdGen,I)) end),
            WaitingRoomActor ! {enter, C},
            create_customers(I+1, FactoryActor, WaitingRoomActor, IdGen)
    end.

customer_factory(WaitingRoomActor, HairsCutSoFar, IdGen) ->
    receive
        {start} ->
            create_customers(0, self(), WaitingRoomActor, IdGen),
            customer_factory(WaitingRoomActor, HairsCutSoFar, plus(IdGen,?Haircuts));
        {returned, Customer} ->
            WaitingRoomActor ! {enter, Customer},
            customer_factory(WaitingRoomActor, HairsCutSoFar, IdGen+1);
        {done} ->
            case HairsCutSoFar+1 == ?Haircuts of
                true ->
                    io:format("finished~n"),
                    WaitingRoomActor ! {exit},
                    done;
                false ->
                    customer_factory(WaitingRoomActor, HairsCutSoFar+1, IdGen)
            end
    end.

customer(FactoryActor, Id) ->
    receive
        {full} ->
            FactoryActor ! {returned, self()},
            customer(FactoryActor, Id);
        {wait} ->
            customer(FactoryActor, Id);
        {start} ->
            customer(FactoryActor, Id);
        {done} ->
            FactoryActor ! {done},
            done
    end.

main() ->
    BarberActor = spawn(fun() -> barber() end),
    WaitingRoomActor = spawn(fun() -> waiting_room(BarberActor, [], true) end),
    FactoryActor = spawn(fun() -> customer_factory(WaitingRoomActor, 0, 0) end),
    FactoryActor ! {start}.
