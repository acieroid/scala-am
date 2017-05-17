%-module(cham).
%-export([main/0]).
%-define(NMeetings, 5).
%-define(NChameneos, 5).
-soter_config(peano).
-define(NMeetings, ?any_nat()).
-define(NChameneos, ?any_nat()).


complement(red, red) -> red;
complement(red, yellow) -> blue;
complement(red, blue) -> yellow;
complement(red, faded) -> faded;
complement(yellow, red) -> blue;
complement(yellow, yellow) -> yellow;
complement(yellow, blue) -> red;
complement(yellow, faded) -> faded;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(blue, blue) -> blue;
complement(blue, faded) -> faded;
complement(faded, _) -> faded.

pick_color(0) -> red;
pick_color(1) -> yellow;
pick_color(2) -> blue;
pick_color(3) -> faded.

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

chameneo_actor(Mall, Meetings, Color, Id) ->
    receive
        {meet, OtherColor, Sender} ->
            io:format("chameneo meet ~w ~w~n", [OtherColor, Sender]),
            NewColor = complement(Color, OtherColor),
            Sender ! {change, NewColor},
            Mall ! {meet, NewColor, self()},
            chameneo_actor(Mall, Meetings, NewColor, Id);
        {change, NewColor} ->
            io:format("chameneo change ~w~n", [NewColor]),
            Mall ! {meet, NewColor, self()},
            chameneo_actor(Mall, Meetings+1, NewColor, Id);
        {exit, Sender} ->
            io:format("chameneo ~w exiting~n", [Id]),
            Sender ! {meetingcount, Meetings, self()},
            done
    end.

create_chameneo(Mall, Color, Id) ->
    io:format(">>> create chameneo ~w ~w~n", [Color, Id]),
    Chameneo = spawn(fun() -> chameneo_actor(Mall, 0, Color, Id) end),
    Mall ! {meet, Color, Chameneo},
    Chameneo.

create_chameneos(I, Mall) ->
    case I == ?NChameneos of
        true ->
            io:format("done creating~n"),
            done;
        false ->
            create_chameneo(Mall, pick_color(modulo(I, 3)), I),
            create_chameneos(I+1, Mall)
    end.

mall_actor(WaitingChameneo, N, SumMeetings, NumFaded) ->
    receive
        {meetingcount, Count, _} ->
            io:format("meetingcount ~w~n", [Count]),
            case (NumFaded+1) == ?NChameneos of
                true -> io:format("mall done~n"), done;
                false -> mall_actor(WaitingChameneo, N, plus(SumMeetings,Count), NumFaded+1)
            end;
        {meet, Color, Sender} ->
            io:format("meet ~w ~w~n", [Color, Sender]),
            case N == 0 of
                false -> case WaitingChameneo of
                             false -> mall_actor(Sender, N, SumMeetings, NumFaded);
                             _ ->
                                 WaitingChameneo ! {meet, Color, Sender},
                                 mall_actor(false, N-1, SumMeetings, NumFaded)
                         end;
                true ->
                    Sender ! {exit, self()},
                    mall_actor(WaitingChameneo, N, SumMeetings, NumFaded)
            end
    end.

create_mall() ->
    Mall = spawn(fun() -> mall_actor(false, ?NMeetings, 0, 0) end),
    create_chameneos(0, Mall),
    Mall.

main() ->
    create_mall().
