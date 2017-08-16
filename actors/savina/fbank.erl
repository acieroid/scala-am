%-module(fbank).
%-export([main/0]).
%-define(NumSimulations, 5).
%-define(NumChannels, 5).
%-define(NumColumns, 5).
%-define(SinkPrintRate, 2).
%-define(H, 5).
%-define(F, 5).
%-define(MaxValue, 1000).

-soter_config(peano).
-define(NumSimulations, ?any_nat()).
-define(NumChannels, ?any_nat()).
-define(NumColumns, ?any_nat()).
-define(SinkPrintRate, ?any_nat()).
-define(H, ?any_nat()).
-define(F, ?any_nat()).
-define(MaxValue, ?any_nat()).

number_to_string(_) ->
    "foo".
string_append(_, _) ->
    "foo".

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_set([], _, _) -> [];
list_set([_ | Xs], 0, Y) -> [Y|Xs];
list_set([X | Xs], I, Y) -> [X | list_set(Xs, I-1, Y)].

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

foldl(_, Base, []) ->
    Base;
foldl(F, Base, [X | Xs]) ->
    foldl(F, F(Base, X), Xs).

build_list_i(I, N, F) ->
    case I == N of
        true -> [];
        false -> [F(I)|build_list_i(I+1,N,F)]
    end.
build_list(N, F) -> build_list_i(0, N, F).

make_list(N, V) ->
    build_list(N, fun(_) -> V end).

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
%    rand:uniform(X)-1.
     modulo(?any_nat(), X).

randbool() ->
    random(2) == 1.

producer(MessagesSent) ->
    receive
        {next, Source} ->
            case MessagesSent == ?NumSimulations of
                true ->
                    Source ! {exit},
                    done;
                false ->
                    Source ! {boot},
                    producer(MessagesSent+1)
            end
    end.

source(Next, Producer, Current) ->
    receive
        {boot} ->
            Next ! {value, Current},
            Producer ! {next, self()},
            source(Next, Producer, modulo(Current+1, ?MaxValue));
        {exit} -> done
    end.

branches(Next, Banks) ->
    receive
        {value, V} ->
            list_foreach(fun(A) -> A ! {value, V} end, Banks),
            branches(Next, Banks);
        {exit} ->
            list_foreach(fun(A) -> A ! {exit} end, Banks),
            done
    end.

tagged_forward(SourceId, Next) ->
    receive
        {value, V} ->
            Next ! {sourced_value, SourceId, V},
            tagged_forward(SourceId, Next);
        {exit} ->
            done
    end.

loop(Data, I, Sum) ->
    case I == ?NumColumns of
        true -> Sum;
        false -> loop(Data, I+1, plus(Sum, times(list_ref(I, Data), random(100))))
    end.

fir_filter(SourceId, Next, Data, DataIndex, DataFull) ->
    receive
        {value, V} ->
            Data2 = list_set(Data, DataIndex, V),
            case DataIndex+1 == ?NumColumns of
                true ->
                    ToSend = loop(Data2, 0, 0),
                    Next ! {value, ToSend},
                    fir_filter(SourceId, Next, Data2, 0, true);
                false ->
                    case DataFull of
                        true ->
                            ToSend = loop(Data2, 0, 0),
                            Next ! {value, ToSend},
                            fir_filter(SourceId, Next, Data2, 0, true);
                        false ->
                            fir_filter(SourceId, Next, Data2, DataIndex+1, DataFull)
                    end
            end;
        {exit} ->
            done
    end.

delay(SourceId, DelayLength, Next, State, PlaceHolder) ->
    receive
        {value, V} ->
            Next ! {value, list_ref(PlaceHolder, State)},
            State2 = list_set(State, PlaceHolder, V),
            delay(SourceId, DelayLength, Next, State2, modulo(PlaceHolder+1, DelayLength));
        {exit} -> done
    end.

sample_filter(SampleRate, Next, SamplesReceived) ->
    receive
        {value, V} ->
            case SamplesReceived == 0 of
                true -> Next ! {value, V};
                false -> Next ! {value, 0}
            end,
            sample_filter(SampleRate, Next, modulo(SamplesReceived+1, SampleRate));
        {exit} -> done
    end.

create_bank(SourceId, Integrator) ->
    Tf = spawn(fun() -> tagged_forward(SourceId, Integrator) end),
    Ff = spawn(fun() -> fir_filter(string_append(number_to_string(SourceId), ".2"), Tf, make_list(?NumColumns, 0), 0, false) end),
    D = spawn(fun() -> delay(string_append(number_to_string(SourceId), ".2"), ?NumColumns-1, Ff, make_list(?NumColumns-1, 0), 0) end),
    Sf = spawn(fun() -> sample_filter(?NumColumns, D, 0) end),
    Ff2 = spawn(fun() -> fir_filter(string_append(number_to_string(SourceId), ".1"), Sf, make_list(?NumColumns, 0), 0, false) end),
    D2 = spawn(fun() -> delay(string_append(number_to_string(SourceId), ".1"), ?NumColumns-1, Ff2, make_list(?NumColumns-1, 0), 0) end),
    FirstActor = D2,
    spawn(fun() -> bank(SourceId, Integrator, FirstActor) end).

bank(SourceId, Integrator, FirstActor) ->
    receive
        {value, V} ->
            FirstActor ! {value, V},
            bank(SourceId, Integrator, FirstActor);
        {exit} -> done
    end.

make_values(I, Acc) ->
    case I == ?NumChannels of
        true -> Acc;
        false -> make_values(I+1, [random(100)|Acc])
    end.

integrator(Next, Data, ExitsReceived) ->
    receive
        {sourced_value, _, _} ->
            case randbool() of
                true -> Next ! {collection, make_values(0, [])};
                false -> nothing
            end,
            integrator(Next, Data, ExitsReceived);
        {exit} ->
            case ExitsReceived+1 == ?NumChannels of
                true -> done;
                false -> integrator(Next, Data, ExitsReceived+1)
            end
    end.

combine(Next) ->
    receive
        {collection, Vs} ->
            Next ! {value, foldl(fun(V, Acc) -> plus(V, Acc) end, 0, Vs)},
            combine(Next);
        {exit} -> done
    end.

sink(PrintRate, Count) ->
    receive
        {value, _} ->
            sink(PrintRate, modulo(Count+1, PrintRate));
        {exit} ->
            done
    end.

main() ->
    ProducerActor = spawn(fun() -> producer(0) end),
    SinkActor = spawn(fun() -> sink(?SinkPrintRate, 0) end),
    CombineActor = spawn(fun() -> combine(SinkActor) end),
    IntegratorActor = spawn(fun() -> integrator(CombineActor, [], 0) end),
    Banks = build_list(?NumChannels, fun(I) -> create_bank(I, IntegratorActor) end),
    BranchesActor = spawn(fun() -> branches(IntegratorActor, Banks) end),
    SourceActor = spawn(fun() -> source(BranchesActor, ProducerActor, 0) end),
    ProducerActor ! {next, SourceActor}.
