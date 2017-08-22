-ifdef(SOTER).
-soter_config(peano).
-define(BinomialParam, ?any_nat()).
-define(MaxNodes, ?any_nat()).
-define(AvgCompSize, ?any_nat()).
-define(CompSize, ?any_nat()).
-define(StdDevCompSize, ?any_nat()).
-define(UrgentNodePercent, ?any_nat()).
-else.
-module(uct).
-export([main/0]).
-define(BinomialParam, 5).
-define(MaxNodes, 2).
-define(AvgCompSize, 5).
-define(CompSize, 5).
-define(StdDevCompSize, 5).
-define(UrgentNodePercent, 5).
-endif.

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

-ifdef(SOTER).
random(X) -> modulo(?any_nat(), X).
rand_bool() -> ?any_bool().
-else.
random(X) -> rand:uniform(X)-1.
rand_bool() -> rand:uniform(2) == 1.
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

list_set([], _, _) -> [];
list_set([_ | Xs], 0, Y) -> [Y|Xs];
list_set([X | Xs], I, Y) -> [X | list_set(Xs, I-1, Y)].

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.



get_next_normal(PMean, PDev) ->
    Loop = fun(Loop, I) ->
                   case I > 0 of
                       true -> I;
                       false ->
                           Temp = plus(random(PDev), PMean),
                           Loop(Loop, Temp)
                   end
           end,
    Loop(Loop, 0).

wait_loop(BusyWait, Dummy) ->
    Loop = fun(Loop, K) ->
                   case K == times(Dummy, BusyWait) of
                       true -> K;
                       false -> Loop(Loop, K+1)
                   end
           end,
    % Loop(Loop, 0).
    wait.

root_actor(Height, Size, Children, HasGrantChildren, Traversed, FinalSizePrinted) ->
    receive
        {generate_tree} ->
            Height2 = Height+1,
            Size2 = plus(Size, ?BinomialParam),
            ComputationSize = get_next_normal(?AvgCompSize, ?StdDevCompSize),
            Self = self(),
            Children2 = build_list(?BinomialParam, fun(I) -> spawn(fun() -> node_actor(Self, Self, Height, plus(Size, I), ComputationSize, false, 0, false, false,
                                                                                       build_list(?BinomialParam, fun(_) -> false end)) end)
                                                   end),
            list_foreach(fun(A) -> A ! {try_generate_children} end, Children2),
            root_actor(Height2, Size2, Children2, build_list(?BinomialParam, fun(_) -> false end), Traversed, FinalSizePrinted);
        {update_grant, ChildId} ->
            root_actor(Height, Size, Children, list_set(HasGrantChildren, ChildId, true), Traversed, FinalSizePrinted);
        {should_generate_children, Sender, ChildHeight} ->
            case plus(Size, ?BinomialParam) =< ?MaxNodes of
                true -> case rand_bool() of
                            true ->
                                ChildComp = get_next_normal(?CompSize, ?StdDevCompSize),
                                case random(100) > ?UrgentNodePercent of
                                    true -> Sender ! {generate_children, Size, ChildComp};
                                    false -> Sender ! {urgent_generate_children, random(?BinomialParam), Size, ChildComp}
                                end,
                                root_actor(case ChildHeight+1 > Height of
                                               true -> ChildHeight+1;
                                               false -> Height
                                           end, plus(Size, ?BinomialParam), Children, HasGrantChildren, Traversed, FinalSizePrinted);
                            false -> root_actor(ChildHeight, Size, Children, HasGrantChildren, Traversed, FinalSizePrinted)
                        end;
                false -> root_actor(Height, Size, Children, HasGrantChildren,
                                    case Traversed of
                                        true -> true;
                                        false -> list_foreach(fun(A) -> A ! {traverse} end, Children), true
                                    end,
                                    case FinalSizePrinted of
                                        true -> true;
                                        false -> io:format("...~n"), true
                                    end)
            end;
        {print_info} ->
            list_foreach(fun(A) -> A ! {print_info} end, Children),
            root_actor(Height, Size, Children, HasGrantChildren, Traversed, FinalSizePrinted);
        {terminate_me} ->
            list_foreach(fun(A) -> A ! {terminate} end, Children),
            done
    end.

node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, HasChildren, Children, HasGrantChildren) ->
    receive
        {try_generate_children} ->
            wait_loop(100, 40000),
            Root ! {should_generate_children, self(), Height},
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, HasChildren, Children, HasGrantChildren);
        {generate_children, CurrentId, ChildrenCompSize} ->
            ArrayId = modulo(Id, ?BinomialParam),
            ChildrenHeight = Height+1,
            Self = self(),
            Children2 = build_list(?BinomialParam, fun(_) ->
                                                           C = spawn(fun() -> node_actor(Self, Root, ChildrenHeight, CurrentId+1, ChildrenCompSize, false, 0, false, false, build_list(?BinomialParam, fun(_) -> false end)) end),
                                                           C ! {try_generate_children},
                                                           C
                                                   end),
            Parent ! {update_grant, ArrayId},
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, true, Children2, HasGrantChildren);
        {urgent_generate_children, UrgentChildId, CurrentId, ChildrenCompSize} ->
            ArrayId = modulo(Id, ?BinomialParam),
            ChildrenHeight = Height+1,
            Self = self(),
            Children2 = build_list(?BinomialParam, fun(_) ->
                                                           C = spawn(fun() -> node_actor(Self, Root, ChildrenHeight, CurrentId+1, ChildrenCompSize, false, 0, false, false, build_list(?BinomialParam, fun(_) -> false end)) end),
                                                           C ! {try_generate_children},
                                                           C
                                                   end),
            Parent ! {update_grant, ArrayId},
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChildId, true, Children2, HasGrantChildren);
        {update_grant, ChildId} ->
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, HasChildren, Children, list_set(HasGrantChildren, ChildId, true));
        {traverse} ->
            wait_loop(CompSize, 40000),
            case HasChildren of
                true ->
                    Loop = fun(Loop, I) ->
                                   case I == list_length(Children) of
                                       true -> true;
                                       false -> case I == UrgentChild of
                                                    true -> list_ref(I, Children) ! {urgent_traverse};
                                                    false -> list_ref(I, Children) ! {traverse}
                                                end,
                                                Loop(Loop, I+1)
                                   end
                           end,
                    Loop(0);
                false -> nothing
            end,
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, HasChildren, Children, HasGrantChildren);
        {print_info} ->
            case HasChildren of
                true -> list_foreach(fun(A) -> A ! {print_info} end, Children);
                false -> nothing
            end,
            node_actor(Parent, Root, Height, Id, CompSize, IsUrgent, UrgentChild, HasChildren, Children, HasGrantChildren);
        {terminate} ->
            case HasChildren of
                true -> list_foreach(fun(A) -> A ! {terminate} end, Children);
                false -> nothing
            end,
            done
    end.

main() ->
    Root = spawn(fun() -> root_actor(1, 1, false, build_list(?BinomialParam, fun(_) -> false end), false, false) end),
    Root ! {generate_tree}.
