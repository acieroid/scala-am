-ifdef(SOTER).
-soter_config(peano).
-define(Alpha, ?any_nat()).
-define(CutoffDepth, ?any_nat()).
-define(GridSize, ?any_nat()).
-define(F, ?any_nat()).
-define(NumPoints, ?any_nat()).
-define(Zero, 0).
-else.
-module(ofl).
-export([main/0]).
-define(Alpha, 2.0).
-define(CutoffDepth, 3).
-define(GridSize, 500.0).
-define(F, 707.0).
-define(NumPoints, 100).
-define(Zero, 0.0).
-endif.

% Constants
-define(Unknown, -2).
-define(Root, -1).
-define(TopLeft, 0).
-define(TopRight, 1).
-define(BotLeft, 2).
-define(BotRight, 3).

foldl(_, Base, []) ->
    Base;
foldl(F, Base, [X | Xs]) ->
    foldl(F, F(Base, X), Xs).

filter(_, []) -> [];
filter(F, [X | Xs]) ->
    case F(X) of
        true -> [X | filter(F, Xs)];
        false -> filter(F, Xs)
    end.

list_empty([]) -> true;
list_empty(_) -> false.

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

list_length([]) -> 0;
list_length([_ | Xs]) -> list_length(Xs)+1.

list_ref(0, []) -> error(err);
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

random_point() ->
    {point, times(divide(random(100)+1, random(100)+1), ?GridSize), times(divide(random(100)+1, random(100)+1), ?GridSize)}.

point_x({point, X, _}) -> X.
point_y({point, Y, _}) -> Y.
box_x1({box, X1, _, _, _}) -> X1.
box_x2({box, _, _, X2, _}) -> X2.
box_y1({box, _, Y1, _, _}) -> Y1.
box_y2({box, _, _, _, Y2}) -> Y2.

box_contains(Box, P) ->
    case box_x1(Box) =< point_x(P) of
        true -> case box_y1(Box) =< point_y(P) of
                    true -> case point_x(P) =< box_x2(Box) of
                                true -> case point_y(P) =< box_y2(Box) of
                                            true -> true;
                                            false -> false
                                        end;
                                false -> false
                            end;
                    false -> false
                end;
        false -> false
    end.

mid_point(Box) ->
    {point, divide(plus(box_x1(Box), box_x2(Box)), 2), divide(plus(box_y1(Box), box_y2(Box)), 2)}.

-ifdef(SOTER).
plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).
-else.
plus(X,Y) -> X+Y.
-endif.

-ifdef(SOTER).
divide(X, Y) ->
    case X < Y of
        true -> 0;
        false -> divide(minus(X,Y),Y)+1
    end.
-else.
divide(X,Y) -> X / Y.
-endif.

-ifdef(SOTER).
minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).
-else.
minus(X, Y) -> X-Y.
-endif.

-ifdef(SOTER).
times(_, 0) -> 0;
times(X, Y) -> plus(X, times(X, Y-1)).
-else.
times(X, Y) -> X*Y.
-endif.

-ifdef(SOTER).
random(X) -> modulo(?any_nat(), X).
-else.
random(X) -> rand:uniform(X)-1.
-endif.

modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

mmax(X, Y) ->
    case X < Y of
        true -> Y;
        false -> X
    end.

-ifdef(SOTER).
sqrt(X) -> X.
-else.
sqrt(X) -> math:sqrt(X).
-endif.

get_distance(P1, P2) ->
    XDiff = minus(point_x(P1), point_x(P2)),
    YDiff = minus(point_y(P1), point_y(P2)),
    sqrt(plus(times(XDiff, XDiff), times(YDiff, YDiff))).

producer_actor(Consumer, ItemsProduced) ->
    receive
        {start} ->
            io:format("[producer] start~n"),
            Consumer ! {customer, self(), random_point()},
            producer_actor(Consumer, ItemsProduced+1);
        {next_customer} ->
            io:format("[producer] next_customer~n"),
            case ItemsProduced < ?NumPoints of
                true ->
                    Consumer ! {customer, self(), random_point()},
                    producer_actor(Consumer, ItemsProduced+1);
                false ->
                    Consumer ! {request_exit},
                    done
            end
    end.

safely_exit(Parent, Children, ChildrenFacilities, FacilityCustomers, SupportCustomers) ->
    case Parent of
        false -> nothing;
        _ ->
            NumFacilities = case list_empty(Children) of
                                true -> ChildrenFacilities+1;
                                false -> ChildrenFacilities
                            end,
            NumCustomers = plus(FacilityCustomers, list_length(SupportCustomers)),
            Parent ! {confirm_exit, NumFacilities, NumCustomers}
    end,
    done.

create_child(Self, Boundary, Position, Customers, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth) ->
    Customers = filter(fun(C) -> box_contains(Boundary, C) end, Customers),
    spawn(fun() -> quadrant_actor(Self, Position, Boundary, Threshold, Depth+1, LocalFacilities, KnownFacilities, MaxDepth, Customers,
                                  [], [], 0, 0, 0, foldl(fun(C, Acc) ->
                                                                 plus(Acc, foldl(fun(Result, Fac) ->
                                                                                         Distance = get_distance(Fac, C),
                                                                                         case Distance < Result of
                                                                                             true -> Distance;
                                                                                             false -> Result
                                                                                         end
                                                                                 end,
                                                                                 0, LocalFacilities))
                                                         end,
                                                         ?Zero, Customers))
          end).

quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold,
               Depth, LocalFacilities, KnownFacilities, MaxDepthOfKnownOpenFacility,
               Customers, Children, ChildrenBoundaries, ChildrenFacilities, FacilityCustomers,
               TerminatedChildCount, TotalCost) ->
    receive
        {customer, Producer, Point} ->
            io:format("[quadrant] customer~n"),
            case Parent of
                false -> Producer ! {next_customer};
                _ -> nothing
            end,
            case list_empty(Children) of
                true ->
                    Cost = foldl(fun(Result, Fac) ->
                                         Distance = get_distance(Fac, Point),
                                         case Distance < Result of
                                             true -> Distance;
                                             false -> Result
                                         end
                                 end,
                                 0, LocalFacilities),
                    case plus(TotalCost, Cost) > Threshold of
                        true ->
                            Facility = mid_point(Boundary),
                            MaxDepth = mmax(MaxDepthOfKnownOpenFacility, Depth),
                            case Parent of
                                false -> nothing;
                                _ -> Parent ! {facility, PositionRelativeToParent, Depth, Facility, true}
                            end,
                            FirstBoundary = {box, box_x1(Boundary), point_y(Facility), point_x(Facility), box_y2(Boundary)},
                            SecondBoundary = {box, point_x(Facility), point_y(Facility), box_x2(Boundary), box_y2(Boundary)},
                            ThirdBoundary = {box, box_x1(Boundary), box_y1(Boundary), point_x(Facility), point_y(Facility)},
                            FourthBoundary = {box, point_x(Facility), box_y1(Boundary), box_x2(Boundary), point_y(Facility)},
                            FirstChild = create_child(self(), FirstBoundary, ?TopLeft, Customers, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth),
                            SecondChild = create_child(self(), SecondBoundary, ?TopRight, Customers, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth),
                            ThirdChild = create_child(self(), ThirdBoundary, ?BotLeft, Customers, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth),
                            FourthChild = create_child(self(), FourthBoundary, ?BotRight, Customers, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth),
                            quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, LocalFacilities, KnownFacilities, MaxDepth,
                                           [], [FirstChild,SecondChild,ThirdChild,FourthChild], [FirstBoundary,SecondBoundary,ThirdBoundary,FourthBoundary],
                                           ChildrenFacilities, FacilityCustomers, TerminatedChildCount, TotalCost);
                        false -> quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, LocalFacilities, KnownFacilities,
                                                MaxDepthOfKnownOpenFacility, [Point|Customers], Children, ChildrenBoundaries, ChildrenFacilities,
                                                FacilityCustomers, TerminatedChildCount, plus(TotalCost, Cost))
                    end;
                false ->
                    Loop = fun(Loop, Index) ->
                                   case Index == 4 of
                                       true -> nothing;
                                       false -> LoopChildBoundary = list_ref(Index, ChildrenBoundaries),
                                                case box_contains(LoopChildBoundary, Point) of
                                                    true -> list_ref(Index, Children) ! {customer, Producer, Point};
                                                    false -> Loop(Loop, Index+1)
                                                end
                                   end
                           end,
                    Loop(Loop, 0),
                    quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, LocalFacilities, KnownFacilities,
                                   MaxDepthOfKnownOpenFacility, Customers, Children, ChildrenBoundaries, ChildrenFacilities,
                                   FacilityCustomers, TerminatedChildCount, TotalCost)
            end;
        {facility, ChildPos, RecvDepth, Point, FromChild} ->
            io:format("[quadrant] facility~n"),
            case FromChild of
                true ->
                    SiblingPos = case ChildPos of
                                     ?TopLeft -> ?BotRight;
                                     ?TopRight -> ?BotLeft;
                                     ?BotRight -> ?TopLeft;
                                     _ -> ?TopRight
                                 end,
                    case Parent of
                        false -> nothing;
                        _ -> Parent ! {facility, PositionRelativeToParent, RecvDepth, Point, true}
                    end,
                    list_ref(SiblingPos, Children) ! {facility, ?Unknown, Depth, Point, false},
                    quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, [Point|LocalFacilities], KnownFacilities+1,
                                   case RecvDepth > MaxDepthOfKnownOpenFacility of
                                       true -> RecvDepth;
                                       false -> MaxDepthOfKnownOpenFacility
                                   end,
                                   Customers, Children, ChildrenBoundaries, ChildrenFacilities, FacilityCustomers,
                                   TerminatedChildCount, TotalCost);
                false ->
                    list_foreach(fun(LoopChild) -> LoopChild ! {facility, ?Unknown, Depth, Point, false} end, Children),
                    quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth,
                                   [Point|LocalFacilities], KnownFacilities+1, MaxDepthOfKnownOpenFacility, Customers,
                                   Children, ChildrenBoundaries, ChildrenFacilities, FacilityCustomers, TerminatedChildCount, TotalCost)
            end;
        {request_exit} ->
            io:format("[quadrant] request_exit~n"),
            case list_empty(Children) of
                false ->
                    list_foreach(fun(LoopChild) -> LoopChild ! {request_exit} end, Children),
                    quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, LocalFacilities,
                                   KnownFacilities, MaxDepthOfKnownOpenFacility, Customers,
                                   Children, ChildrenBoundaries, ChildrenFacilities,
                                   FacilityCustomers, TerminatedChildCount, TotalCost);
                true -> safely_exit(Parent, Children, ChildrenFacilities, FacilityCustomers, Customers)
            end;
        {confirm_exit, Facilities, SupportCustomers} ->
            io:format("[quadrant] confirm_exit~n"),
            case TerminatedChildCount == 3 of
                true -> safely_exit(Parent, Children, ChildrenFacilities, FacilityCustomers, SupportCustomers);
                false -> quadrant_actor(Parent, PositionRelativeToParent, Boundary, Threshold, Depth, LocalFacilities,
                                        KnownFacilities, MaxDepthOfKnownOpenFacility, Customers, Children, ChildrenBoundaries,
                                        plus(ChildrenFacilities, Facilities),
                                        plus(FacilityCustomers, SupportCustomers), TerminatedChildCount+1,
                                        TotalCost)
            end
    end.

main() ->
    Threshold = times(?Alpha, ?F),
    BoundingBox = {box, 0, 0, ?GridSize, ?GridSize},
    RootQuadrant = spawn(fun() -> quadrant_actor(false, ?Root, BoundingBox, Threshold, 0, [mid_point(BoundingBox)], 1, -1, [], [], [], 0, 0, 0, ?Zero) end),
    Producer = spawn(fun() -> producer_actor(RootQuadrant, 0) end),
    Producer ! {start}.
