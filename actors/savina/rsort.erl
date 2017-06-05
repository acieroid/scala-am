-define(NumValues, 5).
-define(MaxValue, 100).

int_source_loop(Actor, 0) -> done;
int_source_loop(Actor, N) ->
    Actor ! {value, random(?MaxValue)},
    int_source_loop(Actor, N-1).

int_source() ->
    receive
        {nextactor, Actor} ->
            int_source_loop(Actor, ?NumValues),
            done
    end.

sort(Radix, NextActor, Array, ValuesSoFar, J) ->
    receive
        {value, V} ->
            {NewJ, NewArray} = case V band Radix == 0 of
                                   true ->
                                       NextActor ! {value, V},
                                       {J, Array};
                                   false ->
                                       {J+1, list_set(Array, J, V)}
                               end,
            case ValuesSoFar+1 == ?NumValues =>
                
        
         ->
                                       
