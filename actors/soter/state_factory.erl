-soter_config(peano).

-uncoverable("state_mail > 1").
% -uncoverable("after_receive > 0, state_mail > 0").

state(N, NewState) ->
    ?label_mail("state_mail"),
    receive
        {P,In}->
            ?label("after_receive"),
            M = NewState(N,In),
            P ! M,
            state(M, NewState)
    end.

factory(N, NewState) ->
    P=spawn(fun() -> state(N,NewState) end),
    fun(In)->
            P!{self(),In},
            receive
                Out-> Out
            end
    end.

main() ->
    FunWithState = factory(?any_nat(), fun(_X,_Y) -> ?any_nat() end),
    call_loop(?any_nat(), FunWithState).

call_loop(1,Fun) -> Fun(?any_nat());
call_loop(N,Fun) -> Fun(?any_nat()),
                  call_loop(N-1,Fun).
