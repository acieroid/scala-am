-soter_config(peano).

-uncoverable("mail>=2").

main()->
    Fun = fun(X)-> X+1 end,
    P1 = spawn(fun() -> sink() end),
    P2 = spawn(fun() -> pipe_node(Fun, P1) end),
    Head = spawn(fun() -> pipe_node(Fun, P2) end),
    Head ! 0.

pipe_node(Fun, Next) ->
    % this will label every mailbox
    % of every process running `pipe_node`
    ?label_mail('mail',self()),
    receive
        Msg -> Next ! Fun(Msg),
               pipe_node(Fun, Next)
    end.

init_pipe( _ , 0 , Next) -> Next;
init_pipe(Fun, N, Next) ->
    New = spawn(fun()-> pipe_node(Fun, Next) end),
    init_pipe(Fun, N-1, New).

sink() ->
    receive X -> X end.
