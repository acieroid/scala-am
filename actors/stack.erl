display_actor()->
    receive
        {message, _} -> display_actor()
    end.

stack_node(Content, Link)->
    receive
        {pop, Customer} -> Customer ! {message, Content}, Link();
        {push, NewContent} -> stack_node(NewContent, fun()->stack_node(Content, Link) end)
    end.

stack_node_empty()->
    receive
        {pop, _} -> ?soter_error("popping an empty stack");
        {push, Content} -> stack_node(Content, stack_node_empty)
    end.


main()->
    P = spawn(fun() -> stack_node_empty() end),
    Disp = spawn(fun() -> display_actor() end),
    P ! {push, 1},
    P ! {push, 2},
    P ! {push, 3},
    P ! {pop, Disp}.
