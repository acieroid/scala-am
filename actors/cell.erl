-uncoverable("cell_mail > 2").
-uncoverable("disp_mail > 1").

cell(Content) ->
    ?label_mail("cell_mail"),
    receive
        {put, NewContent} ->
            cell(NewContent);
        {get, To} ->
            To ! {value, Content},
            cell(Content)
    end.

disp() ->
    ?label_mail("disp_mail"),
    receive
        {value, Content} ->
            if 
                Content == 2 -> disp();
                true -> ?soter_error("Error!")
            end
    end.

main() ->
    C1 = spawn(fun() -> cell(1) end),
    C2 = spawn(fun() -> cell(2) end),
    Disp = spawn(fun() -> disp() end),
    C1 ! {put, 2},
    C2 ! {put, 5},
    C1 ! {get, Disp}.
