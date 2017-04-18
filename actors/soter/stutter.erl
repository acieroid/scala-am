main() ->
    P = spawn(fun()-> stutter(fun(Msg)-> dosmt(Msg) end) end),
    sendA(P).

dosmt(a) -> ?soter_error("We abhorr 'a's.");
dosmt(b) -> 'we love b'.

sendB(P) -> P!b, sendA(P).
sendA(P) -> P!a, sendB(P).

stutter(F) ->
    receive
        _ -> unstutter(F)
    end.

unstutter(F) ->
    receive
        X -> F(X), stutter(F)
    end.
