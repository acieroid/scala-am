-ifdef(SOTER).
-soter_config(peano).
-define(NumAccounts, ?any_nat()).
-define(NumBankings, ?any_nat()).
-define(InitialBalance, ?any_nat()).
-else.
-module(btx).
-export([main/0]).
-define(NumAccounts, 5).
-define(NumBankings, 5).
-define(InitialBalance, 5).
-endif.

plus(X, 0) -> X;
plus(X, Y) -> plus(X+1, Y-1).

minus(X, 0) -> X;
minus(X, Y) -> minus(X-1, Y-1).

-ifdef(SOTER).
modulo(X, Y) ->
    case X < Y of
        true -> X;
        false -> modulo(minus(X,Y), Y)
    end.

random(X) ->
    modulo(?any_nat(), X).
-else.
random(X) ->
    rand:uniform(X)-1.
-endif.

list_ref(0, []) -> error;
list_ref(0, [X | _]) -> X;
list_ref(N, [_ | Xs]) -> list_ref(N-1, Xs).

list_foreach(_, []) -> done;
list_foreach(F, [X | Xs]) ->
    F(X),
    list_foreach(F, Xs).

-ifdef(SOTER).
source_id(N) -> random(N).
-else.
source_id(N) -> round((random(N)/10) * 8).
-endif.

generate_work(I, Accounts) ->
    case I == ?NumBankings of
        true -> done;
        false ->
            SourceId = source_id(?NumAccounts),
            LoopId = random(?NumAccounts-SourceId),
            DestId = plus(SourceId, (case LoopId of 0 -> 1; _ -> LoopId end)),
            Source = list_ref(SourceId, Accounts),
            Dest = list_ref(DestId, Accounts),
            Amount = random(1000),
            Source ! {foo},
            Source ! {credit, Amount, self(), Dest},
            generate_work(I+1, Accounts)
    end.

teller(Accounts, NumCompletedBankings) ->
    receive
        {start} ->
            generate_work(0, Accounts),
            teller(Accounts, NumCompletedBankings);
        {reply} ->
            case (NumCompletedBankings+1) == ?NumBankings of
                true ->
                    list_foreach(fun(A) -> A ! {stop} end, Accounts),
                    io:format("finished~n");
                false -> teller(Accounts, NumCompletedBankings+1)
            end
    end.

account(Master, Id, Balance) ->
    receive
        {debit, Amount, Creditor} ->
            Creditor ! {reply},
            account(Master, Id, plus(Balance,Amount));
        {credit, Amount, Sender, Recipient} ->
            Recipient ! {debit, Amount, Sender},
            Master ! {reply},
            account(Master, Id, minus(Balance,Amount));
        {nevercalled} ->
            1+2;
        {stop} -> self() ! {foo}, done
    end.

create_accounts(I, Master) ->
    case I == ?NumAccounts of
        true -> [];
        false -> [spawn(fun() -> account(Master, I, ?InitialBalance) end) | create_accounts(I+1, Master)]
    end.

main() ->
    Master = spawn(fun() -> teller(create_accounts(0, self()), 0) end),
    Master ! {start}.
