%% @author tongrui
%% @doc @todo Add description to fib.


-module(fib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([fibo/1, printfibo/1, perms/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%% print fibo arg. and result, with function as parameter
printfibo(N) -> 
   Res = fib:fibo(N),
   io:fwrite("~w ~w~n", [N, Res]).

fibo(0) -> 0 ; 
fibo(1) -> 1 ; 
fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2) .

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].