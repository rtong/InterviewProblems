%% @author tongrui
%% @doc @todo Add description to solver.


-module(solver).

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/1, test/0]).

%% ====================================================================
%% Internal functions
%% ====================================================================
main(N) ->
	erase(),
	helper(1, N, 0).

% helper sits in a recursion. I is iterative variable which starts from 1 to infinity.
% use process dictionary as HashMap to keep track of occurance of same set of digits
% 	the key of the process dictionary is maximum permutation list
% 	the value of the process dictionary is cubes that have exactly the same set of digits with the key
% the function returns when N cubes that have the same set of digits are found.
helper(I, N, Level) ->
	Cube = I * I * I,
	MaxPerm = getMaxPerm(Cube),
	
	case get(MaxPerm) of
		undefined -> 
			% =====================================================
			% space complexity optimization based on the fact that
			% the permutations of same digits have the same length
			if
				length(MaxPerm) > Level ->
					erase(),
					NewLevel = Level + 1;
				true ->
					NewLevel = Level
			end,
			% =====================================================
			NewList = [Cube],
			put(MaxPerm, NewList),
			helper(I + 1, N, NewLevel);
		Value ->
			OldList = Value,
			if
				length(OldList) == N - 1 ->
					lists:last(OldList);
				true ->
					NewList = [Cube | OldList],
					put(MaxPerm, NewList),
					helper(I + 1, N, Level)
			end
	end.

% getMaxPerm returns the maximum permutation, as a list, of given Cube
getMaxPerm(Cube) ->
	CubeList = lists:map(fun(X) -> X - 48 end, integer_to_list(Cube)),
	lists:reverse(lists:sort(CubeList)).


%% ====================================================================
%% Test cases
%% ====================================================================
test() ->
	Res = main(5),
	
	IsCube = isCube(Res),
	if
		IsCube ->
			io:format("test 1 passed, the result ~w is a cube.~n", [Res]);
		true ->
			io:format("test 1 failed!!!")
	end,
	
	Exactly5Perms = isExact5Perms(Res),
	if
		Exactly5Perms ->
			io:format("test 2 passed, the result ~w has exactly five permutations of its digits that are cube.~n", [Res]);
		true ->
			io:format("test 2 failed!!!")
	end,
	
	IsSmallestCube = isSmallestCube(Res),
	if
		IsSmallestCube ->
			io:format("test 3 passed, the result ~w is the smallest cube among 5 permutations.~n", [Res]);
		true ->
			io:format("test 3 failed!!!")
	end.

isCube(Res) ->
	binarySearch(1, Res, Res).

% Binary Search
binarySearch(Start, End, Target) ->
	if
		Start + 1 < End ->
			Mid = Start + (End - Start) div 2,
			Cube = Mid * Mid * Mid,
			if
				Cube == Target ->
					true;
				Cube < Target ->
					binarySearch(Mid, End, Target);
				Cube > Target ->
					binarySearch(Start, Mid, Target)
			end;
		true ->
			% tackle the rest
			if 
				Start * Start * Start == Target ->
					true;
				End * End * End == Target ->
					true;
				true ->
					false
			end
	end.
	
isExact5Perms(Res) ->
	erase(),
	MaxRes = getMaxPerm(Res),
	List = testHelper(1, MaxRes),
	length(List) == 5.

isSmallestCube(Res) ->
	erase(),
	MaxRes = getMaxPerm(Res),
	List = testHelper(1, MaxRes),
	Res == lists:min(List).

testHelper(I, MaxRes) ->
	Cube = I * I * I,
	MaxPerm = getMaxPerm(Cube),
	case get(MaxPerm) of
		undefined -> 
			if
				length(MaxPerm) > length(MaxRes) ->
					get(MaxRes);
				true ->
					NewList = [Cube],
					put(MaxPerm, NewList),
					testHelper(I + 1, MaxRes)
			end;
		Value ->
			OldList = Value,
			NewList = [Cube | OldList],
			put(MaxPerm, NewList),
			testHelper(I + 1, MaxRes)
	end.
				
