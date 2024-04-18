-module (pattern_matching).

-export ([start/0]).


start() ->
	List = [1,2],

	[X,Y] = List,

	io:format("pattern matching : ~p ~n",[[X,Y]]),
	io:format("pattern matching : ~p ~n",[[X]]).
