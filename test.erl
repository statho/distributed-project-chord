-module(test).
-export ([main/0]).


main() ->
	{ok, Pid1} = node:start_link(1),
	io:format("Node 1 pid: ~w\n", [Pid1]),
	% node:insert(Pid1, {1, atomo}),
	% io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	node:join(Pid1, {2, undefined, undefined}),
	{_, _, {_, Pid2}, _, _} = sys:get_state(Pid1),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	node:join(Pid1, {4, undefined, undefined}),
	{_, _, {_, Pid4}, _, _} = sys:get_state(Pid1),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	io:format("State 4: ~w\n", [sys:get_state(Pid4)]),

	node:join(Pid1, {3, undefined, undefined}),
	timer:sleep(500),
	{_, _, {_, Pid3}, _, _} = sys:get_state(Pid4),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	io:format("State 3: ~w\n", [sys:get_state(Pid3)]),
	io:format("State 4: ~w\n", [sys:get_state(Pid4)]).

