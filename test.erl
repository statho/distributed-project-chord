-module(test).
-export ([c_join_test/0, c_depart_test/0, c_insert_test/0,
				  extreme_depart_test/0]).

c_join_test() ->
	{ok, Pid1} = node:c_start_link(1, 10),
	% io:format("Node 1 pid: ~w\n", [Pid1]),
	% node:c_insert(Pid1, {1, atomo}),
	% io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	timer:sleep(500),
	node:c_join(Pid1, 2),
	{_, _, {_, Pid2}, _, _, _} = sys:get_state(Pid1),
	% io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	% io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	timer:sleep(500),
	node:c_join(Pid1, 3),
	{_, _, {_, Pid3}, _, _, _} = sys:get_state(Pid1),
	% io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	% io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	% io:format("State 4: ~w\n", [sys:get_state(Pid4)]),

	timer:sleep(500),
	node:c_join(Pid1, 4),
	timer:sleep(500),
	{_, _, {_, Pid4}, _, _, _} = sys:get_state(Pid1),
	% io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	% io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	% io:format("State 3: ~w\n", [sys:get_state(Pid3)]),
	% io:format("State 4: ~w\n", [sys:get_state(Pid4)]),
	{ok, [Pid1,Pid2, Pid3, Pid4]}.




c_depart_test() ->
	{ok, [Pid1,Pid2, Pid3, Pid4]} = c_join_test(),
	node:c_depart(Pid1, 3),
	timer:sleep(500),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	% io:format("State 3: ~w\n", [sys:get_state(Pid3)]),
	io:format("State 4: ~w\n", [sys:get_state(Pid4)]),
	node:c_depart(Pid1, 2),
	timer:sleep(500),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	%io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	% io:format("State 3: ~w\n", [sys:get_state(Pid3)]),
	io:format("State 4: ~w\n", [sys:get_state(Pid4)]),
	node:c_depart(Pid1, 4),
	timer:sleep(500),
	io:format("State 1: ~w\n", [sys:get_state(Pid1)]),
	node:c_depart(Pid1, 1),
	timer:sleep(500).
	%io:format("State 2: ~w\n", [sys:get_state(Pid2)]),
	% io:format("State 3: ~w\n", [sys:get_state(Pid3)]),
	% io:format("State 4: ~w\n", [sys:get_state(Pid4)]),

c_insert_test() ->
	{ok, [Pid1,_, _, _]} = c_join_test(),
	Pid2 = list_to_pid("<0.33.0>"),
	Pid3 = list_to_pid("<0.34.0>"),
	Pid4 = list_to_pid("<0.35.0>"),
	% print_state([Pid1, Pid2, Pid3, Pid4]),
	io:format("Pid1: ~p~n", [Pid1]),

	timer:sleep(500),
	node:c_insert(Pid1, {4, ena}),
	timer:sleep(500),
	% node:c_query(Pid1, "*"),
	node:c_insert(Pid1, {2, dio}),
	timer:sleep(1000),

	% node:c_query(Pid1, "*"),

	timer:sleep(1000),

	node:c_insert(Pid1, {3, tria}),
	node:c_insert(Pid1, {4, tessera}),
	node:c_insert(Pid1, {5, pente}),
	node:c_insert(Pid1, {10, pente}),
	% print_state([Pid1,Pid2, Pid3, Pid4]),
	timer:sleep(500),

	% node:c_query(Pid1, "*"),

	timer:sleep(500),


	node:c_insert(Pid1, {1, miden}),
	% print_state([Pid1,Pid2, Pid3, Pid4]),
	% node:c_query(Pid1, "*"),


	node:c_query(Pid1, 1),
	timer:sleep(500),
	% node:c_query(Pid1, 2),
	timer:sleep(500),
	% node:c_query(Pid1, 3),
	timer:sleep(500),
	% node:c_query(Pid1, 4),
	timer:sleep(500),
	% node:c_query(Pid1, 5),
	
	node:c_query(Pid1, "*"),

	% node:c_query(Pid1, 6),

	timer:sleep(500),


	node:c_join(Pid1, 5),
	% {_, _, {_, Pid5}, _, _, _} = sys:get_state(Pid1),
	% print_state([Pid1,Pid2, Pid3, Pid4, Pid5]),

	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),

	node:c_join(Pid1, 15),
	% {_, _, {_, Pid15}, _, _, _} = sys:get_state(Pid1),
	% print_state([Pid1,Pid2, Pid3, Pid4, Pid5, Pid15]),
	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),


	node:c_delete(Pid1, 1),
	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),
	node:c_delete(Pid1, 2),
	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),
	node:c_delete(Pid1, 3),
	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),
	node:c_delete(Pid1, 4),
	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(500),
	node:c_delete(Pid1, 5),

	timer:sleep(500),
	node:c_query(Pid1, "*"),
	timer:sleep(1000).
	% print_state([Pid1,Pid2, Pid3, Pid4, Pid5, Pid15]).

extreme_depart_test() ->
		{ok, [Pid1,Pid2, Pid3, Pid4]} = c_join_test(),
	% print_state([Pid1,Pid2, Pid3, Pid4]),

	node:c_insert(Pid1, {1, ena}),
	node:c_insert(Pid1, {2, dio}),
	node:c_insert(Pid1, {3, tria}),
	node:c_insert(Pid1, {4, tessera}),
	node:c_insert(Pid1, {5, pente}),
	node:c_insert(Pid1, {10, deka}),
	% print_state([Pid1,Pid2, Pid3, Pid4]),
	timer:sleep(500),

	node:c_insert(Pid1, {1, miden}),
	% print_state([Pid1,Pid2, Pid3, Pid4]),
	timer:sleep(500),

	node:c_query(Pid1, 1),
	timer:sleep(500),
	node:c_query(Pid1, 2),
	timer:sleep(500),
	node:c_query(Pid1, 3),
	timer:sleep(500),
	node:c_query(Pid1, 4),
	timer:sleep(500),
	node:c_query(Pid1, 5),
	timer:sleep(500),

	
	node:c_query(Pid1, "*"),
	timer:sleep(500),

	node:c_query(Pid1, 6),

	timer:sleep(500),


	node:c_join(Pid1, 5),
	{_, _, {_, Pid5}, _, _, _} = sys:get_state(Pid1),
	% print_state([Pid1,Pid2, Pid3, Pid4, Pid5]),

	node:c_join(Pid1, 15),
	{_, _, {_, Pid15}, _, _, _} = sys:get_state(Pid1),
	% print_state([Pid1,Pid2, Pid3, Pid4, Pid5, Pid15]),

	timer:sleep(500),

	node:c_depart(Pid1, 3),
	timer:sleep(500),
	node:c_depart(Pid1, 15),
	timer:sleep(500),
	node:c_depart(Pid1, 2),
	timer:sleep(500),
	node:c_depart(Pid1, 4),
	timer:sleep(500),

	node:c_depart(Pid1, 5),
	timer:sleep(500),

	% print_state([Pid1]),

	node:c_depart(Pid1, 1),
	timer:sleep(500).



print_state([]) ->
	ok;
print_state([P1|Ps]) ->
	try 
		io:format("State: ~w\n", [sys:get_state(P1)])
	catch
		Error:Reason ->
			io:format("~w ~w ~w\n", [P1, Error, Reason])	
	end,
	print_state(Ps).

