-module (testcases).
-export ([main/1, main3/1]).

main([Type, Repl]) ->
	
	register(master, self()),

	case Type of
		linearizability -> 
			Module = node;
		_ ->
			Module = node_eventual
	end,

	K = list_to_integer(atom_to_list(Repl)),
	T0 = erlang:timestamp(),
	Key_vals = read_insert("insert.txt"),
	%% Sanity check
	% io:format("~p~n", [Key_vals]),

	Pids = join_n(K, 10, Module),
	[Pid1|_] = Pids,

	Random_pids = give_random_elements(500, [], Pids),

	Key_vals_pids = lists:zip(Random_pids, Key_vals),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%
	%% Zhtoumeno 1
	%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	T1 = erlang:timestamp(),
	lists:foreach(
    fun({Pid, [Key, Value]}) ->
    	% io:format("Pid: ~p~n", [Pid]),
    	Module:c_insert(Pid, {Key, Value})
    end,
    Key_vals_pids),
    
	receive_loop(500, insert),
	T2 = erlang:timestamp(),

	T10 = timer:now_diff(T1, T0),
	Diff = timer:now_diff(T2, T1),
    io:format("Ok 500 inserts in ~p ms, total time (containing file parsing and nodes joins) ~p ms\n",[Diff, T10]),


    Query_keys = read_query("query.txt"),

    Query_pids = give_random_elements(500, [], Pids),

    Query_keys_pids = lists:zip(Query_pids, Query_keys),


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%
	%% Zhtoumeno 2
	%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    T3 = erlang:timestamp(),

    lists:foreach(
    fun({Pid, Key}) ->
    	% io:format("Pid: ~p~n", [Pid]),
    	Module:c_query(Pid, Key)
    end,
    Query_keys_pids),

    receive_loop(500, query),
	T4 = erlang:timestamp(), 
	Diff_query = timer:now_diff(T4,T3),   
	io:format("Ok 500 queries in ~p ms\n",[Diff_query]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Zhtoumeno 3
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main3([Type, Repl]) ->
	register(master, self()),

	case Type of
		linearizability -> 
			Module = node;
		_ ->
			Module = node_eventual
	end,

	K = list_to_integer(atom_to_list(Repl)),

	T0 = erlang:timestamp(),
	Command_key_vals = read_requests("requests.txt"),
	%% Sanity check
	% io:format("~p~n", [Key_vals]),

	Pids = join_n(K, 10, Module),
	[Pid1|_] = Pids,

	Random_pids = give_random_elements(500, [], Pids),

	Key_vals_pids = lists:zip(Random_pids, Command_key_vals),

	
	T1 = erlang:timestamp(),
	lists:foreach(
    fun({Pid, {Command, Val}}) ->
    	% io:format("Pid: ~p~n", [Pid]),
    	case Command of
    		insert ->		
    			Module:c_insert(Pid, Val);
    		query ->
    			Module:c_query(Pid, Val)
    	end
    end,
    Key_vals_pids),
    
    Sum = receive_loop2(500,0,#{}),
	T2 = erlang:timestamp(),

	T10 = timer:now_diff(T1, T0),
	Diff = timer:now_diff(T2, T1),
    io:format("Ok 500 requests in ~p, ~p\n",[Diff, T10]),
    io:format("Freshness of values ~p\n",[Sum]).


read_requests(Filename) ->
	Lines = readlines(Filename),
	String_key_vals = string:tokens(Lines, "\n"),
	Key_vals = lists:map(
		fun(String) -> 
			case string:tokens(String, ",") of
				["insert", Key, Value] ->
					{insert, {Key, Value}};
				["query", Key] ->
					{query, Key} 
			end
		end, String_key_vals).

read_query(Filename) ->
	Lines = readlines(Filename),
	Keys = string:tokens(Lines, "\n").

read_insert(Filename) ->
	Lines = readlines(Filename),
	String_key_vals = string:tokens(Lines, "\n"),
	Key_vals = lists:map(
		fun(String) -> string:tokens(String, ",") end, String_key_vals).

give_random_elements(0, X, From) ->
	X;
give_random_elements(N, Ls, From) ->
	Index = random:uniform(length(From)),
	Curr = lists:nth(Index,From),
	give_random_elements(N-1, [Curr|Ls], From).

get_pids(0) ->
	[];
get_pids(N) ->
	receive
		{join, {_Node_id, Pid}} ->
			[Pid] ++ get_pids(N-1)
	after 
		5000 ->
			io:format("Kouradeiros\n", []),
			error
	end.

receive_loop(0, _Msg) ->
	ok;
receive_loop(Cnt, Msg) ->
	receive
		{Msg, Val} ->
			receive_loop(Cnt-1, Msg);
		Whatever ->
			receive_loop(Cnt,Msg)
	after 
		5000 ->
			io:format("Kouradeiros\n", []),
			error
	end.

receive_loop2(0,Accu,M) ->
	Accu;
receive_loop2(Cnt,Accu,M) ->
	receive
		{Command, {Key, Val}} ->
			case Command of
				query ->
					%%io:format("~p result: (~p, ~p)~n", [Command, Key, string:strip(Val)]),
					{X,[]} = string:to_integer(string:strip(Val)),
					Prev = maps:get(Key,M,X), %X is the default value if not exists
					Dif = abs(X-Prev),
					receive_loop2(Cnt-1,Accu+Dif,M);
				insert ->
					{X,[]} = string:to_integer(string:strip(Val)),
					M2 = maps:put(Key, X, M),
					receive_loop2(Cnt-1,Accu,M2)
			end;
		Whatever ->
			receive_loop2(Cnt,Accu,M)
	after 
		5000 ->
			io:format("Kouradeiros\n", []),
			error
	end.


extract_keyVals(FileName) ->
	Lines = readlines(FileName),
	String_key_vals = string:tokens(Lines, "\n"),
	Key_vals = lists:map(
		fun(String) -> string:tokens(String, "\"{},.") end, String_key_vals).
	%% Process Key_vals
	%%Key_vals2 = 

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
      eof  -> [];
      Line -> Line ++ get_all_lines(Device)
  end.

join_n(K, N, Module) ->
	{ok, Pid1} = Module:c_start_link(1, K),
	[Pid1] ++ join(Pid1, K, N+1, Module).

join(_Pid1, _K, 1, _Module) ->
	[];
join(Pid1, K, N, Module) ->
	Module:c_join(Pid1, N),
	receive
		{join, {_Node_id, Pid}} ->
			[Pid] ++ join(Pid1, K, N-1, Module)
	after 
		5000 ->
			io:format("Kouradeiros\n", []),
			error
	end.