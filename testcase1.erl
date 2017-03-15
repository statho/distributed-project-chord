-module (testcase1).
-export ([main/0]).

main() ->
	Lines = readlines("insert.txt"),
	String_key_vals = string:tokens(Lines, "\n"),
	Key_vals = lists:map(
		fun(String) -> string:tokens(String, ",") end, String_key_vals),
	%% Sanity check
	% io:format("~p~n", [Key_vals]),

	Pid1 = join_n(1, 10),

	lists:foreach(
    fun([Key, Value]) ->
    	node:c_insert(Pid1, {Key, Value})
    end,
    Key_vals).

	%% TODO: 
	%% 1) Name this printer
	%% 2) Make return value correcter



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

join_n(K, N) ->
	{ok, Pid1} = node:c_start_link(1, K),
	join(Pid1, K, N+2),
	Pid1.

join(_Pid1, _K, 1) ->
	ok;
join(Pid1, K, N) ->
	timer:sleep(500),
	node:c_join(Pid1, N),
	join(Pid1, K, N-1).


