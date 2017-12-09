-module(dis_lib).
-export ([hash/1]).



hash(Key) ->
	Data = io_lib:format("~p", [Key]),
	Binary = crypto:hash(sha512, Data),
	crypto:bytes_to_integer(Binary).
