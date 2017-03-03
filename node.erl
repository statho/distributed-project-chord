-module(node).
-behaviour(gen_server).

-export([start_link/1, insert/2, query/2, 
         delete/2, change_previous/2, change_next/2,
         join/2]).
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { id
               , prev
               , next
               , data = maps:new()
               }).

-define(MAX_KEY, 100).

%%% Client API
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%% This call is asynchronous
insert(Pid, {Key, Value}) ->
    gen_server:cast(Pid, {insert, {Key, Value}}).

%% This call is asynchronous
query(Pid, Key) ->
    gen_server:cast(Pid, {query, Key}).

%% This call is asynchronous
delete(Pid, Key) ->
    gen_server:cast(Pid, {delete, Key}).

change_next(Pid, Bound) ->
    gen_server:cast(Pid, {change_next, Bound}).

change_previous(Pid, Bound) ->
    gen_server:cast(Pid, {change_previous, Bound}).

join(Pid, New_node) ->
    gen_server:cast(Pid, {join, New_node}).


%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([Id]) -> 
    {ok, #state{id=Id, prev= {Id, self()}, next= {Id, self()}}}. 

handle_cast({insert, {Key, Value}}, S = #state{id=Id, prev={Prev, _}, data=Data}) ->
    case check_bounds(Key, {Prev, S#state.id}) of
        true ->
            Data1 = maps:put(Key, Value, Data),
            {noreply, S#state{data=Data1}};
        _ ->
            {_, Next_pid} = S#state.next,
            insert(Next_pid, {Key, Value}),
            {noreply, S}
    end;
    
handle_cast({query, Key}, S = #state{id=Id, prev={Prev, _} , data=Data}) ->
    case Key of
        "*" ->
            Value = maps:to_list(Data),
            return_value(Value),
            {_, Next_pid} = S#state.next,
            query(Next_pid, Key);
        _ ->
            case check_bounds(Key, {Prev, S#state.id}) of
                true ->
                    Value = get_from_data(Key, Data),
                    return_value(Value);
                _ ->
                    {_, Next_pid} = S#state.next,
                    query(Next_pid, Key)
            end
    end,
    {noreply, S};
handle_cast({join, {Node_id, Node_Pid, My_id}}, S = #state{id=My_id}) ->
    {noreply, S};
handle_cast({join, {Node_id, Node_Pid, Identif}}, S = #state{prev={Prev, _}}) ->
    case Identif of
        undefined ->
            {ok, Pid} = start_link(Node_id),
            Identif1 = S#state.id;
        _ ->
            Pid = Node_Pid,
            Identif1 = Identif
    end,
    case check_bounds(Node_id, {Prev, S#state.id}) of
        true ->
            State1 = new_state_from_join({Node_id, Pid}, S);
        _ ->
            State1 = S
    end,
    join(S#state.next, {Node_id, Pid, Identif1}),
    {noreply, State1};


handle_cast({change_next, {Node_id, Pid}}, S) ->    
    {noreply, S#state{next={Node_id, Pid}}};

handle_cast({change_previous, {Node_id, Pid}}, S) ->    
    {noreply, S#state{prev={Node_id, Pid}}}.


handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(normal, _Cats) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 


%% Private functions
check_bounds(Key, {Prev, Right}) ->
    Left = (Prev + 1) rem ?MAX_KEY,
    case Left < Right of
        true ->
            Key =< Right andalso Key >= Left;
        _ ->
            Key =< Right orelse Key >= Left
    end.


new_state_from_join({Node_id, Pid},  S = #state{prev={Prev_id, Prev_pid}}) ->
    New_prev = {Node_id, Pid},
    change_previous(Pid, {Prev_id, Prev_pid}),
    change_next(Pid, {S#state.id, self()}),

    change_next(Prev_pid, New_prev),    
    
    S#state{prev=New_prev}.

return_value(Value) ->
    io:format("~w\n", [Value]).

get_from_data(Key, Data) ->
    maps:get(Key, Data, not_found).