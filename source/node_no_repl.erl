-module(node_no_repl).
-behaviour(gen_server).

-export([c_start_link/1, c_insert/2, c_query/2, 
         c_delete/2, change_previous/2, change_next/2,
         c_join/2, c_depart/2]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, 
         terminate/2, code_change/3]).

-record(state, { id
               , prev
               , next
               , data = maps:new()
               }).

-define(MAX_KEY, 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096).


%%% Client API

c_start_link(Id) ->
    Hid = dis_lib:hash(Id),
    start_link(Hid).

c_insert(Pid, {Key, Value}) ->
    Hkey = dis_lib:hash(Key),
    % io:format("Hashed key: ~p\n", [Hkey]),
    insert(Pid, {Hkey, {Key,Value}}).

%% This call is asynchronous
c_query(Pid, "*") ->
    query(Pid, {"*", undefined});
c_query(Pid, Key) ->
    Hkey = dis_lib:hash(Key),
    % io:format("Hashed key: ~p\n", [Hkey]),
    query(Pid, {Hkey, undefined}).    

%% This call is asynchronous
c_delete(Pid, Key) ->
    Hkey = dis_lib:hash(Key),
    delete(Pid, Hkey).

c_join(Pid, New_node) ->
    Hnode = dis_lib:hash(New_node),
    join(Pid, {Hnode, undefined, undefined}).

c_depart(Pid, Node_id) ->
    Hnode = dis_lib:hash(Node_id),
    depart(Pid, {Hnode, undefined, undefined}).

%% -----------------------------------------------------


start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

%% This call is asynchronous
insert(Pid, {Hkey, {Key, Value}}) ->
    gen_server:cast(Pid, {insert, {Hkey, {Key, Value}}}).

%% This call is asynchronous
query(Pid, Key) ->
    gen_server:cast(Pid, {query, Key}).

%% This call is asynchronous
delete(Pid, Key) ->
    gen_server:cast(Pid, {delete, Key}).

change_next(Pid, Bound) ->
    gen_server:call(Pid, {change_next, Bound}).

change_previous(Pid, Bound) ->
    gen_server:call(Pid, {change_previous, Bound}).

add_key_values(Pid, Map) ->
    gen_server:call(Pid, {add_key_values, Map}).

join(Pid, New_node) ->
    gen_server:cast(Pid, {join, New_node}).

depart(Pid, Node_id) ->
    gen_server:cast(Pid, {depart, Node_id}).


%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([Id]) -> 
    {ok, #state{id=Id, prev= {Id, self()}, next= {Id, self()}}}. 

handle_cast({insert, {Hkey, {Key, Value}}}, S = #state{id=Id, prev={Prev, _}, data=Data}) ->
    case check_bounds(Hkey, {Prev, S#state.id}) of
        true ->
            Data1 = maps:put(Hkey, {Key, Value}, Data),
            {noreply, S#state{data=Data1}};
        _ ->
            {_, Next_pid} = S#state.next,
            insert(Next_pid, {Hkey, {Key, Value}}),
            {noreply, S}
    end;

handle_cast({query, {"*", Id}}, S = #state{id=Id, prev={Prev, _} , data=Data}) ->
    {noreply, S};
handle_cast({query, {Key, First_id}}, S = #state{id=Id, prev={Prev, _} , data=Data}) ->
    case Key of
        "*" ->
            Value = maps:values(Data),
            return_value(Value),
            {_, Next_pid} = S#state.next,
            case First_id of
                undefined ->
                    query(Next_pid, {Key, Id});
                _ ->
                    query(Next_pid, {Key, First_id})
            end;
        _ ->
            case check_bounds(Key, {Prev, S#state.id}) of
                true ->
                    Value = get_from_data(Key, Data),
                    return_value(Value);
                _ ->
                    {_, Next_pid} = S#state.next,
                    query(Next_pid, {Key, First_id})
            end
    end,
    {noreply, S};

handle_cast({delete, Key}, S = #state{id=Id, prev={Prev, _} , data=Data}) ->
    case check_bounds(Key, {Prev, S#state.id}) of
        true ->
            Data1 = delete_from_data(Key, Data);
        _ ->
            {_, Next_pid} = S#state.next,
            delete(Next_pid, Key),
            Data1 = Data
    end,
    {noreply, S#state{data = Data1}};

handle_cast({join, {Node_id, Node_Pid, My_id}}, S = #state{id=My_id}) ->
    % io:format(standard_error, "Cycle over\n", []),
    {noreply, S};
handle_cast({join, {Node_id, Node_Pid, Identif}}, S = #state{prev={Prev, _}, next={Next, NextPid}}) ->
    % io:format(standard_error, " - Pid: ~w received message: ~w\n", [self(), {join, {Node_id, Node_Pid, Identif}}]),
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
            % io:format(standard_error, "yes!\n", []),
            State1 = new_state_from_join({Node_id, Pid}, S);
        _ ->
            % io:format(standard_error, "no!\n", []),
            State1 = S,
            join(NextPid, {Node_id, Pid, Identif1})
    end,
    {noreply, State1};

handle_cast({depart, My_id}, S = #state{id=My_id, prev={Prev_id, Prev_pid}, next={Next_id, Next_pid}}) ->
    case Next_id of
        My_id ->
            {stop, normal, S};
        _ ->
            change_previous(Next_pid, {Prev_id, Prev_pid}),
            change_next(Prev_pid, {Next_id, Next_pid}),

            add_key_values(Next_pid, S#state.data),
            {stop, normal, S}
    end;
handle_cast({depart, Node_id}, S = #state{next={_, Next_pid}}) ->
    depart(Next_pid, Node_id),
    {noreply, S}.


handle_call({add_key_values, New_data}, _From, S = #state{data=Data}) -> 
    Total_data = maps:merge(Data, New_data),   
    {reply, ok, S#state{data=Total_data}};

handle_call({change_next, {Node_id, Pid}}, _From, S) -> 
    % io:format("Change next\n",[]),   
    {reply, ok, S#state{next={Node_id, Pid}}};

handle_call({change_previous, {Node_id, Pid}}, _From, S) ->    
    % io:format("Change previous\n",[]),
    {reply, ok, S#state{prev={Node_id, Pid}}}.


handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(normal, S = #state{id=My_id}) ->
    io:format("Node ~w departed :'(\n", [My_id]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 


%% Private functions
check_bounds(Key, {Prev, Right}) ->
    % io:format(standard_error, "CHeck bounds: ~w\n", [{Key, {Prev, Right}}]),
    Left = (Prev + 1) rem ?MAX_KEY,
    %% changed from Left < Right
    case Left =< Right of
        true ->
            Key =< Right andalso Key >= Left;
        _ ->
            Key =< Right orelse Key >= Left
    end.


new_state_from_join({Node_id, Pid}, S = #state{prev={Prev_id, Prev_pid}}) ->
    New = {Node_id, Pid},
    change_previous(Pid, {Prev_id, Prev_pid}),
    change_next(Pid, {S#state.id, self()}),

    Data = S#state.data,
    Keys = find_keys_in_range({Prev_id, Node_id}, Data),
    
    To_transfer = maps:with(Keys, Data),
    To_keep = maps:without(Keys, Data),
    add_key_values(Pid, To_transfer),

    case self() of
        Prev_pid ->
            New_next={Node_id, Pid};
        _ ->
            change_next(Prev_pid, New),
            New_next=S#state.next   
    end,
    S#state{prev=New, next=New_next, data=To_keep}.

find_keys_in_range({Start, End}, Data) ->
    Filtered = maps:filter(
        fun(Key, Val) ->
            check_bounds(Key, {Start, End})
        end, Data),
    Keys = maps:keys(Filtered).

return_value(Value) ->
    io:format("~w\n", [Value]).

delete_from_data(Key, Data) ->
    maps:remove(Key, Data).

get_from_data(Key, Data) ->
    % io:format("Key: ~p\n Data: ~p\n", [Key, Data]),
    case maps:get(Key, Data, not_found) of
        not_found ->
            not_found;
        {_, Val} ->
            Val
    end.

fst({X,_}) -> X.
snd({_,X}) -> X.    