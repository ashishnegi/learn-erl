-module(todolist).
-compile(export_all).

-record(todo, {name,
	       description,
	       timer,
	       key %% currently pid
	      }).

mk_todo(EmPid, Todo) ->
    receive
	cancel ->
	    EmPid ! {done, cancel, self()};
	_ ->
	    mk_todo(EmPid, Todo)
    after Todo#todo.timer ->
	    %% io:format("todo: ~p timer..~n", [Todo]),
	    EmPid ! {timer_fired, Todo}
    end.

event_manager(Clients, TodoIdPidDict) ->
    %% io:format("ev_mg : clients ~p, todo->pid ~p~n", [Clients, TodoIdPidDict]),
    receive
	{add_todo, Pid, Todo} ->
	    case dict:find(Todo#todo.key, TodoIdPidDict) of
		error ->
		    Pid ! {done, {add_todo, Pid, Todo}},
		    TodoPid = spawn(?MODULE, mk_todo, [self(), Todo]),
		    event_manager(Clients, dict:store(Todo#todo.key,
						      TodoPid, TodoIdPidDict));
		{ok, _} ->
		    Pid ! {error, {add_todo, Pid, Todo}, "Already registered"},
		    event_manager(Clients, TodoIdPidDict)
	    end;

	{delete_todo, Pid, Todo} ->
	    TodoId = Todo#todo.key,
	    case dict:find(TodoId, TodoIdPidDict) of
		{ok, TodoPid} ->
		    Pid ! {done, {delete_todo, Pid, Todo}},
		    %% kill process
		    TodoPid ! cancel,
		    event_manager(Clients, dict:erase(TodoId, TodoIdPidDict));
		_ ->
		    Pid ! {error, {delete_todo, Pid, Todo}, "Unknown Todo"},
		    event_manager(Clients, TodoIdPidDict)
	    end;

	{add_client, Pid} ->
	    Pid ! { done, { add_client, Pid }},
	    event_manager([Pid | Clients], TodoIdPidDict);
	{remove_client, Pid} ->
	    NewClients =
		case lists:member(Pid, Clients) of
		    true ->
			Pid ! {done, {remove_client, Pid}},
			[Client || Client <- Clients, Client /= Pid];
		    false ->
			Pid ! {error, {remove_client, Pid}, "Unknown client"},
			Clients
		end,
	    event_manager(NewClients, TodoIdPidDict);

	{timer_fired, Todo} ->
	    case dict:find(Todo#todo.key, TodoIdPidDict) of
		{ok, _} ->
		    [ClientPid ! {todo_time, Todo} || ClientPid <- Clients],
		    event_manager(Clients, dict:erase(Todo#todo.key, TodoIdPidDict));
		error ->
		    io:format("timer_fired not found.. ~p~n", [Todo]),
		    event_manager(Clients, TodoIdPidDict)
	    end;
	{done, cancel, _TodoPid} ->
	    event_manager(Clients, TodoIdPidDict);

	code_reload ->
	    ?MODULE:event_manager(Clients,TodoIdPidDict);

	OtherMsg ->
	    io:format("ev_mg : Other msg : ~p~n", [OtherMsg]),
	    event_manager(Clients, TodoIdPidDict)
    end.

new_todo(Key, Name, Desc, Timer) ->
    #todo{name = Name,
	  description = Desc,
	  timer = Timer,
	  key = Key
	 }.

new_client(C_ID) ->
    receive
	{todo_time, Todo} ->
	    io:format("client ~p received : ~p~n", [C_ID, Todo]);
	_ ->
	    %% io:format("client ~p received : ~p~n", [C_ID, OtherMsg])
	    {}
    end,
    new_client(C_ID).

teardown(Processes) ->
    timer:sleep(1000),
    [exit(Process, shutdown) || Process <- Processes].

test_add() ->
    io:format("test_add # client 1 and 2 should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    PidEm ! {add_todo, PidC1, T1},
    teardown([PidEm, PidC1, PidC2]).

test_multiple_todos() ->
    io:format("test_multiple_todos # client 1 should get 2 message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidEm = spawn(todolist, event_manager, [[PidC1], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    T2 = todolist:new_todo(2, "Ashish", "second todo", 600),
    PidEm ! {add_todo, PidC1, T1},
    PidEm ! {add_todo, PidC1, T2},
    teardown([PidC1, PidEm]).

test_delete_todo() ->
    io:format("test_delete_todo # No client should get messages~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidEm = spawn(todolist, event_manager, [[PidC1], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    T2 = todolist:new_todo(2, "Ashish", "second todo", 600),
    PidEm ! {add_todo, PidC1, T1},
    PidEm ! {add_todo, PidC1, T2},
    PidEm ! {delete_todo, PidC1, T2},
    PidEm ! {delete_todo, PidC1, T1},
    teardown([PidC1, PidEm]).

test_remove_client() ->
    io:format("test_remove_client # client 1 only should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    PidEm ! {add_todo, PidC1, T1},
    PidEm ! {remove_client, PidC2},
    teardown([PidC1, PidEm, PidC2]).

all_test() ->
    BeforeTestProcessCount = length(erlang:processes()),
    test_add(),
    test_delete_todo(),
    test_multiple_todos(),
    test_remove_client(),
    timer:sleep(1000),
    BeforeTestProcessCount == length(erlang:processes()).
