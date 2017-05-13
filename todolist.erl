-module(todolist).
-compile(export_all).

-record(todo, {name,
	       description,
	       timer,
	       todo_key %% currently pid
	      }).

todo(Todo) ->
    receive
	_ -> todo(Todo)
    after Todo#todo.timer ->
	    exit(timer_fired)
    end.

mk_todo(EmPid, TodoArg) ->
    %% io:format("mk_todo: self() : ~p~n", [self()]),
    Todo = TodoArg#todo {
	     todo_key = self()
	    },
    spawn_monitor(?MODULE, todo, [Todo]),
    receive
	{'DOWN', _, process, _, timer_fired} ->
	    EmPid ! { timer_fired, Todo };
	Err ->
	    EmPid ! { unknown, Err }
    end.

event_manager(Clients, TodoIdPidDict) ->
    %% io:format("ev_mg : clients ~p, todo->pid ~p~n", [Clients, TodoIdPidDict]),
    receive
	{add_todo, Todo} ->
	    TodoPid = spawn(?MODULE, mk_todo, [self(), Todo]),
	    event_manager(Clients, dict:store(Todo#todo.todo_key,
					      TodoPid, TodoIdPidDict));
	{delete_todo, Todo} ->
	    TodoId = Todo#todo.todo_key,
	    case dict:find(TodoId, TodoIdPidDict) of
		{ok, TodoPid} ->
		    %% kill process
		    exit(TodoPid, shutdown),
		    event_manager(Clients, dict:erase(TodoId, TodoIdPidDict));
		_ ->
		    event_manager(Clients, TodoIdPidDict)
	    end;

	{add_client, Pid} ->
	    event_manager([Pid | Clients], TodoIdPidDict);
	{remove_client, Pid} ->
	    NewClients = [Client || Client <- Clients, Client /= Pid],
	    event_manager(NewClients, TodoIdPidDict);

	{timer_fired, Todo} ->
	    [ClientPid ! {todo_time, Todo} || ClientPid <- Clients],
	    event_manager(Clients, dict:erase(Todo#todo.todo_key, TodoIdPidDict))
    end.

new_todo(Key, Name, Desc, Timer) ->
    #todo{name = Name,
	  description = Desc,
	  timer = Timer,
	  todo_key = Key
	 }.

new_client(C_ID) ->
    receive
	{todo_time, Todo} ->
	    io:format("client ~p received : ~p~n", [C_ID, Todo])
    end,
    new_client(C_ID).

test_add() ->
    io:format("test_add # client 1 and 2 should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    PidEm ! {add_todo, T1}.

test_multiple_todos() ->
    io:format("test_multiple_todos # client 1 should get 2 message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidEm = spawn(todolist, event_manager, [[PidC1], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    T2 = todolist:new_todo(2, "Ashish", "second todo", 600),
    PidEm ! {add_todo, T1},
    PidEm ! {add_todo, T2}.

test_delete_todo() ->
    io:format("test_delete_todo # No client should get messages~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidEm = spawn(todolist, event_manager, [[PidC1], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    T2 = todolist:new_todo(2, "Ashish", "second todo", 600),
    PidEm ! {add_todo, T1},
    PidEm ! {add_todo, T2},
    PidEm ! {delete_todo, T2},
    PidEm ! {delete_todo, T1}.

test_remove_client() ->
    io:format("test_remove_client # client 1 only should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo(1, "Ashish", "first todo", 500),
    PidEm ! {add_todo, T1},
    PidEm ! {remove_client, PidC2}.

all_test() ->
    %% sleep only to make sense of output..
    %% tests donot have any asserts but only i check for print statements.. :(
    test_add(),
    timer:sleep(1000),
    test_delete_todo(),
    timer:sleep(1000),
    test_multiple_todos(),
    timer:sleep(1000),
    test_remove_client().
