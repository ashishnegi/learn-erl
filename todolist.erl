-module(todolist).
-compile(export_all).

-record(todo, {name,
	       description,
	       timer
	      }).

todo(Todo) ->
    receive
	_ -> todo(Todo)
    after Todo#todo.timer ->
	    exit(timer_fired)
    end.

mk_todo(EmPid, Todo) ->
    %% io:format("mk_todo: self() : ~p~n", [self()]),
    spawn_monitor(?MODULE, todo, [Todo]),
    receive
	{'DOWN', _, process, _, timer_fired} ->
	    EmPid ! { timer_fired, Todo };
	Err ->
	    EmPid ! { unknown, Err }
    end.

event_manager(Clients, PidTodoDict) ->
    receive
	{add, Todo} ->
	    TodoPid = spawn(?MODULE, mk_todo, [self(), Todo]),
	    event_manager(Clients, dict:store(TodoPid, Todo, PidTodoDict));
	{add_client, Pid} ->
	    event_manager([Pid | Clients], PidTodoDict);
	{remove_client, Pid} ->
	    NewClients = [Client || Client <- Clients, Client /= Pid],
	    event_manager(NewClients, PidTodoDict);
	{timer_fired, Todo} ->
	    [ClientPid ! {todo_time, Todo} || ClientPid <- Clients],
	    event_manager(Clients, PidTodoDict)
    end.

new_todo(Name, Desc, Timer) ->
    #todo{name = Name,
	  description = Desc,
	  timer = Timer
	 }.

new_client(C_ID) ->
    receive
	{todo_time, Todo} ->
	    io:format("client ~p received : ~p~n", [C_ID, Todo])
    end,
    new_client(C_ID).

test_add() ->
    io:format("client 1 and 2 should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo("Ashish", "first todo", 500),
    PidEm ! {add, T1}.

test_remove() ->
    io:format("client 1 only should get message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidC2 = spawn(todolist, new_client, [2]),
    PidEm = spawn(todolist, event_manager, [[PidC1, PidC2], dict:new()]),
    T1 = todolist:new_todo("Ashish", "first todo", 500),
    PidEm ! {add, T1},
    PidEm ! {remove_client, PidC2}.

test_multiple_todos() ->
    io:format("client 1 should get 2 message~n"),
    PidC1 = spawn(todolist, new_client, [1]),
    PidEm = spawn(todolist, event_manager, [[PidC1], dict:new()]),
    T1 = todolist:new_todo("Ashish", "first todo", 500),
    T2 = todolist:new_todo("Ashish", "second todo", 600),
    PidEm ! {add, T1},
    PidEm ! {add, T2}.
