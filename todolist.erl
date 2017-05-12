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
    spawn_monitor(?MODULE, todo, [Todo]),
    receive
	{'DOWN', _, process, _, timer_fired} ->
	    EmPid ! { timer_fired, Todo };
	Err ->
	    EmPid ! { unknown, Err }
    end.


event_manager(Clients) ->
    receive
	{add, Todo} ->
	    spawn(?MODULE, mk_todo, [self(), Todo]);
	{add_client, Pid} ->
	    event_manager([Pid | Clients]);
	{timer_fired, Todo} ->
	    [ClientPid ! {todo_time, Todo} || ClientPid <- Clients],
	    event_manager(Clients)
    end,
    event_manager(Clients).

new_todo(Name, Desc, Timer) ->
    #todo{name = Name,
	  description = Desc,
	  timer = Timer
	 }.

new_client(C_ID) ->
    receive
	{todo_time, Todo} ->
	    io:format("client ~p received : ~p~n", [C_ID, Todo])
    end.

%% f().
%% PidC1 = spawn(todolist, new_client, [1]).
%% PidC2 = spawn(todolist, new_client, [2]).
%% PidEm = spawn(todolist, event_manager, [[PidC1, PidC2]]).
%% T1 = todolist:new_todo("Ashish", "first todo", 2000).
%% PidEm ! {add, T1}.
