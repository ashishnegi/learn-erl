-module(hitchhiker).
-compile(export_all).

spawn_print() ->
    [spawn(fun() -> io:format("~p~n", [X]) end) || X <- lists:seq(1,10)].

send_msg(Msg) ->
    self() ! Msg.

santa() ->
    receive
	{From, tell_a_joke} ->
	    From ! "Do you think i am a joker.. ?",
	    santa();
	{From, banta} ->
	    From ! "Wanna make a joke.. ?",
	    santa();
	{From, die} ->
	    From ! "dieing.."
    end.

start_santa() ->
    spawn(hitchhiker, santa, []).

chain(0) ->
    receive
	M -> io:format("~p~n", [M])
    after 1000 ->
	    see_you
    end;
chain(N) ->
    if (N >= 0) and (N =< 10) ->
	    Pid = spawn_link(fun() -> chain(N-1) end),
	    receive
		msg -> Pid ! msg
	    after (10 - N) * 100 ->
		    Pid ! "stop"
	    end
    end.

%% For this reason, the function spawn_link/1-3 has been added to the language.
%% It takes the same arguments as spawn/1-3, creates a process and links it as
%% if link/1 had been there, except it's all done as an atomic operation
%% (the operations are combined as a single one, which can either fail or
%% succeed, but nothing else). This is generally considered safer and you save
%% a set of parentheses too.
