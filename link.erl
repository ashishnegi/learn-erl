-module(link).
-compile(export_all).

oracle() ->
    receive
	{Pid, ReqRef, "Who am I ?"} ->
	    Pid ! { ReqRef, "You are not that..." };
	{Pid, ReqRef, "Why i am here?"} ->
	    Pid ! { ReqRef, "To find your answers" };
	{Pid, ReqRef, _} ->
	    Pid ! { ReqRef, "Ask something else.." }
    end,
    oracle().

oracle_reincarnate() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, oracle, []),
    register(oracle, Pid),
    receive
	{'EXIT', Pid, normal} ->
	    ok;
	{'EXIT', Pid, shudown} ->
	    ok;
	_ ->
	    oracle_reincarnate()
    end.

ask_oracle(Msg) ->
    ReqRef = make_ref(),
    oracle ! { self(), ReqRef, Msg},
    receive
	{ReqRef, Res} ->
	    Res
    after 1000 ->
	    timeout
    end.

start_oracle() ->
    spawn(?MODULE, oracle_reincarnate, []).

%% 2> link:start_oracle().
%% <0.40.0>
%% 3> link:ask_oracle("Who am I ?").
%% "You are not that..."
%% 4> link:ask_oracle("Why i am here?").
%% "To find your answers"
%% 5> link:ask_oracle("Tell me those answers..").
%% "Ask something else.."
%% 6>
