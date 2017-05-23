-module(kitty).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1,
	 test_boss/0, test_boss_servant/0]).

-record(cat, {name, color=green, description}).

%%% Generic API
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Msg},
    receive
	{'DOWN', Ref, process, Pid, Reason} ->
	    erlang:error(Reason);
	{Ref, Reply} ->
	    erlang:demonitor(Ref, [flush]),
	    Reply
    after 5000 ->
	    erlang:error(timeout)
    end.


%%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%% Synchronous call
close_shop(Pid) ->
    call(Pid, terminate).

%%% Server functions
init() -> loop([]).

loop(Cats) ->
    receive
	{Pid, Ref, {order, Name, Color, Description}} ->
	    if Cats =:= [] ->
		    Pid ! {Ref, make_cat(Name, Color, Description)},
		    loop(Cats);
	       Cats =/= [] -> % got to empty the stock
		    Pid ! {Ref, hd(Cats)},
		    loop(tl(Cats))
	    end;
	{return, Cat = #cat{}} ->
	    loop([Cat|Cats]);
	{Pid, Ref, terminate} ->
	    Pid ! {Ref, ok},
	    terminate(Cats);
	Unknown ->
	    %% do some logging here too
	    io:format("Unknown message: ~p~n", [Unknown]),
	    loop(Cats)
    end.

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.


test_boss()->
    Server = kitty:start_link(),
    Boss = order_cat(Server, "Boss cat", "Red", "Bossing around"),
    order_cat(Server, "Servant cat", "White", "Yes boss.."),
    return_cat(Server, Boss),
    close_shop(Server).

test_boss_servant()->
    Server = kitty:start_link(),
    Boss = order_cat(Server, "Boss cat", "Red", "Bossing around"),
    Servant = order_cat(Server, "Servant cat", "White", "Yes boss.."),
    return_cat(Server, Servant),
    return_cat(Server, Boss),
    close_shop(Server).
