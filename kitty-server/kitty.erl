-module(kitty).
-behaviour(gen_server).
-export([test_boss/0, test_boss_servant/0,
	 init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-record(cat, {name, color=green, description}).

%%% Private functions
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

%%% Client API
start_link() -> gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return_cat, Cat}).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

handle_cast({return_cat, Cat = #cat{}}, Cats) ->
    {noreply, [Cat|Cats]}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
    if Cats =:= [] ->
	    {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] -> % got to empty the stock
	    {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_info(Msg, Cats) ->
    io:format("Unexpected msg ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
    ok.

code_change(_Old, State, _Extra)->
    {ok, State}.

test_boss()->
    {ok, Server} = start_link(),
    Boss = order_cat(Server, "Boss cat", "Red", "Bossing around"),
    order_cat(Server, "Servant cat", "White", "Yes boss.."),
    return_cat(Server, Boss),
    close_shop(Server).

test_boss_servant()->
    {ok, Server} = start_link(),
    Boss = order_cat(Server, "Boss cat", "Red", "Bossing around"),
    Servant = order_cat(Server, "Servant cat", "White", "Yes boss.."),
    return_cat(Server, Servant),
    return_cat(Server, Boss),
    close_shop(Server).
