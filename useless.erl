-module(useless).
-export([add/2, greet/2]).
-author("Erlang ninja").

add(A,B) ->
    A + B.


%% 23> cd("~").
%% /home/ashishnegi/Documents/erlang/learn-erl
%% ok
%% 25> pwd().
%% /home/ashishnegi/Documents/erlang/learn-erl
%% ok
%% c("useless.erl", [debug_info]).
%% {ok,useless}
%% 36> useless:module_info().
%% [{module,useless},
%%  {exports,[{add,2},{module_info,0},{module_info,1}]},
%%  {attributes,[{vsn,[124783009251891948846779862839079774773]}]},
%%  {compile,[{options,[debug_info]},
%%            {version,"6.0.3"},
%%            {time,{2017,5,10,4,40,23}},
%%            {source,"/home/ashishnegi/Documents/erlang/learn-erl/useless.erl"}]},
%%  {md5,<<93,224,85,90,39,134,26,20,122,153,190,231,112,
%%         216,170,53>>}]
%% 40> useless:module_info(exports).
%% [{add,2},{module_info,0},{module_info,1}]
%% 41> EX = useless:module_info(exports).
%% [{add,2},{module_info,0},{module_info,1}]



%% Function clauses must be separated by semicolons (;)
%% and together form a function declaration.
greet(male, Name) ->
    io:format("hello Mr. ~s!~n", [Name]);
greet (female, Name) ->
    io:format("hello Mrs. ~s!!~n", [Name]);
greet (_, Name) ->
    io:format("hello ~s!!!~n", [Name]).
