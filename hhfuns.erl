-module(hhfuns).
-compile(export_all).

map(_, []) ->
    [];
map(F, [H|T]) ->
    [F(H)|map(F, T)].

%% c("hhfuns").
%% hhfuns:map(fun(X) -> X + 1 end, lists:seq(1,9)).

fold(_, Acc, []) ->
    lists:reverse(Acc);
fold(F, Acc, [H|T]) ->
    fold(F, F(Acc,H), T).

%% hhfuns:fold(fun(Acc,X) -> Acc + X end, 0, lists:seq(0,10)).

filter(F, List) ->
    PredFn = fun(AccList, V) ->
		     case F(V) of
			 true -> [V | AccList];
			 false -> AccList
		     end
	     end,
    fold(PredFn, [], List).

%% hhfuns:filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).

my_catch_all(F) ->
    try F() of
	V -> V
    catch
	error:Error ->
	    { my_error, Error };
	Throw ->
	    { my_throw, Throw };
	exit:Exit ->
	    { my_exit, Exit }
    end.

%% 7> hhfuns:my_catch_all(fun () -> 1 end).
%% 1
%% 8> hhfuns:my_catch_all(fun () -> 100 end).
%% 100
%% 9> hhfuns:my_catch_all(fun () -> throw("everything is fine."), end).
%% * 1: syntax error before: 'end'
%% 9> hhfuns:my_catch_all(fun () -> throw("everything is fine.") end).
%% {my_throw,"everything is fine."}
%% 10> hhfuns:my_catch_all(fun () -> error:Exit("everything is fine.") end).
%% * 1: variable 'Exit' is unbound
%% 11> hhfuns:my_catch_all(fun () -> erlang:error("everything is fine.") end).
%% {my_error,"everything is fine."}
%% 12> hhfuns:my_catch_all(fun () -> erlang:error("error.") end).
%% {my_error,"error."}
%% 13> hhfuns:my_catch_all(fun () -> erlang:exit("error.") end).
%% {my_exit,"error."}
