-module(hhfuns).
-compile(export_all).

map(_, []) ->
    [];
map(F, [H|T]) ->
    [F(H)|map(F, T)].

%% c("hhfuns").
%% hhfuns:map(fun(X) -> X + 1 end, lists:seq(1,9)).

fold(_, Acc, []) ->
    Acc;
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
