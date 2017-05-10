-module(functions).
-compile(export_all).

head([H|_]) ->
     H.

same(X,X) ->
    true;
same(_,_) ->
    false.


voting_age(X) when X < 18; X > 200 -> %% > 200 : too old to vote :P
    false;
voting_age(_) ->
    true.

voice(Animal) ->
    Text = if Animal == dog ->
		   "barks..";
	      Animal == cat ->
		   "meow..";
	      Animal == snake ->
		   "hiss..";
	      true ->
		   "<unknown..>"
	   end,
    {Animal, Text}.

%% Erlang is about programming for the right cases: you only program for
%% what you know will happen and what you expect. Everything else should
%% cause errors as soon as possible.
