-module(functions).
-compile(export_all).

head([H|_]) ->
     H.

same(X,X) ->
    true;
same(_,_) ->
    false.


voting_age(X) when X < 18 ->
    false;
voting_age(_) ->
    true.
