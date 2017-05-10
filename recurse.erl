-module(recurse).
-compile(export_all).

zip(_, []) ->
    [];
zip([], _) ->
    [];
zip([X|XS],[Y|YS]) ->
    [{X,Y} | zip(XS, YS)].

tail_zip(X,Y) ->
     tail_zip(X,Y,[]).

tail_zip(_,[],Ans) ->
    Ans;
tail_zip([],_,Ans) ->
    Ans;
tail_zip([X|XS],[Y|YS],ZS) ->
    tail_zip(XS, YS, [{X,Y} | ZS]).
