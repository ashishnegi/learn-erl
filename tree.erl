-module(tree).
-export([empty/0, insert/2, lookup/2]).

empty() ->
    {leaf}.

insert(Tree, Val) ->
    case Tree of
	{leaf} ->
	    {node, Val, empty(), empty()};
	{node, V, Left, Right} ->
	    if V > Val ->
		    {node, V, insert(Left, Val), Right};
	       V < Val ->
		    {node, V, Left, insert(Right, Val)}
	    end
    end.

lookup(Tree, Val) ->
    case Tree of
	{leaf} ->
	    false;
	{node, V, Left, Right} ->
	    if V == Val ->
		    true;
	       V < Val ->
		    lookup(Right, Val);
	       V > Val ->
		    lookup(Left, Val)
	    end
    end.
