-module(lists).

[2*N || N <- [1,2,3,4]].

Weather = [{toronto, rain}, {montreal, storms}, {london, fog},
           {paris, sun}, {boston, fog}, {vancouver, snow}].


Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
<<R:8, G:8, B:8>> = <<Pix1:24>>.
<<R:8, Rest/binary>> = Pixels.

<<X1/signed>> = <<-42>>.
<<X2/integer-signed-little>> =  <<-44>>.

%% Note: Even though bit strings are pretty light, you should avoid using them to tag values.
%% It could be tempting to use string literals to say {<<"temperature">>,50}, but always use atoms
%% when doing that. Previously in this chapter, atoms were said to be taking only 4 or 8 bytes in space,
%% no matter how long they are. By using them, you'll have basically no overhead when copying data from
%% function to function or sending it to another Erlang node on another server.
%% Conversely, do not use atoms to replace strings because they are lighter. Strings can be manipulated
%%  (splitting, regular expressions, etc) while atoms can only be compared and nothing else.

[X || <<X>> <= <<0,1,2,3,4,5,6,7,8,9,10>>, X rem 2 == 0].

RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].
<< <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.

%% Be careful, as the elements of the resulting binary require a clearly defined size if the
%% generator returned binaries:
<< <<Bin/binary>> || Bin <- [<<1,2,3>>] >>.
<< <<(X+1)/integer>> || <<X>> <= <<3,7,5,4,7>> >>.
