%% generate color palette with names
-module(logo_palette_gen).
-export([start/0]).

start() ->
    [emit_color(Index,Lightness) ||
	Lightness <- lists:seq(0,10,1), Index <- lists:seq(0,13)  ].

emit_color(Index,L10) ->
    Name = if L10 =:= 0 ->
		   base_color_name(Index);
	      true ->
		   base_color_name(Index) ++ integer_to_list(L10)
	   end,
    [R0,G0,B0] = color2(Index),
    {H,S,_L} = epx_color:rgb_to_hsl({R0,G0,B0}),
    L = if L10 < 6 ->
		max(0.01, min(0.99,(L10+5)/10));
	   true ->
		max(0.01, min(0.99,(10-L10)/10))
	end,
    {R,G,B} = epx_color:hsl_to_rgb({H,S,L}),
    Num = L10*14 + Index,
    io:format("?COLOR(~w, ~s, {~w,~w,~w}),\n", [Num, Name, R,G,B]).

base_color_name(Index) ->
    element(Index+1, 
	    {"grey","red","orange","brown","yellow","green",
	     "lime","turquoise","cyan","sky","blue","violet",
	     "magenta","pink"}).

color2(I) ->
    element(I+1,
	    {
	     [16#8C,16#8C,16#8C],
	     [16#D7,16#30,16#27],
	     [16#F1,16#69,16#13],
	     [16#9C,16#6D,16#46],
	     [16#ED,16#ED,16#2F],
	     [16#57,16#B0,16#3A],
	     [16#2A,16#D1,16#39],
	     [16#1B,16#9E,16#77],
	     [16#4E,16#B3,16#D3],
	     [16#2B,16#8C,16#BE],
	     [16#45,16#75,16#B4],
	     [16#7B,16#4E,16#A3],
	     [16#A6,16#19,16#69],
	     [16#E0,16#7E,16#95]}).

