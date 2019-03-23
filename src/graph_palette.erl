%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Palette Number => RGB, Name => Number
%%% @end
%%% Created : 22 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_palette).
-export([color_to_rgb/1, color_from_name/1]).
-define(COLOR(I,Color32,RGB,HSL,Name), (I) => (RGB)).
-include("xterm.hrl").

color_to_rgb(xterm) ->
    #{ ?COLOR_LIST }.

-undef(COLOR).
-undef(COLOR_LIST).
-define(COLOR(I,Color32,RGB,HSL,Name), 
	(Name) => (I),
	(??Name) => (I)
).
-include("xterm.hrl").

color_from_name(xterm) ->
    # { ?COLOR_LIST }.
