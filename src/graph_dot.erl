%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%     Save / (Load) file in dot format
%%% @end
%%% Created : 24 Feb 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_dot).

-export([show/1, open/1]).
-export([save/1, save/2]).

show(G) ->
    save("graph.dot", G),  %% fixme
    open("graph.dot").

open(File) ->
    os:cmd("open -a Graphviz " ++ File).

%% emit the graph in DOT format.
save(G = #{type:=graph}) ->
    save("graph.dot", G).

save(Filename, G) ->
    case file:open(Filename, [write]) of
	{ok,Fd} ->
	    try save_fd(Fd, G) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

save_fd(Fd, G = #{ type := graph, is_digraph := Digraph}) ->
    case Digraph of
	true -> io:format(Fd, "digraph G {\n", []);
	false -> io:format(Fd, "graph G {\n", [])
    end,
    io:format(Fd, "node [style=bold,penwidth=1,pencolor=black,ordering=in]\n", []),
    graph:fold_vertices(
      fun(V,_A) -> 
	      Color     = graph:get_vertex_by_id(V, color, G, green),
	      Style     = graph:get_vertex_by_id(V, style, G, solid),
	      FillColor = graph:get_vertex_by_id(V, fillcolor, G, green),
	      Scheme    = graph:get_vertex_by_id(V, colorscheme, G, x11),
	      Label     = graph:get_vertex_by_id(V, label, G, V),
	      XLabel    = graph:get_vertex_by_id(V, xlabel, G, ""),
	      io:format(Fd, "\"~s\" [label=\"~s\",xlabel=\"~s\",colorscheme=~s,color=~s,style=~s,fillcolor=~s];\n",
			[to_string(V),
			 to_string(Label),
			 to_string(XLabel),
			 to_string(Scheme),
			 to_string(Color),
			 to_string(Style),
			 to_string(FillColor)])
      end, ok, G),
    graph:fold_vertices(
      fun(V,_A) ->
	      Es = graph:in_edges(V, G),
	      Es1 = lists:keysort(1,Es),  %% more sorting here
	      lists:foreach(
		fun(E={A,B}) ->
			Color = graph:get_edge_value(E, color, G, black),
			Style = graph:get_edge_value(E, style, G, solid),
			Label = graph:get_edge_value(E, label, G, ""),
			PenWidth = graph:get_edge_value(E, penwidth, G, 1),
			Arrow = if Digraph -> "->"; 
				   true -> "--"
				end,
			if Digraph; A < B ->
				io:format(Fd, "\"~s\" ~s \"~s\" [label=\"~s\",color=~s,style=~s,penwidth=~w];\n", 
					  [to_string(A),Arrow,to_string(B),
					   to_string(Label),
					   to_string(Color),
					   to_string(Style),
					   PenWidth]);
			   true ->
				ok
			end
		end, Es1)
      end, ok, G),
    io:format(Fd, "}\n", []).



to_string(X) when is_atom(X) -> atom_to_list(X);
to_string(X) when is_integer(X) -> integer_to_list(X);
to_string(X) when is_list(X) -> X;
to_string(X) -> lists:flatten(io_lib:format("~w", [X])).



