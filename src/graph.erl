%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Graph implemented with maps
%%% @end
%%% Created : 20 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(graph).

-export([new/0, new/1]).
-export([save/1, save/2]).
-export([load/1, load_fd/1, load_fd/2]).
-export([show/1, open/1]).
-export([is_graph/1, is_digraph/1]).
-export([from_edge_list/1]).
-export([from_neighbour_list/1]).
-export([from_connection_matrix/1]).
-export([put_edge/3, put_edge/4, update_edge/4, is_edge/3]).
-export([remove_edge/3]).
-export([edges/1]).
-export([number_of_edges/1]).
-export([fold_edges/3]).
-export([get_edge_value/3]).
-export([get_edge_value/4]).
-export([set_edge_value/4]).

-export([put_vertex/2, put_vertex/3, update_vertex/3, is_vertex/2]).
-export([remove_vertex/2]).
-export([vertices/1]).
-export([number_of_vertices/1]).
-export([fold_vertices/3]).
-export([get_vertex_value/3]).
-export([get_vertex_value/4]).
-export([set_vertex_value/4]).
-export([set_vertex_values/3]).

-export([fold_out_edges/4]).
-export([out_neighbours/2]).
-export([out_edges/2]).
-export([fanout/2]).

-export([fold_in_edges/4]).
-export([in_neighbours/2]).
-export([in_edges/2]).
-export([fanin/2]).

-export([dijkstra/2]).
-export([valency_sum_sequence_xlabel/2,
	 valency_sum_sequence/1,
	 valency_sum_sequence/2]).

%% edge key is either:
%%  [a,b,out] for real existing directed edge
%%  [b,a,in]  for non existing (reversed edge)
%% 
-record(graph,
	{
	  is_digraph = true :: boolean(),
	  e :: map(),  %% map of edges E -> term()
	  v :: map()   %% map of vertices V -> term()
	}).

-define(is_graph(G), is_map(G)).

new() ->
    new(true).

new(Digraph) when is_boolean(Digraph) ->
    #{ type => graph,
       is_digraph => Digraph,
       e => #{},
       v => #{}
     }.

is_graph(#{ type := graph }) -> true;
is_graph(_) -> false.

is_digraph(#{ type := graph, is_digraph := true }) -> true;
is_digraph(_) -> false.

%%
%% Construct graph from list of edges 
%% {a,b} or
%% {a,b,value}
%%
from_edge_list(Es) ->
    from_edge_list_(Es, new()).

from_edge_list_([{A,B} | Es], G) ->
    from_edge_list_(Es, put_edge(A, B, G));
from_edge_list_([{A,B,Props} | Es], G) ->
    from_edge_list_(Es, put_edge(A, B, Props, G));
from_edge_list_([], G) ->
    G.

from_neighbour_list(Ns) ->
    from_neighbour_list_(Ns, new()).

from_neighbour_list_([{V,Vs}|Ns], G) when is_list(Vs) ->
    G1 = from_edge_list_([ {V, W} || W <- Vs ], G),
    from_neighbour_list_(Ns, G1);
from_neighbour_list_([{V,Props,Vs}|Ns], G) when is_list(Vs) ->
    G1 = put_vertex(V, Props, G),
    G2 = from_edge_list_([ {V, W} || W <- Vs ], G1),
    from_neighbour_list_(Ns, G2);
from_neighbour_list_([], G) ->
    G.

from_connection_matrix(M) ->
    N = length(M),
    from_connection_matrix_(M, 1, N, new()).

from_connection_matrix_([Ri|Rs], I, N, G) ->
    G1 = put_vertex({v,I}, G),
    from_connection_matrix_(Ri, 1, Rs, I, N, G1);
from_connection_matrix_([], I, N, G) when I > N ->
    G.

from_connection_matrix_([Vj|Vs], J, Rs, I, N, G) ->
    case Vj of
	0 -> from_connection_matrix_(Vs, J+1, Rs, I, N, G);
	1 -> 
	    G1 = put_vertex({v,J}, G),
	    G2 = put_edge({v,I},{v,J}, G1),
	    from_connection_matrix_(Vs, J+1, Rs, I, N, G2)
    end;
from_connection_matrix_([], _J, Rs, I, N, G) ->
    from_connection_matrix_(Rs, I+1, N, G).

put_edge(A, B, G) when ?is_graph(G) ->
    case is_edge(A,B,G) of
	true ->
	    G;
	false ->
	    Es0 = G#graph.e,
	    case maps:is_key([A,B,in], Es0) of
		true ->
		    Es1 = maps:remove([A,B,in], Es0),
		    Es2 = maps:put([A,B,out], [], Es1),
		    G#graph { e = Es2 };
		false ->
		    put_edge(A,B,[],G)    
	    end
    end.

put_edge(A,B,Props,G) when ?is_graph(G) ->
    try update_edge(A,B,Props,G) of
	G1 -> G1
    catch
	error:badarg ->
	    G1 = put_vertex(A, G),
	    G2 = put_vertex(B, G1),
	    Es0 = G2#graph.e,
	    X = if G#graph.is_digraph -> in; true -> out end,
	    Es1 = maps:put([A,B,out],Props,Es0),
	    Es2 = maps:put([B,A,X],Props,Es1),
	    G2#graph { e = Es2 }
    end.

update_edge(A, B, List, G = #{ type := graph, e := E, is_digraph:=Digraph }) ->
    Es = maps:update([A,B,out], List, E),
    Es1 = if Digraph -> Es;
	     true -> maps:update([B,A,out], List, Es)
	  end,
    G#graph { e = Es1 }.

remove_edge(A, B, G = #{ type:=graph,e := E,is_digraph:=Digraph }) ->
    try maps:remove([A,B,out], E) of
	Es ->
	    Es1 = if Digraph -> Es;
		     true -> maps:remove([B,A,out], Es)
		  end,
	    G#{ e => Es1 }
    catch
	error:badarg ->
	    G
    end.

is_edge(A, B, #{ type := graph, e := E}) ->
    maps:is_key([A,B,out], E).

edges(G) ->
    maps:fold(fun([V,W,out],_,Acc) -> [{V,W}|Acc];
		 (_,_,Acc) -> Acc
	      end,
	      [], G#graph.e).

number_of_edges(G) ->
    maps:size(G#graph.e).

fold_edges(Fun, Acc0, G) ->
    maps:fold(fun([V,W,out],_Attr,Acc1) -> Fun(V,W,Acc1);
		  ([_V,_W,in],_Attr,Acc1) -> Acc1
	       end, Acc0, G#graph.e).

get_edge_value({V,W}, Key, G) when ?is_graph(G) ->
    List = maps:get([V,W,out], G#graph.e),
    proplists:get_value(Key,  List).

get_edge_value({V,W}, Key, G, Default) when ?is_graph(G) ->
    List = maps:get([V,W,out], G#graph.e),
    proplists:get_value(Key,  List, Default).

set_edge_value({V,W}, Key, Value, G) when ?is_graph(G) ->
    List0 =  maps:get([V,W,out], G#graph.e),
    List1 = case lists:keytake(Key, 1, List0) of
		false -> [{Key,Value}|List0];
		{value,_,L} -> [{Key,Value}|L]
	    end,
    update_edge(V, W, List1, G).


%% @doc
%% Return a list of "out" neighbours of G, that is
%% vertices w such that {v,w} is an edge in G.
%% @end

fold_out_edges(Fun, Acc, V, G=#{type:=graph,e:=E}) ->
    fold_out_edges_(Fun, Acc, maps:interator(E), V, G).

fold_out_edges_(Fun, Acc, I, V, G) when ?is_graph(G) ->
    case maps:next(I) of
	none -> Acc;
	{[V,W,out],_Value,I1} ->
	    fold_out_edges_(Fun, Fun(V,W,Acc), I1, V, G);
	{[V,_,in],_Value,I1} ->
	    fold_out_edges_(Fun, Acc, I1, V, G);
	{_, _, I1} ->
	    fold_out_edges_(Fun, Acc, I1, V, G)
    end.

out_edges(V, G) when ?is_graph(G) ->
    fold_out_edges(fun(_V,W,Acc) -> [{V,W}|Acc] end, [], V, G).

out_neighbours(V, G) when ?is_graph(G) ->
    fold_out_edges(fun(_V,W,Acc) -> [W|Acc] end, [], V, G).

fanout(V, G) when ?is_graph(G) ->
    fold_out_edges(fun(_V,_W,N) -> N+1 end, 0, V, G).

%% @doc
%% Return a list of "in" neighbours of G, that is
%% vertices w such that {w,v} is an edge in G
%% @end

fold_in_edges(Fun, Acc, V, G=#{ type:=graph, e:=E}) ->
    fold_in_edges_(Fun, Acc, maps:iterator(E), V, G).

fold_in_edges_(Fun, Acc, I, V, G=#{type:=graph,e:=E,is_digrapg:=Digraph}) ->
    case maps:next(I) of
	none -> Acc;
	{[V,W,_],_Value,I1} ->
	    if Digraph ->
		    case maps:is_key([W,V,out],E) of
			true  -> 
			    fold_in_edges_(Fun,Fun(W,V,Acc),I1,V,G);
			false -> 
			    fold_in_edges_(Fun,Acc,I1,V,G)
		    end;
	       true ->
		    fold_in_edges_(Fun,Fun(W,V,Acc),I1,V,G)
	    end;
	{_, _, I1} ->
	    fold_in_edges_(Fun,Acc,I1,V,G)
    end.


in_edges(V, G) when ?is_graph(G) ->
    fold_in_edges(fun(W,_V,Acc) -> [{W,V}|Acc] end, [], V, G).

in_neighbours(V, G) when ?is_graph(G) ->
    fold_in_edges(fun(W,_V,Acc) -> [W|Acc] end, [], V, G).

fanin(V, G) when ?is_graph(G) ->
    fold_in_edges(fun(_V,_W,N) -> N+1 end, 0, V, G).

%%
%% Vertices
%%

put_vertex(V, G) when ?is_graph(G) ->
    case is_vertex(V, G) of
	true -> G;
	false -> put_vertex(V, [], G)
    end.

put_vertex(V, List, G) when ?is_graph(G), is_list(List) ->
    Vs = maps:put(V, List, G#graph.v),
    G#graph { v = Vs }.

update_vertex(V, List, G) when ?is_graph(G), is_list(List) ->
    Vs = maps:update(V, List, G#graph.v),
    G#graph { v = Vs }.

get_vertex_value(V, Key, G) when ?is_graph(G) ->
    List = maps:get(V, G#graph.v),
    proplists:get_value(Key,  List).

get_vertex_value(V, Key, G, Default) when ?is_graph(G) ->
    List = maps:get(V, G#graph.v),
    proplists:get_value(Key,  List, Default).

set_vertex_value(V, Key, Value, G) when ?is_graph(G) ->
    L0 =  maps:get(V, G#graph.v),
    L1 = case lists:keytake(Key, 1, L0) of
	     false -> [{Key,Value}|L0];
	     {value,_,L} -> [{Key,Value}|L]
	 end,
    update_vertex(V, L1, G).

set_vertex_values(V, KVs, G) when ?is_graph(G), is_list(KVs) ->
    L0 = maps:get(V, G#graph.v),
    L1 = lists:foldl(
	   fun(Kv={Key,_Value},Li) ->
		   case lists:keytake(Key, 1, Li) of
		       false -> [Kv|Li];
		       {value,_,L} -> [Kv|L]
		   end
	   end, L0, KVs),
    update_vertex(V, L1, G).

remove_vertex(V, G) when ?is_graph(G) ->
    try maps:remove(V, G#graph.v) of
	Vs ->
	    %% this must be improved
	    Es = maps:fold(
		   fun(E=[A,B,_], _, Acc) when A =:= V orelse B =:= V -> 
			   [E|Acc];
		      (_E, _V, Acc) -> Acc
		   end, [], G#graph.e),
	    if Es =:= [] ->
		 G#graph { v = Vs };   
	       true ->
		    Es1 = lists:foldl(fun(E, Esi) ->
					      maps:remove(E, Esi)
				      end, G#graph.e, Es),
		    G#graph { v = Vs, e = Es1 }
	    end
    catch
	error:badarg ->
	    G
    end.

is_vertex(V, G) when ?is_graph(G) ->
    maps:is_key(V, G#graph.v).

vertices(G) when ?is_graph(G) ->
    maps:keys(G#graph.v).

number_of_vertices(G) when ?is_graph(G) ->
    maps:size(G#graph.v).


fold_vertices(Fun, Acc0, G) when ?is_graph(G) ->
    maps:fold(fun(V,_Attr,Acc1) -> Fun(V,Acc1) end,
	      Acc0, G#graph.v).

%% generate frequency (valency sum sequence)
valency_sum_sequence_xlabel(G, N) ->
    G1 = valency_sum_sequence(G,N),
    fold_vertices(
      fun(V, Gv) ->
	      Vo = get_vertex_value(V, vo, Gv, []),
	      Vi = get_vertex_value(V, vi, Gv, []),
	      Xl = lists:flatten(io_lib:format("-~w+~w", [Vi,Vo])),
	      set_vertex_value(V, xlabel, Xl, Gv)
      end, G1, G1).


valency_sum_sequence(G,0) ->
    G;
valency_sum_sequence(G,I) when is_integer(I), I>0 ->
    G1 = valency_sum_sequence(G),
    valency_sum_sequence(G1,I-1).

valency_sum_sequence(G) ->
    fold_vertices(
      fun(V, Gv) ->
	      Vo = 
		  fold_out_edges(
		    fun(_V,W,Sum) ->
			    [S|_] = get_vertex_value(W, vo, G, [1]),
			    S+Sum
		    end, 0, V, G),
	      Vi = fold_in_edges(
		      fun(_V,W,Sum) ->
			      [S|_] = get_vertex_value(W, vi, G, [1]),
			      S+Sum
		      end, 0, V, G),
	      Vos = get_vertex_value(V, vo, G, []),
	      Vis = get_vertex_value(V, vi, G, []),
	      Gv1 = set_vertex_value(V, vo, [Vo|Vos], Gv),
	      Gv2 = set_vertex_value(V, vi, [Vi|Vis], Gv1),
	      Gv2
      end, G, G).
    

%% find shortest path from vertex S to all other vertices
dijkstra(S, G) when ?is_graph(G) ->
    Vs = vertices(G),
    Infinity = length(Vs)+1,
    Dist = maps:from_list([{V,Infinity}||V<-Vs]++[{S,0}]),
    Prev = #{},
    Q = sets:from_list(Vs),
    dijkstra_(G, Q, Dist, Prev, Infinity).

dijkstra_(G, Q, Dist, Prev, Infinity) ->
    case sets:size(Q) of
	0 -> Dist;
	_ ->
	    {U,D} =
		sets:fold(fun(U,UD={_Ui,Di}) ->
				  D = maps:get(U, Dist),
				  if D < Di -> {U,D};
				     true -> UD
				  end
			  end, {'_',Infinity}, Q),
	    if D >= Infinity ->
		    Dist;
	       true ->
		    Vs = out_neighbours(U,G),
		    Q1 = sets:del_element(U,Q),
		    dijkstra_(G, Q1, U, Vs, Dist, Prev, Infinity)
	    end
    end.
	    
dijkstra_(G, Q, _U, [], Dist, Prev, Infinity) ->
    dijkstra_(G, Q, Dist, Prev, Infinity);
dijkstra_(G, Q, U, [V|Vs], Dist, Prev, Infinity) ->
    Alt = maps:get(U, Dist) + 1,  %% dist_between(U,V) = 1
    Dv  = maps:get(V, Dist),
    if Alt < Dv ->
	    Dist1 = maps:put(V,Alt,Dist),
	    Prev1  = maps:put(V,U,Prev),
	    %% decrease-key v in Q?
	    dijkstra_(G, Q, U, Vs, Dist1, Prev1, Infinity);
       true ->
	    dijkstra_(G, Q, U, Vs, Dist, Prev, Infinity)
    end.

show(G) ->
    save("graph.dot", G),  %% fixme
    open("graph.dot").

open(File) ->
    os:cmd("open -a Graphviz " ++ File).

%% emit the graph in DOT format.
save(G) when ?is_graph(G) ->
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

save_fd(Fd,G) when is_record(G, graph) ->
    case G#graph.is_digraph of
	true -> io:format(Fd, "digraph G {\n", []);
	false -> io:format(Fd, "graph G {\n", [])
    end,
    io:format(Fd, "node [style=bold,penwidth=1,pencolor=black,ordering=in]\n", []),
    fold_vertices(
      fun(V,_A) -> 
	      Color     = get_vertex_value(V, color, G, green),
	      Style     = get_vertex_value(V, style, G, solid),
	      FillColor = get_vertex_value(V, fillcolor, G, green),
	      Scheme    = get_vertex_value(V, colorscheme, G, x11),
	      Label     = get_vertex_value(V, label, G, V),
	      XLabel    = get_vertex_value(V, xlabel, G, ""),
	      io:format(Fd, "\"~s\" [label=\"~s\",xlabel=\"~s\",colorscheme=~s,color=~s,style=~s,fillcolor=~s];\n",
			[to_string(V),
			 to_string(Label),
			 to_string(XLabel),
			 to_string(Scheme),
			 to_string(Color),
			 to_string(Style),
			 to_string(FillColor)])
      end, ok, G),
    fold_vertices(
      fun(V,_A) ->
	      Es = in_edges(V, G),
	      Es1 = lists:keysort(1,Es),  %% more sorting here
	      lists:foreach(
		fun(E={A,B}) ->
			Color = get_edge_value(E, color, G, black),
			Style = get_edge_value(E, style, G, solid),
			Label = get_edge_value(E, label, G, ""),
			PenWidth = get_edge_value(E, penwidth, G, 1),
			Arrow = if G#graph.is_digraph -> "->"; 
				   true -> "--"
				end,
			if G#graph.is_digraph; A < B ->
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

%%
%% Load graph in 
%% connection matrix format
%% 
%%
load(Filename) ->
    case file:open(Filename, [read]) of
	{ok, Fd} ->
	    try load_fd(Fd) of
		G -> G
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

load_fd(Fd) ->
    case read_sym_row(Fd) of
	eof -> {error, eof};
	[] ->  {error, missing_size_row};
	[N|_Vs] -> load_fd(Fd, N)
    end.

load_fd(Fd, N) when is_integer(N), N > 0 ->
    try load_lines(Fd, N, N, []) of
	M -> 
	    {ok, from_connection_matrix(M)}
    catch
	error:_ -> {error, badarg}
    end.


load_lines(_Fd, 0, _N, Acc) ->
    lists:reverse(Acc);
load_lines(Fd, I, N, Acc) ->
    Ts = read_bin_row(Fd),
    case length(Ts) of
	N -> load_lines(Fd, I-1, N, [Ts|Acc])
    end.

read_sym_row(Fd) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    transform_sym_row(string:tokens(Line," \t\r\n"));
	eof -> eof;
	{error,_} -> []
    end.

transform_sym_row([H|T]) ->
    [ try list_to_integer(H) 
      catch error:_ -> list_to_atom(H)
      end | transform_sym_row(T)];
transform_sym_row([]) ->
    [].

read_bin_row(Fd) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    transform_bin_row(string:tokens(Line," \t\r\n"));
	eof -> [];
	{error,_} -> []
    end.

%%
%%  "1010" -> [1,0,1,0]
%%  "1 0 1 0" -> [1,0,1,0]
%%  

transform_bin_row([[$0|Rs]|T]) -> [0|transform_bin_row([Rs|T])];
transform_bin_row([[$1|Rs]|T]) -> [1|transform_bin_row([Rs|T])];
transform_bin_row([[]|T]) -> transform_bin_row(T);
transform_bin_row([]) -> [].

to_string(X) when is_atom(X) -> atom_to_list(X);
to_string(X) when is_integer(X) -> integer_to_list(X);
to_string(X) when is_list(X) -> X;
to_string(X) -> lists:flatten(io_lib:format("~w", [X])).
