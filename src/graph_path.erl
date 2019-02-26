%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Path algorithms
%%% @end
%%% Created : 24 Feb 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_path).

-export([dijkstra/2]).

-export([valency_sum_sequence_xlabel/2,
	 valency_sum_sequence/1,
	 valency_sum_sequence/2]).

%%
%% Dijkstras algorithm
%%

%% find shortest path from vertex S to all other vertices
dijkstra(S, G) ->
    Vs = graph:vertices(G),
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
		    Vs = graph:out_neighbours(U,G),
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


%% generate frequency (valency sum sequence)
valency_sum_sequence_xlabel(G, N) ->
    G1 = valency_sum_sequence(G,N),
    graph:fold_vertices(
      fun(V, Gv) ->
	      Vo = graph:get_vertex_by_id(V, vo, Gv, []),
	      Vi = graph:get_vertex_by_id(V, vi, Gv, []),
	      Xl = lists:flatten(io_lib:format("-~w+~w", [Vi,Vo])),
	      graph:put_vertex(V, [{xlabel,Xl}], Gv)
      end, G1, G1).


valency_sum_sequence(G,0) ->
    G;
valency_sum_sequence(G,I) when is_integer(I), I>0 ->
    G1 = valency_sum_sequence(G),
    valency_sum_sequence(G1,I-1).

valency_sum_sequence(G) ->
    graph:fold_vertices(
      fun(V, Gv) ->
	      Vo = 
		  graph:fold_out_edges(
		    fun(_V,W,Sum) ->
			    [S|_] = graph:get_vertex_by_id(W, vo, G, [1]),
			    S+Sum
		    end, 0, V, G),
	      Vi = graph:fold_in_edges(
		      fun(_V,W,Sum) ->
			      [S|_] = graph:get_vertex_by_id(W, vi, G, [1]),
			      S+Sum
		      end, 0, V, G),
	      Vos = graph:get_vertex_by_id(V, vo, G, []),
	      Vis = graph:get_vertex_by_id(V, vi, G, []),
	      Gv1 = graph:set_vertex_by_id(V, vo, [Vo|Vos], Gv),
	      Gv2 = graph:set_vertex_by_id(V, vi, [Vi|Vis], Gv1),
	      Gv2
      end, G, G).
    
