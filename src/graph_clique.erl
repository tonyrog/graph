%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%     Graph clique algorithm
%%% @end
%%% Created : 24 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_clique).

-export([greedy/1]).

greedy(G) ->
    Vs = graph:fold_vertices(fun(V,L) -> 
				     [{graph:fanout(V,G),V}|L]
			     end, [], G),
    greedy_(G, Vs, sets:new()).

greedy_(G, Vs, Q) ->
    case lists:sort(fun({Di,_},{Dj,_}) -> Dj < Di end, Vs) of
	[{_,Vk}|Vs1] ->
	    Nk = graph:out_neighbours(Vk, G),
	    Vs2 = lists:foldr(fun({_Dj,Vj},Acc) ->
				      case lists:member(Vj, Nk) of
					  true ->
					      Nj0 = graph:out_neighbours(Vj,G),
					      Nj = Nk -- (Nk -- Nj0),
					      Dj1 = length(Nj),
					      [{Dj1,Vj}|Acc];
					  false ->
					      Acc
				      end
			      end, [], Vs1),
	    greedy_(G, Vs2, sets:add_element(Vk, Q));
	[] ->
	    sets:to_list(Q)
    end.
