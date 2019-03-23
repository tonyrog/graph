%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Graph coloring algorithm
%%% @end
%%% Created : 22 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_color).
-export([greedy/1, greedy/2, greedy/3]).
-export([order_smallest_last/1]).
-export([first_available/1]).

greedy(G) ->
    Order = order_smallest_last(G),
    greedy(G, Order).

greedy(G, Order) when is_list(Order) ->
    Color = #{},
    greedy(G, Order, Color).

greedy(G, Order, Color) when is_list(Order), is_map(Color) ->
    greedy_(G, Order, Color).

greedy_(G, [V|Vs], Color) ->
    %% List is a list of colors assigned to neighbours to V
    List = graph:fold_out(
	     fun(_V,W,_E,L) ->
		     case maps:find(W,Color) of
			 error -> L;
			 {ok,Wc} -> [Wc|L]
		     end
	     end, [], V, G),
    Cv = first_available(List),
    greedy_(G, Vs, maps:put(V, Cv, Color));
greedy_(_G, [], Color) ->
    Color.

first_available(List) ->
    first_available_(0, lists:sort(List)).

first_available_(I, [C|_Cs]) when I < C -> I;
first_available_(I, [I|Cs]) -> first_available_(I+1, Cs);
first_available_(I, []) -> I.

%% select the vertex V with minimum degree
%% remove that and recurse (save the degree)
order_smallest_last(G) ->
    order_smallest_last_(G, [], 0).

order_smallest_last_(G, Acc, D) ->
    Ds = graph:fold_vertices(fun(V,L) -> [{V,graph:fanout(V,G)}|L] end,[],G),
    case lists:keysort(2, Ds) of
	[] -> Acc;
	[{V,Dv}|_] ->
	    Gi = graph:remove_vertex(V, G),
	    order_smallest_last_(Gi, [V|Acc], max(D, Dv))
    end.
