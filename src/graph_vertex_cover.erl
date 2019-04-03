%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Vertex cover algorithms
%%% @end
%%% Created : 25 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_vertex_cover).

-export([greedy/1]).

greedy(G) ->
    Es = [graph:get_edge(E,G) || E <- graph:edges(G)],
    greedy_(Es, []).

greedy_([{U,V}|Es], C) ->
    Es1 = [E || E={A,B} <- Es, A=/=U, A=/=V, B=/=U, B=/=V],
    %% io:format("U=~w, V=~w, Es'=~w\n", [U,V,Es1]),
    greedy_(Es1, [U,V|C]);
greedy_([], C) ->
    C.
