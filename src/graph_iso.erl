%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Check it two graphs are iso morph
%%% @end
%%% Created : 30 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(graph_iso).
-compile(export_all).

is_automorphic(G) ->
    is_isomorphic(G, G).

is_isomorphic(G, H) ->
    is_isomorphic_0(G, H).

%% check that number of vertices and number of edges match
is_isomorphic_0(G, H) ->
    Gn = graph:number_of_vertices(G),
    Hn = graph:number_of_vertices(H),
    if Gn =/= Hn ->
	    {false, {number_of_vertices,Gn,Hn}};
       true ->
	    Gm = graph:number_of_edges(G),
	    Hm = graph:number_of_edges(H),
	    if Gm =/= Hm ->
		    {false, {number_of_edges,Gm,Gm}};
	       true ->
		    is_isomorphic_1(G, H)
	    end
    end.

%% check that vertices have same fanin,fanout layout
is_isomorphic_1(G, H) ->
    {G1, Gl} = fan_class1(G),
    {H1, Hl} = fan_class1(H),
    %% should be equal of classes
    if Gl =/= Hl ->
	    {false, {signature_1,Gl,Hl}};
       true ->
	    Gc = get_vertex_groups(class,G1),
	    io:format("|G1|=~w, G1=~w\n", [length(Gc),Gc]),
	    Hc = get_vertex_groups(class,H1),
	    io:format("|H1|=~w, H1=~w\n", [length(Hc),Hc]),
	    is_isomorphic_2(G1, H1)
    end.

fan_class1(G) ->
    Vs = graph:vertices(G),
    fan_class1(G, Vs, #{}).

fan_class1(G, [V|Vs], Set) ->
    Fi = graph:fanin(V,G),
    Fo = graph:fanout(V,G),
    fan_class1(graph:put_vertex(V, [{class,{Fi,Fo}}], G),
	      Vs, class_add({Fi,Fo},Set));
fan_class1(G, [], Set) ->
    {G, Set}.


%% now build the fan class based on neighbour classes
%% class2 = #{ {Fi,Fo} => Count1, {Fi,Fo} = Count2 ... }
is_isomorphic_2(G, H) ->
    {G1, Gl} = fan_class2(G),
    {H1, Hl} = fan_class2(H),
    %% should be equal of classes
    if Gl =/= Hl ->
	    {false, {signature_2,Gl,Hl}};
       true ->
	    Gc = get_vertex_groups(class2,G1),
	    io:format("|G2|=~w, G2=~w\n", [length(Gc),Gc]),
	    Hc = get_vertex_groups(class2,H1),
	    io:format("|H2|=~w, H2=~w\n", [length(Hc),Hc]),
	    is_isomorphic_3(G1, H1)
    end.

is_isomorphic_3(_G, _H) -> 
    maybe.

fan_class2(G) ->
    Vs = graph:vertices(G),
    fan_class2(G, Vs, #{}).

fan_class2(G, [V|Vs], Set) ->
    Nsi = graph:in_neighbours(V, G),
    Nso = graph:out_neighbours(V, G),
    Si = fan_class2_ns(Nsi, G, #{}),
    So = fan_class2_ns(Nso, G, #{}),
    fan_class2(graph:put_vertex(V, [{class2,{Si,So}}], G),
	       Vs, class_add({Si,So},Set));
fan_class2(G, [], Set) ->
    {G, Set}.
    
fan_class2_ns([W|Ws], G, Set) ->
    FiFo = graph:get_vertex_by_id(W, class, G),
    fan_class2_ns(Ws, G, class_add(FiFo, Set));
fan_class2_ns([], _G, Set) ->
    Set.

%% return a list of lists of vertex groups sorted by key

get_vertex_groups(Key, G) ->
    Vs = graph:vertices(G),
    get_vertex_groups(Vs, Key, G, #{}).

get_vertex_groups([V|Vs], Key, G, Map) ->
    E = graph:get_vertex_by_id(V, Key, G),
    Ws = maps:get(E, Map, []),
    get_vertex_groups(Vs, Key, G, maps:put(E, [V|Ws], Map));
get_vertex_groups([], _Key, _G, Map) ->
    [Vs || {_E,Vs} <- maps:to_list(Map)].

%% calculate {fanin,fanout} for each vertex attribute class


class_add(Key, Set) ->
    N = maps:get(Key, Set, 0),
    maps:put(Key, N+1, Set).
	    
