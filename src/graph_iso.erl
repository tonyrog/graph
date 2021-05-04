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

%% Test if mapping F : V(G) -> V(H)
%% is such that for all edges (u,v) in G then 
%%  (F(u),G(v)) is an edge in H

is_isomorphic(F, G, H) ->
    is_isomorphic_(F, G, H) andalso is_isomorphic_(inverse_map(F), H, G).

is_isomorphic_(F, G, H) ->
    Es = graph:edges(G),
    isomorphic_edges_(F, Es, H).

isomorphic_edges_(F, [E|Es], H) ->
    #{ pt1:=U, pt2:=V } = E,
    case graph:is_edge(maps:get(U,F), maps:get(V,F), H) of
	true ->
	    isomorphic_edges_(F, Es, H);
	false ->
	    false
    end;
isomorphic_edges_(_F, [], _H) ->
    true.

inverse_map(F) ->
    Fs = maps:to_list(F),
    LFs = length(Fs),
    Is = [{V,K} || {K,V} <- Fs],
    LFs = length(Is),  %% verify, fail if not bijection
    maps:from_list(Is).

fan_class1(G) ->
    Vs = graph:vertices(G),
    %% create a fanin/fanout number pair
    L = [{V,fan(V,G)} || V <- Vs],
    %% map to "class" numbers
    ClassList = class_list(L),
    %% create a graph class signature
    Signature = graph_signature(ClassList),
    %% assign ClassList to vertices
    G1 = lists:foldl(fun({V,Class},Gi) ->
			     graph:put_vertex(V,
					      [{class,Class},{color,Class}], Gi)
		     end, G, ClassList),
    {G1, Signature}.

fan(V, G) ->
    case graph:is_digraph(G) of
	true ->
	    {graph:fanin(V, G),graph:fanout(V, G)};
	false ->
	    graph:fanin(V, G)
    end.
	     
fan_class2(G) ->
    Vs = graph:vertices(G),
    %% create a fanin/fanout number pair
    L = [{V,ns_class(V, G)} || V <- Vs],
    %% io:format("L = ~p\n", [L]),
    %% map to "class" numbers
    ClassList = class_list(L),
    %% io:format("ClassList = ~p\n", [ClassList]),
    %% create a graph class signature
    Signature = graph_signature(ClassList),
    %% assign ClassList to vertices
    G1 = lists:foldl(fun({V,Class},Gi) ->
			     graph:put_vertex(V, 
					      [{class,Class},{color,Class}], 
					      Gi)
		     end, G, ClassList),
    {G1, Signature}.

ns_class(V, G) ->
    case graph:is_digraph(G) of
	true ->
	    {ns_in_class(V, G),ns_out_class(V, G)};
	false ->
	    ns_in_class(V, G)
    end.

ns_in_class(V, G) ->
    Ns = graph:in_neighbours(V, G),
    %% io:format("in of ~p =  ~p\n", [V, Ns]),
    ns_class(V, Ns, G).

ns_out_class(V, G) ->
    Ns = graph:out_neighbours(V, G),
    %% io:format("out of ~p =  ~p\n", [V, Ns]),
    ns_class(V, Ns, G).

ns_class(V, Ws, G) ->
    L = [{W,graph:get_vertex_by_id(W, class, G)} || W <- Ws],
    %% io:format("ns_class L = ~p\n", [L]),
    %% ClassList = class_list(L),
    %% io:format("ns_class ClassList = ~p\n", [ClassList]),
    Signature = graph_signature(L),
    %% io:format("ns_class Signature = ~p\n", [Signature]),
    Class = {graph:get_vertex_by_id(V, class, G), Signature},
    %% io:format("ns_class Class = ~p\n", [Class]),
    Class.

%% from [{vertex,class}] => [{class,count}]
graph_signature(VCList) ->
    M = lists:foldl(fun({_V,Class},Si) -> class_add(Class,Si) end, #{}, VCList),
    lists:keysort(1, maps:to_list(M)).
    
%% from [{vertex,data}] create {vertex,class-num}
class_list(VDList) ->
    class_list(VDList, #{}, 1, []).

class_list([{V,Data}|Vs], Set, I, Acc) ->
    case maps:get(Data, Set, undefined) of
	undefined ->
	    class_list(Vs, Set#{ Data=>I }, I+1, [{V,I}|Acc]);
	J ->
	    class_list(Vs, Set, I, [{V,J}|Acc])
    end;
class_list([], _Set, _I, Acc) ->
    Acc.

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

test_iso_1() ->
    G = test_iso_1_G(),
    H = test_iso_1_H(),
    {G1,SigG1} = fan_class1(G),
    {H1,SigH1} = fan_class1(H),
    io:format("SIG(G1) = ~w\n", [SigG1]),
    io:format("SIG(H1) = ~w\n", [SigH1]),

    {G2,SigG2} = fan_class2(G1),
    {H2,SigH2} = fan_class2(H1),

    io:format("SIG(G2) = ~w\n", [SigG2]),
    io:format("SIG(H2) = ~w\n", [SigH2]),

    {G2,H2}.

test_iso_1_G() ->
    graph:from_neighbour_list([{0,[1,2,4]},{1,[0,3]},{2,[0,3,6]},{3,[1,2]},{4,[0,5,6]},{5,[4,7]},{6,[2,4,7]},{7,[5,6]}]).

test_iso_1_H() ->
    graph:from_neighbour_list([{0,[4,5]},{1,[4,5]},{2,[3,6]},{3,[2,4,7]},{4,[0,1,3]},{5,[0,1,6]},{6,[2,5,7]},{7,[3,6]}]).
    
     

    
    
    
    
    
