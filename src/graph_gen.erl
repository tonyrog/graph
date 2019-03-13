%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Graph generator
%%% @end
%%% Created : 14 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_gen).

-compile(export_all).

-export([uniform/2]).

%% generate V number of vertices and E number of edges
uniform(V, E) ->
    G = graph:new(false),
    {G1,Vs} = add_vertices(V, 640, 480, [], G),
    
    %% all possible edges
    Es = [{I,J} || I <- lists:seq(1,V), J <- lists:seq(1,V), I < J ],
    %% associate with random number
    REs = [{rand:uniform(),E1} || E1 <- Es],
    %% sort and select first E elements
    {REs2,_} = lists:split(E, lists:keysort(1, REs)),
    add_edges(REs2, Vs, G1).


add_vertices(0, _W, _H, Vs, G) ->
    {G, list_to_tuple(lists:reverse(Vs))};
add_vertices(I, W, H, Vs, G) ->
    V = make_ref(),
    X = rand:uniform(W),
    Y = rand:uniform(H),
    Attr = [{x,X},{y,Y},{color,red},{width,16},{height,16},{shape,ellipse}],
    G1 = graph:put_vertex(V, Attr, G),
    add_vertices(I-1, W, H, [V|Vs], G1).

add_edges([{_R,{I,J}}|Es], Vs, G) ->
    V = element(I, Vs),
    W = element(J, Vs),
    G1 = graph:put_edge(V, W, [{color,black}], G),
    add_edges(Es, Vs, G1);
add_edges([], _Vs, G) ->
    G.
