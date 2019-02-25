%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Graph implemented with maps
%%% @end
%%% Created : 20 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(graph).

-export([new/0, new/1]).

-export([is_graph/1, is_digraph/1]).
-export([from_edge_list/1]).
-export([from_undirected_edge_list/1]).
-export([to_edge_list/1]).
-export([from_neighbour_list/1]).
-export([from_undirected_neighbour_list/1]).
-export([to_neighbour_list/1]).
-export([from_connection_matrix/1]).

-export([is_vertex/2]).
-export([put_vertex/2, put_vertex/3]).
-export([get_vertex/2, get_vertex/3, get_vertex/4]).
-export([remove_vertex/2]).
-export([vertices/1]).
-export([number_of_vertices/1]).
-export([fold_vertices/3,fold_xvertices/3]).

-export([is_edge/2, is_edge/3, edge/2, edge/3]).
-export([put_edge/3, put_edge/4]).
-export([remove_edge/2, remove_edge/3]).
-export([edges/1]).
-export([number_of_edges/1]).
-export([fold_edges/3, fold_xedges/3]).
-export([get_edge/2, get_edge/3, get_edge/4, get_edge/5]).

-export([fold_out/4]).
-export([out_neighbours/2]).
-export([out_edges/2]).
-export([fanout/2]).

-export([fold_in/4]).
-export([in_neighbours/2]).
-export([in_edges/2]).
-export([fanin/2]).

%% internal edge/vertex attributes
-define(ID,   '$ID').
-define(TYPE, '$TYPE').
-define(IN,   '$IN').
-define(OUT,  '$OUT').
-define(PT1,  '$PT1').
-define(PT2,  '$PT2').

%%
%% Vertex
%%    Key        Value
%%    a          #{ id=>a, out=>[{b,e1},{c,e2}], in=>[{a,e1},{d,e3}] }
%%    b          #{ id=>b  }
%%    c          #{ id=>c }
%%    d          #{ id=>d }
%%
%% edge key is either:
%% UNDIRECTED graph
%%    Key        Value
%%    e1         #{ id=>e1, pt1=>a, pt2=>b     (a < b)
%%    e2         #{ id=>e2, pt1=>b, pt2=>c     (b < c)
%%
%% DIRECTED graph
%%    Key        Value
%%    e1         #{ id=e1, pt1 => a, pt2 => b
%%    e2         #{ id=e2, pt1 => a, pt2 => c
%%    e3         #{ id=e3, pt1 => d, pt2 => b
%%
%%

new() ->
    new(true).

new(Digraph) when is_boolean(Digraph) ->
    #{ ?TYPE => graph,
       is_digraph => Digraph,
       e => #{},
       v => #{}
     }.

is_graph(#{ ?TYPE := graph }) -> true;
is_graph(_) -> false.

is_digraph(#{ ?TYPE := graph, is_digraph := true }) -> true;
is_digraph(_) -> false.

sort(A,B,true) -> {A,B};
sort(A,B,_) when A =< B ->  {A,B};
sort(A,B,_) -> {B,A}.

%%
%% Construct graph from list of edges 
%% {a,b} or
%% {a,b,value}
%%
from_edge_list(Es) ->
    from_edge_list_(Es, new()).

from_undirected_edge_list(Es) ->
    from_edge_list_(Es, new(false)).

from_edge_list_([{A,B} | Es], G) ->
    from_edge_list_(Es, put_edge(A, B, G));
from_edge_list_([{A,B,E} | Es], G) ->
    from_edge_list_(Es, put_edge(A,B,E,[],G));
from_edge_list_([{A,B,E,EProps} | Es], G) ->
    from_edge_list_(Es, put_edge(A,B,E,EProps,G));
from_edge_list_([], G) ->
    G.

to_edge_list(G) ->
    fold_edges(
      fun(V,W,E,Acc) -> 
	      if is_reference(E) ->
		      [{V,W}|Acc];
		 true ->
		      [{V,W,E}|Acc]
	      end
      end, [], G).

from_neighbour_list(Ns) ->
    from_neighbour_list_(Ns, new()).

from_undirected_neighbour_list(Ns) ->
    from_neighbour_list_(Ns, new(false)).

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

to_neighbour_list(G) ->
    fold_vertices(
      fun(V,Ai) ->
	      Ns = out_neighbours(V, G),
	      case get_vertex(V,G) of
		  [] -> [{V,Ns} | Ai];
		  VProps -> [{V,VProps,Ns} | Ai]
	      end
      end, [], G).

put_edge(E,Props,G=#{?TYPE:=graph,e:=Es}) when is_list(Props) ->
    Es1 = add_edge_props_(E, Props, Es),
    G# { e => Es1 };
put_edge(A0,B0,G=#{ ?TYPE:=graph,is_digraph:=Digraph}) ->
    {A,B} = sort(A0,B0,Digraph),
    case edge(A,B,G) of
	{_AorB,_E} ->
	    G;
	false ->
	    insert_edge_(A,B,make_ref(),[],G)
    end.
	    
put_edge(A0,B0,Props,G=#{ ?TYPE:=graph,e:=Es,is_digraph:=Digraph}) ->
    {A,B} = sort(A0,B0,Digraph),
    case edge(A,B,G) of
	{_AorB,E} ->
	    Es1 = add_edge_props_(E,Props,Es),
	    G#{ e => Es1 };
	false ->
	    insert_edge_(A,B,make_ref(),Props,G)
    end.

%% put edge given a edge id! if edge id already exist it must be 
%% removed before added or perhaps just update properties.

put_edge(A0,B0,E,Props,G = #{?TYPE:=graph,e:=Es,is_digraph:=Digraph}) ->
    {A,B} = sort(A0,B0,Digraph),
    case maps:find(E,Es) of
	error ->
	    insert_edge_(A,B,E,Props,G);
	{ok,Ex} ->
	    case Ex of
		#{ ?PT1 := A, ?PT2 := B } -> %% keep edge
		    Es1 = add_edge_props_(E,Props,Es),
		    G#{ e => Es1 };		    
		_ ->
		    %% remove edge and add it again
		    G1 = remove_edge(E, G),
		    Ex1 = Ex#{ ?TYPE=>edge,?ID=>E,?PT1=>A,?PT2=>B},
		    add_edge_(A,B,E,Ex1,Props,G1)
	    end
    end.

insert_edge_(A,B,E,Props,G=#{?TYPE:=graph}) ->
    Ex = #{ ?TYPE=>edge,?ID=>E,?PT1=>A,?PT2=>B},
    add_edge_(A,B,E,Ex,Props,G).

add_edge_(A,B,E,Ex,Props0,G=#{?TYPE:=graph,e:=Es0,v:=Vs0}) ->
    Ax = put_vertex_(A,[],Vs0),
    Ax1 = add_edge_out_({B,E}, Ax),
    Bx = put_vertex_(B,[],Vs0),
    Bx1 = add_edge_in_({A,E}, Bx),
    Ex1 = if Props0 =:= [] -> Ex;
	     true -> 
		  Props = filter_props(Props0),
		  maps:merge(Ex, maps:from_list(Props))
	  end,
    G#{ v => Vs0# { A => Ax1, B => Bx1 }, e => Es0#{ E => Ex1} }.

%% merge edge properties only
add_edge_props_(E, Props0, Es) ->
    Ex = maps:get(E, Es),
    Props = filter_props(Props0),
    Ex1 = maps:merge(Ex, maps:from_list(Props)),
    Es# { E => Ex1 }.

is_edge(E,#{ ?TYPE :=graph, e:=Es}) ->
    maps:is_key(E, Es).

is_edge(A,B,G) ->
    case edge(A,B,G) of
	false -> false;
	_ -> true
    end.

edge(A,B, #{ ?TYPE := graph, v := Vs, is_digraph:=Digraph}) ->
    edge(sort(A,B,Digraph), Vs).

edge({A,B}, Vs) ->
    case maps:find(A, Vs) of
	error -> false;
	{ok,Vx} ->
	    Out = vertex_out_(Vx),
	    lists:keyfind(B,1,Out)
    end.
    
edges(#{ ?TYPE := graph, e := Es}) ->
    maps:keys(Es).

number_of_edges(#{ ?TYPE := graph, e:=Es }) ->
    maps:size(Es).

fold_edges(Fun, Acc, #{ ?TYPE := graph, e := Es}) ->
    maps:fold(fun(E,Ex,Ai) ->
		      #{ ?PT1 := V, ?PT2 := W } = Ex,
		      Fun(V,W,E,Ai) 
	      end, Acc, Es).

%% like fold_edges but pass the vertex map instead
fold_xedges(Fun, Acc, #{ ?TYPE := graph, e := Es}) ->
    maps:fold(fun(_E,Ex,Ai) -> Fun(Ex,Ai) end, Acc, Es).

get_edge(E, #{?TYPE:=graph,e:=Es}) ->
    Ex = maps:get(E, Es),
    filter_props(maps:to_list(Ex)).

get_edge(E, Key, #{?TYPE:=graph,e:=Es}) ->
    Ex = maps:get(E, Es),
    maps:get(Key,Ex).

get_edge(A0,B0,Key,#{?TYPE:=graph,e:=Es,v:=Vs,is_digraph:=Digraph}) ->
    {A,B} = sort(A0,B0,Digraph),
    Ax = maps:get(A,Vs),
    Out = vertex_out_(Ax),
    case lists:keyfind(B,1,Out) of
	{B,E} ->
	    Ex = maps:get(E,Es),
	    maps:get(Key,Ex)
    end.

get_edge(A0,B0,Key,#{?TYPE:=graph,e:=Es,v:=Vs,is_digraph:=Digraph},Default) ->
    {A,B} = sort(A0,B0,Digraph),
    Ax = maps:get(A,Vs),
    Out = vertex_out_(Ax),
    case lists:keyfind(B,1,Out) of
	{B,E} ->
	    Ex = maps:get(E,Es),
	    maps:get(Key,Ex,Default)
    end.

%% @doc
%% Return a list of "out" neighbours of G, that is
%% vertices w such that {v,w} is an edge in G.
%% @end

fold_out(Fun, Acc, V, #{?TYPE:=graph,v:=Vs,is_digraph:=Digraph}) ->
    Vx = maps:get(V, Vs),
    case Digraph of
	true ->
	    lists:foldl(fun({W,E},Ai) -> Fun(V,W,E,Ai) end, 
			Acc, vertex_out_(Vx));
	false ->
	    lists:foldl(fun({W,E},Ai) -> Fun(V,W,E,Ai) end, 
			Acc, vertex_out_(Vx) ++ vertex_in_(Vx))
    end.

out_edges(V, G = #{?TYPE:=graph}) ->
    fold_out(fun(Vi,W,_E,Acc) -> [{Vi,W}|Acc] end, [], V, G).

out_neighbours(V, G = #{?TYPE:=graph}) ->
    fold_out(fun(Vi,W,_E,Acc) ->
		     if V =:= Vi -> [W|Acc];
			true -> [Vi|Acc]
		     end
	     end, [], V, G).

fanout(V, #{?TYPE:=graph, v:=Vs, is_digraph:=Digraph}) ->
    Vx = maps:get(V, Vs),
    case Digraph of
	true -> length(vertex_out_(Vx));
	false ->length(vertex_out_(Vx)) +
		    length(vertex_in_(Vx))
    end.

%% @doc
%% Return a list of "in" neighbours of G, that is
%% vertices w such that {w,v} is an edge in G
%% @end

fold_in(Fun, Acc, V, #{?TYPE:=graph,v:=Vs,is_digraph:=Digraph}) ->
    Vx = maps:get(V, Vs),
    case Digraph of
	true ->	
	    lists:foldl(fun({W,E},Ai) -> Fun(W,V,E,Ai) end, 
			Acc, vertex_in_(Vx));
	false ->
	    lists:foldl(fun({W,E},Ai) -> Fun(V,W,E,Ai) end, 
			Acc, vertex_out_(Vx) ++ vertex_in_(Vx))
    end.

in_edges(V, G = #{?TYPE:=graph}) ->
    fold_in(fun(W,Vi,_E,Acc) ->
		    if Vi =:= V -> [{W,Vi}|Acc];
		       true -> [{Vi,W}|Acc]
		    end
	    end, [], V, G).

in_neighbours(V, G = #{?TYPE:=graph}) ->
    fold_in(fun(W,Vi,_E,Acc) ->
		    if Vi =:= V -> [W|Acc];
		       true -> [Vi|Acc]
		    end
	    end, [], V, G).

fanin(V, #{?TYPE:=graph, v:=Vs, is_digraph:=Digraph}) ->
    Vx = maps:get(V, Vs),
    case Digraph of
	true ->
	    length(vertex_in_(Vx));
	false ->
	    length(vertex_in_(Vx)) + length(vertex_out_(Vx))
    end.

%%
%% Vertices
%%

is_vertex(V, #{ ?TYPE := graph, v := Vs}) ->
    maps:is_key(V, Vs).

put_vertex(V, G) ->
    put_vertex(V, [], G).

put_vertex(V, Props, G = #{ ?TYPE := graph, v := Vs0}) when is_list(Props) ->
    Vx1 = put_vertex_(V, Props, Vs0),
    G# { v => Vs0#{ V => Vx1 }}.

put_vertex_(V, Props0, Vs) ->
    Props = filter_props(Props0),
    Vx = case maps:find(V, Vs) of
	     error -> #{ ?ID => V, ?TYPE => vertex, ?IN => [], ?OUT => [] };
	     {ok,Vx0} -> Vx0
	 end,
    maps:merge(Vx, maps:from_list(Props)).

get_vertex(V, #{ ?TYPE := graph, v := Vs}) ->
    filter_props(maps:to_list(maps:get(V, Vs))).

get_vertex(V, Key, #{ ?TYPE := graph, v := Vs}) ->
    Vx = maps:get(V, Vs),
    maps:get(Key, Vx).

get_vertex(V, Key, #{ ?TYPE := graph, v := Vs}, Default) ->
    Vx = maps:get(V, Vs),
    maps:get(Key, Vx, Default).

remove_vertex(V, G = #{ ?TYPE := graph, v := Vs0, e := Es0}) ->
    Vx = maps:get(V, Vs0),
    Edges = lists:usort([E||{_W,E} <- vertex_out_(Vx)] ++
			[E||{_V,E} <- vertex_in_(Vx)]),
    {Es1,Vs1} = remove_edge_list_(Edges,Es0,Vs0),
    Vs2 = maps:remove(V, Vs1),
    G# { e => Es1, v => Vs2 }.


remove_edge(E, G = #{ ?TYPE:=graph, e := Es, v := Vs }) ->
    case maps:is_key(E, Es) of
	true ->
	    {Es1,Vs1} = remove_edge_(E, Es, Vs),
	    G# { e => Es1, v => Vs1 };
	false ->
	    G
    end.

remove_edge(A, B, G = #{ ?TYPE:=graph,e:=Es,v:=Vs}) ->
    case edge(A, B, G) of
	{_AorB, E} ->
	    {Es1,Vs1} = remove_edge_(E, Es, Vs),
	    G# { e => Es1, v => Vs1 };
	false ->
	    G
    end.

remove_edge_(E, Es, Vs) ->
    io:format("remove edge ~w\n", [E]),
    Ex = maps:get(E,Es),    
    #{ ?PT1 := V, ?PT2 := W } = Ex,
    Vx = maps:get(V, Vs),
    Vx1 = remove_edge_out(E, Vx),
    io:format("remove edge out from vertex ~w vx1=~p\n", [V,Vx1]),
    Wx = maps:get(W, Vs),
    Wx1 = remove_edge_in(E, Wx),
    io:format("remove edge in from vertex ~w wx1=~p\n", [W,Wx1]),
    Es1 = maps:remove(E,Es),
    Vs1 = Vs#{ V => Vx1, W => Wx1 },
    {Es1,Vs1}.

remove_edge_list_([E|EdgeList], Es, Vs) ->
    {Es1, Vs1} = remove_edge_(E, Es, Vs),
    remove_edge_list_(EdgeList, Es1, Vs1);
remove_edge_list_([], Es, Vs) -> 
    {Es,Vs}.

vertex_out_(Vx) ->
    maps:get(?OUT, Vx).

vertex_in_(Vx) ->
    maps:get(?IN, Vx).

add_edge_out_(NE={_N,E}, Vx) ->
    Out = vertex_out_(Vx),
    Out1 = case lists:keytake(E, 2, Out) of
	       false -> [NE|Out];
	       {value,{_M,E},Out0} -> [NE|Out0]
	   end,
    Vx#{ ?OUT => Out1 }.

add_edge_in_(NE={_N,E}, Vx) ->
    In = vertex_in_(Vx),
    In1 = case lists:keytake(E, 2, In) of
	       false -> [NE|In];
	       {value,{_M,E},In0} -> [NE|In0]
	   end,
    Vx#{ ?IN => In1 }.

remove_edge_out(E, Vx) ->
    Out = vertex_out_(Vx),
    case lists:keydelete(E, 2, Out) of
	Out -> Vx;
	Out1 -> Vx#{ ?OUT=>Out1}
    end.

remove_edge_in(E, Vx) ->
    In = vertex_in_(Vx),
    case lists:keydelete(E, 2, In) of
	In -> Vx;
	In1 -> Vx#{ ?IN=>In1}
    end.

vertices(#{ ?TYPE := graph, v := Vs}) ->
    maps:keys(Vs).

number_of_vertices(#{ ?TYPE := graph, v := Vs}) ->
    maps:size(Vs).

fold_vertices(Fun, Acc0, #{ ?TYPE := graph, v := Vs}) ->
    maps:fold(fun(V,_Vx,Acc1) -> Fun(V,Acc1) end, Acc0, Vs).

%% like fold_vertices but pass the vertex map instead
fold_xvertices(Fun, Acc0, #{ ?TYPE := graph, v := Vs}) ->
    maps:fold(fun(_V,Vx,Acc1) -> Fun(Vx,Acc1) end, Acc0, Vs).

is_internal_prop(?ID) -> true;
is_internal_prop(?TYPE) -> true;
is_internal_prop(?IN) -> true;
is_internal_prop(?OUT) -> true;
is_internal_prop(?PT1) -> true;
is_internal_prop(?PT2) -> true;
is_internal_prop(_) -> false.

filter_props(Prop) ->
    lists:filter(fun({K,_V}) -> not is_internal_prop(K) end, Prop).
