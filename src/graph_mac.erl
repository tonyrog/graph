%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    MacGraph import/export
%%% @end
%%% Created :  2 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_mac).

-export([load/1]).
-export([save/2]).
-compile(export_all).

-type int16() :: -16#8000 .. 16#7fff.
-type uint16() :: 16#0000 .. 16#ffff.
-type int32() :: -16#80000000 .. 16#7fffffff.
-type uint32() :: 16#00000000 .. 16#ffffffff.

-record(vpoint,
	{
	 v :: int32(),
	 h :: int32()
	}).

-record(point,
	{
	 v :: int16(),
	 h :: int16()
	}).

-record(rect,
	{
	 x :: int16(),
	 y :: int16(),
	 w :: uint16(),
	 h :: uint16()
	}).

-define(NameLabel,    0).
-define(NumberLabel,  1).
-define(CNameLabel,   2).
-define(CNumberLabel, 3).

-type labeltype() :: name_label | number_label | cname_label | cnumber_label.

%% resource fork
-record(doc_state,
	{
	 theShowVertexLabels :: boolean(),
	 theShowEdgeLabels :: boolean(),
	 theVertexLabelType :: labeltype(),
	 theEdgeLabelType :: labeltype(),
	 theLastVertexName :: int32(),
	 theLastEdgeName :: int32(),
	 theWindowRect :: #rect{},
	 theScrollPosition :: #vpoint{}, 
	 theGridShows :: boolean(),
	 theHGridOn :: boolean(),
	 theVGridOn :: boolean(),
	 theGridStep :: #point{}
	}).

%% Graph object type numbers
-define(IDVertex,               1).     %% ID of TVertex
-define(IDComplexVertex,	2).     %% ID of TComplexVertex 
	
-define(IDNode,			3).	%% ID of TNode 
-define(IDComplexNode,		4).	%% ID of TComplexNode 
	
-define(IDEdge,			5).	%% ID of TEdge 
-define(IDDirectedEdge,		6).	%% ID of TDirectedEdge 

-define(IDLineEdge,		7).	%% ID of TEdge 
-define(IDDirectedLineEdge,	8).	%% ID of TDirectedEdge 
	
-define(kMaxID,			8).

-define(kNodeRadius,		5).
-define(kComplexNodeRadius,	10).

-record(graph_data,
	{
	 nVertices :: int16(),
	 nEdges :: int16()
	}).

-record(graph_object_data,
	{
	 theSelected :: boolean() 
	}).

-record(vertex_data,
	{
	 theSuper :: int16(),	%% The enumerated super vertex or -1 for none 
	 theEnum :: int16()	%% The enumerated data unique for this vertex on file 
	}).

-record(node_data,
	{
	 thePosition :: #point{}, %% Midpoint of the vertex 
	 theColor :: int16()	 %% Current node color 
	}).

-record(edge_data,
	{
	 theDirected :: boolean(), %% True if directed from x to y 
	 theX :: int16(),      %% The vertex corresponding to theX enumeration 
	 theY :: int16()
	}).

-record(line_edge_data,
	{
	 theColor :: int16(),    %% The edge's current color 
	 theWeight :: int16()    %% weight info 
	}).


load(File) ->
    case file:read_file(File) of
	{ok,Bin} -> decode(Bin);
	Error -> Error
    end.

save(_File, _G) ->
    not_yet.

decode(<<N:16, M:16, Bin/binary>>) ->
    {Es,Tail} = decode_(Bin, []),
    {ok,{#graph_data{nVertices=N, nEdges=M},Es,Tail}}.

%% decoding sofar
decode_(<<?IDNode:16, Len:16, Name:Len/binary,
	  Selected, _Pad,
	  TheSuper:16/signed, TheEnum:16/signed,
	  V:16/signed, H:16/signed,
	  TheColor:16/signed, Bin/binary>>, Acc) ->
    GD = #graph_object_data { theSelected = (Selected=/=0) },
    VD = #vertex_data { theSuper = TheSuper,
			theEnum = TheEnum },
    ND = #node_data { thePosition = #point{ v=V, h=H},
		      theColor = TheColor },
    decode_(Bin, [{node,Name,GD,VD,ND}|Acc]);

decode_(<<?IDLineEdge:16, Len:16, Name:Len/binary,
	  Selected, _Pad1,
	  TheDirected, _Pad2,
	  TheX:16/signed, TheY:16/signed,
	  TheColor:16/signed, TheWeight:16/signed,
	  Bin/binary>>, Acc) ->
    GD = #graph_object_data { theSelected = (Selected=/=0) },
    ED = #edge_data { theDirected = (TheDirected=/=0),
		      theX = TheX,
		      theY = TheY },
    LD = #line_edge_data { theColor = TheColor, theWeight = TheWeight},
    decode_(Bin, [{edge,Name,GD,ED,LD}|Acc]);
decode_(Other, Acc) ->
    {lists:reverse(Acc), Other}.

import({#graph_data{nVertices=_Nv,nEdges=_Ne},Elements,_Tail}) ->
    import(Elements, graph:new(), [], #{}).

import([{node,Name,GD,VD,ND}|Es], G, Selected, Vmap) ->
    V = graph:unique_vertex(),
    #vertex_data { theEnum = TheEnum } = VD,
    #node_data{thePosition=P, theColor=Color} = ND,
    #point{ v=Y, h=X} = P,
    G1 = graph:put_vertex(V,
			  [{name,Name},
			   {enum,TheEnum},
			   {x,X},{y,Y},
			   {color,graph_edit:color(Color)},
			   {width,16},
			   {height,16},
			   {shape, ellipse}], G),
    Vmap1 = maps:put(TheEnum, V, Vmap),
    if GD#graph_object_data.theSelected ->
	    import(Es, G1, [V|Selected], Vmap1);
       true ->
	    import(Es, G1, Selected, Vmap1)
    end;
import([{edge,Name,_GD,ED,LD}|Es], G, Selected, Vmap) ->
    #edge_data { theDirected = TheDirected,
		 theX = TheX,
		 theY = TheY } = ED,
    #line_edge_data { theColor = TheColor, theWeight = TheWeight} = LD,
    V = maps:get(TheX, Vmap),
    W = maps:get(TheY, Vmap),
    G1 = graph:put_edge(V, W, 
			[{name,Name},
			 {color,TheColor},
			 {weight,TheWeight},
			 {directed,TheDirected}],
			G),
    %% if GD#graph_object_data.theSelected -> ...
    import(Es, G1, Selected, Vmap);
import([], G, Selected, _VMap) ->
    {G, Selected}.



    
    
    
