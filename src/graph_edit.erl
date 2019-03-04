%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Simple? Graph editor
%%% @end
%%% Created : 24 Feb 2019 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(graph_edit).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([load_mac/1]).
-export([color/1, shape/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(WIDTH,  800).
-define(HEIGHT, 480).

%% color profile with default values
-record(profile,
	{
	 screen_color                  = beige,
	 selection_alpha               = 100,
	 selection_color               = gray,
	 selection_border_width        = 1,
	 selection_border_color        = black,
	 vertex_shape                  = ellipse,
	 vertex_width                  = 16,
	 vertex_height                 = 16,
	 vertex_color                  = darkgray,
	 vertex_border_width           = 1,
	 vertex_border_color           = black,
	 vertex_select_color           = darkgreen,
	 vertex_select_border_width    = 2,
	 vertex_select_border_color    = black,
	 vertex_highlight_color        = darkgray,
	 vertex_highlight_border_width = 2,
	 vertex_highlight_border_color = red,
	 edge_color                    = darkgray,
	 edge_select_color             = darkgreen,
	 edge_highlight_color          = white
	}).
	 
-record(state,
	{
	 backend,
	 window,
	 background_pixels,
	 pixmap,
	 width,
	 height,
	 profile,         %% color profile
	 pt,              %% last button press position
	 pt1,
	 pt2,
	 operation = none :: none | select | move | vertex | edge,
	 selected = [],   %% list of selected vertices (and edges?)
	 shift = false,   %% add to selection
	 ctrl  = false,   %% add vertex
	 alt   = false,   %% add edge
	 graph,           %% the graph
	 clip             %% the cut/copy graph
	}).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start([true]).

start([TTYLogger]) ->
    (catch error_logger:tty(TTYLogger)),
    application:start(lager),
    application:load(epx),
    application:load(graph),
    Width  = application:get_env(graph, screen_width, ?WIDTH),
    Height = application:get_env(graph, screen_height, ?HEIGHT),
    application:ensure_all_started(epx),
    Opts = [{screen_width,Width},{screen_height,Height}],
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

load_mac(File) ->
    gen_server:call(?SERVER, {load_mac, File}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
			      {ok, State :: term(), Timeout :: timeout()} |
			      {ok, State :: term(), hibernate} |
			      {stop, Reason :: term()} |
			      ignore.
init(Options) ->
    process_flag(trap_exit, true),
    %% options list override env
    Env = Options ++ application:get_all_env(graph),
    Width  = proplists:get_value(screen_width, Env, ?WIDTH),
    Height = proplists:get_value(screen_height, Env, ?HEIGHT),
    Profile = load_profile(Env),
    Backend = epx_backend:default(),
    Window = epx:window_create(40, 40, Width, Height,
			        [key_press,key_release,
				 motion, left,  %% motion-left-button
				 resize,
				 button_press,button_release]),
    epx:window_attach(Window, Backend),

    BackgroundPx = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_attach(BackgroundPx, Backend),

    Pixmap = epx:pixmap_create(Width, Height, argb),

    State = #state{ backend = Backend,
		    window = Window,
		    background_pixels = BackgroundPx,
		    profile = Profile,
		    pixmap = Pixmap,
		    width  = Width,
		    height = Height,
		    graph = graph:new(false)
		  },
    invalidate(State),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
			 {reply, Reply :: term(), NewState :: term()} |
			 {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
			 {reply, Reply :: term(), NewState :: term(), hibernate} |
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_call({load_mac,File}, _From, State) ->
    case graph_mac:load(File) of
	Error = {error,_Reason} ->
	    {reply, Error, State};
	{ok,GraphData} ->
	    {G,Selected} = macgraph:import(GraphData),
	    invalidate(State),
	    {reply, ok, State#state { graph = G, 
				      clip = undefined,
				      selected = Selected}};
	{_, _} ->
	    {reply, {error, decode_problem}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
			 {noreply, NewState :: term()} |
			 {noreply, NewState :: term(), Timeout :: timeout()} |
			 {noreply, NewState :: term(), hibernate} |
			 {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({epx_event,W,Event}, State) when State#state.window =:= W ->
    %% io:format("Epx event ~p\n", [Event]),
    handle_epx_event(Event, State);
handle_info(Event, State)  ->
    io:format("Got event ~p\n", [Event]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
				      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_epx_event(Event, State) ->
    case Event of
	{button_press, [left], _Where={X,Y,_Z}} ->
	    if State#state.ctrl ->  %% Add vertex
		    Color = (State#state.profile)#profile.vertex_color,
		    Width = (State#state.profile)#profile.vertex_width,
		    Height = (State#state.profile)#profile.vertex_height,
		    Shape = (State#state.profile)#profile.vertex_shape,
		    V = make_ref(),
		    G1 = graph:put_vertex(V,
					  [{x,X},{y,Y},
					   {color,Color},
					   {height,Height},
					   {width,Width},
					   {shape,Shape}],
					  State#state.graph),
		    invalidate(State),
		    {noreply, State#state { operation = vertex,
					    pt1 = {X,Y}, 
					    pt2 = {X,Y},
					    graph = G1 }};

	       State#state.alt ->  %% Add edge
		    case select({X,Y},State#state.graph,[]) of
			[] ->
			    invalidate(State),
			    {noreply, State#state { selected = [],
						    operation = none }};
			[V|_] ->
			    Window = State#state.window,
			    epx:window_enable_events(Window,[motion]),
			    invalidate(State),
			    {noreply, State#state { operation = edge,
						    selected = [V],
						    pt1 = {X,Y}, pt2 = {X,Y}
						  }}
		    end;

	       true ->
		    Window = State#state.window,
		    epx:window_enable_events(Window,[motion]),
		    invalidate(State),
		    Sel0 = State#state.selected,
		    case select({X,Y},State#state.graph,[]) of
			[] ->
			    Sel = if State#state.shift -> Sel0; true -> [] end,
			    {noreply,
			     State#state { operation = select,
					   selected = Sel,
					   pt1 = {X,Y}, pt2 = {X,Y} }};
			[V|_] ->
			    case lists:member(V, Sel0) of
				true ->
				    Sel = if State#state.shift ->
						  Sel0 -- [V];
					     true ->
						  Sel0
					  end,
				    {noreply,
				     State#state { operation = move,
						   selected = Sel,
						   pt1={X,Y}, pt2={X,Y} }};
				false ->
				    if State#state.shift ->
					    {noreply,
					     State#state { operation = select,
							   selected = [V|Sel0],
							   pt1={X,Y},pt2={X,Y}}};
				       true ->
					    {noreply,
					     State#state { operation = move,
							   selected = [V],
							   pt1={X,Y},pt2={X,Y}}}
				    end
			    end
		    end
	    end;

	{button_release, [left], _Where={X,Y,_Z}} ->
	    Window = State#state.window,
	    epx:window_disable_events(Window,[motion]),
	    State1 = State#state { pt = {X,Y} },
	    case State1#state.pt1 of
		undefined ->
		    {noreply,State1};
	       Pt1 ->
		    Pt2 = {X,Y},
		    Sel0 = State1#state.selected,
		    case State1#state.operation of
			move ->
			    Offset = coords_sub(Pt2, Pt1),
			    G = move_vertices(Sel0,Offset,State1#state.graph),
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined, 
						     pt2 = undefined,
						     operation = none,
						     graph = G }};
			select ->
			    Rect = coords_to_rect(Pt1,Pt2),
			    Sel1 = if State1#state.shift->Sel0; true -> [] end,
			    Sel = select_area(Rect,State1#state.graph,Sel1),
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined,
						    pt2 = undefined,
						    operation = none,
						    selected = Sel }};
			edge ->
			    invalidate(State1),
			    case select({X,Y},State1#state.graph,[]) of
				[] ->
				    {noreply, State1#state { pt1 = undefined,
							    pt2 = undefined,
							    operation = none }};
				[W|_] when W =:= hd(State1#state.selected) ->
				    {noreply, State1#state { pt1 = undefined,
							    pt2 = undefined,
							    operation = none }};
				[W|_] ->
				    [V] = State1#state.selected,
				    Color = (State1#state.profile)#profile.edge_color,
				    G = graph:put_edge(V, W, 
						       [{color,Color}],
						       State1#state.graph),
				    {noreply, State1#state { pt1 = undefined,
							    pt2 = undefined,
							    selected = [W],
							    graph = G,
							    operation = none }}
			    end;
			_ ->
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined,
						    pt2 = undefined,
						    operation = none }}
		    end
	    end;

	{motion, [left], {X,Y,_Z}} ->
	    case State#state.pt1 of
		undefined ->
		    {noreply, State};
	       _Pt1 ->
		    invalidate(State),
		    {noreply, State#state { pt2 = {X,Y}}}
	    end;

	{key_press, Sym, Mod, _code} ->
	    %% io:format("Key press ~p mod=~p\n", [Sym,Mod]),
	    Shift = case lists:member(shift,Mod) of
			true -> true;
			false -> State#state.shift
		    end,
	    Ctrl = case lists:member(ctrl,Mod) of
		       true -> true;
		       false -> State#state.ctrl
		   end,
	    Alt = case lists:member(alt,Mod) of
		      true -> true;
		      false -> State#state.alt				   
		  end,
	    State1 = State#state { shift=Shift, ctrl=Ctrl, alt=Alt},
	    State2 = graph_command(Sym, State#state.selected, State1),
	    invalidate(State2),
	    {noreply, State2};

	{key_release, _Sym, Mod, _code} ->
	    %% %% io:format("Key release ~p mod=~p\n", [_Sym,Mod]),
	    Shift = case lists:member(shift,Mod) of
			true -> false;
			false -> State#state.shift
		    end,
	    Ctrl = case lists:member(ctrl,Mod) of
		       true -> false;
		       false -> State#state.ctrl
		   end,
	    Alt = case lists:member(alt,Mod) of
		      true -> false;
		      false -> State#state.alt
		  end,
	    {noreply, State#state { shift = Shift, ctrl = Ctrl, alt = Alt }};

	{resize, {_W,_H,_D}} ->
	    invalidate(State),
	    {noreply, State};
	
	redraw ->
	    flush_redraw(State),
	    try draw(State) of
		_ ->
		    ok
	    catch
		error:Reason:Stack ->
		    io:format("Error: ~p\n~p\n", [Reason,Stack])
	    end,
	    {noreply, State};

	close -> 
	    {stop,normal,State};

	destroy ->
	    epx:window_detach(State#state.window),
	    {stop,normal,State};

	_ ->
	    io:format("Epx event = ~p\n", [Event]),
	    {noreply,State}
    end.

graph_command(up, Selected, State) ->
    G = move_vertices(Selected, {0,-1}, State#state.graph),
    State#state { graph=G };
graph_command(down, Selected, State) ->
    G = move_vertices(Selected, {0,1}, State#state.graph),
    State#state { graph=G };
graph_command(left, Selected, State) ->
    G = move_vertices(Selected, {-1,0}, State#state.graph),
    State#state { graph=G };
graph_command(right, Selected, State) ->
    G = move_vertices(Selected, {1,0}, State#state.graph),
    State#state { graph=G };
graph_command(Command, Selected, State) when Command >= $0, Command =< $9,
					     State#state.ctrl ->
    G = set_vertices(Selected, [{color,color(Command-$0)}],State#state.graph),
    State#state { graph=G };
graph_command(Command, Selected, State) when Command >= $0, Command =< $9,
					     State#state.alt ->
    G = set_vertices(Selected, [{shape,shape(Command-$0)}],State#state.graph),
    State#state { graph=G };
graph_command($\b, Selected, State) ->
    G = kill_graph(Selected, State#state.graph),
    State#state { graph=G, selected = [] };
graph_command($x, Selected, State) when State#state.ctrl ->
    {G,Clip} = if Selected =:= [] ->
		       {State#state.graph, State#state.clip};
		  true ->
		       cut_graph(Selected, State#state.graph)
	       end,
    State#state { graph=G, clip=Clip, selected = [] };
graph_command($c, Selected, State) when State#state.ctrl ->
    G = if Selected =:= [] -> State#state.graph;
	   true -> copy_graph(Selected, State#state.graph)
	end,
    State#state { clip=G };
graph_command($v, _Selected, State) when State#state.ctrl ->
    Pos = case State#state.pt of
	      undefined -> {State#state.width div 2,
			    State#state.height div 2};
	      Pt -> Pt
	  end,
    {G,Vs} = paste_graph(State#state.graph, State#state.clip, Pos),
    State#state { graph=G, selected = Vs };
graph_command($C, Selected, State) ->
    G = complete_graph(Selected, State#state.graph, State),
    State#state { graph=G };
graph_command($I, Selected, State) ->
    G = complement_graph(Selected, State#state.graph, State),
    State#state { graph=G };

graph_command(Command, _Selected, State) ->
    io:format("Command = ~p\n", [Command]),
    State.

color(0) -> gray;
color(1) -> red;
color(2) -> green;
color(3) -> blue;
color(4) -> brown;
color(5) -> yellow;
color(6) -> orange;
color(7) -> purple;
color(8) -> drakgreen;
color(9) -> lightgreen;
color(_) -> gray.

shape(0) -> ellipse;
shape(1) -> rectangle;
shape(2) -> roundrect;
shape(3) -> triangle;
shape(_) -> ellipse.

-define(env(Key, Env, Default),
	proplists:get_value(Key, Env, Default#profile.Key)).

%% load #profile from environment
load_profile(E) ->
    D = #profile{},
    {Width,Height} =
	case proplists:get_value(size, E, unset) of
	    unset ->
		{?env(vertex_width, E, D),?env(vertex_height, E, D)};
	    Size ->
		{Size, Size}
	end,
    #profile {
       screen_color = ?env(screen_color, E, D),
       selection_alpha = ?env(selection_alpha, E, D),
       selection_color = ?env(selection_color, E, D),
       selection_border_width = ?env(selection_border_width, E, D),
       selection_border_color = ?env(selection_border_color, E, D),
       vertex_shape           = ?env(vertex_shape, E, D),
       vertex_width           = Width,
       vertex_height          = Height,
       vertex_color           = ?env(vertex_color, E, D),
       vertex_border_width     = ?env(vertex_border_width, E, D),
       vertex_border_color    = ?env(vertex_border_color, E, D),
       vertex_select_color    = ?env(vertex_select_color, E, D),
       vertex_select_border_width = ?env(vertex_select_border_width,E,D),
       vertex_select_border_color= ?env(vertex_select_border_color,E,D),
       vertex_highlight_color = ?env(vertex_highlight_color,E,D),
       vertex_highlight_border_width = ?env(vertex_highlight_border_width,E,D),
       vertex_highlight_border_color = ?env(vertex_highlight_border_color,E,D),
       edge_color = ?env(edge_color,E,D),
       edge_select_color = ?env(edge_select_color,E,D),
       edge_highlight_color = ?env(edge_highlight_color,E,D)
      }.

complete_graph(Vs, G, State) ->
    lists:foldl(
      fun(V, Gi) ->
	      lists:foldl(
		fun(W, Gj) ->
			case graph:is_edge(V, W, G) of
			    true -> Gj;
			    false ->
				Color = (State#state.profile)#profile.edge_color,
				graph:put_edge(V, W, 
					       [{color,Color}],
					       Gj)
			end
		end, Gi, Vs--[V])
      end, G, Vs).

complement_graph(Vs, G, State) ->
    lists:foldl(
      fun(V, Gi) ->
	      lists:foldl(
		fun(W, Gj) ->
			case graph:is_edge(V, W, G) of
			    true ->
				graph:remove_edge(V, W, Gj);
			    false ->
				Color = (State#state.profile)#profile.edge_color,
				graph:put_edge(V, W, 
					       [{color,Color}],
					       Gj)
			end
		end, Gi, Vs--[V])
      end, G, Vs).

%% copy a graph H into existing graph G on new coordinates
paste_graph(G, H, {X,Y}) ->
    Vs = graph:vertices(H),
    Map = maps:from_list([{V,make_ref()}||V <- Vs]),
    Gn1 = lists:foldl(
	    fun(V,Gi) ->
		    Props0 = graph:get_vertex_by_id(V, H),
		    {value,{x,Xv},Props1} = lists:keytake(x, 1, Props0),
		    {value,{y,Yv},Props2} = lists:keytake(y, 1, Props1),
		    A = maps:get(V, Map),
		    Xi = Xv + X,
		    Yi = Yv + Y,
		    graph:put_vertex(A, [{x,Xi},{y,Yi}|Props2], Gi)
	    end, G, Vs),
    Gn2 = graph:fold_edges(
	    fun(V,W,E,Gj) ->
		    Props = graph:get_edge_by_id(E,H),
		    A = maps:get(V, Map),
		    B = maps:get(W, Map),
		    graph:put_edge(A, B, Props, Gj)
	    end, Gn1, H),
    {Gn2, maps:fold(fun(_K,V,Acc) -> [V|Acc] end, [], Map)}.
	    

%% Copy selected vertices in G into a new graph
copy_graph(Vs, G) ->
    Gn = graph:new(false),
    Map = maps:from_list([{V,make_ref()}||V <- Vs]),
    Gn1 = lists:foldl(
	    fun(V,Gi) -> 
		    Props = graph:get_vertex_by_id(V, G),
		    A = maps:get(V, Map),
		    graph:put_vertex(A, Props, Gi)
	    end, Gn, Vs),
    {Xc,Yc} = graph_center(Gn1),
    Gn2 = offset_graph(Gn1, {-Xc,-Yc}),
    Es = [{V,W} || V <- Vs, W <- Vs, V < W],
    lists:foldl(
      fun({V,W},Gj) ->
	      case graph:is_edge(V, W, G) of
		  true ->
		      Props = graph:get_edge(V, W, G),
		      A = maps:get(V, Map),
		      B = maps:get(W, Map),
		      graph:put_edge(A, B, Props, Gj);
		  false ->
		      Gj
	      end
      end, Gn2, Es).

cut_graph(Vs, G) ->
    {kill_graph(Vs, G),copy_graph(Vs, G)}.

kill_graph(Vs, G) ->
    remove_vertices(Vs, G).

remove_vertices([V|Vs], G) ->
    remove_vertices(Vs, graph:remove_vertex(V, G));
remove_vertices([], G) -> G.

move_vertices([V|Vs], Offset, G) ->
    Pos = get_vertex_coord(V, G),
    Pos1 = coords_add(Pos, Offset),
    G1 = set_vertex_pos(V, G, Pos1),
    move_vertices(Vs, Offset, G1);
move_vertices([], _Offset, G) ->
    G.

set_vertices([V|Vs], Attr, G) ->
    G1 = graph:put_vertex(V, Attr, G),
    set_vertices(Vs, Attr, G1);
set_vertices([], _Attr, G) ->
    G.

%% find center point in graph 
graph_center(undefined) -> {0,0};
graph_center(G) ->
    N = graph:number_of_vertices(G),
    {Xs,Ys} = graph:fold_vertices(
		fun(V, Sum) ->
			X = graph:get_vertex_by_id(V, x, G, 0),
			Y = graph:get_vertex_by_id(V, y, G, 0),
			coords_add(Sum,{X,Y})
		end, {0,0}, G),
    if N =:= 0 -> {0,0};
       true -> {round(Xs/N), round(Ys/N)}
    end.

offset_graph(G, Offset) ->
    move_vertices(graph:vertices(G), Offset, G).

%% fixme only select first!
select(Pos,G,Selected) ->
    graph:fold_vertices(
      fun(V, Sel) ->
	      case lists:member(V, Sel) of
		  false ->
		      Rv = vertex_rect(V, G),
		      case point_in_rect(Pos, Rv) of
			  true ->
			      [V | Sel];
			  false ->
			      Sel
		      end;
		  true ->
		      Sel
	      end
      end, Selected, G).

select_area(Rect,G,Selected) ->
    graph:fold_vertices(
      fun(V, Sel) ->
	      case lists:member(V, Sel) of
		  false ->
		      Rv = vertex_rect(V, G),
		      case rect_overlap(Rect, Rv) of
			  true ->
			      [V | Sel];
			  false ->
			      Sel
		      end;
		  true ->
		      Sel
	      end
      end, Selected, G).

rect_offset({X,Y,W,H}, {X1,Y1}) ->
    {X+X1,Y+Y1,W,H}.

coords_sub({X1,Y1},{X0,Y0}) ->
    {X1-X0,Y1-Y0}.

coords_add({X1,Y1},{X0,Y0}) ->
    {X1+X0,Y1+Y0}.

coords_to_rect({X0,Y0},{X1,Y1}) ->
    X = min(X0,X1),
    Y = min(Y0,Y1),
    W = abs(X1-X0) + 1,
    H = abs(Y1-Y0) + 1,
    {X,Y,W,H}.

get_vertex_coord(V,G) ->
    {graph:get_vertex_by_id(V, x, G, 0), graph:get_vertex_by_id(V, y, G, 0)}.

set_vertex_pos(V, G, {X,Y}) ->
    graph:put_vertex(V, [{x,X},{y,Y}], G).

vertex_rect(V, G) ->
    Width = graph:get_vertex_by_id(V, width, G, 16),
    Height = graph:get_vertex_by_id(V, height, G, 16),
    X = graph:get_vertex_by_id(V, x, G, 0)-(Width div 2),
    Y = graph:get_vertex_by_id(V, y, G, 0)-(Height div 2),
    {X,Y,Width,Height}.

vertex_shape(V, G) ->
    graph:get_vertex_by_id(V, shape, G, ellipse).

rect_overlap(R1,R2) ->
    case epx_rect:intersect(R1, R2) of
	{_,_,0,0} -> false;
	_ -> true
    end.

point_in_rect({X1,Y1}, {X2,Y2,W,H}) ->
    (X1 >= X2) andalso (X1 =< X2+W-1) andalso
    (Y1 >= Y2) andalso (Y1 =< Y2+H-1).

invalidate(State) ->
    self() ! {epx_event, State#state.window, redraw}.

flush_redraw(State) ->
    receive
	{epx_event, Window, redraw} when State#state.window =:= Window ->
	    flush_redraw(State)
    after 0 ->
	    State
    end.

alpha_color(A,{_,R,G,B}) -> {A,R,G,B};
alpha_color(A,{R,G,B}) -> {A,R,G,B};
alpha_color(A,Name) when is_list(Name); is_atom(Name) ->
    alpha_color(A, epx_color:from_name(Name)).

draw(State = #state { graph = G, selected = Selected, profile = Profile }) ->
    epx:pixmap_fill(State#state.background_pixels,Profile#profile.screen_color),
    epx_gc:set_fill_style(solid),
    Offset = if State#state.operation =:= move ->
		     coords_sub(State#state.pt2,State#state.pt1);
		true ->
		     {0,0}
	     end,
    EdgeColor = Profile#profile.edge_color,
    graph:fold_edges(
      fun(V,W,E,Acc) ->
	      Vxy0 = get_vertex_coord(V, G),
	      Vxy  = case lists:member(V, Selected) of
			 true ->
			     coords_add(Vxy0,Offset);
			 false ->
			     Vxy0
		     end,
	      Wxy0 = get_vertex_coord(W, G),
	      Wxy  = case lists:member(W, Selected) of
			 true ->
			     coords_add(Wxy0,Offset);
			 false ->
			     Wxy0
		     end,
	      Color = graph:get_edge_by_id(E, color, G, EdgeColor),
	      epx_gc:set_foreground_color(Color),
	      epx_gc:set_line_width(1),
	      epx:draw_line(State#state.background_pixels,Vxy, Wxy),
	      Acc
      end, [], G),

    VertexColor = Profile#profile.vertex_color,
    graph:fold_vertices(
      fun(V, Acc) ->
	      Rect0 = vertex_rect(V, G),
	      Rect = 
		  case lists:member(V, Selected) of
		      true ->
			  epx_gc:set_border_width(
			    Profile#profile.vertex_select_border_width),
			  epx_gc:set_border_color(
			    Profile#profile.vertex_select_border_color),
			  rect_offset(Rect0, Offset);
		      false ->
			  epx_gc:set_border_width(
			    Profile#profile.vertex_border_width),
			  epx_gc:set_border_color(
			    Profile#profile.vertex_border_color),
			  Rect0
		  end,
	      %% high light vertext under pt2
	      if State#state.operation =:= edge ->
		      case point_in_rect(State#state.pt2, Rect) of
			  true ->
			      epx_gc:set_border_width(
				Profile#profile.vertex_highlight_border_width),
			      epx_gc:set_border_color(
				Profile#profile.vertex_highlight_border_color),
			      epx_gc:set_fill_color(
				Profile#profile.vertex_highlight_color);
			  false ->
			      Color = graph:get_vertex_by_id(V,color,G, 
							     VertexColor),
			      epx_gc:set_fill_color(Color)
		      end;
		 true ->
		      Color = graph:get_vertex_by_id(V,color,G, 
						     VertexColor),
		      epx_gc:set_fill_color(Color)
	      end,
	      case vertex_shape(V, G) of
		  ellipse ->
		      %% io:format("draw circle ~w\n", [Rect]),
		      epx:draw_ellipse(State#state.background_pixels,Rect);
		  rectangle ->
		      %% io:format("draw square rect ~w\n", [Rect]),
		      epx:draw_rectangle(State#state.background_pixels,Rect);
		  roundrect ->
		      %% io:format("draw round rect ~w\n", [Rect]),
		      epx:draw_roundrect(State#state.background_pixels,Rect,8,8);
		  triangle ->
		      {X,Y,W,H} = Rect,
		      P0 = {X + (W div 2), Y},
		      P1 = {X, Y+H-1},
		      P2 = {X+W-1, Y+H-1},
		      epx:draw_triangle(State#state.background_pixels,
					P0, P1, P2)
	      end,
	      Acc
      end, [], G),

    if State#state.pt1 =:= undefined; State#state.pt2 =:= undefined -> 
	    ok;
       true ->
	    case State#state.operation of
		select ->
		    Rect = coords_to_rect(State#state.pt1,State#state.pt2),
		    epx_gc:set_border_width(
		      Profile#profile.selection_border_width),
		    Color = alpha_color(Profile#profile.selection_alpha,
					Profile#profile.selection_color),
		    epx_gc:set_fill_color(Color),
		    epx_gc:set_fill_style(blend),
		    %% io:format("draw selection rect ~w\n", [Rect]),
		    epx:draw_rectangle(State#state.background_pixels, Rect);
		edge ->
		    epx_gc:set_foreground_color(
		      Profile#profile.edge_highlight_color),
		    epx_gc:set_line_width(1),
		    epx:draw_line(State#state.background_pixels,
				  State#state.pt1,State#state.pt2);
		_ ->
		    ok
	    end
    end,
    epx:pixmap_draw(State#state.background_pixels, State#state.window,
		    0, 0, 0, 0, 
		    State#state.width, State#state.height).
