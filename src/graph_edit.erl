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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(WIDTH,  800).
-define(HEIGHT, 480).
-define(VERTEX_SIZE,  16).

-define(SCREEN_COLOR, beige).
-define(SELECT_COLOR, {100,100,100,100}).

-define(VERTEX_COLOR,           darkgray).
-define(VERTEX_BORDER_COLOR,    burlyWood).
-define(VERTEX_HIGHLIGHT_COLOR, white).
-define(EDGE_COLOR,             darkgray).
-define(EDGE_HIGHLIGHT_COLOR,   darkgreen).


-record(state,
	{
	 backend,
	 window,
	 background_pixels,
	 pixmap,
	 width,
	 height,
	 background_color = ?SCREEN_COLOR,
	 select_color = ?SELECT_COLOR,
	 vertex_color = ?VERTEX_COLOR,
	 vertex_highlight_color = ?VERTEX_HIGHLIGHT_COLOR,
	 vertex_border_color = ?VERTEX_BORDER_COLOR,
	 edge_color = ?EDGE_COLOR,
	 edge_highlight_color = ?EDGE_HIGHLIGHT_COLOR,
	 pt1    = false,
	 pt2    = false,
	 operation = none :: none | select | move | vertex | edge,
	 selected = [],   %% list of selected vertices (and edges?)
	 shift = false,   %% add to selection
	 ctrl  = false,   %% add vertex
	 alt   = false,   %% add edge
	 graph
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
    BgColor = proplists:get_value(screen_color, Env, ?SCREEN_COLOR),
    SelectColor = proplists:get_value(select_color, Env, ?SELECT_COLOR),
    VertexColor = proplists:get_value(vertex_color, Env, ?VERTEX_COLOR),
    VertexBorderColor = proplists:get_value(vertex_border_color, Env, 
					    ?VERTEX_BORDER_COLOR),
    VertexHighlightColor = proplists:get_value(vertex_highlight_color, Env, 
					       ?VERTEX_HIGHLIGHT_COLOR),
    EdgeColor = proplists:get_value(edge_color, Env, ?EDGE_COLOR),
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
		    background_color  = BgColor,
		    select_color = SelectColor,
		    vertex_color  = VertexColor,
		    vertex_border_color  = VertexBorderColor,
		    vertex_highlight_color  = VertexHighlightColor,
		    edge_color  = EdgeColor,
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
		    VertexColor = State#state.vertex_color,
		    G1 = graph:put_vertex(make_ref(),
					  [{x,X},{y,Y},{color,VertexColor}],
					  State#state.graph),
		    invalidate(State),
		    {noreply, State#state { operation = vertex,
					    pt1 = {X,Y}, pt2 = {X,Y},
					    graph = G1 }};

	       State#state.alt ->  %% Add edge
		    case select(X,Y,State#state.graph,[]) of
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
		    case select(X,Y,State#state.graph,[]) of
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
	    case State#state.pt1 of
	       false ->
		    {noreply,State};
	       Pt1 ->
		    Pt2 = {X,Y},
		    Sel0 = State#state.selected,
		    case State#state.operation of
			move ->
			    Offset = coords_sub(Pt2, Pt1),
			    G = move_vertices(Sel0,Offset,State#state.graph),
			    invalidate(State),
			    {noreply, State#state { pt1 = false, pt2 = false,
						    operation = none,
						    graph = G }};
			select ->
			    Rect = coords_to_rect(Pt1,Pt2),
			    Sel1 = if State#state.shift -> Sel0; true -> [] end,
			    Sel = select_area(Rect,State#state.graph,Sel1),
			    invalidate(State),
			    {noreply, State#state { pt1 = false, pt2 = false,
						    operation = none,
						    selected = Sel }};
			edge ->
			    invalidate(State),
			    case select(X,Y,State#state.graph,[]) of
				[] ->
				    {noreply, State#state { pt1 = false, 
							    pt2 = false,
							    operation = none }};
				[W|_] when W =:= hd(State#state.selected) ->
				    {noreply, State#state { pt1 = false, 
							    pt2 = false,
							    operation = none }};
				[W|_] ->
				    [V] = State#state.selected,
				    EdgeColor = State#state.edge_color,
				    G = graph:put_edge(V, W, 
						       [{color,EdgeColor}],
						       State#state.graph),
				    {noreply, State#state { pt1 = false,
							    pt2 = false,
							    selected = [W],
							    graph = G,
							    operation = none }}
			    end;
			_ ->
			    invalidate(State),
			    {noreply, State#state { pt1 = false, pt2 = false,
						    operation = none }}
		    end
	    end;

	{motion, [left], {X,Y,_Z}} ->
	    case State#state.pt1 of
	       false ->
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
	    G = State#state.graph,
	    Sel0 = State#state.selected,
	    G1 = case Sym of
		     up    -> move_vertices(Sel0, {0, -1}, G);
		     down  -> move_vertices(Sel0, {0,  1}, G);
		     left  -> move_vertices(Sel0, {-1, 0}, G);
		     right -> move_vertices(Sel0, {1,  0}, G);
		     $\b   -> remove_vertices(Sel0, G);
		     _ -> G
		 end,
	    Sel = case Sym of
		      $\b -> [];
		      _ -> State#state.selected
		  end,
	    invalidate(State),
	    {noreply, State#state { graph=G1, selected = Sel,
				    shift=Shift, ctrl=Ctrl, alt=Alt}};

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

select(X,Y,G,Selected) ->
    Side2 = ?VERTEX_SIZE div 2,
    graph:fold_vertices(
      fun(V, Sel) ->
	      case lists:member(V, Sel) of
		  false ->
		      Xi = graph:get_vertex_by_id(V, x, G, 0)-Side2,
		      if X >= Xi, X < Xi+?VERTEX_SIZE ->
			      Yi = graph:get_vertex_by_id(V, y, G, 0)-Side2,
			      if Y >= Yi, Y < Yi+?VERTEX_SIZE ->
				      [V | Sel];
				 true ->
				      Sel
			      end;
			 true ->
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
    Side2 = ?VERTEX_SIZE div 2,
    X = graph:get_vertex_by_id(V, x, G, 0)-Side2,
    Y = graph:get_vertex_by_id(V, y, G, 0)-Side2,
    {X,Y,?VERTEX_SIZE,?VERTEX_SIZE}.

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

draw(State = #state { graph = G, selected = Selected }) ->
    epx:pixmap_fill(State#state.background_pixels,State#state.background_color),
    epx_gc:set_fill_style(solid),
    Offset = if State#state.operation =:= move ->
		     coords_sub(State#state.pt2,State#state.pt1);
		true ->
		     {0,0}
	     end,
    EdgeColor = State#state.edge_color,
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
    VertexColor = State#state.vertex_color,
    graph:fold_vertices(
      fun(V, Acc) ->
	      Rect0 = vertex_rect(V, G),
	      Rect = 
		  case lists:member(V, Selected) of
		      true ->
			  epx_gc:set_border_width(2),
			  epx_gc:set_border_color(
			    State#state.vertex_border_color),
			  rect_offset(Rect0, Offset);
		      false ->
			  epx_gc:set_border_width(0),
			  Rect0
		  end,
	      %% high light vertext under pt2
	      if State#state.operation =:= edge ->
		      case point_in_rect(State#state.pt2, Rect) of
			  true ->
			      epx_gc:set_border_width(2),
			      epx_gc:set_border_color(
				State#state.vertex_highlight_color);
			  false ->
			      ok
		      end;
		 true ->
		      ok
	      end,
	      Color = graph:get_vertex_by_id(V, color, G, VertexColor),
	      epx_gc:set_fill_color(Color),
	      epx:draw_ellipse(State#state.background_pixels,Rect),
	      Acc
      end, [], G),

    epx_gc:set_border_width(1),
    if State#state.pt1 =:= false; State#state.pt2 =:= false -> 
	    ok;
       true ->
	    case State#state.operation of
		select ->
		    Rect = coords_to_rect(State#state.pt1,State#state.pt2),
		    epx_gc:set_fill_color(State#state.select_color),
		    epx_gc:set_fill_style(blend),
		    epx:draw_rectangle(State#state.background_pixels, Rect);
		edge ->
		    epx_gc:set_foreground_color(
		      State#state.edge_highlight_color),
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
