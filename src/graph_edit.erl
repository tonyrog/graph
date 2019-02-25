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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state,
	{
	 backend,
	 window,
	 background_pixels,
	 pixmap,
	 width,
	 height,
	 background = {255,0,0},
	 pt1    = false,
	 pt2    = false,
	 move   = false,
	 selected = [],
	 shift = false,
	 ctrl  = false,
	 graph
	}).

-define(WIDTH,  800).
-define(HEIGHT, 480).

-define(VERTEX_SIDE, 16).
-define(VERTEX_COLOR, {0,255,0}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start([true]).
start([TTYLogger]) ->
    (catch error_logger:tty(TTYLogger)),
    application:start(lager),
    application:load(epx),
    Width  = application:get_env(graph, screen_width, ?WIDTH),
    Height = application:get_env(graph, screen_height, ?HEIGHT),
    application:ensure_all_started(epx),
    start_link([{screen_width,Width},{screen_height,Height}]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link([{atom(),term()}]) ->
			{ok, Pid :: pid()} |
			{error, Error :: {already_started, pid()}} |
			{error, Error :: term()} |
			ignore.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

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
    Backend = epx_backend:default(),
    Window = epx:window_create(40, 40, Width, Height,
			        [key_press,key_release,
				 motion, left,  %% motion-left-button
				 resize,
				 button_press,button_release]),
    BackgroundPx = epx:pixmap_create(Width, Height, argb),
    Pixmap = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_attach(BackgroundPx, Backend),
    epx:window_attach(Window, Backend),
    State = #state{ backend = Backend,
		    window = Window,
		    background_pixels = BackgroundPx,
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
	    case State#state.ctrl of
		true ->
		    G1 = graph:put_vertex(make_ref(), 
					  [{x,X},{y,Y},{color,?VERTEX_COLOR}],
					  State#state.graph),
		    invalidate(State),
		    {noreply, State#state { graph = G1 }};
		false ->
		    Window = State#state.window,
		    epx:window_enable_events(Window,[motion]),
		    invalidate(State),
		    Sel0 = State#state.selected,
		    case select(X,Y,State#state.graph,[]) of
			[] ->
			    Sel = if State#state.shift -> Sel0; true -> [] end,
			    {noreply,
			     State#state { move = false,
					   selected = Sel,
					   pt1 = {X,Y}, pt2 = {X,Y} }};
			[V|_] ->
			    case lists:member(V, Sel0) of
				true ->
				    {noreply,
				     State#state { move = true,
						   selected = Sel0,
						   pt1={X,Y}, pt2={X,Y} }};
				false ->
				    if State#state.shift ->
					    {noreply,
					     State#state { move = false,
							   selected = [V|Sel0],
							   pt1={X,Y},pt2={X,Y}}};
				       true ->
					    {noreply,
					     State#state { move = true,
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
		    case State#state.move of
			true ->
			    Offset = coords_sub(Pt2, Pt1),
			    G = offset_vertices(State#state.graph,Offset,Sel0),
			    invalidate(State),
			    {noreply, State#state { pt1 = false, pt2 = false,
						    move = false,
						    graph = G }};
			false ->
			    Rect = coords_to_rect(Pt1,Pt2),
			    Sel1 = if State#state.shift -> Sel0; true -> [] end,
			    Sel = select_area(Rect,State#state.graph,Sel1),
			    invalidate(State),
			    {noreply, State#state { pt1 = false, pt2 = false,
						    move = false,
						    selected = Sel }}
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

	{key_press, $\b, _Mod, _code} ->
	    G = lists:foldl(
		  fun(V, Gi) ->
			  graph:remove_vertex(V, Gi)
		  end, State#state.graph, State#state.selected),
	    invalidate(State),
	    {noreply, State#state { graph = G, selected = [] }};

	{key_press, _Sym, Mod, _code} ->
	    %% io:format("Key press ~p mod=~p\n", [_Sym,_Mod]),
	    Shift = lists:member(shift,Mod),
	    Ctrl  = lists:member(ctrl,Mod),
	    State1 = 
		if Shift -> State#state { shift = true };
		   true -> State
		end,
	    State2 = 
		if Ctrl -> State1#state { ctrl = true };
		   true -> State1
		end,
	    {noreply, State2};

	{key_release, _Sym, Mod, _code} ->
	    %% io:format("Key release ~p mod=~p\n", [_Sym,Mod]),
	    Shift = lists:member(shift,Mod),
	    Ctrl  = lists:member(ctrl,Mod),
	    State1 = 
		if Shift -> State#state { shift = false };
		   true -> State
		end,
	    State2 = 
		if Ctrl -> State1#state { ctrl = false };
		   true -> State1
		end,
	    {noreply, State2};

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

select(X,Y,G,Selected) ->
    Side2 = ?VERTEX_SIDE div 2,
    graph:fold_vertices(
      fun(V, Sel) ->
	      case lists:member(V, Sel) of
		  false ->
		      Xi = graph:get_vertex(V, x, G, 0)-Side2,
		      if X >= Xi, X < Xi+?VERTEX_SIDE ->
			      Yi = graph:get_vertex(V, y, G, 0)-Side2,
			      if Y >= Yi, Y < Yi+?VERTEX_SIDE ->
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

offset_vertices(G, Offset, Selected) ->
    graph:fold_vertices(
      fun(V, Gi) ->
	      case lists:member(V, Selected) of
		  false -> Gi;
		  true ->
		      Pos = get_vertex_coord(V, Gi),
		      Pos1 = coords_add(Pos, Offset),
		      set_vertex_pos(V, Gi, Pos1)
	      end
      end, G, G).

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
    {graph:get_vertex(V, x, G, 0), graph:get_vertex(V, y, G, 0)}.

set_vertex_pos(V, G, {X,Y}) ->
    graph:put_vertex(V, [{x,X},{y,Y}], G).

vertex_rect(V, G) ->
    Side2 = ?VERTEX_SIDE div 2,
    Xi = graph:get_vertex(V, x, G, 0)-Side2,
    Yi = graph:get_vertex(V, y, G, 0)-Side2,
    {Xi,Yi,?VERTEX_SIDE,?VERTEX_SIDE}.

rect_overlap(R1,R2) ->
    case epx_rect:intersect(R1, R2) of
	{_,_,0,0} -> false;
	_ -> true
    end.

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
    epx:pixmap_fill(State#state.background_pixels, State#state.background),
    epx_gc:set_fill_style(solid),
    epx_gc:set_border_color(black),
    Side2 = (?VERTEX_SIDE div 2),
    VertexLT = {-Side2,-Side2},
    Offset = if State#state.move ->
		     coords_sub(State#state.pt2,State#state.pt1);
		true ->
		     {0,0}
	     end,
    graph:fold_vertices(
      fun(V, Acc) ->
	      {X,Y} = 
		  case lists:member(V, Selected) of
		      true ->
			  epx_gc:set_border_width(2),
			  Coord = coords_add(get_vertex_coord(V, G),Offset),
			  coords_add(Coord, VertexLT);
		      false ->
			  epx_gc:set_border_width(0),
			  Coord = get_vertex_coord(V, G),
			  coords_add(Coord, VertexLT)
		  end,
	      Color = graph:get_vertex(V, color, G, ?VERTEX_COLOR),
	      epx_gc:set_fill_color(Color),
	      epx:draw_ellipse(State#state.background_pixels,
			       X, Y, ?VERTEX_SIDE, ?VERTEX_SIDE),
	      Acc
      end, [], G),

    epx_gc:set_border_width(1),
    if State#state.move -> ok;
       State#state.pt1 =:= false; State#state.pt2 =:= false -> ok;
       true ->
	    Rect = coords_to_rect(State#state.pt1,State#state.pt2),
	    epx_gc:set_fill_color({100,100,100,100}),
	    epx_gc:set_fill_style(blend),
	    epx:draw_rectangle(State#state.background_pixels, Rect)
    end,
    epx:pixmap_draw(State#state.background_pixels, State#state.window,
		    0, 0, 0, 0, 
		    State#state.width, State#state.height).
