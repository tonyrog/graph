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
-export([graph/1]).
-export([load_mac/1]).
-export([use_graph/1]).
-export([shape/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(WIDTH,  800).
-define(HEIGHT, 480).

-define(USE_OFF_SCREEN, true).
-define(USE_EXPOSURE, false).

-define(WHITE, grey5).
-define(BLACK, grey10).

-include_lib("epx/include/epx_menu.hrl").

%% color profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 screen_color                  = grey2,
	 selection_alpha               = 100,
	 selection_color               = grey,
	 selection_border_width        = 1,
	 selection_border_color        = ?BLACK,
	 vertex_shape                  = ellipse,
	 vertex_width                  = 16,
	 vertex_height                 = 16,
	 vertex_color                  = grey5,
	 vertex_border_width           = 1,
	 vertex_border_color           = ?BLACK,
	 vertex_select_color           = green2,
	 vertex_select_border_width    = 2,
	 vertex_select_border_color    = ?BLACK,
	 vertex_highlight_color        = grey6,
	 vertex_highlight_border_width = 2,
	 vertex_highlight_border_color = red,
	 edge_color                    = 0,
	 edge_select_color             = green2,
	 edge_highlight_color          = ?WHITE,
	 label_font                    = "Arial",
	 label_font_size               = 12,
	 label_font_color              = ?WHITE,
	 label_background_color        = ?BLACK,
	 label_border_color            = yellow,
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = ?WHITE,
	 menu_background_color         = ?BLACK,
	 menu_border_color             = green
	}).

	 
-record(state,
	{
	 backend,
	 window,
	 pixels,
	 width,
	 height,
	 profile,         %% color profile
	 pt,              %% last button press position
	 pt1,
	 pt2,
	 operation = none :: none | menu | select | move | vertex | edge,
	 selected = [],   %% list of selected vertices (and edges?)
	 keymod = #keymod{}, %% modifiers
	 graph,           %% the graph
	 grid,            %% undefined | {Xstep,Ystep}
	 zoom = 1,        %% zoom factor
	 clip,            %% the cut/copy graph
	 menu,            %% global menu state
	 label_font
	}).

-define(SHIFT(State), (State#state.keymod)#keymod.shift).
-define(CTRL(State), (State#state.keymod)#keymod.ctrl).
-define(ALT(State), (State#state.keymod)#keymod.alt).

%%%===================================================================
%%% API
%%%===================================================================

graph(G) ->
    start([true, {graph,G}]).

start() ->
    start([false]).

start([TTYLogger|Opts0]) ->
    %% (catch error_logger:tty(TTYLogger)),
    application:start(lager),
    application:load(epx),
    application:set_env(epx, use_off_screen, ?USE_OFF_SCREEN),
    application:set_env(epx, use_exposure, ?USE_EXPOSURE),
    %% epx:debug(debug),
    application:load(graph),
    Width  = application:get_env(graph, screen_width, ?WIDTH),
    Height = application:get_env(graph, screen_height, ?HEIGHT),
    application:ensure_all_started(epx),
    Opts = [{screen_width,Width},{screen_height,Height}|Opts0],
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

load_mac(File) ->
    gen_server:call(?SERVER, {load_mac, File}).

use_graph(G) ->
    gen_server:call(?SERVER, {use_graph, G}).

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
    G = proplists:get_value(graph, Env, graph:new(false)),
    Profile = load_profile(Env),
    MenuProfile = create_menu_profile(Profile),
    Backend = epx_backend:default(),
    Events = 
	[key_press,key_release,
	 %% wheel, left, right, %% motion-left-button
	 configure,    %% resize,
	 button_press,button_release] ++
	case ?USE_EXPOSURE of
	    true -> [expose];
	    _ -> []
	end,
    Window = epx:window_create(40, 40, Width, Height, Events),

    epx:window_attach(Window, Backend),
    epx:window_adjust(Window, [{name, "MacGraph2"}]),

    Pixels = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_attach(Pixels, Backend),

    Menu = epx_menu:create(MenuProfile, menu(global)),

    State = #state{ backend = Backend,
		    window = Window,
		    pixels = Pixels,
		    profile = Profile,
		    width  = Width,
		    height = Height,
		    menu   = Menu,
		    graph = G
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
handle_call({use_graph,G}, _From, State) ->
    case graph:is_graph(G) of
	true ->
	    invalidate(State),
	    {reply, ok, State#state { graph = G,
				      clip = undefined,
				      selected = []}};
	false ->
	    {reply, {error, einval}, State}
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
	{button_press, [left], _Where={Xm,Ym,_Zm}} ->
	    XY = {X,Y} = izm(Xm,Ym,State#state.zoom),
	    if 
		State#state.operation =:= menu ->
		    case epx_menu:find_row(State#state.menu, 
					   State#state.pt1,
					   {Xm,Ym}) of
			{-1, _Menu} ->
			    {noreply, State};
			{_Row, Menu} ->
			    case epx_menu:command(Menu) of
				none ->
				    {noreply, State#state { menu=Menu }};
				{Cmd,Mod} ->
				    State1 = State#state { menu=Menu },
				    State2 = command(Cmd,State1#state.selected,
						     Mod,State),
				    invalidate(State2),
				    {noreply, State2}
			    end
		    end;
	       ?CTRL(State) ->  %% Add vertex
		    Profile = State#state.profile, 
		    Color = Profile#profile.vertex_color,
		    Width = Profile#profile.vertex_width,
		    Height = Profile#profile.vertex_height,
		    Shape = Profile#profile.vertex_shape,
		    V = graph:unique_vertex(),
		    G1 = graph:put_vertex(V,
					  [{x,X},{y,Y},
					   {color,Color},
					   {height,Height},
					   {width,Width},
					   {shape,Shape}],
					  State#state.graph),
		    invalidate(State),
		    {noreply, State#state { operation = vertex,
					    pt1 = XY,
					    pt2 = XY,
					    graph = G1 }};

		?ALT(State) ->  %% Add edge
		    case select(XY,State#state.graph,[]) of
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
						    pt1 = XY, pt2 = XY
						  }}
		    end;

	       true ->
		    Window = State#state.window,
		    epx:window_enable_events(Window,[motion]),
		    invalidate(State),
		    Sel0 = State#state.selected,
		    case select(XY,State#state.graph,[]) of
			[] ->
			    Sel = if ?SHIFT(State) -> Sel0; true -> [] end,
			    {noreply,
			     State#state { operation = select,
					   selected = Sel,
					   pt1 = XY, pt2 = XY }};
			[V|_] ->
			    case lists:member(V, Sel0) of
				true ->
				    Sel = if ?SHIFT(State) ->
						  Sel0 -- [V];
					     true ->
						  Sel0
					  end,
				    {noreply,
				     State#state { operation = move,
						   selected = Sel,
						   pt1 = XY, pt2 = XY }};
				false ->
				    if ?SHIFT(State) ->
					    {noreply,
					     State#state { operation = select,
							   selected = [V|Sel0],
							   pt1 = XY, pt2 = XY}};
				       true ->
					    {noreply,
					     State#state { operation = move,
							   selected = [V],
							   pt1 = XY, pt2 = XY}}
				    end
			    end
		    end
	    end;

	{button_release, [left], _Where={Xm,Ym,_Zm}} ->
	    XY = izm(Xm,Ym,State#state.zoom),
	    Window = State#state.window,
	    epx:window_disable_events(Window,[motion]),
	    State1 = State#state { pt = XY },
	    case State1#state.pt1 of
		undefined ->
		    {noreply,State1};
	       Pt1 ->
		    Pt2 = XY,
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
			    Sel1 = if ?SHIFT(State1) -> Sel0; true -> [] end,
			    Sel = select_area(Rect,State1#state.graph,Sel1),
			    invalidate(State1),
			    {noreply, State1#state { pt1 = undefined,
						     pt2 = undefined,
						     operation = none,
						     selected = Sel }};
			edge ->
			    invalidate(State1),
			    case select(XY,State1#state.graph,[]) of
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
				    Profile = State1#state.profile,
				    EdgeColor = Profile#profile.edge_color,
				    G = graph:put_edge(V, W, 
						       [{color,EdgeColor}],
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

	{button_press, [right], _Where={X,Y,_Zm}} ->
	    %% MI = State#state.menu_info,
	    %% epx_gc:set_font(MI#menu_info.font),
	    %% Menu = menu(global),
	    %% MenuGeometry = graph_menu:calc_menu_size(Menu,MI),
	    epx:window_enable_events(State#state.window,[motion]),
	    Menu = epx_menu:set_row(State#state.menu, -1),
	    State1 = State#state { pt1 = {X,Y}, operation = menu,
				   menu = Menu },
	    invalidate(State1),
	    {noreply, State1};
			  
	{button_press,[wheel_down],{Xm,Ym,_Zm}} ->
	    %% scale + selected graph
	    XY = izm(Xm,Ym,State#state.zoom),
	    flush_wheel(State#state.window),
	    Selected = State#state.selected,
	    Center = if ?SHIFT(State) ->
			     graph_center(Selected,State#state.graph);
			true ->
			     XY
		     end,
	    G = scale_vertices(Selected, 1.2, Center, State#state.graph),
	    State1 = State#state { graph = G },
	    invalidate(State1),
	    {noreply, State1};

	{button_press,[wheel_up],{Xm,Ym,_Zm}} ->
	    %% scale - selected graph
	    XY = izm(Xm,Ym,State#state.zoom),
	    flush_wheel(State#state.window),
	    Selected = State#state.selected,
	    Center = if ?SHIFT(State) ->
			     graph_center(Selected,State#state.graph);
			true ->
			     XY
		     end,
	    G = scale_vertices(Selected, 0.8, Center, State#state.graph),
	    State1 = State#state { graph = G },
	    invalidate(State1),
	    {noreply, State1};

	{button_press,[wheel_left],{_Xm,_Ym,_Zm}} ->
	    %% resize +
	    %% XY = izm(Xm,Ym,State#state.zoom),
	    flush_wheel(State#state.window),
	    {noreply, State};
	
	{button_press,[wheel_right],{_Xm,_Ym,_Zm}} ->
	    %% resize -
	    %% XY = izm(Xm,Ym,State#state.zoom),
	    flush_wheel(State#state.window),
	    {noreply, State};

	{motion, [], {Xm,Ym,_Zm}} ->
	    flush_motions(State#state.window),
	    if State#state.operation =:= menu ->
		    %% check menu row
		    {Row,Menu} = epx_menu:find_row(State#state.menu,
						   State#state.pt1,
						   {Xm,Ym}),
		    if Row =:= -1 ->
			    {noreply, State#state { menu=Menu }};
		       true ->
			    State1 = State#state { menu = Menu },
			    invalidate(State1),
			    {noreply, State1};
		       true ->
			    {noreply, State}
		    end;
	       true ->
		    {noreply, State}
	    end;

	{motion, [left], {Xm,Ym,_Zm}} ->
	    flush_motions(State#state.window),
	    XY = izm(Xm,Ym,State#state.zoom),
	    case State#state.pt1 of
		undefined ->
		    {noreply, State};
		_Pt1 ->
		    invalidate(State),
		    {noreply, State#state { pt2 = XY}}
	    end;

	{key_press, Sym, Mod, _code} ->
	    %% io:format("Key press ~p mod=~p\n", [Sym,Mod]),
	    M = set_mod(State#state.keymod, Mod),
	    State1 = State#state { keymod=M },
	    State2 = command(Sym, State1#state.selected, M, State1),
	    invalidate(State2),
	    {noreply, State2};

	{key_release, _Sym, Mod, _code} ->
	    %% %% io:format("Key release ~p mod=~p\n", [_Sym,Mod]),
	    M = clr_mod(State#state.keymod, Mod),
	    {noreply, State#state { keymod = M }};

	{configure, {_X,_Y,W,H}} ->
	    %% io:format("Configure x=~w,y=~w,w=~w,h=~w\n", [_X,_Y,W,H]),
	    Pixels = resize_pixmap(State#state.pixels,W,H),
	    State1 = State#state { pixels = Pixels, width=W, height=H },
	    maybe_invalidate(not ?USE_EXPOSURE, State1),
	    {noreply, State1};

	{expose, {_X,_Y,_W,_H}} ->
	    %% io:format("Expose x=~w,y=~w,w=~w,h=~w\n", [_X,_Y,_W,_H]),
	    draw(State),
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

%% update mod keys
set_mod(M, [shift|Mod]) ->  set_mod(M#keymod {shift = true}, Mod);
set_mod(M, [ctrl|Mod]) ->   set_mod(M#keymod {ctrl = true}, Mod);
set_mod(M, [alt|Mod]) ->    set_mod(M#keymod {alt = true}, Mod);
set_mod(M, [_|Mod]) ->      set_mod(M, Mod);
set_mod(M, []) -> M.

clr_mod(M, [shift|Mod]) ->  clr_mod(M#keymod {shift = false}, Mod);
clr_mod(M, [ctrl|Mod]) ->   clr_mod(M#keymod {ctrl = false}, Mod);
clr_mod(M, [alt|Mod]) ->    clr_mod(M#keymod {alt = false}, Mod);
clr_mod(M, [_|Mod]) ->      clr_mod(M, Mod);
clr_mod(M, []) -> M.


%%
%% key commands:
%%   up / down / left / right move selected graph
%%   ctrl 0 - 9               set color on selected vertices
%%   alt  0 - 9               set shape on selected vertices
%%   \b                       delete selected vertices
%%   ctrl x                   cut selected vertices to clipboard
%%   ctrl c                   copy selected vertices to clipboard
%%   ctrl v                   paste graph from clipboard
%%   ctrl s                   save graph go "graph.g"
%%   C                        complete selected sub graph
%%   I                        complement selected sub graph
%%   G                        toggle snap to grid 
%%   +                        zoom in
%%   -                        zoom out
%%   r                        find greedy coloring of graph
%%   q                        find greedy clique
%%   o                        find greedy vertex cover
%%
command(up, Selected, _Mod, State) ->
    Dir = case State#state.grid of
	      undefined -> {0,-1};
	      {_Xs,Ys} -> {0,-Ys}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command(down, Selected, _Mod, State) ->
    Dir = case State#state.grid of
	      undefined -> {0,1};
	      {_Xs,Ys} -> {0,Ys}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command(left, Selected, _Mod, State) ->
    Dir = case State#state.grid of
	      undefined -> {-1,0};
	      {Xs,_Ys} -> {-Xs,0}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command(right, Selected, _Mod, State) ->
    Dir = case State#state.grid of
	      undefined -> {1,0};
	      {Xs,_Ys} -> {Xs,0}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command(Command, Selected, Mod, State) when Command >= $0, Command =< $9,
					    Mod#keymod.ctrl ->
    G = set_vertices(Selected, [{color,Command-$0}],State#state.graph),
    State#state { graph=G };
command(Command, Selected, Mod, State) when Command >= $0, Command =< $9,
					    Mod#keymod.alt ->
    G = set_vertices(Selected, [{shape,shape(Command-$0)}],State#state.graph),
    State#state { graph=G };
command($\e, _Selected, _Mod, State) ->
    %% escape from operation, fixme stop ongoing operations
    epx:window_disable_events(State#state.window,[motion]),
    State#state { operation = none };
command($\b, Selected, _Mod, State) ->
    G = kill_graph(Selected, State#state.graph),
    State#state { graph=G, selected = [] };
command($x, Selected, Mod, State) when Mod#keymod.ctrl ->
    {G,Clip} = if Selected =:= [] ->
		       {State#state.graph, State#state.clip};
		  true ->
		       cut_graph(Selected, State#state.graph)
	       end,
    State#state { graph=G, clip=Clip, selected = [] };
command($c, Selected, Mod, State) when Mod#keymod.ctrl ->
    G = if Selected =:= [] -> State#state.graph;
	   true -> copy_graph(Selected, State#state.graph)
	end,
    State#state { clip=G };
command($v, _Selected, Mod, State) when Mod#keymod.ctrl ->
    Pos = case State#state.pt of
	      undefined ->
		  zm({State#state.width div 2,State#state.height div 2},
		     State#state.zoom);
	      Pt -> Pt
	  end,
    {G,Vs} = paste_graph(State#state.graph, State#state.clip, Pos),
    State#state { graph=G, selected = Vs };
command($s, _Selected, Mod, State) when Mod#keymod.ctrl ->
    graph:save("graph.g", State#state.graph),
    State;
command($C, Selected, _Mod, State) ->
    G = complete_graph(Selected, State#state.graph, State),
    State#state { graph=G };
command($I, Selected, _Mod, State) ->
    G = complement_graph(Selected, State#state.graph, State),
    State#state { graph=G };
command($G, _Selected, _Mod, State) ->
    Grid = case State#state.grid of
	       undefined -> {8,8};
	       _ -> undefined
	   end,
    State#state { grid = Grid };
command($+, _Selected, _Mod, State) ->
    %% fixme: make fixed steps? more predictable (yes!)
    Zoom = min(100.0, State#state.zoom * 1.07),
    %% io:format("Zoom factor ~w\n", [Zoom]),
    State#state { zoom = Zoom };
command($-, _Selected, _Mod, State) ->
    Zoom = max(0.01, State#state.zoom / 1.07),
    %% io:format("Zoom factor ~w\n", [Zoom]),
    State#state { zoom = Zoom };
command($r, _Selected, _Mod, State) ->
    ColorMap = graph_color:greedy(State#state.graph),
    io:format("Colors = ~p\n", [ColorMap]),
    G = maps:fold(fun(V,Color,Gi) ->
			  graph:put_vertex(V, [{color,Color}], Gi)
		  end, State#state.graph, ColorMap),
    State#state { graph = G };
command($q, _Selected, _Mod, State) ->
    Vs = graph_clique:greedy(State#state.graph),
    io:format("Clique = ~p\n", [Vs]),
    State#state { selected = Vs };
command($o, _Selected, _Mod, State) ->
    Vs = graph_vertex_cover:greedy(State#state.graph),
    io:format("Cover = ~p\n", [Vs]),
    State#state { selected = Vs };
    
command(Command, _Selected, _Mod, State) ->
    io:format("Command = ~p\n", [Command]),
    State.

shape(0) -> ellipse;
shape(1) -> rectangle;
shape(2) -> roundrect;
shape(3) -> triangle;
shape(_) -> ellipse.

-define(ld(Key, Env, Default),
	proplists:get_value(Key, Env, Default#profile.Key)).

-define(ldc(Scheme, Key, Env, Default),
	epx_profile:color_number(Scheme, ?ld(Key,Env,Default))).

%% load #profile from environment
load_profile(E) ->
    D = #profile{},
    %% Special case
    {Width,Height} =
	case proplists:get_value(size, E, unset) of
	    unset ->
		{?ld(vertex_width, E, D),?ld(vertex_height, E, D)};
	    Size ->
		{Size, Size}
	end,
    S = ?ld(scheme, E, D),
    #profile {
       scheme = S,
       screen_color = ?ldc(S,screen_color, E, D),
       selection_alpha = ?ld(selection_alpha, E, D),
       selection_color = ?ldc(S,selection_color, E, D),
       selection_border_width = ?ld(selection_border_width, E, D),
       selection_border_color = ?ldc(S,selection_border_color, E, D),
       vertex_shape           = ?ld(vertex_shape, E, D),
       vertex_width           = Width,
       vertex_height          = Height,
       vertex_color           = ?ldc(S,vertex_color, E, D),
       vertex_border_width     = ?ld(vertex_border_width, E, D),
       vertex_border_color    = ?ldc(S,vertex_border_color, E, D),
       vertex_select_color    = ?ldc(S,vertex_select_color, E, D),
       vertex_select_border_width=?ld(vertex_select_border_width,E,D),
       vertex_select_border_color=?ldc(S,vertex_select_border_color,E,D),
       vertex_highlight_color=?ldc(S,vertex_highlight_color,E,D),
       vertex_highlight_border_width=?ld(vertex_highlight_border_width,E,D),
       vertex_highlight_border_color=?ldc(S,vertex_highlight_border_color,E,D),
       edge_color = ?ldc(S,edge_color,E,D),
       edge_select_color = ?ldc(S,edge_select_color,E,D),
       edge_highlight_color = ?ldc(S,edge_highlight_color,E,D),
       label_font = ?ld(label_font, E, D),
       label_font_size = ?ld(label_font_size, E, D),
       label_font_color = ?ldc(S,label_font_color,E,D),
       menu_font_name = ?ld(menu_font_name, E, D),
       menu_font_size = ?ld(menu_font_size, E, D),
       menu_font_color = ?ldc(S,menu_font_color,E,D),
       menu_background_color = ?ldc(S,menu_background_color,E,D),
       menu_border_color = ?ldc(S,menu_border_color,E,D)
      }.

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

resize_pixmap(undefined, W, H) ->
    Pixmap = next_pixmap(W,H),
    epx:pixmap_attach(Pixmap),
    Pixmap;
resize_pixmap(Pixmap, W, H) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    epx:pixmap_detach(Pixmap),
	    Pixmap1 = next_pixmap(W,H),
	    epx:pixmap_attach(Pixmap1),
	    Pixmap1;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H) ->
    NPW = 1 bsl ceil(math:log2(W)),
    NPH = 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH, argb).

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
    Map = maps:from_list([{V,graph:unique_vertex()}||V <- Vs]),
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
    Map = maps:from_list([{V,graph:unique_vertex()}||V <- Vs]),
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
graph_center(G) ->
    graph_center(graph:vertices(G), G).

graph_center([],_G) ->
    {0,0};
graph_center(Vs,G) ->
    N = length(Vs),
    {X,Y} = graph_center_(Vs,0,0,G),
    {round(X/N), round(Y/N)}.

graph_center_([V|Vs],X,Y,G) ->
    {Xv,Yv} = get_vertex_coord(V, G),
    graph_center_(Vs, Xv+X, Yv+Y, G);
graph_center_([],X,Y,_G) ->
    {X,Y}.

offset_graph(G, Offset) ->
    move_vertices(graph:vertices(G), Offset, G).

%% scale vertex list
scale_vertices([V|Vs], Scale, Center, G) ->
    Pos = get_vertex_coord(V, G),
    Pos1 = coords_add(scale(coords_sub(Pos, Center), Scale), Center),
    G1 = set_vertex_pos(V, G, Pos1),
    scale_vertices(Vs, Scale, Center, G1);
scale_vertices([], _Scale, _Center, G) ->
    G.

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

maybe_invalidate(true, State) ->
    invalidate(State);
maybe_invalidate(_, _) ->
    ok.

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

menu(global) ->
    [
     {"Cut", "Ctrl+X"},
     {"Copy", "Ctrl+C"},
     {"Paste", "Ctrl+V"},
     {"Delete", "Del"},
     {"---", ""},
     {"Save",  "Ctrl+S"},
     {"---", ""},
     {"Complete", "Shift+C"},
     {"Complement", "Shift+I"},
     {"Snap to grid", "Shift+G"},
     {"Zoom in", "+"},
     {"Zoom out", "-"},
     {"Colour", "R"},
     {"Clique", "Q"},
     {"Vertex Cover", "O"}
    ].


%% FIXME when using operation=move only draw the selected subgraph
%% avoid redraw all node
draw(State = #state { graph = G, selected = Selected, profile = Profile }) ->
    Zoom = State#state.zoom,
    Grid = State#state.grid,
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    epx:pixmap_fill(State#state.pixels,ScreenColor),
    epx_gc:set_fill_style(solid),
    Offset = if State#state.operation =:= move ->
		     coords_sub(State#state.pt2,State#state.pt1);
		true ->
		     {0,0}
	     end,
    %% Draw info bar
    %%   Zoom
    %% | 1%       Pt1                  Pt2        Delta 
    %% | 10000% | x:-1000.0 y:1000.2 | x:0  y:0 | dx:0  dy:0 | x:16 y:16 |
    %% 
    EdgeColor = epx_profile:color(Scheme, Profile#profile.edge_color),
    graph:fold_edges(
      fun(V,W,E,Acc) ->
	      Vxy0 = get_vertex_coord(V, G),
	      Vxy1 = case lists:member(V, Selected) of
			 true -> coords_add(Vxy0,Offset);
			 false -> Vxy0
		     end,
	      Vxy2 = snap(Vxy1, Grid),
	      Vxy3 = scale(Vxy2, Zoom),
	      Wxy0 = get_vertex_coord(W, G),
	      Wxy1  = case lists:member(W, Selected) of
			  true -> coords_add(Wxy0,Offset);
			  false -> Wxy0
		      end,
	      Wxy2 = snap(Wxy1, Grid),
	      Wxy3 = scale(Wxy2, Zoom),
	      Color = graph:get_edge_by_id(E, color, G, EdgeColor),
	      RGB = epx_profile:color(Scheme, Color),
	      epx_gc:set_foreground_color(RGB),
	      epx_gc:set_line_width(zm(1,Zoom)),
	      epx:draw_line(State#state.pixels,Vxy3, Wxy3),
	      Acc
      end, [], G),

    VertexColor = epx_profile:color(Scheme, Profile#profile.vertex_color),
    VertexSelectBorderColor = epx_profile:color(Scheme,Profile#profile.vertex_select_border_color),
    VertexBorderColor = epx_profile:color(Scheme,Profile#profile.vertex_border_color),
    VertexHighLightColor = epx_profile:color(Scheme, Profile#profile.vertex_highlight_color),
    VertexHighLightBorderColor = epx_profile:color(Scheme, Profile#profile.vertex_highlight_border_color),
    graph:fold_vertices(
      fun(V, Acc) ->
	      Rect0 = vertex_rect(V, G),
	      Rect1 = 
		  case lists:member(V, Selected) of
		      true ->
			  Bw1 = zm(Profile#profile.vertex_select_border_width,Zoom),
			  epx_gc:set_border_width(Bw1),
			  epx_gc:set_border_color(VertexSelectBorderColor),
			  rect_offset(Rect0, Offset);
		      false ->
			  Bw2 = zm(Profile#profile.vertex_border_width,Zoom),
			  epx_gc:set_border_width(Bw2),
			  epx_gc:set_border_color(VertexBorderColor),
			  Rect0
		  end,
	      Rect2 = snap(Rect1, Grid),
	      Rect  = scale(Rect2, Zoom),
	      %% high light vertext under pt2
	      if State#state.operation =:= edge ->
		      %% check with snap?
		      case point_in_rect(State#state.pt2, Rect1) of 
			  true ->
			      Bw3 = zm(Profile#profile.vertex_highlight_border_width,Zoom),
			      epx_gc:set_border_width(Bw3),
			      epx_gc:set_border_color(VertexHighLightBorderColor),
			      epx_gc:set_fill_color(VertexHighLightColor);
			  false ->
			      Color = graph:get_vertex_by_id(V,color,G, 
							     VertexColor),
			      RGB = epx_profile:color(Scheme, Color),
			      epx_gc:set_fill_color(RGB)
		      end;
		 true ->
		      Color = graph:get_vertex_by_id(V,color,G, 
						     VertexColor),
		      RGB = epx_profile:color(Scheme, Color),
		      epx_gc:set_fill_color(RGB)
	      end,
	      case vertex_shape(V, G) of
		  ellipse ->
		      epx:draw_ellipse(State#state.pixels,Rect);
		  rectangle ->
		      epx:draw_rectangle(State#state.pixels,Rect);
		  roundrect ->
		      Rw = zm(8, Zoom),
		      Rh = zm(8, Zoom),
		      epx:draw_roundrect(State#state.pixels,Rect,Rw,Rh);
		  triangle ->
		      {X,Y,W,H} = Rect,
		      P0 = {X + (W / 2), Y},
		      P1 = {X, Y+H-1},
		      P2 = {X+W-1, Y+H-1},
		      epx:draw_triangle(State#state.pixels,
					P0, P1, P2)
	      end,
	      Acc
      end, [], G),

    SelectionColor = epx_profile:color(Scheme, Profile#profile.selection_color),
    EdgeHighLightColor = epx_profile:color(Scheme, Profile#profile.edge_highlight_color),
    if State#state.pt1 =/= undefined, State#state.operation =:= menu ->
	    epx_menu:draw(State#state.menu, State#state.pixels, 
			  State#state.pt1);
       State#state.pt1 =:= undefined; State#state.pt2 =:= undefined ->
	    ok;
       true ->
	    case State#state.operation of
		select ->
		    Bw4 = zm(Profile#profile.selection_border_width,Zoom),
		    Bc = Profile#profile.selection_border_color,
		    epx_gc:set_border_color(epx_profile:color(Scheme,Bc)),
		    epx_gc:set_border_width(Bw4),
		    Color = alpha_color(Profile#profile.selection_alpha,
					SelectionColor),
		    epx_gc:set_fill_color(Color),
		    epx_gc:set_fill_style(blend),
		    Rect = scale(coords_to_rect(State#state.pt1,State#state.pt2), Zoom),
		    epx:draw_rectangle(State#state.pixels, Rect);
		edge ->
		    epx_gc:set_foreground_color(EdgeHighLightColor),
		    epx_gc:set_line_width(zm(1,Zoom)),
		    Pt1 = scale(State#state.pt1, Zoom),
		    Pt2 = scale(State#state.pt2, Zoom),
		    epx:draw_line(State#state.pixels, Pt1, Pt2);
		_ ->
		    ok
	    end
    end,

    epx:pixmap_draw(State#state.pixels, State#state.window,
		    0, 0, 0, 0, 
		    State#state.width, State#state.height).


zm(W,Zoom) -> max(1, W*Zoom).
zm(X,Y,Zoom) -> {X*Zoom, Y*Zoom}.

izm(W,Zoom) -> max(1, W/Zoom).
izm(X,Y,Zoom) -> {X/Zoom, Y/Zoom}.

snap(PointOrRect, undefined) -> PointOrRect;
snap({X,Y}, {Xs, Ys}) ->
    {Xs*trunc(X / Xs), Ys*trunc(Y / Ys)};
snap({X,Y,W,H}, {Xs, Ys}) ->
    {Xs*trunc(X / Xs), Ys*trunc(Y / Ys), W, H}.


scale(PointOrRect, Scale) when Scale == 1 -> PointOrRect;
scale({X,Y}, Zoom) ->
    {X*Zoom, Y*Zoom};
scale({X,Y,W,H}, Zoom) ->
    {X*Zoom, Y*Zoom, max(1,W*Zoom), max(1,H*Zoom)}.

%% catch up with motions
flush_motions(Window) ->
    receive
	{epx_event,Window,{motion,_,_}} ->
	    flush_motions(Window)
    after 0 ->
	    ok
    end.

flush_wheel(Window) ->
    receive
	{epx_event,Window,{_,[wheel_down],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_left],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_right],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_up],_}} ->
	    flush_wheel(Window)
    after 0 ->
	    ok
    end.
