%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Simple? Graph editor
%%% @end
%%% Created : 24 Feb 2019 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(graph_edit).

-behaviour(epxw).

%% API
-export([start/0]).
-export([graph/1]).
-export([load/1, load_mac/1, load_matrix/1]).
-export([save/1]).
-export([set_graph/1]).
-export([shape/1]).

%% epxw/gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
%% epxw callbacks
-export([button_press/2, 
	 button_release/2, 
	 command/3,
	 select/2,
	 draw/3,
	 motion/2,
	 menu/2
	]).

-define(SERVER, ?MODULE).

-define(WIDTH,  800).
-define(HEIGHT, 480).

-define(WHITE, grey5).
-define(BLACK, grey10).

-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_window_content.hrl").

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
	 menu_border_color             = green,

	 window_font_name              = "Arial",
	 window_font_size              = 12,
	 window_font_color             = ?BLACK,
	 scroll_bar_color              = grey6,
	 scroll_hndl_color             = gray8,
	 scroll_horizontal             = right,
	 scroll_vertical               = bottom,
	 top_bar_color                 = red,
	 left_bar_color                = green,
	 right_bar_color               = blue,
	 bottom_bar_color              = white
	}).

-record(state,
	{
	 profile,         %% color profile
	 pt,              %% last button press position
	 pt1,
	 pt2,
	 operation = none :: none | select | move | vertex | edge,
	 selected = [],   %% list of selected vertices (and edges?)
	 selected_once = [],  %% temporary selected (one command)
	 selection,       %% selected area
	 graph,           %% the graph
	 grid,            %% undefined | {Xstep,Ystep}
	 clip,            %% the cut/copy graph
	 graph_menu,
	 vertex_ctrl_menu,
	 vertex_alt_menu,
	 edge_ctrl_menu,
	 label_font
	}).

menu(graph) ->
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
    ];
menu(vertex_ctrl) ->
    [
     {"Red",    "Ctrl+1"},
     {"Orange", "Ctrl+2"},
     {"Brown",  "Ctrl+3"},
     {"Yellow", "Ctrl+4"},
     {"Green",  "Ctrl+5"},
     {"Lime",   "Ctrl+6"},
     {"Turquoise", "Ctrl+7"},
     {"Cyan",   "Ctrl+8"}
    ];
menu(vertex_alt) ->
    [
     {"Circle",    "Alt+0"},
     {"Rectangle", "Alt+1"},
     {"RoundRect", "Alt+2"},
     {"Triangle",  "Alt+3"}
    ];
menu(edge_ctrl) ->
    [
     {"Red",    "Ctrl+1"},
     {"Orange", "Ctrl+2"},
     {"Brown",  "Ctrl+3"},
     {"Yellow", "Ctrl+4"},
     {"Green",  "Ctrl+5"},
     {"Lime",   "Ctrl+6"},
     {"Turquoise", "Ctrl+7"},
     {"Cyan",   "Ctrl+8"}
    ].

%%%===================================================================
%%% API
%%%===================================================================

graph(G) ->
    start([{graph,G}]).

start() ->
    start([]).

start(_Opts0) ->
    application:ensure_all_started(epx),
    application:load(graph),
    epxw:start(?MODULE,
	       [hello,world],
	       [{title, "MacGraphII"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   right},   %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar, 32},
		{top_bar, 20},
		{right_bar, 8},
		{width, ?WIDTH},
		{height, ?HEIGHT},
		{view_width,?WIDTH},
		{view_height,?HEIGHT}]).

load(File) ->
    gen_server:call(?SERVER, {load, File}).

save(File) ->
    gen_server:call(?SERVER, {save, File}).

load_matrix(File) ->
    gen_server:call(?SERVER, {load_matrix, File}).

load_mac(File) ->
    gen_server:call(?SERVER, {load_mac, File}).

set_graph(G) ->
    gen_server:call(?SERVER, {set_graph, G}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> 
	  {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init(Options) ->
    %% options list override env
    Env = Options ++ application:get_all_env(graph),
    G = proplists:get_value(graph, Env, graph:new(false)),
    Profile = load_profile(Env),
    MenuProfile = create_menu_profile(Profile),
    %% context menu? vertex menu? and graph menu...
    GraphMenu = epx_menu:create(MenuProfile, menu(graph)),
    VertexCtrlMenu = epx_menu:create(MenuProfile, menu(vertex_ctrl)),
    VertexAltMenu = epx_menu:create(MenuProfile, menu(vertex_alt)),
    EdgeCtrlMenu = epx_menu:create(MenuProfile, menu(edge_ctrl)),

    State = #state{ profile = Profile,
		    graph_menu  = GraphMenu,
		    vertex_ctrl_menu = VertexCtrlMenu,
		    vertex_alt_menu  = VertexAltMenu,
		    edge_ctrl_menu   = EdgeCtrlMenu,
		    graph = G
		  },
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

handle_call({load,File}, _From, State) ->
    case graph:load(File) of
	Error = {error,_Reason} ->
	    {reply, Error, State};
	G ->
	    epxw:invalidate(),
	    {reply, ok, State#state { graph = G, 
				      clip = undefined,
				      selected = [] }}
    end;
handle_call({save,File}, _From, State) ->
    case graph:save(File) of
	Error = {error,_Reason} ->
	    {reply, Error, State};
	ok ->
	    %% Fixme: update save status
	    {reply, ok, State}
    end;
handle_call({load_matrix,File}, _From, State) ->
    case graph_file:load(File) of
	Error = {error,_Reason} ->
	    {reply, Error, State};
	{ok,G} ->
	    epxw:invalidate(),
	    {reply, ok, State#state { graph = G, 
				      clip = undefined,
				      selected = [] }}
    end;
handle_call({load_mac,File}, _From, State) ->
    case graph_mac:load(File) of
	Error = {error,_Reason} ->
	    {reply, Error, State};
	{ok,GraphData} ->
	    {G,Selected} = macgraph:import(GraphData),
	    epxw:invalidate(),
	    {reply, ok, State#state { graph = G, 
				      clip = undefined,
				      selected = Selected}};
	{_, _} ->
	    {reply, {error, decode_problem}, State}
    end;
handle_call({set_graph,G}, _From, State) ->
    case graph:is_graph(G) of
	true ->
	    epxw:invalidate(),
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
    io:format("Got cast ~p\n", [_Request]),
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
handle_info(_Event, State)  ->
    io:format("Got event ~p\n", [_Event]),
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

button_press({_, [left], Pos}, State) ->
    KM = epxw:keymod(),
    if
	KM#keymod.ctrl ->  %% Add vertex
	    Profile = State#state.profile, 
	    Color = Profile#profile.vertex_color,
	    Width = Profile#profile.vertex_width,
	    Height = Profile#profile.vertex_height,
	    Shape = Profile#profile.vertex_shape,
	    V = graph:unique_vertex(),
	    {X,Y} = Pos,
	    G1 = graph:put_vertex(V,
				  [{x,X},{y,Y},
				   {color,Color},
				   {height,Height},
				   {width,Width},
				   {shape,Shape}],
				  State#state.graph),
	    epxw:invalidate(),
	    State#state { operation = vertex,
			  pt1 = Pos, pt2 = Pos, graph = G1 };
	
	KM#keymod.alt ->
	    %% Add edge: Multi edge all selected to ONE ?
	    case select_pos(Pos,State#state.graph,[]) of
		[] ->
		    epxw:invalidate(),
		    State#state { selected = [], operation = none };
		[V|_] ->
		    epxw:invalidate(),
		    epxw:enable_motion(),
		    State#state { operation = edge,
				  selected = [V],
				  pt1=Pos, pt2=Pos
				}
	    end;
	true ->
	    case select_pos(Pos,State#state.graph,[]) of
		[] ->
		    epxw:invalidate(),
		    if KM#keymod.shift ->
			    State#state { operation = select };
		       true ->
			    State#state { selected = [], operation = select }
		    end;
		[V|_] ->
		    epxw:invalidate(),
		    epxw:enable_motion(),
		    Selected = State#state.selected,
		    case lists:member(V, Selected) of
			true ->
			    State#state { operation = move, pt1=Pos, pt2=Pos };
			false ->
			    Selected1 = 
				if KM#keymod.shift -> [V|Selected];
				   true -> [V]
				end,
			    State#state { operation = move, 
					  selected = Selected1, 
					  pt1=Pos, pt2=Pos }
		    end
	    end
    end;
button_press(_Event, State) ->
    State.

button_release(_Event={_,_,Pos}, State) ->
    io:format("button release ~p\n", [_Event]),
    case State#state.operation of
	move ->
	    io:format("move done\n"),
	    Selected = State#state.selected,
	    Offset = coords_sub(State#state.pt2, State#state.pt1),
	    G = move_vertices(Selected,Offset,State#state.graph),
	    epxw:invalidate(),
	    epxw:disable_motion(),
	    State#state { pt1 = undefined, pt2 = undefined, operation = none,
			  selection = undefined, graph = G };
	edge ->
	    epxw:invalidate(),
	    epxw:disable_motion(),
	    case select_pos(Pos,State#state.graph,[]) of
		[] ->
		    State#state { pt1 = undefined,
				  pt2 = undefined,
				  selection = undefined,
				  operation = none };
		[W|_] when W =:= hd(State#state.selected) ->
		    State#state { pt1 = undefined,
				  pt2 = undefined,
				  selection = undefined,
				  operation = none };
		[W|_] ->
		    [V] = State#state.selected,
		    Profile = State#state.profile,
		    EdgeColor = Profile#profile.edge_color,
		    G = graph:put_edge(V, W, 
				       [{color,EdgeColor}],
				       State#state.graph),
		    State#state { pt1 = undefined,
				  pt2 = undefined,
				  selection = undefined,
				  selected = [W],
				  graph = G,
				  operation = none }
	    end;
	_ ->
	    State
    end.

motion({motion,_Button,Pos}, State) ->
    io:format("motion ~p\n", [Pos]),
    case State#state.operation of
	move ->
	    epxw:invalidate(),
	    State#state { pt2 = Pos };
	edge ->
	    epxw:invalidate(),
	    State#state { pt2 = Pos };
	_ -> State
    end.

menu({menu,Pos}, State) ->
    case select_pos(Pos,State#state.graph,[]) of
	[] ->
	    {reply, State#state.graph_menu, State};
	[V|_] ->
	    KM = epxw:keymod(),
	    case lists:member(V, State#state.selected) of
		true when KM#keymod.ctrl ->
		    {reply, State#state.vertex_ctrl_menu, State};
		true when KM#keymod.alt ->
		    {reply, State#state.vertex_alt_menu, State};
		true ->
		    {reply, State#state.vertex_ctrl_menu, State};

		false when KM#keymod.alt ->
		    {reply, State#state.vertex_alt_menu,
		     State#state { selected_once = [V] }};
		false when KM#keymod.ctrl ->
		    {reply, State#state.vertex_ctrl_menu,
		     State#state { selected_once = [V] }};
		false ->
		    io:format("once = ~p\n", [[V]]),
		    {reply, State#state.vertex_ctrl_menu,
		     State#state { selected_once = [V] }}
	    end
    end.

select({start,Area}, State) ->
    KM = epxw:keymod(),
    OldRect = State#state.selection,
    epxw:invalidate(OldRect),
    epxw:invalidate(Area),
    Sel0 = if KM#keymod.shift -> State#state.selected; true -> [] end,
    case State#state.operation of
	select ->
	    Sel = select_area(Area,State#state.graph, Sel0),
	    State#state {   selected = Sel, selection = Area };
	_ ->
	    State
    end;
select({continue,Area}, State) ->
    case State#state.operation of
	select ->
	    KM = epxw:keymod(),
	    OldRect = State#state.selection,
	    epxw:invalidate(OldRect),
	    epxw:invalidate(Area),
	    Sel0 = if KM#keymod.shift -> State#state.selected; true -> [] end,
	    Sel = select_area(Area,State#state.graph, Sel0),
	    State#state {  selected = Sel, selection = Area };
	_ ->
	    State
    end;
select({stop,_Area}, State) ->
    case State#state.operation of
	select ->
	    State#state { operation = none, selection = undefined };
	_ ->
	    State
    end.

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

command(Sym, Mod, State) ->
    io:format("command = ~w,~w once=~p\n", 
	      [Sym,Mod,State#state.selected_once]),
    {Selected,State1}  = case State#state.selected_once of
			     [] -> {State#state.selected, State};
			     Sel -> {Sel, State#state { selected_once = [] }}
			 end,
    case command_(Sym, Mod, Selected, State1) of
	false ->
	    {reply, {Sym,Mod}, State1};  %% use default handler, unmodified
	State2 ->
	    epxw:invalidate(),
	    {noreply,State2}
    end.

command_(up, _Mod, Selected, State) ->
    Dir = case State#state.grid of
	      undefined -> {0,-1};
	      {_Xs,Ys} -> {0,-Ys}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command_(down, _Mod, Selected, State) ->
    Dir = case State#state.grid of
	      undefined -> {0,1};
	      {_Xs,Ys} -> {0,Ys}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command_(left, _Mod, Selected, State) ->
    Dir = case State#state.grid of
	      undefined -> {-1,0};
	      {Xs,_Ys} -> {-Xs,0}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command_(right, _Mod, Selected, State) ->
    Dir = case State#state.grid of
	      undefined -> {1,0};
	      {Xs,_Ys} -> {Xs,0}
	  end,
    G = move_vertices(Selected, Dir, State#state.graph),
    State#state { graph=G };
command_(Command, Mod, Selected, State) when Command >= $0, Command =< $9,
					     Mod#keymod.ctrl ->
    io:format("set color = ~w of ~p\n", [Command-$0, Selected]),
    G = set_vertices(Selected, [{color,Command-$0}],State#state.graph),
    State#state { graph=G };
command_(Command, Mod, Selected, State) when Command >= $0, Command =< $9,
					     Mod#keymod.alt ->
    G = set_vertices(Selected, [{shape,shape(Command-$0)}],State#state.graph),
    State#state { graph=G };
command_($\b, _Mod, Selected, State) ->
    G = kill_graph(Selected, State#state.graph),
    State#state { graph=G, selected = [] };
command_($x, Mod, Selected, State) when Mod#keymod.ctrl ->
    {G,Clip} = if Selected =:= [] ->
		       {State#state.graph, State#state.clip};
		  true ->
		       cut_graph(Selected, State#state.graph)
	       end,
    State#state { graph=G, clip=Clip, selected = [] };
command_($c, Mod, Selected, State) when Mod#keymod.ctrl ->
    G = if Selected =:= [] -> State#state.graph;
	   true -> copy_graph(Selected, State#state.graph)
	end,
    State#state { clip=G };
command_($v, Mod, _Selected, State) when Mod#keymod.ctrl ->
    Pos = case State#state.pt of
	      undefined ->
		  %% FIXME: pase in visible area! get window in view coords!
		  W = epxw:width(),
		  H = epxw:height(),
		  {W div 2, H div 2};
	      Pt ->
		  Pt
	  end,
    {G,Vs} = paste_graph(State#state.graph, State#state.clip, Pos),
    State#state { graph=G, selected = Vs };
command_($s, Mod, _Selected, State) when Mod#keymod.ctrl ->
    graph:save("graph.g", State#state.graph),
    State;
command_($C, _Mod, Selected, State) ->
    G = complete_graph(Selected, State#state.graph, State),
    State#state { graph=G };
command_($I, _Mod, Selected, State) ->
    G = complement_graph(Selected, State#state.graph, State),
    State#state { graph=G };
command_($G, _Mod, _Selected, State) ->
    Grid = case State#state.grid of
	       undefined -> {8,8};
	       _ -> undefined
	   end,
    State#state { grid = Grid };
command_($r, _Mod, _Selected, State) ->
    ColorMap = graph_color:greedy(State#state.graph),
    io:format("Colors = ~p\n", [ColorMap]),
    G = maps:fold(fun(V,Color,Gi) ->
			  graph:put_vertex(V, [{color,Color}], Gi)
		  end, State#state.graph, ColorMap),
    State#state { graph = G };
command_($q, _Mod, _Selected, State) ->
    Vs = graph_clique:greedy(State#state.graph),
    io:format("Clique = ~p\n", [Vs]),
    State#state { selected = Vs };
command_($o, _Mod, _Selected, State) ->
    Vs = graph_vertex_cover:greedy(State#state.graph),
    io:format("Cover = ~p\n", [Vs]),
    State#state { selected = Vs };
command_(_Command, _Mod, _Selected, _State) ->
    io:format("Command = ~p, Mod = ~p\n", [_Command, _Mod]),
    false.

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

%% fixme only select first!
select_pos(Pos,G,Selected) ->
    graph:fold_vertices(
      fun(V, Sel) ->
	      case lists:member(V, Sel) of
		  false ->
		      Rv = vertex_rect(V, G),
		      case epx_rect:contains(Rv,Pos) of
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
    coords_to_rect_(min(X0,X1),max(X0,X1),min(Y0,Y1),max(Y0,Y1)).

coords_to_rect_(X0,X1,Y0,Y1) ->
    {X0,Y0,X1-X0,Y1-Y0}.

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

alpha_color(A,{_,R,G,B}) -> {A,R,G,B};
alpha_color(A,{R,G,B}) -> {A,R,G,B};
alpha_color(A,Name) when is_list(Name); is_atom(Name) ->
    alpha_color(A, epx_color:from_name(Name)).


%% FIXME when using operation=move only draw the selected subgraph
%% avoid redraw all node

draw(Pixels, _Dirty, State = #state { graph = G, selected = Selected, 
				      profile = Profile }) ->
    Grid = State#state.grid,
    Scheme = Profile#profile.scheme,
    ScreenColor = epx_profile:color(Scheme, Profile#profile.screen_color),
    epx:pixmap_fill(Pixels,ScreenColor),
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
	      Vxy3 = Vxy2,
	      Wxy0 = get_vertex_coord(W, G),
	      Wxy1  = case lists:member(W, Selected) of
			  true -> coords_add(Wxy0,Offset);
			  false -> Wxy0
		      end,
	      Wxy2 = snap(Wxy1, Grid),
	      Wxy3 = Wxy2,
	      Color = graph:get_edge_by_id(E, color, G, EdgeColor),
	      RGB = epx_profile:color(Scheme, Color),
	      epx_gc:set_foreground_color(RGB),
	      epx_gc:set_line_width(1), %% scale me!
	      epx:draw_line(Pixels,Vxy3, Wxy3),
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
			  %% scale me!
			  Bw1 = Profile#profile.vertex_select_border_width,
			  epx_gc:set_border_width(Bw1),
			  epx_gc:set_border_color(VertexSelectBorderColor),
			  rect_offset(Rect0, Offset);
		      false ->
			  %% scale me!
			  Bw2 = Profile#profile.vertex_border_width,
			  epx_gc:set_border_width(Bw2),
			  epx_gc:set_border_color(VertexBorderColor),
			  Rect0
		  end,
	      Rect2 = snap(Rect1, Grid),
	      Rect  = Rect2,
	      %% high light vertext under pt2
	      if State#state.operation =:= edge ->
		      %% check with snap?
		      case epx_rect:contains(Rect1,State#state.pt2) of 
			  true ->
			      %% scale me!
			      Bw3 = Profile#profile.vertex_highlight_border_width,
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
		      epx:draw_ellipse(Pixels,Rect);
		  rectangle ->
		      epx:draw_rectangle(Pixels,Rect);
		  roundrect ->
		      %% scale me!
		      Rw = 8,
		      Rh = 8,
		      epx:draw_roundrect(Pixels,Rect,Rw,Rh);
		  triangle ->
		      {X,Y,W,H} = Rect,
		      P0 = {X + (W / 2), Y},
		      P1 = {X, Y+H-1},
		      P2 = {X+W-1, Y+H-1},
		      epx:draw_triangle(Pixels, P0, P1, P2)
	      end,
	      Acc
      end, [], G),

    SelectionColor = epx_profile:color(Scheme, Profile#profile.selection_color),
    EdgeHighLightColor = epx_profile:color(Scheme, Profile#profile.edge_highlight_color),
    if State#state.pt1 =:= undefined; State#state.pt2 =:= undefined ->
	    ok;
       true ->
	    case State#state.operation of
		select ->
		    %% scale me!
		    Bw4 = Profile#profile.selection_border_width,
		    Bc = Profile#profile.selection_border_color,
		    epx_gc:set_border_color(epx_profile:color(Scheme,Bc)),
		    epx_gc:set_border_width(Bw4),
		    Color = alpha_color(Profile#profile.selection_alpha,
					SelectionColor),
		    epx_gc:set_fill_color(Color),
		    epx_gc:set_fill_style(blend),
		    Rect = coords_to_rect(State#state.pt1,State#state.pt2),
		    epx:draw_rectangle(Pixels, Rect);
		edge ->
		    epx_gc:set_foreground_color(EdgeHighLightColor),
		    %% scale me!
		    epx_gc:set_line_width(1),
		    {X1,Y1} = State#state.pt1,
		    {X2,Y2} = State#state.pt2,
		    epx:draw_line(Pixels, X1, Y1, X2, Y2);
		_ ->
		    ok
	    end
    end,
    case State#state.selection of
	undefined -> empty;
	{_,_,Sw,Sh} when Sw < 2, Sh < 2 -> empty;
	Selection ->
	    epx_gc:set_fill_style(blend),
	    epx_gc:set_fill_color({127,127,127,127}),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(2),
	    epx:draw_rectangle(Pixels, Selection)
    end,
    State.

snap(PointOrRect, undefined) -> PointOrRect;
snap({X,Y}, {Xs, Ys}) ->
    {Xs*trunc(X / Xs), Ys*trunc(Y / Ys)};
snap({X,Y,W,H}, {Xs, Ys}) ->
    {Xs*trunc(X / Xs), Ys*trunc(Y / Ys), W, H}.
