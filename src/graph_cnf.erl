%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate "CNF" graph
%%% @end
%%% Created : 29 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(graph_cnf).
-compile(export_all).

load(File) ->
    case varp_dimacs:load(File) of
	{cnf,{_NumVars,_NumClause,Sections,Cs}} ->
	    Syms = maps:get(syms, Sections, #{}),
	    clg(Cs, Syms);
	{snf,{_NumVars,_NumClause,Sections,Cs}} ->
	    Syms = maps:get(syms, Sections, #{}),
	    clg(Cs,Syms);
	Error = {error,_} ->
	    Error
    end.

clg(Cs,Syms) ->
    clg(Cs, 1, graph:new(false), #{}, Syms).

clg([C|Cs],Ci,G, Lm,Syms) ->
    Cv = {cl,Ci},
    G1 = graph:put_vertex(Cv,[{x,30+Ci*16},{y,100},{color,blue}],G),
    {G2,Lm1} = add_clause(C, Cv, G1, Lm),
    clg(Cs,Ci+1,G2,Lm1,Syms);
clg([],_Ci,G,Lm,Syms) ->
    clg_lit(maps:to_list(Lm), 10, Lm, G, Syms).

%% connect literals when needed
clg_lit([{L,true}|Ls], Xi, Lm, G, Syms) ->
    Li = negate_literal(L),
    case graph:is_vertex(Li, G) of
	true ->
	    io:format("vertex: ~w  x=~w, ~w\n", [L, Xi, Xi+16]),
	    Attr1 = add_label(L, [{x,Xi},{y,150}], Syms),
	    G1 = graph:put_vertex(L, Attr1, G),
	    Attr2 = add_label(Li, [{x,Xi+16},{y,150}], Syms),
	    G2 = graph:put_vertex(Li, Attr2, G1),
	    G3 = graph:put_edge(L, Li, G2),
	    Ls1 = Ls -- [{Li,true}],
	    clg_lit(Ls1, Xi+40, Lm, G3, Syms);
	false ->
	    io:format("vertex: ~w  x=~w\n", [L, Xi]),
	    Attr1 = add_label(L, [{x,Xi},{y,150}], Syms),
	    G1 = graph:put_vertex(L, Attr1, G),
	    clg_lit(Ls, Xi+20, Lm, G1, Syms)
    end;
clg_lit([], _Xi, _Lm, G, _Syms) ->
    G.

add_clause([L|Ls], Cv, G, Lm) ->
    Lv = if is_integer(L) -> {li,L};
	    true -> L
	 end,
    case graph:is_vertex(Lv, G) of
	true ->
	    G1 = graph:put_edge(Lv, Cv, G),
	    add_clause(Ls, Cv, G1, Lm#{ Lv => true });
	false ->
	    G1 = graph:put_vertex(Lv,[{color,color(Lv)}],G),
	    G2 = graph:put_edge(Lv, Cv, G1),
	    add_clause(Ls, Cv, G2, Lm#{ Lv => true })
    end;
add_clause([], _Cv, G, Lm) ->
    {G,Lm}.

add_label(Lit, Attr, Syms) ->
    case name(Lit, Syms) of
	"" -> Attr;
	Name -> [{label,Name}|Attr]
    end.

name({'p',Name,_},_Syms) -> atom_to_list(Name);
name({'not',{'p',Name,_}},_Syms) -> "!"++atom_to_list(Name);
name({li,L},Syms) when L < 0 -> 
    case maps:get(-L, Syms, undefined) of
	undefined -> "";
	[{p,Name,_}] -> "!"++atom_to_list(Name)
    end;
name({li,L},Syms) when L > 0 -> 
    case maps:get(L, Syms, undefined) of
	undefined -> "";
	[{p,Name,_}] -> atom_to_list(Name)
    end;
name(_, _) -> 
    "".

color({li,L}) when is_integer(L), L < 0 -> red;
color({li,L}) when is_integer(L), L > 0 -> green;
color({'not',{p,_,_}}) -> red;
color({p,_,_}) -> green.

negate_literal({li,L}) when is_integer(L) -> {li,-L};
negate_literal({'not',L}) -> L;
negate_literal(L) -> {'not',L}.

