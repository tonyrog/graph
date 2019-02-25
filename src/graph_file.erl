%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Some file functions
%%% @end
%%% Created : 24 Feb 2019 by Tony Rogvall <tony@rogvall.se>

-module(graph_file).

-export([load/1, load_fd/1, load_fd/2]).

%%
%% Load graph in 
%% connection matrix format
%% 
%%
load(Filename) ->
    case file:open(Filename, [read]) of
	{ok, Fd} ->
	    try load_fd(Fd) of
		G -> G
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

load_fd(Fd) ->
    case read_sym_row(Fd) of
	eof -> {error, eof};
	[] ->  {error, missing_size_row};
	[N|_Vs] -> load_fd(Fd, N)
    end.

load_fd(Fd, N) when is_integer(N), N > 0 ->
    try load_lines(Fd, N, N, []) of
	M -> 
	    {ok, graph:from_connection_matrix(M)}
    catch
	error:_ -> {error, badarg}
    end.


load_lines(_Fd, 0, _N, Acc) ->
    lists:reverse(Acc);
load_lines(Fd, I, N, Acc) ->
    Ts = read_bin_row(Fd),
    case length(Ts) of
	N -> load_lines(Fd, I-1, N, [Ts|Acc])
    end.

read_sym_row(Fd) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    transform_sym_row(string:tokens(Line," \t\r\n"));
	eof -> eof;
	{error,_} -> []
    end.

transform_sym_row([H|T]) ->
    [ try list_to_integer(H) 
      catch error:_ -> list_to_atom(H)
      end | transform_sym_row(T)];
transform_sym_row([]) ->
    [].

read_bin_row(Fd) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    transform_bin_row(string:tokens(Line," \t\r\n"));
	eof -> [];
	{error,_} -> []
    end.

%%
%%  "1010" -> [1,0,1,0]
%%  "1 0 1 0" -> [1,0,1,0]
%%  

transform_bin_row([[$0|Rs]|T]) -> [0|transform_bin_row([Rs|T])];
transform_bin_row([[$1|Rs]|T]) -> [1|transform_bin_row([Rs|T])];
transform_bin_row([[]|T]) -> transform_bin_row(T);
transform_bin_row([]) -> [].
