% 6 april 2001. Nicos.

:- ensure_loaded( library('io/fput_line') ). % /2.
:- ensure_loaded( library(requires) ).
:- requires( portray_clause/2 ).

%%%  file+stream to_list_of_lines
list_of_lines_with_portray_to_file( Lines, File ) :-
	open( File, write, Stream ),
	list_of_lines_with_portray_to_stream( Lines, Stream ),
	close( Stream ).

list_of_lines_with_portray_to_stream( [], _Stream ).
list_of_lines_with_portray_to_stream( [L|Ls], Stream ) :-
	( L = portray_directive(Drctv) ->
		write( Stream, ':- ' ),
		portray_clause(Stream,Drctv)
		;
		fput_line( Stream, L )
	),
	list_of_lines_with_portray_to_stream( Ls, Stream ).
