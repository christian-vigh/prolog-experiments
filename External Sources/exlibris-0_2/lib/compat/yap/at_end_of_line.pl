:- ensure_loaded( library(defines) ).		%.
:- defines( yap(_), [at_end_of_line/1] ). 

:- ensure_loaded( library('io/io_code') ).

at_end_of_line( Stream ) :-
	io_code( nl, NL ),
	peek_code( Stream, NL ).
