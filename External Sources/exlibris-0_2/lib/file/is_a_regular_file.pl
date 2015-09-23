:- ensure_loaded( library('compat/if_pl') ).		%.
:- if_pl( not(swi(_)), ensure_loaded(library(system)) ).

is_a_regular_file( File ) :-
	if_pl( swi(_), exists_file(File), 
			(file_exists(File),file_property(File,type(regular)))
		).
