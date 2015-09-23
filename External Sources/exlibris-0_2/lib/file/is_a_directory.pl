:- ensure_loaded( library('compat/if_pl') ).		% /2.
:- if_pl( not(swi(_)), ensure_loaded( library(system)) ).

is_a_directory( Dir ) :-
	if_pl( swi(_), exists_directory(Dir), 
				(
				  file_exists(Dir),
				  file_property(Dir,type(directory))
				)
		).

