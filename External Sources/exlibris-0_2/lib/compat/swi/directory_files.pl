:- ensure_loaded( library(defines) ).
:- defines( swi(_), [directory_files/2] ).

:- ensure_loaded( library(requires) ).
:- requires( [file_to_list_of_lines/2,atoms_codes/2] ).

directory_files( Dir, Files ) :-
	tmp_file( file_directory, Tmp ),
	atom_concat( 'ls -1 ', Dir, LsPfx ),
	atom_concat( LsPfx, ' >& ', LsMfx ),
	atom_concat( LsMfx, Tmp, Ls ),
	shell( Ls ),
	file_to_list_of_lines( Tmp, Lines ),
	atoms_codes( Files, Lines ),
	delete_file( Tmp ),
	!.
