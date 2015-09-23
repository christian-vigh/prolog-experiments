:- ensure_loaded( library(requires) ).		% /1.
:- requires( append/3 ).

dirpath_first_dir_and( DirPath, First, And ) :-
	atom_codes( DirPath, DirPathCs ),
	append( [A|FCs], [0'/|AndCs], DirPathCs ),
	!,
	atom_codes( First, [A|FCs] ),
	atom_codes( And, AndCs ).
