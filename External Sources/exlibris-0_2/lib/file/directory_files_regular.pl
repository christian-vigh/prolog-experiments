:- ensure_loaded( library(requires) ).
:- requires( directory_files/2 ).

directory_files_regular( Dir, Regulars ) :-
	directory_files( Dir, All ),
	working_directory( Old, Dir ),
	sieve_regular_files( All, Regulars ),
	working_directory( _DirA, Old ).

sieve_regular_files( [], [] ).
sieve_regular_files( [H|T], Regulars ) :-
	% we assume inputs exist...
	( file_property(H,type(regular)) ->
		Regulars = [H|Treg]
		;
		Regulars = Treg
	),
	sieve_regular_files( T, Treg ).
