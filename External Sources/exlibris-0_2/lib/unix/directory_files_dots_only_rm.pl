:- ensure_loaded( library(system) ).		% directory_files/2.

directory_files_dots_only_rm( Dir, Fs ) :-
	directory_files( Dir, AllFs ),
	( select( '.', AllFs, NodFs ) ->
		true
		;
		NodFs = AllFs 
	),
	( select( '..', NodFs, Fs ) ->
		true
		;
		Fs = NodFs 
	).
