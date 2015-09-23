:- ensure_loaded( library('file/directory_files_nodots') ). % /2.
:- ensure_loaded( library('file/is_a_directory') ). 		% /1.
:- ensure_loaded( library('file/is_a_regular_file') ).		% /1.
	
directory_contents( Dir, Files, SubDirs, Other ) :-
	directory_files_nodots( Dir, Entries ),
	working_directory( Current, Dir ),
	split_directory_contents( Entries, Files, SubDirs, Other ),
	working_directory( _DirHere, Current ).

split_directory_contents( [], [], [], [] ).
split_directory_contents( [H|T], Files, SubDirs, Other ) :-
	( is_a_regular_file(H) ->
		Files = [H|TFiles], 
		SubDirs = TSubDirs,
		Other = TOther
		;
		( is_a_directory(H) ->
			Files = TFiles,
			SubDirs = [H|TSubDirs],
			Other = TOther
			;
			Files = TFiles,
			SubDirs = TSubDirs,
			Other = [H|TOther]
		)
	),
	split_directory_contents( T, TFiles, TSubDirs, TOther ).
