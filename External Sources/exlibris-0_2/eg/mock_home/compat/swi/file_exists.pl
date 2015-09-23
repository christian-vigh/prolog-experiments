% Do it like sicstus.
file_exists( FileOrDir ) :-
	( exists_file(FileOrDir) ->
		true
		;
		exists_directory(FileOrDir)
	).
