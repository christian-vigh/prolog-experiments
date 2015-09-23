:- ensure_loaded( library(defines) ).		%.
:- defines( swi(_), file_exists/1 ).

% Do it like sicstus.
file_exists( FileOrDir ) :-
	( exists_file(FileOrDir) ->
		true
		;
		exists_directory(FileOrDir)
	).
