:- ensure_loaded( 'compat/if_pl' ).		%.

:- multifile library_directory/1.

add_library_directory( Dir ) :-
	( Dir = abs_ald(RelDir) ->
		prolog_load_context( directory, LoadDir ),
		atom_concat( LoadDir, '/', SLoadDir ),
		atom_concat( SLoadDir, RelDir, ConcDir ),
		absolute_file_name( ConcDir, AbsDir )
		;
		if_pl(swi(_),expand_file_name(Dir,[AbsDir|_]),AbsDir=Dir)
	),
	!,
	asserta( library_directory(AbsDir) ).
