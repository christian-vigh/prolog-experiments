:- ensure_loaded( library(requires) ).		%.
:- requires(is_a_directory/1).
:- requires(reverse/2).
:- requires(delete_leading/3).
:- requires(break_list_on/4).

fname_dir_name( Fname, Dir, Name ) :-
	atom_codes( Fname, FnameCs ),
	reverse( FnameCs, RevCs ),
	( is_a_directory(Fname) ->
		delete_leading( RevCs, 0'/, ClRevCs ),
		reverse( ClRevCs, DirCs ),
		atom_codes( Dir, DirCs ),
		Name = ''
		;
		( break_list_on( RevCs, 0'/, Left, Right ) ->
			reverse( Left, NameCs ),
			atom_codes( Name, NameCs ),
			reverse( Right, DirCs ),
			atom_codes( Dir, DirCs )
			;
			Dir = '.',
			Name = Fname
		)
	).
