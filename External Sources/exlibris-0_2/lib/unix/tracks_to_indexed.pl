:- ensure_loaded( library('unix/directory_files_dots_only_rm') ). %/2.
:- ensure_loaded( library('io/file_to_list_of_lines') ). %/2.
:- ensure_loaded( library(lists) ). % append/3, reverse/2.
:- ensure_loaded( library('list/break_list_on') ). %/4.
:- ensure_loaded( library(system) ). % system/1.

tracks_to_indexed( [Dir] ) :-
	atom_codes( Dir, DirCs ),
	append( DirCs, "/00-Index", IndexCs ),
	atom_codes( Index, IndexCs ),
	file_to_list_of_lines( Index, Lines ),
	index_lines_to_titles( Lines, Titles ),
	directory_files_dots_only_rm( Dir, Files ),
	atoms_codes( Files, FilesCs ),
	mv_tracks( Titles, FilesCs ).

mv_tracks( [], InFiles ) :-
	( select("00-Index",InFiles,Files) ->
		true
		;
		Files = InFiles
	),
	( Files == [] ->
		write( 'done.' ), nl
		;
		atoms_codes( FileAtoms, Files ),
		write( unfinished_files(FileAtoms) ), nl
	).
mv_tracks( [Digits-H|T], Files ) :-
	findall( File, 
		(member(File,Files),sublist(Digits,File)),
		MatchAll ),
	( MatchAll == [] ->
		write( user_error, none_matching_for(Digits-H) ),
		nl( user_error )
		;
		( MatchAll = [First] ->
			true
			;
			MatchAll = [First|_Rest],
			write( user_error, too_many_matching_for(Digits-H,MatchAll) ),
			nl( user_error )
		),
		fnamecs_has_extension( First, Ext ),
		append( "mv ", First, Prfx ),
		append( Prfx, [0' |Digits], Mdfx ),
		append( Mdfx, [0'-|H], Crfx ),
		append( Crfx, [0'.|Ext], MvCs ),
		atom_codes( Mv, MvCs ),
		system( Mv ),
		write( Mv ), nl
	),
	select( First, Files, Next ),
	!,
	mv_tracks( T, Next ).


index_lines_to_titles( [], [] ).
index_lines_to_titles( [H|T], Titles ) :-
	( title_line(H,Digits,Htitle) ->
		Titles = [Digits-Htitle|Ttitles]
		;
		Titles = Ttitles
	),
	index_lines_to_titles( T, Ttitles ).

title_line( Line, [Fd,Sd], Title ) :-
	% atom_codes( Line, LineCs ),
	Line = [Fd,Sd,0'-|RemLine],
	digit( Fd ),
	digit( Sd ),
	strip_time( RemLine, Title ).
	% atom_codes( Title, TitleCs ).

digit( Digit ) :-
	0'0 =< Digit,
	Digit =< 0'9.

strip_time( Line, Title ) :-
	reverse( Line, RevLine ),
	( (
		break_list_on( RevLine, 0'-, Left, Right ),
		is_time_part(Left)
		) ->
		reverse( Right, Title )
		;
		( append( Title, ".", Line ) ->
			true
			;
			Title = Line
		)
	).

is_time_part( Spec ) :-
	break_list_on( Spec, 0':, Left, [First|_Right] ),
	!,
	last( Left, Last ),
	digit( First ),
	digit( Last ).

atoms_codes( [], [] ).
atoms_codes( [H|T], [HCs|TCs] ) :-
	atom_codes( H, HCs ),
	atoms_codes( T,TCs ).

fnamecs_has_extension( Fname, Ext ) :-
	reverse( Fname, RevFname ),
	break_list_on( RevFname, 0'., Left, _Right ),
	!,
	reverse( Left, Ext ).
