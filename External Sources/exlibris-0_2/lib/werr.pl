werr( [] ) :-
	!,
	nl( user_error ).
werr( [H|T] ) :-
	!,
	werr( H ),
	werr( T ). 
werr( Atom ) :-
	write( user_error, Atom ).
