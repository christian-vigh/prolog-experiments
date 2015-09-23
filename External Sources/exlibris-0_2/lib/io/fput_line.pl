% put in autonomous file, 18ht june 1999. Nicos.

:- ensure_loaded( io_code ).

fput_line( Stream, [] ) :-
	io_code( nl, NLcode ),
	put( Stream, NLcode ),
	!.
fput_line( Stream, [C|Cs] ) :-
	put( Stream, C ),
	fput_line( Stream, Cs ).
