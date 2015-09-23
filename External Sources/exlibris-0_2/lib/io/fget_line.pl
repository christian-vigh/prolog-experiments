% put in autonomous file, 18ht june 1999. Nicos.
:- ensure_loaded( library(requires) ).
:- requires( at_end_of_line/1 ).

fget_line( Stream, [] ) :-
	at_end_of_line( Stream ),
	get0( Stream, _C),
	!.
fget_line( Stream, [C|Cs] ) :-
	get0( Stream, C ),
	fget_line( Stream, Cs ).
