:- ensure_loaded( library(defines) ).		% /2.
:- defines( swi(_), delete_unif/3 ).

:- ensure_loaded( library(lists) ).		% delete/3.

delete_unif( A, B, C ) :-
	delete( A, B, C ).
