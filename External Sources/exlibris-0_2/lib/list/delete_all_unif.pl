:- ensure_loaded( library(requires) ).		% /1.
:- requires( delete_unif/3 ).

delete_all_unif( [], List, List ).
delete_all_unif( [H|T], InList, OutList ) :-
	delete_unif( InList, H, NxList ),
	delete_all_unif( T, NxList, OutList ).
