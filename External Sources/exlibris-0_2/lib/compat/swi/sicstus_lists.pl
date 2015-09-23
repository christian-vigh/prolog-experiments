:- ensure_loaded( library(defines) ).		%.
:- for( swi(_) ).

remove_duplicates( [], [] ).
remove_duplicates( [H|T], [H|More] ) :-
	delete( T, H, Rest ),
	remove_duplicates( Rest, More ).

nth( A, B, C ) :-
	nth1( A, B, C ).

nth( 1, [H|T], H, T ).
nth( N, [H|T], El, [H|R] ) :-
	nth( PrvN, T, El, R ),
	N is PrvN + 1.

max_list( [H|T], Max ) :-
	max_list( T, H, Max ).

max_list( [H|T], Acc, Max ) :-
	( H > Acc -> 
		NxAcc is H
		;
		NxAcc is Acc 
	),
	maxlist( T, NxAcc, Max ).
max_list( [], Max, Max ).
